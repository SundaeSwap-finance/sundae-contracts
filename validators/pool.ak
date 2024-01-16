use aiken/builtin
use aiken/bytearray
use aiken/dict
use aiken/hash
use aiken/interval
use aiken/list
use aiken/math
use aiken/option
use aiken/transaction.{
  Datum, InlineDatum, Input, Mint, NoDatum, Output, OutputReference,
  ScriptContext, Spend, Transaction, TransactionId,
}
use aiken/transaction/credential.{
  Address, ScriptCredential, VerificationKeyCredential,
}
use aiken/transaction/value.{MintedValue, PolicyId, Value, ada_policy_id}
use calculation/process.{pool_input_to_state, process_orders}
use calculation/shared.{PoolState} as calc_shared
use shared.{AssetClass, Ident, spent_output, pool_nft_name, pool_lp_name, has_exact_token_count, count_orders}
use sundae/multisig
use tests/examples/ex_settings.{mk_valid_settings_input}
use tests/examples/ex_shared.{wallet_address, script_address, mk_output_reference, mk_tx_hash}
use types/order.{Destination, OrderDatum, Swap, Deposit}
use types/pool.{
  CreatePool, MintLP, PoolDatum, PoolMintRedeemer, PoolRedeemer, PoolScoop, WithdrawFees
}
use types/settings.{SettingsDatum, find_settings_datum}

fn minted_correct_pool_tokens(
  pool_policy_id: PolicyId,
  mint: MintedValue,
  datum: PoolDatum,
  outcome: PoolState,
) -> Bool {
  let minted_tokens =
    value.tokens(value.from_minted_value(mint), pool_policy_id)

  if datum.circulating_lp == outcome.quantity_lp.3rd {
    dict.is_empty(minted_tokens)
  } else {
    dict.to_list(minted_tokens) == [
      (
        pool_lp_name(datum.identifier),
        outcome.quantity_lp.3rd - datum.circulating_lp,
      ),
    ]
  }
}

fn has_expected_pool_value(
  pool_script_hash: PolicyId,
  identifier: Ident,
  output_value: Value,
  outcome: PoolState,
  final_protocol_fees: Int,
) -> Bool {
  // OPTIMIZATION: check that lovelace and assets match, rather than constructing a value
  let PoolState { quantity_a, quantity_b, .. } = outcome
  let (quantity_a_policy_id, quantity_a_name, quantity_a_amt) = quantity_a
  let (quantity_b_policy_id, quantity_b_name, quantity_b_amt) = quantity_b
  if quantity_a_policy_id == ada_policy_id {
    expect has_exact_token_count(output_value, 3)
    expect value.lovelace_of(output_value) == final_protocol_fees + quantity_a_amt
    expect
      value.quantity_of(output_value, quantity_b_policy_id, quantity_b_name) == quantity_b_amt
    expect
      value.quantity_of(
        output_value,
        pool_script_hash,
        pool_nft_name(identifier),
      ) == 1
    True
  } else {
    expect has_exact_token_count(output_value, 4)
    expect value.lovelace_of(output_value) == final_protocol_fees
    expect
      value.quantity_of(output_value, quantity_a_policy_id, quantity_a_name) == quantity_a_amt
    expect
      value.quantity_of(output_value, quantity_b_policy_id, quantity_b_name) == quantity_b_amt
    expect
      value.quantity_of(
        output_value,
        pool_script_hash,
        pool_nft_name(identifier),
      ) == 1
    True
  }
}

fn compare_asset_class(a: AssetClass, b: AssetClass) {
  let (aPolicy, aToken) = a
  let (bPolicy, bToken) = b
  when bytearray.compare(aPolicy, bPolicy) is {
    Less -> Less
    Equal -> bytearray.compare(aToken, bToken)
    Greater -> Greater
  }
}

fn int_to_ident(n: Int) -> Ident {
  expect n < 256
  bytearray.push(#"", n)
}

validator(settings_policy_id: PolicyId) {
  fn spend(datum: PoolDatum, redeemer: PoolRedeemer, ctx: ScriptContext) {
    let transaction = ctx.transaction
    let Transaction {
      inputs,
      outputs,
      reference_inputs,
      mint,
      datums,
      extra_signatories,
      validity_range,
      ..
    } = transaction

    // Find the input being spent
    let pool_input = spent_output(ctx)

    expect ScriptCredential(pool_script_hash) = pool_input.address.payment_credential

    // Find the pool output
    expect Some(pool_output) = list.head(outputs)
    expect pool_output.address == pool_input.address
    expect InlineDatum(output_datum) = pool_output.datum
    expect output_datum: PoolDatum = output_datum

    let PoolDatum {
      identifier: actual_identifier,
      circulating_lp: actual_circulating_lp,
      protocol_fees: actual_protocol_fees,
      ..
    } = output_datum

    // Find the settings datum
    let settings_datum = find_settings_datum(reference_inputs, settings_policy_id) 

    when redeemer is {
      PoolScoop{ signatory_index, scooper_index, input_order, .. } -> {
        // Deconstruct the settings datum with the fields we need for a scoop
        let SettingsDatum { authorized_scoopers, base_fee, simple_fee, strategy_fee, .. } =
          settings_datum

        // Do a simple scan over the orders to count up the number of orders we'll be processing
        // This is unavoidable, because it's part of making sure that the provided redeemer set isn't
        // excluding orders
        let real_order_count = count_orders(inputs)
        // Calculate the portion of the fee that each order will be
        // entitled to pay;
        // Because the division is rounded down, we add real_order_count and subtact 1
        // to ensure that we take the ceiling instead, and round in the protocols favor.
        let amortized_base_fee = (base_fee + real_order_count - 1) / real_order_count
        // Make sure it's not negative, for example if base_fee was negative
        expect amortized_base_fee >= 0
        // Construct the initial pool state from the datum and the locked values
        let (initial_state, fees_per_10_thousand, initial_protocol_fees) = pool_input_to_state(pool_script_hash, datum, pool_input, validity_range.lower_bound)
        // Process the orders in order, and decide the final pool state we should see
        // OPTIMIZATION: pass in the list of outputs, to pop off and compare in parallel
        let (outcome, simple_count, strategy_count) =
          process_orders(
            actual_identifier,
            validity_range,
            datums,
            initial_state,
            input_order,
            fees_per_10_thousand,
            amortized_base_fee,
            simple_fee,
            strategy_fee,
            0,
            inputs,
            inputs,
            list.drop(outputs, 1),
            0,
            0,
            0,
          )
        let PoolState {
          quantity_lp: expected_circulating_lp,
          ..
        } = outcome

        // We need to make sure that the number of orders matches the amount that we processed
        // so the scooper doesn't "under-report" the orders and steal the funds on the order
        expect simple_count + strategy_count == real_order_count

        // We calculate the expected total collected protocol fee
        // We multiply amortized_base_fee, which everyone paid, by the number of orders
        // and then the respective fees for each simple order and strategy order
        let expected_fees_collected =
          amortized_base_fee * real_order_count +
          simple_count * simple_fee +
          strategy_count * strategy_fee

        // Make sure we actually increased the protocol fee by exactly this amount
        expect actual_protocol_fees == initial_protocol_fees + expected_fees_collected

        // The pool should have all of the scooper fees, and the quantity of each token of the outcome
        // Note that initializing the state with `-transaction.fee` means this gets subracted out of the protocol fees
        // Calculate all the neccesary and sufficient conditions
        expect minted_correct_pool_tokens(pool_script_hash, mint, datum, outcome)

        // Slightly awkward construction here because we need to check that the scooper is authorized
        expect when authorized_scoopers is {
          Some(authorized_scoopers) -> {
            // OPTIMIZATION: skip 10 entries at a time
            // OPTIMIZATION: assume scooper is first extra_signatory? have to assume there will only ever be one extra_signatory
            expect Some(scooper_sig) = list.at(extra_signatories, signatory_index)
            expect Some(scooper) = list.at(authorized_scoopers, scooper_index)
            // must be an authorized scooper
            scooper_sig == scooper
          }
          _ -> True
        }
        // the market must have opened
        expect interval.is_entirely_after(validity_range, datum.market_open)

        // expect funds_moved_correctly
        expect output_datum.circulating_lp == outcome.quantity_lp.3rd
        expect
          has_expected_pool_value(
            pool_script_hash,
            output_datum.identifier,
            pool_output.value,
            outcome,
            actual_protocol_fees,
          )
        // expect pool_output_correct
        // Must update the circulating LP if tokens are minted
        expect actual_circulating_lp == expected_circulating_lp.3rd
        // Must not update the immutable fields
        expect datum.identifier == output_datum.identifier
        expect datum.assets == output_datum.assets
        expect datum.fees_per_10_thousand == output_datum.fees_per_10_thousand
        expect datum.market_open == output_datum.market_open
        expect
          pool_input.address.stake_credential == pool_output.address.stake_credential
        True
      }
      WithdrawFees { amount, treasury_output } -> {
        let PoolDatum {
          protocol_fees: initial_protocol_fees,
          ..
        } = datum
        // Can't withdraw more than we've collected
        expect amount <= initial_protocol_fees

        // Must update the protocol fees and nothing else
        let expected_datum = PoolDatum {
          identifier: datum.identifier,
          assets: datum.assets,
          circulating_lp: datum.circulating_lp,
          fees_per_10_thousand: datum.fees_per_10_thousand,
          fee_finalized: datum.fee_finalized,
          market_open: datum.market_open,
          protocol_fees: initial_protocol_fees - amount,
        }
        expect output_datum == expected_datum

        // Must withdraw only what we claim to withdraw
        let expected_output_value = value.merge(pool_input.value, value.from_lovelace(-amount))
        expect pool_output.value == expected_output_value

        // Must be signed by the treasury admin multisig
        expect multisig.satisfied(
          settings_datum.treasury_admin,
          extra_signatories,
          validity_range,
        )

        // Some portion of the treasury can be withdrawn for administrative purposes
        // such as paying scooper fees, etc.
        let allowance = amount * settings_datum.treasury_allowance.1st / settings_datum.treasury_allowance.2nd
        let to_treasury = amount - allowance
        expect Some(treasury_output) = list.at(outputs, treasury_output)
        // The remainder must be sent to the treasury address with the Void datum
        expect treasury_output.address == settings_datum.treasury_address
        expect treasury_output.datum == InlineDatum(Void)
        expect value.lovelace_of(treasury_output.value) >= to_treasury
        True
      }
    }
  }

  fn mint(r: PoolMintRedeemer, ctx: ScriptContext) {
    when r is {
      CreatePool(assets, pool_output_ix, metadata_output_ix) -> {
        expect Mint(own_policy_id) = ctx.purpose

        // The assets on the pool must be sorted
        let (asset_a, asset_b) = assets
        let coin_pair_ordering_is_canonical =
          compare_asset_class(asset_a, asset_b) == Less

        expect Some(first_input) = list.at(ctx.transaction.inputs, 0)
        let first_input_index =
          int_to_ident(first_input.output_reference.output_index)

        // Calculate the identifier by hashing the txRef of the first input
        // this makes the pool unique
        // With CIP-68 identifiers, we have 28 bytes of space; ideally we'd use blake2b-224,
        // but that won't be on-chain until plutus v3;
        // So, we use blake2b-256 and drop 4 bytes; this is still 112 bits of security against collisions
        let new_pool_id =
          first_input.output_reference.transaction_id.hash
            |> bytearray.concat(#"23") // '#' character
            |> bytearray.concat(first_input_index)
            |> hash.blake2b_256
            |> bytearray.drop(4) 

        // Derive the CIP-68 token identifiers
        let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) = shared.pool_token_names(new_pool_id)

        let reference_inputs = ctx.transaction.reference_inputs

        let settings_datum = find_settings_datum(reference_inputs, settings_policy_id)

        expect Some(pool_output) =
          list.at(ctx.transaction.outputs, pool_output_ix)

        let coin_a_amt =
          value.quantity_of(pool_output.value, asset_a.1st, asset_a.2nd)
        let coin_b_amt =
          value.quantity_of(pool_output.value, asset_b.1st, asset_b.2nd)

        // Only ada has a null policy id. If coin A is ada, subtract the 2 ada
        // rider from the output to get the true amount in the pool.
        let coin_a_amt_sans_rider =
          if bytearray.is_empty(asset_a.1st) {
            coin_a_amt - 2_000_000
          } else {
            coin_a_amt
          }

        expect Some(initial_lq) = math.sqrt(coin_a_amt_sans_rider * coin_b_amt)

        // TODO: can we use own_script_hash here?
        let new_pool_ref = (own_policy_id, new_pool_ref_token)
        let new_pool_nft = (own_policy_id, new_pool_nft_token)
        let new_pool_lq = (own_policy_id, new_pool_lp_token)

        let expected_mint =
              shared.to_value((new_pool_ref.1st, new_pool_ref.2nd, 1))
            |> value.merge(shared.to_value((new_pool_nft.1st, new_pool_nft.2nd, 1)))
            |> value.merge(shared.to_value((new_pool_lq.1st, new_pool_lq.2nd, initial_lq)))
        let mint_is_correct =
          value.from_minted_value(ctx.transaction.mint) == expected_mint

        // TODO: can we use own_script_hash here?
        let funds_spent_to_pool =
          coin_a_amt_sans_rider >= 1 && coin_b_amt >= 1 && list.length(
            value.flatten(pool_output.value),
          ) <= 3 && value.quantity_of(
            pool_output.value,
            own_policy_id,
            new_pool_nft_token,
          ) == 1
        
        // Make sure we send the pool metadata token to the metadata admin
        expect Some(metadata_output) =
          list.at(ctx.transaction.outputs, metadata_output_ix)
        expect metadata_output.address == settings_datum.metadata_admin
        // TODO: can we use own_script_hash here?
        expect value.quantity_of(metadata_output.value, own_policy_id, new_pool_ref_token) == 1

        expect InlineDatum(d) = pool_output.datum
        expect pool_output_datum: PoolDatum = d

        let pool_output_datum_correct =
          pool_output_datum.identifier == new_pool_id && pool_output_datum.assets == (
            asset_a,
            asset_b,
          ) && pool_output_datum.circulating_lp == initial_lq
          && pool_output_datum.market_open <= pool_output_datum.fee_finalized

        coin_pair_ordering_is_canonical && mint_is_correct && funds_spent_to_pool && pool_output_datum_correct
      }
      // It is sufficient to check that either a pool nft is being spent or the
      // settings nft is being spent, *provided that* the settings and pool
      // contracts check that they are minting the correct token
      MintLP(pool_ident) -> {
        expect Mint(own_policy_id) = ctx.purpose
        let pool_nft_name = shared.pool_nft_name(pool_ident)
        let allows_to_spend =
          fn(v) {
            value.quantity_of(v, own_policy_id, pool_nft_name) == 1
          }
        list.any(
          ctx.transaction.inputs,
          fn(input) { allows_to_spend(input.output.value) },
        )
      }
    }
  }
}


type ScoopTestOptions {
  edit_escrow_1_value: Option<Value>,
  edit_escrow_2_value: Option<Value>,
  edit_escrow_destination: Option<Address>,
  edit_fee: Option<Value>,
  edit_swap_fees: Option<(Int,Int)>,
  edit_pool_output_value: Option<Value>,
  edit_settings_datum: Option<Datum>,
}

fn default_scoop_test_options() -> ScoopTestOptions {
  ScoopTestOptions {
    edit_escrow_1_value: None,
    edit_escrow_2_value: None,
    edit_escrow_destination: None,
    edit_fee: None,
    edit_swap_fees: None,
    edit_pool_output_value: None,
    edit_settings_datum: None,
  }
}

test ok_scoop_swap_swap() {
  let options = default_scoop_test_options()
  scoop(options)
}

test ok_scoop_swap_deposit() {
  let options = default_scoop_test_options()
  scoop_swap_deposit(options)
}

test scoop_bad_destination() fail {
  let burn_addr =
    wallet_address(#"12000000000000000000000000000000000000000000000000000000")
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_escrow_destination: Some(burn_addr),
    }
  scoop(options)
}

test scoop_payouts_swapped() fail {
  let dummy_policy_id =
    #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_escrow_1_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_702_095),
      ),
      edit_escrow_2_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_896_088),
      ),
    }
  scoop(options)
}

test pool_validator_ignores_fee() {
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_fee: Some(value.from_lovelace(1_000_000_000)),
    }
  scoop(options)
}

test scoop_high_swap_fees() {
  let hash_of_pool_script =
    #"00000000000000000000000000000000000000000000000000000000"
  let pool_id = #"00000000000000000000000000000000000000000000000000000000"
  let pool_nft_name = shared.pool_nft_name(pool_id)
  let dummy_policy_id =
    #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let swap_fee = 100
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_swap_fees: Some((swap_fee, swap_fee)),
      edit_escrow_1_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_802_950),
      ),
      edit_escrow_2_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_611_678),
      ),
      edit_pool_output_value: Some(
        value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
          |> value.add(
               dummy_policy_id,
               dummy_asset_name,
               1_000_000_000 - ( 9_802_950 + 9_611_678 ),
             )
          |> value.add(hash_of_pool_script, pool_nft_name, 1),
      ),
    }
  scoop(options)
}

test output_missing_nft() fail {
  let dummy_policy_id =
    #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_pool_output_value: Some(
        value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
          |> value.add(
               dummy_policy_id,
               dummy_asset_name,
               1_000_000_000 - ( 9_896_088 + 9_702_095 ),
             ),
      ),
    }
  scoop(options)
}

test scooper_not_in_settings() fail {
  let somebody = #"11111111111111111111111111111111111111111111111111111111"
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_settings_datum: Some(
        InlineDatum(
          SettingsDatum {
            settings_admin: multisig.AnyOf([]),
            metadata_admin: Address(
              VerificationKeyCredential(
                #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
              ),
              None,
            ),
            treasury_admin: multisig.AnyOf([]),
            treasury_address: Address(
              VerificationKeyCredential(
                #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
              ),
              None,
            ),
            treasury_allowance: (1, 10),
            authorized_scoopers: Some([somebody]),
            authorized_staking_keys: [],
            base_fee: 0,
            simple_fee: 2_500_000,
            strategy_fee: 5_000_000,
            extensions: Void,
          },
        ),
      ),
    }
  scoop(options)
}

fn scoop(options: ScoopTestOptions) {
  let settings_policy_id =
    #"00000000000000000000000000000000000000000000000000000000"
  let dummy_policy_id =
    #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let scooper = #"00000000000000000000000000000000000000000000000000000000"
  let hash_of_pool_script =
    #"00000000000000000000000000000000000000000000000000000000"
  let hash_of_escrow_script =
    #"00000000000000000000000000000000000000000000000000000000"
  let user_addr =
    wallet_address(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513")
  let owner =
    multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    )
  let pool_id = #"00000000000000000000000000000000000000000000000000000000"
  let pool_datum =
    PoolDatum {
      identifier: pool_id,
      assets: ((#"", #""), (dummy_policy_id, dummy_asset_name)),
      circulating_lp: 1_000_000_000,
      fees_per_10_thousand: option.or_else(options.edit_swap_fees, (5,5)),
      market_open: 0,
      fee_finalized: 100,
      protocol_fees: 2_000_000,
    }
  let pool_out_datum =
    PoolDatum {
      identifier: pool_id,
      assets: ((#"", #""), (dummy_policy_id, dummy_asset_name)),
      circulating_lp: 1_000_000_000,
      fees_per_10_thousand: option.or_else(options.edit_swap_fees, (5,5)),
      market_open: 0,
      fee_finalized: 0,
      protocol_fees: 7_000_000,
    }
  let pool_nft_name = shared.pool_nft_name(pool_id)
  let pool_address = script_address(hash_of_pool_script)
  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: value.from_lovelace(1_000_000_000 + 2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 1_000_000_000)
          |> value.add(hash_of_pool_script, pool_nft_name, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }
  let dest = Destination { address: user_addr, datum: NoDatum }
  let swap =
    Swap((#"", #"", 10_000_000), (dummy_policy_id, dummy_asset_name, 0))
  let escrow_datum =
    OrderDatum {
      pool_ident: None,
      owner,
      max_protocol_fee: 2_500_000,
      destination: dest,
      details: swap,
      extension: builtin.i_data(0),
    }
  let escrow_address = script_address(hash_of_escrow_script)
  let escrow1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: escrow_address,
        value: value.from_lovelace(4_500_000 + 10_000_000),
        datum: InlineDatum(escrow_datum),
        reference_script: None,
      },
    }
  let escrow2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: escrow_address,
        value: value.from_lovelace(4_500_000 + 10_000_000),
        datum: InlineDatum(escrow_datum),
        reference_script: None,
      },
    }
  let settings_input = {
    let Input { output_reference, output } =
      mk_valid_settings_input([scooper], 1)
    let updated_output =
      Output {
        ..output,
        datum: option.or_else(options.edit_settings_datum, output.datum),
      }
    Input { output_reference, output: updated_output }
  }
  let escrow1_out =
    Output {
      address: option.or_else(options.edit_escrow_destination, user_addr),
      value: option.or_else(
        options.edit_escrow_1_value,
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_896_088),
      ),
      datum: NoDatum,
      reference_script: None,
    }
  let escrow2_out =
    Output {
      address: option.or_else(options.edit_escrow_destination, user_addr),
      value: option.or_else(
        options.edit_escrow_2_value,
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_702_095),
      ),
      datum: NoDatum,
      reference_script: None,
    }
  let pool_output =
    Output {
      address: pool_address,
      value: option.or_else(
        options.edit_pool_output_value,
        value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
          |> value.add(
               dummy_policy_id,
               dummy_asset_name,
               1_000_000_000 - ( 9_896_088 + 9_702_095 ),
             )
          |> value.add(hash_of_pool_script, pool_nft_name, 1),
      ),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }
  let ctx =
    ScriptContext {
      transaction: Transaction {
        inputs: [pool_input, escrow1_in, escrow2_in],
        reference_inputs: [settings_input],
        outputs: [pool_output, escrow1_out, escrow2_out],
        fee: option.or_else(options.edit_fee, value.from_lovelace(1_000_000)),
        mint: value.to_minted_value(value.from_lovelace(0)),
        certificates: [],
        withdrawals: dict.new(),
        validity_range: interval.between(1, 2),
        extra_signatories: [scooper],
        redeemers: dict.new(),
        datums: dict.new(),
        id: mk_tx_hash(1),
      },
      purpose: Spend(pool_input.output_reference),
    }
  let pool_redeemer = PoolScoop(0, 0, 0, [(1, None), (2, None)])
  let result = spend(settings_policy_id, pool_datum, pool_redeemer, ctx)
  result
}

fn scoop_swap_deposit(options: ScoopTestOptions) {
  let settings_policy_id = #"00000000000000000000000000000000000000000000000000000000"
  let dummy_policy_id = #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let scooper = #"00000000000000000000000000000000000000000000000000000000"
  let hash_of_pool_script = #"00000000000000000000000000000000000000000000000000000000"
  let hash_of_escrow_script = #"00000000000000000000000000000000000000000000000000000000"
  let user_addr = wallet_address(
    #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"
  )
  let owner = multisig.Signature(
    #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
  )
  let pool_id = #"000000000000000000000000000000000000000000000000000000000000"
  let pool_datum = PoolDatum {
    identifier: pool_id,
    assets: (
      (#"", #""),
      (dummy_policy_id, dummy_asset_name),
    ),
    circulating_lp: 1_000_000_000,
    fees_per_10_thousand: option.or_else(options.edit_swap_fees, (5,5)),
    market_open: 0,
    fee_finalized: 0,
    protocol_fees: 2_000_000,
  }
  let pool_out_datum = PoolDatum {
    identifier: pool_id,
    assets: (
      (#"", #""),
      (dummy_policy_id, dummy_asset_name),
    ),
    circulating_lp: 1_009_900_990,
    fees_per_10_thousand: option.or_else(options.edit_swap_fees, (5,5)),
    market_open: 0,
    fee_finalized: 0,
    protocol_fees: 7_000_000,
  }
  let pool_nft_name = shared.pool_nft_name(pool_id)
  let pool_address = script_address(hash_of_pool_script)
  let pool_input = Input {
    output_reference: mk_output_reference(0),
    output: Output {
      address: pool_address,
      value: value.from_lovelace(1_000_000_000 + 2_000_000)
        |> value.add(dummy_policy_id, dummy_asset_name, 1_000_000_000)
        |> value.add(hash_of_pool_script, pool_nft_name, 1),
      datum: InlineDatum(pool_datum),
      reference_script: None,
    },
  }
  let dest = Destination { address: user_addr, datum: NoDatum }
  let swap =
    Swap(
      (#"", #"", 10_000_000),
      (dummy_policy_id, dummy_asset_name, 0),
    )
  let deposit =
    Deposit((
      (#"", #"", 10_000_000),
      (dummy_policy_id, dummy_asset_name, 10_000_000),
    ))
  let escrow_datum_1 = OrderDatum {
    pool_ident: None,
    owner: owner,
    max_protocol_fee: 2_500_000,
    destination: dest,
    details: swap,
    extension: builtin.i_data(0),
  }
  let escrow_datum_2 = OrderDatum {
    pool_ident: None,
    owner: owner,
    max_protocol_fee: 2_500_000,
    destination: dest,
    details: deposit,
    extension: builtin.i_data(0),
  }
  let escrow_address = script_address(hash_of_escrow_script)
  let escrow1_in = Input {
    output_reference: mk_output_reference(2),
    output: Output {
      address: escrow_address,
      value: value.from_lovelace(4_500_000 + 10_000_000),
      datum: InlineDatum(escrow_datum_1),
      reference_script: None,
    },
  }
  let escrow2_in = Input {
    output_reference: mk_output_reference(3),
    output: Output {
      address: escrow_address,
      value: value.from_lovelace(4_500_000 + 10_000_000)
        |> value.add(dummy_policy_id, dummy_asset_name, 10_000_000),
      datum: InlineDatum(escrow_datum_2),
      reference_script: None,
    },
  }
  let settings_input = {
    let Input {output_reference, output} = mk_valid_settings_input([scooper], 1)
    let updated_output = Output {
      ..output,
      datum: option.or_else(options.edit_settings_datum, output.datum)
    }
    Input {
      output_reference: output_reference,
      output: updated_output,
    }
  }
  let escrow1_out = Output {
    address: option.or_else(options.edit_escrow_destination, user_addr),
    value: option.or_else(options.edit_escrow_1_value,
      value.from_lovelace(2_000_000)
        |> value.add(dummy_policy_id, dummy_asset_name, 9_896_088)),
    datum: NoDatum,
    reference_script: None,
  }
  let escrow2_out = Output {
    address: option.or_else(options.edit_escrow_destination, user_addr),
    value: value.from_lovelace(2_000_000)
      |> value.add(hash_of_pool_script, pool_lp_name(pool_id), 9_900_990)
      |> value.add(dummy_policy_id, dummy_asset_name, 196_990),
    datum: NoDatum,
    reference_script: None,
  }
  let pool_output = Output {
    address: pool_address,
    value: option.or_else(options.edit_pool_output_value,
      value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
        |> value.add(dummy_policy_id, dummy_asset_name, 1_000_000_000 - 9_896_088 + 10_000_000 - 196_990)
        |> value.add(hash_of_pool_script, pool_nft_name, 1)),
    datum: InlineDatum(pool_out_datum),
    reference_script: None,
  }

  let ctx = ScriptContext {
    transaction: Transaction {
      inputs: [pool_input, escrow1_in, escrow2_in],
      reference_inputs: [settings_input],
      outputs: [pool_output, escrow1_out, escrow2_out],
      fee: option.or_else(options.edit_fee, value.from_lovelace(1_000_000)),
      mint: value.to_minted_value(
        value.from_lovelace(0)
          |> value.add(hash_of_pool_script, pool_lp_name(pool_id), 9_900_990)
      ),
      certificates: [],
      withdrawals: dict.new(),
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      redeemers: dict.new(),
      datums: dict.new(),
      id: mk_tx_hash(1),
    },
    purpose: Spend(pool_input.output_reference),
  }
  let pool_redeemer = PoolScoop(0, 0, 0, [(1, None),(2, None)])
  let result = spend(settings_policy_id, pool_datum, pool_redeemer, ctx)
  result
}

test mint_test() {
  let settings_policy_id = #"00000000000000000000000000000000000000000000000000000000"
  let hash_of_pool_script = #"00000000000000000000000000000000000000000000000000000000"
  let pool_address = script_address(hash_of_pool_script)
  let rberry_policy_id = #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let rberry_token_name = #"524245525259"
  let user_address =
    wallet_address(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513")
  let settings_input = mk_valid_settings_input([], 1)
  let funds_input = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(0),
      output_index: 0
    },
    output: Output {
      address: user_address,
      value: value.from_lovelace(10_000_000_000)
        |> value.add(rberry_policy_id, rberry_token_name, 1_000_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
  let pool_id =
    funds_input.output_reference.transaction_id.hash
      |> bytearray.concat(#"23") // '#' character
      |>
      bytearray.concat(int_to_ident(funds_input.output_reference.output_index))
      |> hash.blake2b_256
      |> bytearray.drop(4)
  let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) = shared.pool_token_names(pool_id)
  let pool_output = Output {
    address: pool_address,
    value: value.from_lovelace(1_002_000_000)
      |> value.add(rberry_policy_id, rberry_token_name, 1_000_000_000)
      |> value.add(hash_of_pool_script, new_pool_nft_token, 1),
    datum: InlineDatum(PoolDatum {
      identifier: pool_id,
      assets: ((#"", #""), (rberry_policy_id, rberry_token_name)),
      circulating_lp: 1_000_000_000,
      fees_per_10_thousand: (5, 5),
      market_open: 0,
      fee_finalized: 0,
      protocol_fees: 2_000_000,
    }),
    reference_script: None,
  }
  let lp_output = Output {
    address: user_address,
    value: value.from_lovelace(2_000_000)
      |> value.add(hash_of_pool_script, pool_lp_name(pool_id), 1_000_000_000),
    datum: NoDatum,
    reference_script: None,
  }
  let ref_output = Output {
    address: user_address,
    value: value.from_lovelace(2_000_000)
      |> value.add(hash_of_pool_script, new_pool_ref_token, 1),
    datum: NoDatum,
    reference_script: None,
  }
  let poolMintRedeemer = CreatePool {
    assets: ((#"", #""), (rberry_policy_id, rberry_token_name)),
    pool_output: 0,
    metadata_output: 2,
  }
  let ctx = ScriptContext {
    transaction: Transaction {
      inputs: [funds_input],
      reference_inputs: [settings_input],
      outputs: [pool_output, lp_output, ref_output],
      fee: value.from_lovelace(1_000_000),
      mint: value.to_minted_value(
        value.from_lovelace(0)
          |> value.add(hash_of_pool_script, new_pool_lp_token, 1_000_000_000)
          |> value.add(hash_of_pool_script, new_pool_nft_token, 1)
          |> value.add(hash_of_pool_script, new_pool_ref_token, 1)
      ),
      certificates: [],
      withdrawals: dict.new(),
      validity_range: interval.between(1, 2),
      extra_signatories: [],
      redeemers: dict.new(),
      datums: dict.new(),
      id: mk_tx_hash(1),
    },
    purpose: Mint(hash_of_pool_script),
  }
  let result = mint(settings_policy_id, poolMintRedeemer, ctx)
  result
}
