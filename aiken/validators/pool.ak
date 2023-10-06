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
use shared.{AssetClass, Ident, is_after, spent_output}
use sundae/multisig
use types/order.{Destination, OrderDatum, Swap, Deposit}
use types/pool.{
  CreatePool, MintLP, PoolDatum, PoolMintRedeemer, PoolRedeemer, lp_token_name,
  pool_token_name,
}
use types/settings.{SettingsDatum}

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
        lp_token_name(datum.identifier),
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
) -> Bool {
  // OPTIMIZATION: check that lovelace and assets match, rather than constructing a value
  let PoolState { quantity_a, quantity_b, rewards, .. } = outcome
  let (quantity_a_policy_id, quantity_a_name, quantity_a_amt) = quantity_a
  let (quantity_b_policy_id, quantity_b_name, quantity_b_amt) = quantity_b
  if quantity_a_policy_id == ada_policy_id {
    expect 3 == list.length(value.flatten(output_value))
    expect value.lovelace_of(output_value) == rewards + quantity_a_amt
    expect
      value.quantity_of(output_value, quantity_b_policy_id, quantity_b_name) == quantity_b_amt
    expect
      value.quantity_of(
        output_value,
        pool_script_hash,
        pool_token_name(identifier),
      ) == 1
    True
  } else {
    expect 4 == list.length(value.flatten(output_value))
    expect value.lovelace_of(output_value) == rewards
    expect
      value.quantity_of(output_value, quantity_a_policy_id, quantity_a_name) == quantity_a_amt
    expect
      value.quantity_of(output_value, quantity_b_policy_id, quantity_b_name) == quantity_b_amt
    expect
      value.quantity_of(
        output_value,
        pool_script_hash,
        pool_token_name(identifier),
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
    let PoolRedeemer { signatory_index, scooper_index, input_order } = redeemer
    // Grab the fields we'll need to determine whether we can execute this tx
    // Find the input being spent
    let pool_input = spent_output(ctx)
    // Assume the first output is the pool output
    expect Some(pool_output) = list.head(outputs)
    expect pool_output.address == pool_input.address
    expect InlineDatum(output_datum) = pool_output.datum
    expect output_datum: PoolDatum = output_datum
    let PoolDatum {
      identifier: _actual_identifier,
      circulating_lp: actual_circulating_lp,
      rewards: actual_rewards,
      ..
    } = output_datum

    // Find the reference input with the settings token and it's datum
    let settings_output = builtin.head_list(reference_inputs).output
    let settings_value_list = dict.to_list(value.to_dict(settings_output.value))
    expect [_, (found_policy_id, settings_dict)] = settings_value_list
    expect found_policy_id == settings_policy_id
    expect dict.to_list(settings_dict) == [(shared.settings_nft_name, 1)]
    expect InlineDatum(settings_datum) = settings_output.datum
    expect settings_datum: SettingsDatum = settings_datum
    let SettingsDatum { pool_script_hash, authorized_scoopers, .. } =
      settings_datum

    // Construct the initial pool state from the datum and the locked values
    let initial_state = pool_input_to_state(pool_script_hash, datum, pool_input)

    // Process the orders in order, and decide the final pool state we should see
    // OPTIMIZATION: pass in the list of outputs, to pop off and compare in parallel
    let outcome =
      process_orders(
        datums,
        initial_state,
        input_order,
        inputs,
        list.drop(outputs, 1),
        0,
      )
    let PoolState {
      quantity_lp: expected_circulating_lp,
      rewards: expected_rewards,
      ..
    } = outcome

    // The pool should have all of the scooper fees, and the quantity of each token of the outcome
    // Note that initializing the state with `-transaction.fee` means this gets subracted out of the rewards
    // Calculate all the neccesary and sufficient conditions
    let minted_correct_lp_tokens =
      minted_correct_pool_tokens(pool_script_hash, mint, datum, outcome)

    // OPTIMIZATION: skip 10 entries at a time
    // OPTIMIZATION: assume scooper is first extra_signatory? have to assume there will only ever be one extra_signatory
    let licensed_scooper =
      list.at(extra_signatories, signatory_index) == list.at(
        authorized_scoopers,
        scooper_index,
      )
    let market_open = is_after(validity_range, datum.market_open)

    // expect pool_can_be_scooped
    // expect !list.is_empty(orders)
    // Must have at least one order
    expect market_open
    expect licensed_scooper
    // expect funds_moved_correctly
    expect minted_correct_lp_tokens
    expect output_datum.circulating_lp == outcome.quantity_lp.3rd
    expect
      has_expected_pool_value(
        pool_script_hash,
        output_datum.identifier,
        pool_output.value,
        outcome,
      )
    // expect pool_output_correct
    // Must update the circulating LP if tokens are minted
    expect actual_circulating_lp == expected_circulating_lp.3rd
    // Must update the rewards field as expected
    expect actual_rewards == expected_rewards
    // Must not update the immutable fields
    expect datum.identifier == output_datum.identifier
    expect datum.assets == output_datum.assets
    expect datum.fees_per_10_thousand == output_datum.fees_per_10_thousand
    expect datum.market_open == output_datum.market_open
    expect
      pool_input.address.stake_credential == pool_output.address.stake_credential
    True
  }

  fn mint(r: PoolMintRedeemer, ctx: ScriptContext) {
    when r is {
      CreatePool(assets, pool_output_ix) -> {
        let (asset_a, asset_b) = assets
        let coin_pair_ordering_is_canonical =
          compare_asset_class(asset_a, asset_b) == Less

        expect Some(first_input) = list.at(ctx.transaction.inputs, 0)
        let first_input_index =
          int_to_ident(first_input.output_reference.output_index)
        let new_pool_id =
          first_input.output_reference.transaction_id.hash
            |> bytearray.concat(#"23") // '#' character
            |> bytearray.concat(first_input_index)
            |> hash.blake2b_256
            |> bytearray.drop(1)
        let new_pool_p_token = shared.pool_nft_name(new_pool_id)
        let new_pool_l_token = shared.pool_lq_name(new_pool_id)

        let reference_inputs = ctx.transaction.reference_inputs

        let settings_output = builtin.head_list(reference_inputs).output
        expect InlineDatum(settings_datum) = settings_output.datum
        expect settings_datum: SettingsDatum = settings_datum

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

        let new_pool_nft = (settings_datum.pool_script_hash, new_pool_p_token)
        let new_pool_lq = (settings_datum.pool_script_hash, new_pool_l_token)

        let expected_mint =
          shared.to_value((new_pool_nft.1st, new_pool_nft.2nd, 1))
            |> value.merge(
                 shared.to_value((new_pool_lq.1st, new_pool_lq.2nd, initial_lq)),
               )
        let mint_is_correct =
          value.from_minted_value(ctx.transaction.mint) == expected_mint

        let funds_spent_to_pool =
          coin_a_amt_sans_rider >= 1 && coin_b_amt >= 1 && list.length(
            value.flatten(pool_output.value),
          ) <= 3 && value.quantity_of(
            pool_output.value,
            settings_datum.pool_script_hash,
            new_pool_p_token,
          ) == 1

        expect InlineDatum(d) = pool_output.datum
        expect pool_output_datum: PoolDatum = d

        let pool_output_datum_correct =
          pool_output_datum.identifier == new_pool_id && pool_output_datum.assets == (
            asset_a,
            asset_b,
          ) && pool_output_datum.circulating_lp == initial_lq && list.has(
            shared.legal_swap_fees(),
            pool_output_datum.fees_per_10_thousand,
          )

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
            value.quantity_of(v, own_policy_id, pool_nft_name) >= 1 || value.quantity_of(
              v,
              settings_policy_id,
              shared.settings_nft_name,
            ) >= 1
          }
        list.any(
          ctx.transaction.inputs,
          fn(input) { allows_to_spend(input.output.value) },
        )
      }
    }
  }
}

fn mk_tx_hash(n: Int) -> TransactionId {
  expect n < 256
  let h =
    bytearray.push(
      #"00000000000000000000000000000000000000000000000000000000000000",
      n,
    )
  TransactionId { hash: h }
}

fn mk_output_reference(n: Int) -> OutputReference {
  OutputReference { transaction_id: mk_tx_hash(0), output_index: n }
}

fn script_address(hash: ByteArray) -> Address {
  Address { payment_credential: ScriptCredential(hash), stake_credential: None }
}

fn wallet_address(hash: ByteArray) -> Address {
  Address(VerificationKeyCredential(hash), None)
}

fn mk_settings_input(
  psh: ByteArray,
  scoopers: List<ByteArray>,
  ix: Int,
) -> Input {
  let hash_of_settings_script =
    #"00000000000000000000000000000000000000000000000000000000"
  let settings_nft_name = "settings"
  let settings_address = script_address(hash_of_settings_script)
  let settings_datum =
    SettingsDatum {
      pool_script_hash: psh,
      settings_admin: multisig.AnyOf([]),
      rewards_admin: multisig.AnyOf([]),
      authorized_scoopers: scoopers,
      authorized_staking_keys: [],
    }
  let settings_input =
    Input {
      output_reference: mk_output_reference(ix),
      output: Output {
        address: settings_address,
        value: value.from_lovelace(2_000_000)
          |> value.add(hash_of_settings_script, settings_nft_name, 1),
        datum: InlineDatum(settings_datum),
        reference_script: None,
      },
    }
  settings_input
}

type ScoopTestOptions {
  edit_escrow_1_value: Option<Value>,
  edit_escrow_2_value: Option<Value>,
  edit_escrow_destination: Option<Address>,
  edit_fee: Option<Value>,
  edit_swap_fees: Option<Int>,
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
  let pool_id = #"000000000000000000000000000000000000000000000000000000000000"
  let pool_nft_name = shared.pool_nft_name(pool_id)
  let dummy_policy_id =
    #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let swap_fee = 100
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_swap_fees: Some(swap_fee),
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
  let hash_of_pool_script =
    #"00000000000000000000000000000000000000000000000000000000"
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_settings_datum: Some(
        InlineDatum(
          SettingsDatum {
            pool_script_hash: hash_of_pool_script,
            settings_admin: multisig.AnyOf([]),
            rewards_admin: multisig.AnyOf([]),
            authorized_scoopers: [somebody],
            authorized_staking_keys: [],
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
  let pool_id = #"000000000000000000000000000000000000000000000000000000000000"
  let pool_datum =
    PoolDatum {
      identifier: pool_id,
      assets: ((#"", #""), (dummy_policy_id, dummy_asset_name)),
      circulating_lp: 1_000_000_000,
      fees_per_10_thousand: option.or_else(options.edit_swap_fees, 5),
      market_open: 0,
      rewards: 2_000_000,
    }
  let pool_out_datum =
    PoolDatum {
      identifier: pool_id,
      assets: ((#"", #""), (dummy_policy_id, dummy_asset_name)),
      circulating_lp: 1_000_000_000,
      fees_per_10_thousand: option.or_else(options.edit_swap_fees, 5),
      market_open: 0,
      rewards: 7_000_000,
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
      owner,
      scooper_fee: 2_500_000,
      destination: dest,
      order: swap,
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
      mk_settings_input(hash_of_pool_script, [scooper], 1)
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
        validity_range: interval.between(0, 1),
        extra_signatories: [scooper],
        redeemers: dict.new(),
        datums: dict.new(),
        id: mk_tx_hash(1),
      },
      purpose: Spend(pool_input.output_reference),
    }
  let pool_redeemer = PoolRedeemer(0, 0, [1, 2])
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
    fees_per_10_thousand: option.or_else(options.edit_swap_fees, 5),
    market_open: 0,
    rewards: 2_000_000,
  }
  let pool_out_datum = PoolDatum {
    identifier: pool_id,
    assets: (
      (#"", #""),
      (dummy_policy_id, dummy_asset_name),
    ),
    circulating_lp: 1_009_900_990,
    fees_per_10_thousand: option.or_else(options.edit_swap_fees, 5),
    market_open: 0,
    rewards: 7_000_000,
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
    owner: owner,
    scooper_fee: 2_500_000,
    destination: dest,
    order: swap,
    extension: builtin.i_data(0),
  }
  let escrow_datum_2 = OrderDatum {
    owner: owner,
    scooper_fee: 2_500_000,
    destination: dest,
    order: deposit,
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
    let Input {output_reference, output} = mk_settings_input(hash_of_pool_script, [scooper], 1)
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
      |> value.add(hash_of_pool_script, lp_token_name(pool_id), 9_900_990)
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
          |> value.add(hash_of_pool_script, lp_token_name(pool_id), 9_900_990)
      ),
      certificates: [],
      withdrawals: dict.new(),
      validity_range: interval.between(0, 1),
      extra_signatories: [scooper],
      redeemers: dict.new(),
      datums: dict.new(),
      id: mk_tx_hash(1),
    },
    purpose: Spend(pool_input.output_reference),
  }
  let pool_redeemer = PoolRedeemer(0, 0, [1,2])
  let result = spend(settings_policy_id, pool_datum, pool_redeemer, ctx)
  result
}
