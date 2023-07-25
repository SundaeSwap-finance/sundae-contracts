use aiken/builtin
use aiken/bytearray
use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/math
use aiken/transaction.{NoDatum, DatumHash, InlineDatum, ScriptContext, Transaction}
use aiken/transaction/credential.{Script}
use aiken/transaction/value.{MintedValue, PolicyId, Value, ada_policy_id}
use calculation/process.{
  pool_input_to_state, process_orders,
}
use calculation/shared.{PoolState, get_sorted_orders} as calc_shared
use shared.{AssetClass, Ident, datum_of, is_after, spent_output}
use types/pool.{
  CreatePool, MintLP, PoolDatum, PoolRedeemer, PoolMintRedeemer, lp_token_name, pool_token_name,
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
  if outcome.quantity_a.1st == ada_policy_id {
    expect 3 == list.length(value.flatten(output_value))
    expect
      value.lovelace_of(output_value) == outcome.rewards + outcome.quantity_a.3rd
    expect
      value.quantity_of(
        output_value,
        outcome.quantity_b.1st,
        outcome.quantity_b.2nd,
      ) == outcome.quantity_b.3rd
    expect
      value.quantity_of(
        output_value,
        pool_script_hash,
        pool_token_name(identifier),
      ) == 1
    True
  } else {
    expect 4 == list.length(value.flatten(output_value))
    expect value.lovelace_of(output_value) == outcome.rewards
    expect
      value.quantity_of(
        output_value,
        outcome.quantity_a.1st,
        outcome.quantity_a.2nd,
      ) == outcome.quantity_a.3rd
    expect
      value.quantity_of(
        output_value,
        outcome.quantity_b.1st,
        outcome.quantity_b.2nd,
      ) == outcome.quantity_b.3rd
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
      fee,
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
      identifier: actual_identifier,
      circulating_lp: actual_circulating_lp,
      rewards: actual_rewards,
      ..
    } = output_datum

    // Find the reference input with the settings token and it's datum
    let settings_output = builtin.head_list(reference_inputs).output
    let settings_value_list = dict.to_list(value.to_dict(settings_output.value))
    expect [_, (found_policy_id, settings_dict)] = settings_value_list
    expect settings_found_policy_id = settings_policy_id
    expect dict.to_list(settings_dict) == [("factory", 1)]
    expect InlineDatum(settings_datum) = settings_output.datum
    expect settings_datum: SettingsDatum = settings_datum
    let SettingsDatum { pool_script_hash, authorized_scoopers, .. } =
      settings_datum
    // Sort the orders according to the redeemer
    let orders = get_sorted_orders(input_order, inputs)

    // Construct the initial pool state from the datum and the locked values
    let initial_state = pool_input_to_state(pool_script_hash, datum, pool_input)

    // Process the orders in order, and decide the final pool state we should see
    // OPTIMIZATION: pass in the list of outputs, to pop off and compare in parallel
    let outcome =
      process_orders(datums, initial_state, orders, list.drop(outputs, 1))
    let PoolState {
      quantity_lp: expected_circulating_lp,
      rewards: expected_rewards,
      ..
    } = outcome

    // The pool should have all of the scooper fees, and the quantity of eahc token of the outcome
    // Note that initializing the state with `-transaction.fee` means this gets subracted out of the rewards
    // Calculate all the neccesary and sufficient conditions
    let minted_correct_lp_tokens =
      minted_correct_pool_tokens(pool_script_hash, mint, output_datum, outcome)

    // OPTIMIZATION: skip 10 entries at a time
    // OPTIMIZATION: assume scooper is first extra_signatory? have to assume there will only ever be one extra_signatory
    let licensed_scooper =
      list.at(extra_signatories, signatory_index) == list.at(
        authorized_scoopers,
        scooper_index,
      )
    let market_open = is_after(validity_range, datum.market_open)

    // expect pool_can_be_scooped
    expect !list.is_empty(orders)
    // Must have at least one order
    expect market_open
    expect licensed_scooper
    // expect funds_moved_correctly
    expect minted_correct_lp_tokens
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
    expect datum.fees == output_datum.fees
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
        let new_pool_p_token = #"70" |> bytearray.concat(new_pool_id)
        let new_pool_l_token = #"6c" |> bytearray.concat(new_pool_id)

        let reference_inputs = ctx.transaction.reference_inputs
        let settings_input = 0

        let settings_output = builtin.head_list(reference_inputs).output
        expect InlineDatum(settings_datum) = settings_output.datum
        expect settings_datum: SettingsDatum = settings_datum

        expect Some(pool_output) = list.at(ctx.transaction.outputs, pool_output_ix)

        let coin_a_amt = value.quantity_of(
          pool_output.value,
          asset_a.1st,
          asset_a.2nd)
        let coin_b_amt = value.quantity_of(
          pool_output.value,
          asset_b.1st,
          asset_b.2nd)

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
            |> value.merge(shared.to_value((new_pool_lq.1st, new_pool_lq.2nd, initial_lq)))
        let mint_is_correct =
          value.from_minted_value(ctx.transaction.mint) == expected_mint

        let funds_spent_to_pool =
          coin_a_amt_sans_rider >= 1 &&
          coin_b_amt >= 1 &&
          list.length(value.flatten(pool_output.value)) <= 3 &&
          value.quantity_of(pool_output.value, settings_datum.pool_script_hash, new_pool_p_token) == 1

        expect InlineDatum(d) = pool_output.datum
        expect pool_output_datum: PoolDatum = d

        let pool_output_datum_correct =
          pool_output_datum.identifier == new_pool_id
            && pool_output_datum.assets == (asset_a, asset_b)
            && pool_output_datum.circulating_lp == initial_lq
            && list.has(shared.legal_swap_fees(), pool_output_datum.fees)

        coin_pair_ordering_is_canonical
          && mint_is_correct
          && funds_spent_to_pool
          && pool_output_datum_correct
      }
      MintLP(_) -> False
    }
  }
}
