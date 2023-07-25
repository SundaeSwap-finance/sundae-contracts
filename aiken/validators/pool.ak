use aiken/dict
use aiken/list
use aiken/transaction.{ScriptContext, Transaction}
use aiken/transaction/value.{MintedValue, Value, ada_policy_id}
use calculation/process.{pool_input_to_state, process_orders}
use calculation/shared.{PoolState, get_sorted_orders} as calc_shared
use shared.{Ident, datum_of, is_after, spent_output}
use types/pool.{
  PoolDatum, PoolMintRedeemer, PoolRedeemer, lp_token_name, pool_token_name,
}
use types/settings.{SettingsDatum}

fn minted_correct_pool_tokens(
  settings: SettingsDatum,
  mint: MintedValue,
  datum: PoolDatum,
  outcome: PoolState,
) -> Bool {
  let minted_tokens =
    value.tokens(value.from_minted_value(mint), settings.pool_script_hash)

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
  settings: SettingsDatum,
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
        settings.pool_script_hash,
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
        settings.pool_script_hash,
        pool_token_name(identifier),
      ) == 1
    True
  }
}

validator {
  fn spend(datum: PoolDatum, redeemer: PoolRedeemer, ctx: ScriptContext) {
    let transaction = ctx.transaction
    let Transaction {
      inputs,
      outputs,
      reference_inputs,
      mint,
      fee,
      extra_signatories,
      validity_range,
      ..
    } = transaction
    let PoolRedeemer {
      settings_input,
      signatory_index,
      scooper_index,
      input_order,
    } = redeemer
    // Grab the fields we'll need to determine whether we can execute this tx
    // Find the input being spent
    let pool_input = spent_output(ctx)
    // Assume the first output is the pool output
    expect Some(pool_output) = list.head(outputs)
    expect pool_output.address == pool_input.address
    expect Some(output_datum) = datum_of(ctx, pool_output)
    expect output_datum: PoolDatum = output_datum
    let PoolDatum {
      identifier: actual_identifier,
      circulating_lp: actual_circulating_lp,
      rewards: actual_rewards,
      ..
    } = output_datum

    // Find the reference input with the settings token and it's datum
    let settings_output =
      shared.get_input_with_token(
        reference_inputs,
        settings_input,
        shared.settings_asset_class(),
      )
    expect Some(settings_datum) = datum_of(ctx, settings_output)
    expect settings_datum: SettingsDatum = settings_datum
    // Sort the orders according to the redeemer
    let orders = get_sorted_orders(input_order, inputs)

    // Construct the initial pool state from the datum and the locked values
    let initial_state =
      pool_input_to_state(
        settings_datum.pool_script_hash,
        datum,
        pool_input,
        value.lovelace_of(fee),
      )

    // Process the orders in order, and decide the final pool state we should see
    // OPTIMIZATION: pass in the list of outputs, to pop off and compare in parallel
    let outcome =
      process_orders(ctx, initial_state, orders, list.drop(outputs, 1))
    let PoolState {
      quantity_lp: expected_circulating_lp,
      rewards: expected_rewards,
      ..
    } = outcome

    // The pool should have all of the scooper fees, and the quantity of eahc token of the outcome
    // Note that initializing the state with `-transaction.fee` means this gets subracted out of the rewards
    // Calculate all the neccesary and sufficient conditions
    let minted_correct_lp_tokens =
      minted_correct_pool_tokens(settings_datum, mint, output_datum, outcome)

    // OPTIMIZATION: skip 10 entries at a time
    // OPTIMIZATION: assume scooper is first extra_signatory? have to assume there will only ever be one extra_signatory
    let licensed_scooper =
      list.at(extra_signatories, signatory_index) == list.at(
        settings_datum.authorized_scoopers,
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
        settings_datum,
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

  fn mint(_r: PoolMintRedeemer, _ctx: ScriptContext) {
    True
  }
}
