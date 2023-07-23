use aiken/dict
use aiken/list
use aiken/transaction.{ScriptContext, Transaction}
use aiken/transaction/value.{MintedValue, Value}
use calculation/process.{
  check_satisfies_payouts, pool_input_to_state, process_orders,
}
use calculation/shared.{PoolState, get_sorted_orders} as calc_shared
use shared.{datum_of, is_after, spent_output}
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

fn expected_pool_value(
  settings: SettingsDatum,
  datum: PoolDatum,
  outcome: PoolState,
) -> Value {
  value.from_lovelace(outcome.rewards)
    |> value.merge(
         shared.to_value(
           (settings.pool_script_hash, pool_token_name(datum.identifier), 1),
         ),
       )
    |> value.merge(shared.to_value(outcome.quantity_a))
    |> value.merge(shared.to_value(outcome.quantity_b))
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
    let outputs = transaction.outputs
    // Grab the fields we'll need to determine whether we can execute this tx
    // Find the input being spent
    let pool_input = spent_output(ctx)
    // Assume the first output is the pool output
    expect Some(pool_output) = list.head(outputs)
    expect pool_output.address == pool_input.address
    expect Some(output_datum) = datum_of(ctx, pool_output)
    expect output_datum: PoolDatum = output_datum

    // Find the reference input with the settings token and it's datum
    let settings_output =
      shared.get_input_with_token(
        reference_inputs,
        redeemer.settings_input,
        shared.settings_asset_class(),
      )
    expect Some(settings_datum) = datum_of(ctx, settings_output)
    expect settings_datum: SettingsDatum = settings_datum
    // Sort the orders according to the redeemer
    let orders = get_sorted_orders(redeemer.input_order, inputs)

    // Construct the initial pool state from the datum and the locked values
    let initial_state =
      pool_input_to_state(
        settings_datum.pool_script_hash,
        datum,
        pool_input,
        value.lovelace_of(fee),
      )

    // Process the orders in order, and decide the final pool state we should see
    let outcome = process_orders(ctx, initial_state, orders)

    // The pool should have all of the scooper fees, and the quantity of eahc token of the outcome
    // Note that initializing the state with `-transaction.fee` means this gets subracted out of the rewards
    let expected_pool_value =
      expected_pool_value(settings_datum, output_datum, outcome)

    // Calculate all the neccesary and sufficient conditions
    let at_least_one_order = !list.is_empty(orders)
    let updated_circulating_lp =
      output_datum.circulating_lp == outcome.quantity_lp.3rd
    let minted_correct_lp_tokens =
      minted_correct_pool_tokens(settings_datum, mint, output_datum, outcome)
    let updated_rewards = output_datum.rewards == outcome.rewards
    let satisfies_payouts =
      check_satisfies_payouts(
        // We prepend each payout to the list, meaning we need to reverse it to match the outputs
        // Candidate for optimization, just build the outputs in the reverse order
        outcome.payouts,
        list.drop(outputs, 1),
      )
    let pool_has_correct_amounts = pool_output.value == expected_pool_value
    let pool_has_correct_staking_key =
      pool_input.address.stake_credential == pool_output.address.stake_credential
    let pool_immutables_unchanged =
      datum.identifier == output_datum.identifier && datum.assets == output_datum.assets && datum.fees == output_datum.fees && datum.market_open == output_datum.market_open
    let licensed_scooper =
      list.at(extra_signatories, redeemer.signatory_index) == list.at(
        settings_datum.authorized_scoopers,
        redeemer.scooper_index,
      )
    let market_open = is_after(validity_range, datum.market_open)

    // Check that the fields on the datum have been updated (or not) correctly
    let datum_updated_correctly =
      updated_circulating_lp && updated_rewards && pool_immutables_unchanged

    // High-level intermediate goals
    let pool_can_be_scooped =
      at_least_one_order && market_open && licensed_scooper
    let funds_moved_correctly =
      minted_correct_lp_tokens && satisfies_payouts && pool_has_correct_amounts
    let pool_output_correct =
      datum_updated_correctly && pool_has_correct_staking_key
    // And then AND these all together
    pool_can_be_scooped && funds_moved_correctly && pool_output_correct
  }

  fn mint(_r: PoolMintRedeemer, _ctx: ScriptContext) {
    True
  }
}
