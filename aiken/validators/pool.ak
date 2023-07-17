use aiken/dict
use aiken/list
use aiken/math
use aiken/transaction.{Datum, Input, Output, ScriptContext}
use aiken/transaction/credential.{Address, ScriptCredential}
use aiken/transaction/value.{Value}
use shared.{Rational, SingletonValue, datum_of, is_after, spent_output}
use types/order.{OrderDatum}
use types/pool.{PoolDatum, PoolRedeemer}
use types/settings.{SettingsDatum}

fn get_sorted_orders(
  input_order: List<Int>,
  inputs: List<Input>,
) -> List<Output> {
  list.map(
    input_order,
    fn(idx) {
      expect Some(input) = list.at(inputs, idx)
      expect ScriptCredential(input_script) =
        input.output.address.payment_credential
      expect True = input_script == shared.order_script_hash
      input.output
    },
  )
}

type Payout {
  address: Address,
  datum: Datum,
  value: Value,
}

type PoolState {
  quantity_a: SingletonValue,
  quantity_b: SingletonValue,
  quantity_lp: SingletonValue,
  fee: Rational,
  rewards: Int,
  payouts: List<Payout>,
}

fn swap_takes(
  pool_give: SingletonValue,
  pool_take: SingletonValue,
  pool_fee: Rational,
  order_give: SingletonValue,
  input_value: Value,
) -> (Int, Value) {
  let (numerator, denominator) = pool_fee
  let difference = denominator - numerator
  if pool_give.1st != order_give.1st {
    error
  } else {
    let takes =
      pool_take.2nd * order_give.2nd * difference / (
        pool_give.2nd * denominator + order_give.2nd * difference
      )
    let remainder =
      value.add(
        input_value,
        order_give.1st.1st,
        order_give.1st.2nd,
        -order_give.2nd,
      )
    let out_value =
      value.add(remainder, pool_take.1st.1st, pool_take.1st.2nd, takes)
    (takes, out_value)
  }
}

fn do_swap(pool_state: PoolState, input: Output, order: OrderDatum) -> PoolState {
  expect order.Swap { offer, min_received } = order.order
  if offer.1st == pool_state.quantity_a.1st {
    if min_received.1st != pool_state.quantity_b.1st {
      error
    } else {
      let (takes, out_value) =
        swap_takes(
          pool_state.quantity_a,
          pool_state.quantity_b,
          pool_state.fee,
          offer,
          input.value,
        )
      PoolState {
        quantity_a: (
          pool_state.quantity_a.1st,
          pool_state.quantity_a.2nd + offer.2nd,
        ),
        quantity_b: (
          pool_state.quantity_b.1st,
          pool_state.quantity_b.2nd - takes,
        ),
        quantity_lp: pool_state.quantity_lp,
        fee: pool_state.fee,
        rewards: pool_state.rewards + order.scooper_fee,
        payouts: [
          Payout {
            address: order.destination.address,
            datum: order.destination.datum,
            value: out_value,
          },
          ..pool_state.payouts
        ],
      }
    }
  } else if offer.1st == pool_state.quantity_b.1st {
    let (takes, out_value) =
      swap_takes(
        pool_state.quantity_b,
        pool_state.quantity_a,
        pool_state.fee,
        offer,
        input.value,
      )
    PoolState {
      quantity_a: (pool_state.quantity_a.1st, pool_state.quantity_a.2nd - takes),
      quantity_b: (
        pool_state.quantity_b.1st,
        pool_state.quantity_b.2nd + offer.2nd,
      ),
      quantity_lp: pool_state.quantity_lp,
      fee: pool_state.fee,
      rewards: pool_state.rewards + order.scooper_fee,
      payouts: [
        Payout {
          address: order.destination.address,
          datum: order.destination.datum,
          value: out_value,
        },
        ..pool_state.payouts
      ],
    }
  } else {
    error
  }
}

fn do_deposit(
  pool_state: PoolState,
  input: Output,
  order: OrderDatum,
) -> PoolState {
  expect order.Deposit { assets } = order.order
  let (asset_a, asset_b) = assets

  expect True = asset_a.1st == pool_state.quantity_a.1st
  expect True = asset_b.1st == pool_state.quantity_b.1st

  let real_a =
    math.max(
      asset_a.2nd,
      value.quantity_of(input.value, asset_a.1st.1st, asset_a.1st.2nd),
    )
  let real_b =
    math.max(
      asset_b.2nd,
      value.quantity_of(input.value, asset_b.1st.1st, asset_b.1st.2nd),
    )
  let b_in_units_of_a =
    real_b * pool_state.quantity_a.2nd / pool_state.quantity_b.2nd

  let (final_a, final_b) =
    if b_in_units_of_a > real_a {
      (
        real_a,
        real_b - pool_state.quantity_b.2nd * ( b_in_units_of_a - real_a ) / pool_state.quantity_a.2nd,
      )
    } else {
      (real_a - b_in_units_of_a, real_b)
    }

  let issued_lp_tokens =
    final_a * pool_state.quantity_lp.2nd / pool_state.quantity_a.2nd

  let remainder =
    shared.to_value((asset_a.1st, final_a))
      |> value.merge(shared.to_value((asset_b.1st, final_b)))
      |> value.negate
      |> value.merge(input.value)
      |> value.merge(
           value.from_asset(
             pool_state.quantity_lp.1st.1st,
             pool_state.quantity_lp.1st.2nd,
             issued_lp_tokens,
           ),
         )
  PoolState {
    quantity_a: (pool_state.quantity_a.1st, pool_state.quantity_a.2nd + final_a),
    quantity_b: (pool_state.quantity_b.1st, pool_state.quantity_b.2nd + final_b),
    quantity_lp: (
      pool_state.quantity_lp.1st,
      pool_state.quantity_lp.2nd + issued_lp_tokens,
    ),
    fee: pool_state.fee,
    rewards: pool_state.rewards + order.scooper_fee,
    payouts: if value.zero() == remainder {
      pool_state.payouts
    } else {
      [
        Payout {
          address: order.destination.address,
          datum: order.destination.datum,
          value: remainder,
        },
        ..pool_state.payouts
      ]
    },
  }
}

fn do_withdrawal(
  pool_state: PoolState,
  input: Output,
  order: OrderDatum,
) -> PoolState {
  expect order.Withdrawal { amount } = order.order
  let (lp_token, amount) = amount

  expect True = lp_token == pool_state.quantity_lp.1st

  let withdrawn =
    math.max(amount, value.quantity_of(input.value, lp_token.1st, lp_token.2nd))
  let withdrawn_a =
    withdrawn * pool_state.quantity_a.2nd / pool_state.quantity_lp.2nd
  let withdrawn_b =
    withdrawn * pool_state.quantity_b.2nd / pool_state.quantity_lp.2nd
  let remainder =
    shared.to_value((lp_token, withdrawn))
      |> value.negate
      |> value.merge(input.value)
  PoolState {
    quantity_a: (
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd - withdrawn_a,
    ),
    quantity_b: (
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd - withdrawn_b,
    ),
    quantity_lp: (
      pool_state.quantity_lp.1st,
      pool_state.quantity_lp.2nd - withdrawn,
    ),
    fee: pool_state.fee,
    rewards: pool_state.rewards + order.scooper_fee,
    payouts: if value.zero() == remainder {
      pool_state.payouts
    } else {
      [
        Payout {
          address: order.destination.address,
          datum: order.destination.datum,
          value: remainder,
        },
        ..pool_state.payouts
      ]
    },
  }
}

fn do_donation(
  pool_state: PoolState,
  input: Output,
  datum: OrderDatum,
) -> PoolState {
  expect order.Donation { assets } = datum.order
  expect True = assets.1st.1st == pool_state.quantity_a.1st
  expect True = assets.2nd.1st == pool_state.quantity_b.1st
  let remainder =
    shared.to_value(assets.1st)
      |> value.merge(shared.to_value(assets.2nd))
      |> value.negate
      |> value.merge(input.value)
  PoolState {
    quantity_a: (
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd + assets.1st.2nd,
    ),
    quantity_b: (
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd + assets.2nd.2nd,
    ),
    quantity_lp: pool_state.quantity_lp,
    fee: pool_state.fee,
    rewards: pool_state.rewards + datum.scooper_fee,
    payouts: if value.zero() == remainder {
      pool_state.payouts
    } else {
      [
        Payout {
          address: datum.destination.address,
          datum: datum.destination.datum,
          value: remainder,
        },
        ..pool_state.payouts
      ]
    },
  }
}

fn process_order(
  ctx: ScriptContext,
  initial: PoolState,
  input: Output,
) -> PoolState {
  expect Some(datum) = datum_of(ctx, input)
  expect datum: OrderDatum = datum
  when datum.order is {
    order.Swap(..) -> do_swap(initial, input, datum)
    order.Deposit(..) -> do_deposit(initial, input, datum)
    order.Withdrawal(..) -> do_withdrawal(initial, input, datum)
    // order.Zap(..) -> do_zap(initial, input, datum)
    order.Donation(..) -> do_donation(initial, input, datum)
  }
}

fn process_orders(
  ctx: ScriptContext,
  initial: PoolState,
  orders: List<Output>,
) -> PoolState {
  list.reduce(orders, initial, fn(a, b) { process_order(ctx, a, b) })
}

fn pool_input_to_state(datum: PoolDatum, input: Output, fee: Int) -> PoolState {
  let assets = datum.assets
  let min_utxo =
    if assets.1st.1st == value.ada_policy_id {
      datum.rewards
    } else {
      0
    }
  PoolState {
    quantity_a: (
      assets.1st,
      value.quantity_of(input.value, assets.1st.1st, assets.1st.2nd) - min_utxo,
    ),
    quantity_b: (
      assets.2nd,
      value.quantity_of(input.value, assets.2nd.1st, assets.2nd.2nd),
    ),
    quantity_lp: ((#"", #""), datum.circulating_lp),
    fee: datum.fees,
    rewards: datum.rewards - fee,
    payouts: [],
  }
}

fn check_satisfies_payouts(payouts: List<Payout>, outputs: List<Output>) -> Bool {
  when (payouts, outputs) is {
    ([next_payout, ..rest_payouts], [next_output, ..rest_outputs]) -> {
      expect True = next_output.address == next_payout.address
      expect True = next_output.datum == next_payout.datum
      expect True = next_output.value == next_payout.value
      check_satisfies_payouts(rest_payouts, rest_outputs)
    }
    ([], []) -> True
    _ -> False
  }
}

validator {
  fn spend(datum: PoolDatum, redeemer: PoolRedeemer, ctx: ScriptContext) {
    // Grab the fields we'll need to determine whether we can execute this tx
    // Find the input being spent
    let pool_input = spent_output(ctx)
    // Assume the first output is the pool output
    expect Some(pool_output) = list.head(ctx.transaction.outputs)
    expect True = pool_output.address == pool_input.address
    expect Some(output_datum) = datum_of(ctx, pool_output)
    expect output_datum: PoolDatum = output_datum
    // Find the reference input with the settings token and it's datum
    let settings_output =
      shared.get_input_with_token(
        ctx.transaction.reference_inputs,
        redeemer.settings_input,
        shared.settings_asset_class(),
      )
    expect Some(settings_datum) = datum_of(ctx, settings_output)
    expect settings_datum: SettingsDatum = settings_datum
    // Sort the orders according to the redeemer
    let orders = get_sorted_orders(redeemer.input_order, ctx.transaction.inputs)
    // Extract the amount of pool tokens that were minted
    let minted_pool_tokens =
      value.tokens(
        value.from_minted_value(ctx.transaction.mint),
        settings_datum.pool_script_hash,
      )

    // Construct the initial pool state from the datum and the locked values
    let initial_state =
      pool_input_to_state(
        datum,
        pool_input,
        value.lovelace_of(ctx.transaction.fee),
      )
    // Process the orders in order, and decide the final pool state we should see
    let outcome = process_orders(ctx, initial_state, orders)

    let expected_pool_value =
      value.from_lovelace(outcome.rewards)
        |> value.merge(shared.to_value(outcome.quantity_a))
        |> value.merge(shared.to_value(outcome.quantity_b))

    // Calculate all the neccesary and sufficient conditions
    let at_least_one_order = !list.is_empty(orders)
    let updated_circulating_lp =
      output_datum.circulating_lp == outcome.quantity_lp.2nd
    let minted_correct_lp_tokens =
      if datum.circulating_lp == outcome.quantity_lp.2nd {
        dict.is_empty(minted_pool_tokens)
      } else {
        dict.to_list(minted_pool_tokens) == [
          (#"", outcome.quantity_lp.2nd - datum.circulating_lp),
        ]
      }
    let updated_rewards = output_datum.rewards == outcome.rewards
    let satisfies_payouts =
      check_satisfies_payouts(
        outcome.payouts,
        list.drop(ctx.transaction.outputs, 1),
      )
    let pool_has_correct_amounts = pool_output.value == expected_pool_value
    let pool_has_correct_staking_key =
      pool_input.address.stake_credential == pool_output.address.stake_credential
    let pool_immutables_unchanged =
      datum.identifier == output_datum.identifier && datum.assets == output_datum.assets && datum.fees == output_datum.fees && datum.market_open == output_datum.market_open
    let licensed_scooper =
      list.any(
        ctx.transaction.extra_signatories,
        fn(k) { list.has(settings_datum.authorized_scoopers, k) },
      )
    let market_open =
      is_after(ctx.transaction.validity_range, datum.market_open)

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
}
