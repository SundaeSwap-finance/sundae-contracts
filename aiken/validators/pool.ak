use aiken/dict
use aiken/list
use aiken/transaction.{Datum, Input, Output, ScriptContext, find_script_outputs}
use aiken/transaction/credential.{Address, ScriptCredential}
use aiken/transaction/value.{PolicyId, Value}
use shared.{Rational, SingletonValue, datum_of, is_after, spent_output}
use types/order.{OrderDatum}
use types/pool.{PoolDatum, PoolRedeemer}

fn get_sorted_orders(
  redeemer: PoolRedeemer,
  inputs: List<Input>,
) -> List<Output> {
  list.map(
    redeemer.input_order,
    fn(idx) {
      expect Some(input) = list.at(inputs, idx)
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
  quantity_lp: Int,
  fee: Rational,
  rewards: Int,
  payouts: List<Payout>,
}

fn swap_takes(
  pool_give: SingletonValue,
  pool_take: SingletonValue,
  pool_fee: Rational,
  escrow_give: SingletonValue,
  input_value: Value,
) -> (Int, Value) {
  let (numerator, denominator) = pool_fee
  let difference = denominator - numerator
  if pool_give.1st != escrow_give.1st {
    error
  } else {
    let takes =
      pool_take.2nd * escrow_give.2nd * difference / (
        pool_give.2nd * denominator + escrow_give.2nd * difference
      )
    let remainder =
      value.add(
        input_value,
        escrow_give.1st.1st,
        escrow_give.1st.2nd,
        -escrow_give.2nd,
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

fn process_order(
  ctx: ScriptContext,
  initial: PoolState,
  input: Output,
) -> PoolState {
  expect Some(datum) = datum_of(ctx, input)
  expect datum: OrderDatum = datum
  when datum.order is {
    order.Swap(..) -> do_swap(initial, input, datum)
    _ -> todo
  }
}

fn process_orders(
  ctx: ScriptContext,
  initial: PoolState,
  orders: List<Output>,
) -> PoolState {
  list.reduce(orders, initial, fn(a, b) { process_order(ctx, a, b) })
}

fn pool_input_to_state(datum: PoolDatum, input: Output) -> PoolState {
  let assets = datum.assets
  PoolState {
    quantity_a: (
      assets.1st,
      value.quantity_of(input.value, assets.1st.1st, assets.1st.2nd),
    ),
    quantity_b: (
      assets.2nd,
      value.quantity_of(input.value, assets.2nd.1st, assets.2nd.2nd),
    ),
    quantity_lp: datum.circulating_lp,
    fee: datum.fees,
    rewards: datum.rewards,
    payouts: [],
  }
}

validator {
  fn spend(datum: PoolDatum, redeemer: PoolRedeemer, ctx: ScriptContext) {
    // Grab the fields we'll need to determine whether we can execute this tx
    let pool_input = spent_output(ctx)
    expect ScriptCredential(script_hash) = pool_input.address.payment_credential
    expect [pool_output] =
      find_script_outputs(ctx.transaction.outputs, script_hash)
    // TODO: from settings
    let pool_policy: PolicyId = #""
    expect Some(output_datum) = datum_of(ctx, pool_output)
    expect output_datum: PoolDatum = output_datum
    // TODO: faster with pool redeemer?
    let orders = get_sorted_orders(redeemer, ctx.transaction.inputs)
    let initial_state = pool_input_to_state(datum, pool_input)
    let outcome = process_orders(ctx, initial_state, orders)
    let minted_pool_tokens =
      value.tokens(value.from_minted_value(ctx.transaction.mint), pool_policy)

    // Calculate all the neccesary and sufficient conditions
    let at_least_one_order = !list.is_empty(orders)
    let updated_circulating_lp =
      output_datum.circulating_lp == outcome.quantity_lp
    let minted_correct_lp_tokens =
      if datum.circulating_lp == outcome.quantity_lp {
        dict.is_empty(minted_pool_tokens)
      } else {
        dict.to_list(minted_pool_tokens) == [
          (#"", outcome.quantity_lp - datum.circulating_lp),
        ]
      }
    let updated_rewards =
      output_datum.rewards == outcome.rewards - value.lovelace_of(
        ctx.transaction.fee,
      )
    let satisfies_payouts = True
    let pool_has_correct_staking_key = True
    let pool_has_correct_amounts = True
    let pool_immutables_unchanged = True
    let licensed_scooper = True
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
