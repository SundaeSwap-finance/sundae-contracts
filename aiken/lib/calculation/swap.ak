use aiken/transaction/value.{Value}
use calculation/shared.{Payout, PoolState} as calc_shared
use shared.{Rational, SingletonValue}
use types/order.{OrderDatum}

/// Compute the amount of token a swap yields; returns the amount of token received, and the remainder to be sent to the user
pub fn swap_takes(
  pool_give: SingletonValue,
  pool_take: SingletonValue,
  pool_fee: Rational,
  scooper_fee: Int,
  order_give: SingletonValue,
  input_value: Value,
) -> (Int, Value) {
  // We compute the AMM formula, but 
  // algebraically rearranged to minimize rounding error
  let (numerator, denominator) = pool_fee
  let difference = denominator - numerator

  // Make sure we're giving up the same asset the pool accepts
  expect pool_give.1st == order_give.1st
  // Compute the amount of token received
  let takes =
    pool_take.3rd * order_give.3rd * difference / (
      pool_give.3rd * denominator + order_give.3rd * difference
    )

  // And then compute the remainder, using the input value minus the given amount
  let remainder =
    input_value
      |> value.add(order_give.1st, order_give.2nd, -order_give.3rd)
      |> value.add(ada_policy_id, ada_asset_name, scooper_fee)
  // And then add in the `takes` value
  let out_value = value.add(remainder, pool_take.1st, pool_take.2nd, takes)

  (takes, out_value)
}

/// The amount of the given asset already in the pool
/// The amount of the taken asset in the pool to draw from
/// The fee to charge
/// The amount of token being given up as part of the swap
/// The value coming in from the UTXO
/// Take a pool state, and compute the next pool state
pub fn do_swap(
  pool_state: PoolState,
  input_value: Value,
  order: OrderDatum,
) -> PoolState {
  expect order.Swap { offer, min_received } = order.order

  if
  offer.1st == pool_state.quantity_a.1st && offer.2nd == pool_state.quantity_a.2nd{
  
    expect min_received.1st == pool_state.quantity_b.1st
    expect min_received.2nd == pool_state.quantity_b.2nd
    let (takes, out_value) =
      swap_takes(
        pool_state.quantity_a,
        pool_state.quantity_b,
        pool_state.fee,
        order.scooper_fee,
        offer,
        input_value,
      )
    PoolState {
      quantity_a: (
        pool_state.quantity_a.1st,
        pool_state.quantity_a.2nd,
        pool_state.quantity_a.3rd + offer.3rd,
      ),
      quantity_b: (
        pool_state.quantity_b.1st,
        pool_state.quantity_b.2nd,
        pool_state.quantity_b.3rd - takes,
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
  } else if offer.1st == pool_state.quantity_b.1st && offer.2nd == pool_state.quantity_b.2nd {
    expect min_received.1st == pool_state.quantity_a.1st
    expect min_received.2nd == pool_state.quantity_a.2nd
    let (takes, out_value) =
      swap_takes(
        pool_state.quantity_b,
        pool_state.quantity_a,
        pool_state.fee,
        scooper_fee,
        offer,
        input_value,
      )
    PoolState {
      quantity_a: (
        pool_state.quantity_a.1st,
        pool_state.quantity_a.2nd,
        pool_state.quantity_a.3rd - takes,
      ),
      quantity_b: (
        pool_state.quantity_b.1st,
        pool_state.quantity_b.2nd,
        pool_state.quantity_b.3rd + offer.3rd,
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
    fail
  }
}
/// The pool state before processing this swap
/// The value being processed
/// The datum describing the order 
