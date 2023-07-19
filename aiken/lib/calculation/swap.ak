use calculation/shared.{PoolState, Payout} as calc_shared
use shared.{SingletonValue, Rational}
use types/order.{OrderDatum}
use aiken/transaction/value.{Value}

/// Compute the amount of token a swap yields; returns the amount of token received, and the remainder to be sent to the user
pub fn swap_takes(
  /// The amount of the given asset already in the pool
  pool_give: SingletonValue,
  /// The amount of the taken asset in the pool to draw from
  pool_take: SingletonValue,
  /// The fee to charge
  pool_fee: Rational,
  /// The amount of token being given up as part of the swap
  order_give: SingletonValue,
  /// The value coming in from the UTXO
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
    pool_take.2nd * order_give.2nd * difference / (
      pool_give.2nd * denominator + order_give.2nd * difference
    )

  // And then compute the remainder, using the input value minus the given amount
  let remainder =
    value.add(
      input_value,
      order_give.1st.1st,
      order_give.1st.2nd,
      -order_give.2nd,
    )
  // And then add in the `takes` value
  let out_value =
    value.add(remainder, pool_take.1st.1st, pool_take.1st.2nd, takes)

  (takes, out_value)
}

/// Take a pool state, and compute the next pool state
pub fn do_swap(
  /// The pool state before processing this swap
  pool_state: PoolState,
  /// The value being processed
  input_value: Value,
  /// The datum describing the order 
  order: OrderDatum,
) -> PoolState {
  expect order.Swap { offer, min_received } = order.order

  if offer.1st == pool_state.quantity_a.1st {
    expect min_received.1st == pool_state.quantity_b.1st

    let (takes, out_value) =
      swap_takes(
        pool_state.quantity_a,
        pool_state.quantity_b,
        pool_state.fee,
        offer,
        input_value,
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
  } else if offer.1st == pool_state.quantity_b.1st {
    expect min_received.1st == pool_state.quantity_a.1st

    let (takes, out_value) =
      swap_takes(
        pool_state.quantity_b,
        pool_state.quantity_a,
        pool_state.fee,
        offer,
        input_value,
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
    fail
  }
}
