use calculation/shared.{PoolState, Payout} as calc_shared
use aiken/transaction/value
use aiken/transaction.{Output}
use types/order.{OrderDatum}
use shared

pub fn do_donation(
  pool_state: PoolState,
  input: Output,
  datum: OrderDatum,
) -> PoolState {
  expect order.Donation { assets } = datum.order
  expect assets.1st.1st == pool_state.quantity_a.1st
  expect assets.2nd.1st == pool_state.quantity_b.1st
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