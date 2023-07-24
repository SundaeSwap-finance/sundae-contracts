use aiken/transaction.{Output}
use aiken/transaction/value.{Value}
use calculation/shared.{PoolState} as calc_shared
use shared
use types/order.{OrderDatum}

pub fn do_donation(
  pool_state: PoolState,
  input_value: Value,
  order: OrderDatum,
  output: Output,
) -> (PoolState, Bool) {
  expect order.Donation { assets } = order.order
  expect assets.1st.1st == pool_state.quantity_a.1st
  expect assets.1st.2nd == pool_state.quantity_a.2nd
  expect assets.2nd.1st == pool_state.quantity_b.1st
  expect assets.2nd.2nd == pool_state.quantity_b.2nd
  let remainder =
    shared.to_value(assets.1st)
      |> value.merge(shared.to_value(assets.2nd))
      |> value.negate
      |> value.merge(input_value)

  let has_remainder = remainder != value.zero()
  expect _ =
    if has_remainder {
      expect output.address == order.destination.address
      expect output.datum == order.destination.datum
      expect output.value == remainder
      Void
    } else {
      Void
    }
  (
    PoolState {
      quantity_a: (
        pool_state.quantity_a.1st,
        pool_state.quantity_a.2nd,
        pool_state.quantity_a.3rd + assets.1st.3rd,
      ),
      quantity_b: (
        pool_state.quantity_b.1st,
        pool_state.quantity_b.2nd,
        pool_state.quantity_b.3rd + assets.2nd.3rd,
      ),
      quantity_lp: pool_state.quantity_lp,
      fee: pool_state.fee,
      rewards: pool_state.rewards + order.scooper_fee,
    },
    has_remainder,
  )
}
