use aiken/math
use aiken/transaction.{Output}
use aiken/transaction/value.{Value}
use calculation/shared.{PoolState} as calc_shared
use shared
use types/order.{OrderDatum}

pub fn do_deposit(
  pool_state: PoolState,
  input_value: Value,
  order: OrderDatum,
  output: Output,
) -> PoolState {
  expect order.Deposit { assets } = order.order
  let (asset_a, asset_b) = assets

  expect asset_a.1st == pool_state.quantity_a.1st
  expect asset_b.1st == pool_state.quantity_b.1st
  let real_a =
    math.max(
      asset_a.3rd,
      value.quantity_of(input_value, asset_a.1st, asset_a.2nd),
    )
  let real_b =
    math.max(
      asset_b.3rd,
      value.quantity_of(input_value, asset_b.1st, asset_b.2nd),
    )
  let b_in_units_of_a =
    real_b * pool_state.quantity_a.3rd / pool_state.quantity_b.3rd

  let (final_a, final_b) =
    if b_in_units_of_a > real_a {
      (
        real_a,
        real_b - pool_state.quantity_b.3rd * ( b_in_units_of_a - real_a ) / pool_state.quantity_a.3rd,
      )
    } else {
      (real_a - b_in_units_of_a, real_b)
    }

  let issued_lp_tokens =
    final_a * pool_state.quantity_lp.3rd / pool_state.quantity_a.3rd

  let remainder =
    shared.to_value((asset_a.1st, asset_a.2nd, final_a))
      |> value.merge(shared.to_value((asset_b.1st, asset_b.2nd, final_b)))
      |> value.negate
      |> value.merge(input_value)
      |> value.merge(
           value.from_asset(
             pool_state.quantity_lp.1st,
             pool_state.quantity_lp.2nd,
             issued_lp_tokens,
           ),
         )

  expect output.address == order.destination.address
  expect output.datum == order.destination.datum
  expect output.value == remainder
  PoolState {
    quantity_a: (
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd,
      pool_state.quantity_a.3rd + final_a,
    ),
    quantity_b: (
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd,
      pool_state.quantity_b.3rd + final_b,
    ),
    quantity_lp: (
      pool_state.quantity_lp.1st,
      pool_state.quantity_lp.2nd,
      pool_state.quantity_lp.3rd + issued_lp_tokens,
    ),
    fee: pool_state.fee,
    rewards: pool_state.rewards + order.scooper_fee,
  }
}
