use aiken/math
use aiken/transaction.{Output}
use aiken/transaction/value.{Value}
use calculation/shared.{PoolState} as calc_shared
use shared
use types/order.{OrderDatum}

pub fn do_withdrawal(
  pool_state: PoolState,
  input_value: Value,
  order: OrderDatum,
  output: Output,
) -> PoolState {
  expect order.Withdrawal { amount } = order.order
  let (lp_policy, lp_asset_name, amount) = amount

  expect lp_policy == pool_state.quantity_lp.1st
  expect lp_asset_name == pool_state.quantity_lp.2nd
  let withdrawn =
    math.max(amount, value.quantity_of(input_value, lp_policy, lp_asset_name))
  let withdrawn_a =
    withdrawn * pool_state.quantity_a.3rd / pool_state.quantity_lp.3rd
  let withdrawn_b =
    withdrawn * pool_state.quantity_b.3rd / pool_state.quantity_lp.3rd
  let remainder =
    shared.to_value((lp_policy, lp_asset_name, withdrawn))
      |> value.negate
      |> value.merge(input_value)

  // Check that the payout is satisfied by the next output
  expect output.address == order.destination.address
  expect output.datum == order.destination.datum
  expect output.value == remainder
  PoolState {
    quantity_a: (
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd,
      pool_state.quantity_a.3rd - withdrawn_a,
    ),
    quantity_b: (
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd,
      pool_state.quantity_b.3rd - withdrawn_b,
    ),
    quantity_lp: (
      pool_state.quantity_lp.1st,
      pool_state.quantity_lp.2nd,
      pool_state.quantity_lp.3rd - withdrawn,
    ),
    fee: pool_state.fee,
    rewards: pool_state.rewards + order.scooper_fee,
  }
}
