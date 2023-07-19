use calculation/shared.{PoolState, Payout} as calc_shared
use aiken/transaction.{Output}
use types/order.{OrderDatum}
use aiken/math
use aiken/transaction/value
use shared

pub fn do_withdrawal(
  pool_state: PoolState,
  input: Output,
  order: OrderDatum,
) -> PoolState {
  expect order.Withdrawal { amount } = order.order
  let (lp_token, amount) = amount

  expect lp_token == pool_state.quantity_lp.1st

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