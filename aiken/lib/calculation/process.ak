use aiken/bytearray.{to_hex}
use aiken/cbor.{serialise}
use aiken/list
use aiken/transaction.{Output, ScriptContext}
use aiken/transaction/value.{PolicyId}
use calculation/deposit
use calculation/donation
use calculation/shared.{Payout, PoolState} as calc_shared
use calculation/swap
use calculation/withdrawal
use shared.{datum_of}
use types/order.{OrderDatum}
use types/pool.{PoolDatum, lp_token_name}

pub fn pool_input_to_state(
  pool_token_policy: PolicyId,
  datum: PoolDatum,
  input: Output,
  fee: Int,
) -> PoolState {
  let assets = datum.assets
  let min_utxo =
    if assets.1st.1st == value.ada_policy_id {
      datum.rewards
    } else {
      0
    }
  PoolState {
    quantity_a: (
      assets.1st.1st,
      assets.1st.2nd,
      value.quantity_of(input.value, assets.1st.1st, assets.1st.2nd) - min_utxo,
    ),
    quantity_b: (
      assets.2nd.1st,
      assets.2nd.2nd,
      value.quantity_of(input.value, assets.2nd.1st, assets.2nd.2nd),
    ),
    quantity_lp: (
      pool_token_policy,
      lp_token_name(datum.identifier),
      datum.circulating_lp,
    ),
    fee: datum.fees,
    // Pay the transaction fee out of the rewards
    rewards: datum.rewards,
    payouts: [],
  }
}

pub fn process_order(
  ctx: ScriptContext,
  initial: PoolState,
  input: Output,
) -> PoolState {
  expect Some(datum) = datum_of(ctx, input)
  expect datum: OrderDatum = datum
  when datum.order is {
    order.Swap(..) -> swap.do_swap(initial, input.value, datum)
    order.Deposit(..) -> deposit.do_deposit(initial, input, datum)
    order.Withdrawal(..) -> withdrawal.do_withdrawal(initial, input, datum)
    // order.Zap(..) -> do_zap(initial, input, datum)
    order.Donation(..) -> donation.do_donation(initial, input, datum)
  }
}

pub fn process_orders(
  ctx: ScriptContext,
  initial: PoolState,
  orders: List<Output>,
) -> PoolState {
  list.reduce(orders, initial, fn(a, b) { process_order(ctx, a, b) })
}

pub fn check_satisfies_payouts(
  payouts: List<Payout>,
  outputs: List<Output>,
) -> Bool {
  when (payouts, outputs) is {
    ([next_payout, ..rest_payouts], [next_output, ..rest_outputs]) -> {
      expect next_output.address == next_payout.address
      expect next_output.datum == next_payout.datum
      expect next_output.value == next_payout.value
      check_satisfies_payouts(rest_payouts, rest_outputs)
    }
    ([], _) -> True
    _ -> False
  }
}
