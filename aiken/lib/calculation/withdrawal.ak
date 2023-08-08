use aiken/cbor
use aiken/math
use aiken/transaction.{NoDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{Value}
use calculation/shared.{PoolState} as calc_shared
use shared
use sundae/multisig
use types/order.{Destination, OrderDatum}

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
    math.min(amount, value.quantity_of(input_value, lp_policy, lp_asset_name))
  let withdrawn_a =
    withdrawn * pool_state.quantity_a.3rd / pool_state.quantity_lp.3rd
  let withdrawn_b =
    withdrawn * pool_state.quantity_b.3rd / pool_state.quantity_lp.3rd
  let remainder =
    shared.to_value((lp_policy, lp_asset_name, withdrawn))
      |> value.merge(shared.to_value(("", "", order.scooper_fee)))
      |> value.negate
      |> value.merge(input_value)
      |> value.merge(
           value.from_asset(
             pool_state.quantity_a.1st,
             pool_state.quantity_a.2nd,
             withdrawn_a,
           ))
      |> value.merge(
           value.from_asset(
             pool_state.quantity_b.1st,
             pool_state.quantity_b.2nd,
             withdrawn_b,
           ))

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
    fees_per_10_thousand: pool_state.fees_per_10_thousand,
    rewards: pool_state.rewards + order.scooper_fee,
  }
}

test withdrawal_test() {
  let addr =
    Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let rberry = (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  let pool_state = PoolState {
    quantity_a: (#"", #"", 1_010_000_000),
    quantity_b: (rberry.1st, rberry.2nd, 1_010_000_000),
    quantity_lp: (lp.1st, lp.2nd, 1_010_000_000),
    fee: (1, 2000),
    rewards: 2_000_000,
  }
  let input_value =
    value.from_lovelace(4_500_000)
      |> value.add(lp.1st, lp.2nd, 10_000_000)
  let order = OrderDatum {
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    scooper_fee: 2_500_000,
    destination: Destination {
      address: addr,
      datum: NoDatum,
    },
    order: order.Withdrawal {
      amount: (lp.1st, lp.2nd, 10_000_000),
    },
    extension: Void,
  }
  let output = Output {
    address: addr,
    value: value.from_lovelace(2_000_000)
      |> value.add(#"", #"", 10_000_000)
      |> value.add(rberry.1st, rberry.2nd, 10_000_000),
    datum: NoDatum,
    reference_script: None,
  }
  let final_pool_state = do_withdrawal(pool_state, input_value, order, output)
  expect final_pool_state.quantity_a.3rd == 1_000_000_000
  expect final_pool_state.quantity_b.3rd == 1_000_000_000
  expect final_pool_state.rewards == 4_500_000
  True
}
