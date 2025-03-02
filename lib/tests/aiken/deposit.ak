use aiken/math
use calculation/deposit.{do_deposit}
use calculation/shared.{PoolState} as calculation_shared
use cardano/address.{Address, VerificationKey}
use cardano/assets
use cardano/transaction.{InlineDatum, NoDatum, Output}
use sundae/multisig
use types/order.{Fixed, OrderDatum}

test deposit_test() {
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  deposit_test_schema(
    1_000_000_000,
    1_000_000_000,
    100_000_000,
    50_000_000,
    200_000_000,
    500_000_000,
    assets.from_lovelace(47_500_000)
      |> assets.add(lp.1st, lp.2nd, 50_000_000),
  )
}

test deposit_test_minimum() fail {
  deposit_test_schema(
    661_278_808_416,
    188_253_559_159_646,
    153_000_000,
    150,
    151_171_875,
    150,
    assets.from_lovelace(152_000_000),
  )
}

fn deposit_test_schema(
  qa: Int,
  qb: Int,
  has_a: Int,
  has_b: Int,
  gives_a: Int,
  gives_b: Int,
  out_value: assets.Value,
) {
  let addr =
    Address(
      VerificationKey(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let ada = (#"", #"")
  let rberry =
    (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  expect Some(q_lp) = math.sqrt(qa * qb)
  let pool_state =
    PoolState {
      quantity_a: (#"", #"", qa),
      quantity_b: (rberry.1st, rberry.2nd, qb),
      quantity_lp: (lp.1st, lp.2nd, q_lp),
    }
  let input_value =
    assets.from_lovelace(has_a)
      |> assets.add(rberry.1st, rberry.2nd, has_b)
  let assets = ((ada.1st, ada.2nd, gives_a), (rberry.1st, rberry.2nd, gives_b))
  let order =
    OrderDatum {
      pool_ident: None,
      owner: multisig.Signature(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      max_protocol_fee: 2_500_000,
      destination: Fixed { address: addr, datum: NoDatum },
      details: order.Deposit { assets },
      extension: Void,
    }
  let output =
    Output {
      address: addr,
      value: out_value,
      datum: NoDatum,
      reference_script: None,
    }
  let input =
    Output {
      address: addr,
      value: input_value,
      datum: InlineDatum(order),
      reference_script: None,
    }
  let
    _,
    _,
    _,
  <-
    do_deposit(
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd,
      pool_state.quantity_a.3rd,
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd,
      pool_state.quantity_b.3rd,
      pool_state.quantity_lp.1st,
      pool_state.quantity_lp.2nd,
      pool_state.quantity_lp.3rd,
      0,
      pool_state.quantity_lp.3rd,
      pool_state.quantity_lp.3rd,
      input,
      assets,
      order.destination,
      2_500_000,
      output,
    )
  // Test should pass as long as do_deposit didn't throw
  True
}
