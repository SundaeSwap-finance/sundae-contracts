use sundae/multisig
use aiken/math
use aiken/transaction.{NoDatum, InlineDatum, Output}
use aiken/transaction/value
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use calculation/shared.{PoolState}
use calculation/deposit.{do_deposit}
use types/order.{OrderDatum, Fixed}

test deposit_test() {
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  deposit_test_schema(
    1_000_000_000,
    1_000_000_000,
    100_000_000,
    50_000_000,
    200_000_000,
    500_000_000,
    value.from_lovelace(47_500_000)
      |> value.add(lp.1st, lp.2nd, 50_000_000)
  )
}

fn deposit_test_schema(qa: Int, qb: Int, has_a: Int, has_b: Int, gives_a: Int, gives_b: Int, out_value: value.Value) {
  let addr =
    Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let ada = (#"", #"")
  let rberry = (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  expect Some(q_lp) = math.sqrt(qa * qb)
  let pool_state = PoolState {
    quantity_a: (#"", #"", qa),
    quantity_b: (rberry.1st, rberry.2nd, qb),
    quantity_lp: (lp.1st, lp.2nd, q_lp),
  }
  let input_value =
    value.from_lovelace(has_a)
      |> value.add(rberry.1st, rberry.2nd, has_b)
  let assets = ((ada.1st, ada.2nd, gives_a), (rberry.1st, rberry.2nd, gives_b))
  let order = OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2_500_000,
    destination: Fixed {
      address: addr,
      datum: NoDatum,
    },
    details: order.Deposit {
      assets: assets,
    },
    extension: Void,
  }
  let output = Output {
    address: addr,
    value: out_value,
    datum: NoDatum,
    reference_script: None,
  }
  let input = Output {
    address: addr,
    value: input_value,
    datum: InlineDatum(order),
    reference_script: None,
  }
  let PoolState{..} = do_deposit(
    pool_state,
    input,
    assets,
    order.destination,
    2_500_000,
    output
  )
  // Test should pass as long as do_deposit didn't throw
  True
}
