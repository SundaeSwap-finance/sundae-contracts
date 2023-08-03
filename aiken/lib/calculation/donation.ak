use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction.{NoDatum, Output}
use aiken/transaction/value.{Value}
use calculation/shared.{PoolState} as calc_shared
use shared
use sundae/multisig
use types/order.{Destination, OrderDatum}

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
  let Void =
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

test donation() {
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
  let pool_state = PoolState {
    quantity_a: (#"", #"", 1_000_000_000),
    quantity_b: (rberry.1st, rberry.2nd, 1_000_000_000),
    quantity_lp: (lp.1st, lp.2nd, 1_000_000_000),
    fee: (1, 2000),
    rewards: 2_000_000,
  }
  let input_value =
    value.from_lovelace(1_000_000)
      |> value.add(rberry.1st, rberry.2nd, 1_000_000)
  let order = OrderDatum {
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    scooper_fee: 2_500_000,
    destination: Destination {
      address: addr,
      datum: NoDatum,
    },
    order: order.Donation {
      assets: ((ada.1st, ada.2nd, 1_000_000), (rberry.1st, rberry.2nd, 1_000_000)),
    },
    extension: Void,
  }
  // There's no remainder so do_donation totally ignores this Output record
  let output = Output {
    address: addr,
    value: value.from_lovelace(999_999_999_999_999_999),
    datum: NoDatum,
    reference_script: None,
  }
  let (final_pool_state, has_remainder) = do_donation(pool_state, input_value, order, output)
  expect !has_remainder
  expect final_pool_state.quantity_a.3rd == 1_001_000_000
  expect final_pool_state.quantity_b.3rd == 1_001_000_000
  expect final_pool_state.rewards == 4_500_000
  True
}
