use aiken/transaction.{NoDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value
use calculation/shared.{PoolState} as calc_shared
use calculation/donation.{do_donation}
use sundae/multisig
use types/order.{Destination, OrderDatum}

test donation() {
  let addr =
    Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let ada = (#"", #"")
  let rberry =
    (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  let pool_state =
    PoolState {
      quantity_a: (#"", #"", 1_000_000_000),
      quantity_b: (rberry.1st, rberry.2nd, 1_000_000_000),
      quantity_lp: (lp.1st, lp.2nd, 1_000_000_000),
    }
  let input_value =
    value.from_lovelace(3_500_000)
      |> value.add(rberry.1st, rberry.2nd, 1_000_000)
  let assets = (
          (ada.1st, ada.2nd, 1_000_000),
          (rberry.1st, rberry.2nd, 1_000_000),
        )
  let order =
    OrderDatum {
      pool_ident: None,
      owner: multisig.Signature(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      max_protocol_fee: 2_500_000,
      destination: Destination { address: addr, datum: NoDatum },
      details: order.Donation {
        assets: assets,
      },
      extension: Void,
    }
  // There's no remainder so do_donation totally ignores this Output record
  let output =
    Output {
      address: addr,
      value: value.from_lovelace(999_999_999_999_999_999),
      datum: NoDatum,
      reference_script: None,
    }
  let (final_pool_state, has_remainder) =
    do_donation(pool_state, input_value, assets, order.destination, 2_500_000, output)
  expect !has_remainder
  expect final_pool_state.quantity_a.3rd == 1_001_000_000
  expect final_pool_state.quantity_b.3rd == 1_001_000_000
  True
}