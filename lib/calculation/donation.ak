use aiken/transaction.{NoDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{Value, PolicyId, AssetName}
use shared
use sundae/multisig
use types/order.{Destination, OrderDatum}

pub fn do_donation(
  // pool_state: PoolState
  asset_a_policy: PolicyId,
  asset_a_name: AssetName,
  asset_b_policy: PolicyId,
  asset_b_name: AssetName,
  asset_a_reserve: Int,
  asset_b_reserve: Int,

  input_value: Value,
  order: OrderDatum,
  actual_protocol_fee: Int,
  output: Output,
) -> (
  Int, // new_a_reserve
  Int, // new_b_reserve
  Bool, // consume_output
) {
  expect order.Donation { assets } = order.details
  expect assets.1st.1st == asset_a_policy
  expect assets.1st.2nd == asset_a_name
  expect assets.2nd.1st == asset_b_policy
  expect assets.2nd.2nd == asset_b_name
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
    asset_a_reserve + assets.1st.3rd,
    asset_b_reserve + assets.2nd.3rd,
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
  let rberry =
    (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let input_value =
    value.from_lovelace(1_000_000)
      |> value.add(rberry.1st, rberry.2nd, 2_000_000)
  let order =
    OrderDatum {
      pool_ident: None,
      owner: multisig.Signature(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      max_protocol_fee: 2_500_000,
      destination: Destination { address: addr, datum: NoDatum },
      details: order.Donation {
        assets: (
          (ada.1st, ada.2nd, 1_000_000),
          (rberry.1st, rberry.2nd, 2_000_000),
        ),
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
  let (new_asset_a_reserve, new_asset_b_reserve, has_remainder) =
    do_donation(
      #"",
      #"",
      rberry.1st,
      rberry.2nd,
      1_000_000_000,
      2_000_000_000,
      input_value,
      order,
      2_500_000,
      output,
    )
  expect !has_remainder
  expect new_asset_a_reserve == 1_001_000_000
  expect new_asset_b_reserve == 2_002_000_000
  True
}
