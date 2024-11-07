use calculation/donation.{do_donation}
use calculation/shared.{PoolState} as calc_shared
use cardano/address.{Address, VerificationKey}
use cardano/assets
use cardano/transaction.{InlineDatum, NoDatum, Output}
use sundae/multisig
use types/order.{Fixed, OrderDatum}

test donation() {
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
  let pool_state =
    PoolState {
      quantity_a: (#"", #"", 1_000_000_000),
      quantity_b: (rberry.1st, rberry.2nd, 1_000_000_000),
      quantity_lp: (lp.1st, lp.2nd, 1_000_000_000),
    }
  let input_value =
    assets.from_lovelace(3_500_000)
      |> assets.add(rberry.1st, rberry.2nd, 1_000_000)
  let assets =
    ((ada.1st, ada.2nd, 1_000_000), (rberry.1st, rberry.2nd, 1_000_000))
  let order =
    OrderDatum {
      pool_ident: None,
      owner: multisig.Signature(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      max_protocol_fee: 2_500_000,
      destination: Fixed { address: addr, datum: NoDatum },
      details: order.Donation { assets },
      extension: Void,
    }
  let input =
    Output {
      address: addr,
      value: input_value,
      datum: InlineDatum(order),
      reference_script: None,
    }
  // There's no remainder so do_donation totally ignores this Output record
  let output =
    Output {
      address: addr,
      value: assets.from_lovelace(999_999_999_999_999_999),
      datum: NoDatum,
      reference_script: None,
    }

  let
    new_a,
    new_b,
    has_remainder,
  <-
    do_donation(
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd,
      pool_state.quantity_a.3rd,
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd,
      pool_state.quantity_b.3rd,
      input,
      assets,
      order.destination,
      2_500_000,
      output,
    )
  expect !has_remainder
  expect new_a == 1_001_000_000
  expect new_b == 1_001_000_000
  True
}

test donation_with_remainder() {
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
  let pool_state =
    PoolState {
      quantity_a: (#"", #"", 1_000_000_000),
      quantity_b: (rberry.1st, rberry.2nd, 1_000_000_000),
      quantity_lp: (lp.1st, lp.2nd, 1_000_000_000),
    }
  let input_value =
    assets.from_lovelace(3_500_000)
      |> assets.add(rberry.1st, rberry.2nd, 1_000_000)
  let assets = ((ada.1st, ada.2nd, 0), (rberry.1st, rberry.2nd, 1_000_000))
  let order =
    OrderDatum {
      pool_ident: None,
      owner: multisig.Signature(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      max_protocol_fee: 2_500_000,
      destination: Fixed { address: addr, datum: NoDatum },
      details: order.Donation { assets },
      extension: Void,
    }
  let input =
    Output {
      address: addr,
      value: input_value,
      datum: InlineDatum(order),
      reference_script: None,
    }
  let output =
    Output {
      address: addr,
      value: assets.from_lovelace(1_000_000),
      datum: NoDatum,
      reference_script: None,
    }

  let
    new_a,
    new_b,
    has_remainder,
  <-
    do_donation(
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd,
      pool_state.quantity_a.3rd,
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd,
      pool_state.quantity_b.3rd,
      input,
      assets,
      order.destination,
      2_500_000,
      output,
    )
  expect has_remainder
  expect new_a == 1_000_000_000
  expect new_b == 1_001_000_000
  True
}

test donation_with_wrong_remainder() fail {
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
  let pool_state =
    PoolState {
      quantity_a: (#"", #"", 1_000_000_000),
      quantity_b: (rberry.1st, rberry.2nd, 1_000_000_000),
      quantity_lp: (lp.1st, lp.2nd, 1_000_000_000),
    }
  let input_value =
    assets.from_lovelace(3_500_000)
      |> assets.add(rberry.1st, rberry.2nd, 1_000_000)
  let assets = ((ada.1st, ada.2nd, 0), (rberry.1st, rberry.2nd, 1_000_000))
  let order =
    OrderDatum {
      pool_ident: None,
      owner: multisig.Signature(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      max_protocol_fee: 2_500_000,
      destination: Fixed { address: addr, datum: NoDatum },
      details: order.Donation { assets },
      extension: Void,
    }
  let input =
    Output {
      address: addr,
      value: input_value,
      datum: InlineDatum(order),
      reference_script: None,
    }
  let output =
    Output {
      address: addr,
      value: assets.from_lovelace(123),
      datum: NoDatum,
      reference_script: None,
    }

  let
    new_a,
    new_b,
    has_remainder,
  <-
    do_donation(
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd,
      pool_state.quantity_a.3rd,
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd,
      pool_state.quantity_b.3rd,
      input,
      assets,
      order.destination,
      2_500_000,
      output,
    )
  expect has_remainder
  expect new_a == 1_000_000_000
  expect new_b == 1_001_000_000
  True
}
