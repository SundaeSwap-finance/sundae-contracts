use calculation/shared.{PoolState} as calc_shared
use calculation/swap.{do_swap}
use cardano/address.{Address, VerificationKey}
use cardano/assets
use cardano/transaction.{InlineDatum, NoDatum, Output}
use sundae/multisig
use types/order.{Fixed, OrderDatum}

test swap_mintakes_too_high() fail {
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
  let input_value = assets.from_lovelace(14_500_000)
  let swap_offer = (ada.1st, ada.2nd, 10_000_000)
  let swap_min_received = (rberry.1st, rberry.2nd, 10_000_000)
  let order =
    OrderDatum {
      pool_ident: None,
      owner: multisig.Signature(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      max_protocol_fee: 2_500_000,
      destination: Fixed { address: addr, datum: NoDatum },
      details: order.Swap { offer: swap_offer, min_received: swap_min_received },
      extension: Void,
    }
  let output =
    Output {
      address: addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry.1st, rberry.2nd, 9_994_750),
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
    new_a,
    new_b,
    delta_fee_a,
    delta_fee_b,
  <-
    do_swap(
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd,
      pool_state.quantity_a.3rd,
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd,
      pool_state.quantity_b.3rd,
      200,
      2_000_000_000,
      2_000_005_000,
      input,
      order.destination,
      5,
      5,
      0,
      0,
      2_500_000,
      swap_offer,
      swap_min_received,
      output,
    )
  expect new_a == 1_000_000_000 + 10_000_000
  expect new_b == 1_000_000_000 - 9_896_088
  expect delta_fee_a == 0
  expect delta_fee_b == 0
  True
}

test swap_different_bid_ask() {
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
    assets.from_lovelace(4_500_000)
      |> assets.add(rberry.1st, rberry.2nd, 10_000_000)
  let swap_offer = (rberry.1st, rberry.2nd, 10_000_000)
  let swap_min_received = (ada.1st, ada.2nd, 0)
  let order =
    OrderDatum {
      pool_ident: None,
      owner: multisig.Signature(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      max_protocol_fee: 2_500_000,
      destination: Fixed { address: addr, datum: NoDatum },
      details: order.Swap { offer: swap_offer, min_received: swap_min_received },
      extension: Void,
    }
  let output =
    Output {
      address: addr,
      value: assets.from_lovelace(2_000_000 + 9_899_753),
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
    new_a,
    new_b,
    delta_fee_a,
    delta_fee_b,
  <-
    do_swap(
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd,
      pool_state.quantity_a.3rd,
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd,
      pool_state.quantity_b.3rd,
      200,
      2_000_000_000,
      2_000_100_000,
      input,
      order.destination,
      5,
      100,
      0,
      0,
      2_500_000,
      swap_offer,
      swap_min_received,
      output,
    )
  // If we charged 5%, from the bid fee, it would be `9_994_750`, but because we charged a higher fee, the user got less in return
  expect new_a == 1_000_000_000 - 9_899_753
  expect new_b == 1_000_000_000 + 10_000_000
  expect delta_fee_a == 0
  expect delta_fee_b == 0
  True
}
