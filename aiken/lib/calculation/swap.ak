use aiken/transaction.{NoDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{
  AssetName, PolicyId, Value, ada_asset_name, ada_policy_id,
}
use calculation/shared.{PoolState} as calc_shared
use shared.{SingletonValue}
use sundae/multisig
use types/order.{Destination, OrderDatum}

/// Compute the amount of token a swap yields; returns the amount of token received, and the remainder to be sent to the user
pub fn swap_takes(
  give_policy_id: PolicyId,
  give_asset_name: AssetName,
  take_policy_id: PolicyId,
  take_asset_name: AssetName,
  pool_give: Int,
  pool_take: Int,
  fees_per_10_thousand: Int,
  protocol_fees: Int,
  order_give: Int,
  input_value: Value,
) -> (Int, Value) {
  // We compute the AMM formula, but 
  // algebraically rearranged to minimize rounding error
  let difference = 10000 - fees_per_10_thousand

  // Compute the amount of token received
  let takes =
    pool_take * order_give * difference / (
      pool_give * 10000 + order_give * difference
    )

  // And then compute the remainder, using the input value minus the given amount
  let remainder =
    input_value
      |> value.add(give_policy_id, give_asset_name, -order_give)
      |> value.add(ada_policy_id, ada_asset_name, -protocol_fees)
  // And then add in the `takes` value
  let out_value = value.add(remainder, take_policy_id, take_asset_name, takes)

  (takes, out_value)
}

/// The amount of the given asset already in the pool
/// The amount of the taken asset in the pool to draw from
/// The fee to charge
/// The amount of token being given up as part of the swap
/// The value coming in from the UTXO
/// Take a pool state, and compute the next pool state
pub fn do_swap(
  pool_state: PoolState,
  input_value: Value,
  destination: Destination,
  protocol_fee: Int,
  offer: SingletonValue,
  min_received: SingletonValue,
  output: Output,
) -> PoolState {
  let (offer_policy_id, offer_asset_name, offer_amt) = offer
  let PoolState {
    quantity_a,
    quantity_b,
    quantity_lp,
    fees_per_10_thousand,
    protocol_fees,
  } = pool_state
  let (a_policy_id, a_asset_name, a_amt) = quantity_a
  let (b_policy_id, b_asset_name, b_amt) = quantity_b

  if offer_policy_id == a_policy_id && offer_asset_name == a_asset_name {
    expect min_received.1st == b_policy_id
    expect min_received.2nd == b_asset_name
    let (takes, out_value) =
      swap_takes(
        offer_policy_id,
        offer_asset_name,
        b_policy_id,
        b_asset_name,
        a_amt,
        b_amt,
        fees_per_10_thousand,
        protocol_fee,
        offer_amt,
        input_value,
      )

    // Check that the output satisfies this swap
    expect output.address == destination.address
    expect output.datum == destination.datum
    expect output.value == out_value
    // Check that mintakes is satisfied
    expect takes >= min_received.3rd
    PoolState {
      quantity_a: (a_policy_id, a_asset_name, a_amt + offer_amt),
      quantity_b: (b_policy_id, b_asset_name, b_amt - takes),
      quantity_lp,
      fees_per_10_thousand,
      protocol_fees: protocol_fees + protocol_fee,
    }
  } else if offer_policy_id == b_policy_id && offer_asset_name == b_asset_name {
    expect min_received.1st == a_policy_id
    expect min_received.2nd == a_asset_name
    let (takes, out_value) =
      swap_takes(
        offer_policy_id,
        offer_asset_name,
        a_policy_id,
        a_asset_name,
        b_amt,
        a_amt,
        fees_per_10_thousand,
        protocol_fee,
        offer_amt,
        input_value,
      )

    expect output.address == destination.address
    expect output.datum == destination.datum
    expect output.value == out_value
    // Check that mintakes is satisfied
    expect takes >= min_received.3rd
    PoolState {
      quantity_a: (a_policy_id, a_asset_name, a_amt - takes),
      quantity_b: (b_policy_id, b_asset_name, b_amt + offer_amt),
      quantity_lp,
      fees_per_10_thousand,
      protocol_fees: protocol_fees + protocol_fee,
    }
  } else {
    fail
  }
}
/// The pool state before processing this swap
/// The value being processed
/// The datum describing the order 

test swap_mintakes_too_high() fail {
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
    fees_per_10_thousand: 5,
    protocol_fees: 2_000_000,
  }
  let input_value =
    value.from_lovelace(14_500_000)
  let swap_offer = (ada.1st, ada.2nd, 10_000_000)
  let swap_min_received = (rberry.1st, rberry.2nd, 10_000_000)
  let order = OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    protocol_fee: 2_500_000,
    destination: Destination {
      address: addr,
      datum: NoDatum,
    },
    order: order.Swap { offer: swap_offer, min_received: swap_min_received, },
    extension: Void,
  }
  let output = Output {
    address: addr,
    value: value.from_lovelace(2_000_000)
      |> value.add(rberry.1st, rberry.2nd, 9_896_088),
    datum: NoDatum,
    reference_script: None,
  }
  let final_pool_state = do_swap(pool_state, input_value, order.destination, order.protocol_fee, swap_offer, swap_min_received, output)
  expect final_pool_state.quantity_a.3rd == 1_000_000_000 + 10_000_000
  expect final_pool_state.quantity_b.3rd == 1_000_000_000 - 9_896_088
  expect final_pool_state.protocol_fees == 4_500_000
  True
}
