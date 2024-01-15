use aiken/transaction.{NoDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{
  AssetName, PolicyId, Value, ada_asset_name, ada_policy_id,
}
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
  actual_protocol_fee: Int,
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
  
  // And then compute the desired output
  // which is the input value, minus the tokens being swapped, minus the protocol fee
  // plus the asset that we get as part of the swap
  let out_value =
    input_value
      |> value.add(give_policy_id, give_asset_name, -order_give)
      |> value.add(ada_policy_id, ada_asset_name, -actual_protocol_fee)
      |> value.add(take_policy_id, take_asset_name, takes)

  (takes, out_value)
}

/// The amount of the given asset already in the pool
/// The amount of the taken asset in the pool to draw from
/// The fee to charge
/// The amount of token being given up as part of the swap
/// The value coming in from the UTXO
/// Take a pool state, and compute the next pool state
pub fn do_swap(
  // pool_state: PoolState, unpacked for performance
  asset_a_policy: PolicyId,
  asset_a_name: AssetName,
  asset_b_policy: PolicyId,
  asset_b_name: AssetName,
  asset_a_reserve: Int,
  asset_b_reserve: Int,

  input_value: Value,
  destination: Destination,
  fees_per_10_thousand: Int,
  actual_protocol_fee: Int,
  offer: SingletonValue,
  min_received: SingletonValue,
  output: Output,
) -> (
  // PooLState, unpacked for performance
  Int, // new_asset_a_reserves
  Int, // new_asset_b_reserves
) {
  let (offer_policy_id, offer_asset_name, offer_amt) = offer

  if offer_policy_id == asset_a_policy && offer_asset_name == asset_a_name {
    expect min_received.1st == asset_b_policy
    expect min_received.2nd == asset_b_name
    let (takes, out_value) =
      swap_takes(
        offer_policy_id,
        offer_asset_name,
        asset_b_policy,
        asset_b_name,
        asset_a_reserve,
        asset_b_reserve,
        fees_per_10_thousand,
        actual_protocol_fee,
        offer_amt,
        input_value,
      )

    // Check that the output satisfies this swap
    expect output.address == destination.address
    expect output.datum == destination.datum
    expect output.value == out_value
    // Check that mintakes is satisfied
    expect takes >= min_received.3rd
    (
      asset_a_reserve + offer_amt,
      asset_b_reserve - takes,
    )
  } else if offer_policy_id == asset_b_policy && offer_asset_name == asset_b_name {
    expect min_received.1st == asset_a_policy
    expect min_received.2nd == asset_a_name
    let (takes, out_value) =
      swap_takes(
        offer_policy_id,
        offer_asset_name,
        // We're receiving asset A
        asset_a_policy,
        asset_a_name,
        // NOTE: the reserves are swapped! We're giving asset B, and taking asset A
        asset_b_reserve,
        asset_a_reserve,
        fees_per_10_thousand,
        actual_protocol_fee,
        offer_amt,
        input_value,
      )

    expect output.address == destination.address
    expect output.datum == destination.datum
    expect output.value == out_value
    // Check that mintakes is satisfied
    expect takes >= min_received.3rd
    (
      asset_a_reserve - takes,
      asset_b_reserve + offer_amt,
    )
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
  let input_value =
    value.from_lovelace(14_500_000)
  let swap_offer = (ada.1st, ada.2nd, 10_000_000)
  let swap_min_received = (rberry.1st, rberry.2nd, 10_000_000)
  let order = OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2_500_000,
    destination: Destination {
      address: addr,
      datum: NoDatum,
    },
    details: order.Swap { offer: swap_offer, min_received: swap_min_received, },
    extension: Void,
  }
  let output = Output {
    address: addr,
    value: value.from_lovelace(2_000_000)
      |> value.add(rberry.1st, rberry.2nd, 9_896_088),
    datum: NoDatum,
    reference_script: None,
  }
  let (new_reserve_a, new_reserve_b) = do_swap(
    #"",
    #"",
    rberry.1st,
    rberry.2nd,
    1_000_000_000,
    1_000_000_000,
    input_value,
    order.destination,
    5,
    2_500_000,
    swap_offer,
    swap_min_received,
    output,
  )
  expect new_reserve_a == 1_000_000_000 + 10_000_000
  expect new_reserve_b == 1_000_000_000 - 9_896_088
  True
}
