use aiken/transaction.{Output}
use aiken/transaction/value.{
  AssetName, PolicyId, Value, ada_asset_name, ada_policy_id,
}
use shared.{SingletonValue}
use types/order.{Destination, Fixed, Self}

/// Compute the amount of token a swap yields; returns the amount of token received, and the remainder to be sent to the user
///
/// As in other places, we have lots of parameters to avoid destructuring
/// TODO: it'd be nice to support partial fills, so long as the ratio between `order_give` and `min_takes` is preserved 
pub fn swap_takes(
  // The policy ID of the token being given up by the user
  give_policy_id: PolicyId,
  // The asset name of the token being given up by the user
  give_asset_name: AssetName,
  // The policy ID of the token that should be *received* as a result of the swap
  take_policy_id: PolicyId,
  // The asset name of the token that should be *received* as a result of the swap
  take_asset_name: AssetName,
  // The quantity of the given token that the pool currently has
  pool_give: Int,
  // The quantity of the token that should be given to the user that the pool already has
  pool_take: Int,
  // The percentage fee collected on behalf of the liquidity providers, represented as the numerator of a fraction over 10,000 (commonly referred to as basis points)
  // already adjusted for bid or ask
  fees_per_10_thousand: Int,
  // The ADA fee to deduct as part of the protocol fee
  actual_protocol_fee: Int,
  // The amount of the given token that the user has offered to swap
  order_give: Int,
  // The input value on the order UTXO, to calculate change
  input_value: Value,
  // A continuation once we've calculated the output; this is more efficient that constructing a tuple, especially with backpassing syntax
  continuation: fn(Int, Value) -> Bool,
) -> Bool {
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

  continuation(takes, out_value)
}

/// Calculate the new pool state after performing a swap, and validate that the output is correct according to the order
pub fn do_swap(
  // The pool quantities
  pool_policy_a: PolicyId,
  pool_asset_name_a: AssetName,
  pool_quantity_a: Int,
  pool_policy_b: PolicyId,
  pool_asset_name_b: AssetName,
  pool_quantity_b: Int,
  /// The full UTXO for this swap
  input_utxo: Output,
  /// Where the results of the swap need to be paid
  destination: Destination,
  /// The liquidity provider fee to charge for bid (A -> B) or ask (B -> A) orders
  bid_fees_per_10_thousand: Int,
  ask_fees_per_10_thousand: Int,
  actual_protocol_fee: Int,
  /// The amount of value from the UTXO that is "on offer" for the trade
  offer: SingletonValue,
  /// The minimum token that *must* be received for the order to execute; aka a limit price
  min_received: SingletonValue,
  /// The output to compare against
  output: Output,
  // A continuation to call with the updated pool state
  continuation: fn(Int, Int) -> Bool,
) -> Bool {
  let Output { value: input_value, .. } = input_utxo
  // Destructure the pool state
  // TODO: it'd make the code nightmarish, but things would be way more efficient to just always pass these values destructured as parameters...
  let (offer_policy_id, offer_asset_name, offer_amt) = offer

  // let (a_policy_id, a_asset_name, a_amt) = pool_quantity_a
  // let (b_policy_id, b_asset_name, b_amt) = pool_quantity_b

  // Make sure we're paying the result to the correct destination (both the address and the datum),
  // with the correct amount; In the special case where Datum is "Self" (for example for a repeating strategy)
  // use the input datum for validation
  expect when destination is {
    Fixed { address, datum } -> {
      and {
        output.address == address,
        output.datum == datum
      }
    }
    Self -> {
      let Output { address: input_address, datum: input_datum, .. } = input_utxo
      and {
        output.address == input_address,
        output.datum == input_datum
      }
    }
  }

  // There are two symmetric cases, depending on whether you're giving order A or order B
  // If there's a clever way to write this as one branch it might be clearer to read
  if offer_policy_id == pool_policy_a && offer_asset_name == pool_asset_name_a {
    // Make sure we're actually swapping against a pool with the correct assets
    // We do this, instead of checking the pool ident, to allow orders to be agnostic and execute against an arbitrary pool
    expect min_received.1st == pool_policy_b
    expect min_received.2nd == pool_asset_name_b
    // Compute the actual takes / change
    let takes, out_value <-
      swap_takes(
        offer_policy_id,
        offer_asset_name,
        pool_policy_b,
        pool_asset_name_b,
        pool_quantity_a,
        pool_quantity_b,
        bid_fees_per_10_thousand,
        actual_protocol_fee,
        offer_amt,
        input_value,
      )

    // Make sure the correct value (including change) carries through to the output
    expect output.value == out_value

    // And check that the min_received (the lowest amount of tokens the user is willing to receive, aka a limit price)
    // is satisfied
    expect takes >= min_received.3rd
    continuation(
      pool_quantity_a + offer_amt,
      pool_quantity_b - takes,
    )
  } else if offer_policy_id == pool_policy_b && offer_asset_name == pool_asset_name_b {
    // This code is a mirror of the above, so should be scrutinized extra carefully during audit;
    // it's easy to make a mistake when copy-pasting. If there's an easier way to express this symmetry that would be good too
    expect min_received.1st == pool_policy_a
    expect min_received.2nd == pool_asset_name_a
    let takes, out_value <-
      swap_takes(
        offer_policy_id,
        offer_asset_name,
        pool_policy_a,
        pool_asset_name_a,
        pool_quantity_b,
        pool_quantity_a,
        ask_fees_per_10_thousand,
        actual_protocol_fee,
        offer_amt,
        input_value,
      )

    // Make sure the correct value (including change) carries through to the output
    expect output.value == out_value
    
    // Check that mintakes is satisfied
    expect takes >= min_received.3rd
    continuation(
      pool_quantity_a - takes,
      pool_quantity_b + offer_amt,
    )
  } else {
    // If neither of the assets match what's being offered, fail the transaction, the scooper has incorrectly chosen an order that can't be satisfied
    fail
  }
}
