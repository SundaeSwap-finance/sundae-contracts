use aiken/transaction.{Output}
use aiken/transaction/value.{
  AssetName, PolicyId, Value, ada_asset_name, ada_policy_id,
}
use calculation/shared.{PoolState} as calc_shared
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
  order_offer: Int,
  // The input value on the order UTXO, to calculate change
  input_value: Value,
  // The output value on the order UTXO, to calculate the real order give amount
  output_value: Value,
) -> (Int, Int, Value) {
  // We compute the AMM formula, but 
  // algebraically rearranged to minimize rounding error
  // Also, we want to ensure that:
  //  - The full order is filled (the user gets the most `takes` for the tokens they've offered)
  //  - The user couldnot have gotten those `takes` any cheaper
  //
  // For example, consider a pool with 50 XDIAMOND and 20,000 ADA
  // If a user offers 799 ADA for the swap, the AMM formula calculates roughly 1.9 XDIAMOND
  // We have to round that down, because we can't give out fractional units, and rounding up would allow the pool to be drained
  // But, 799 is dramatically overpaying for 1 XDIAMOND, as they could have gotten 1 XDIAMOND with as little as 409.391440 ADA.
  //
  // To ensure this, we let the scooper calculate the right amount off-chain, then verify it on-chain

  let difference = 10000 - fees_per_10_thousand

  // Check how much the user is actually giving up, by comparing the difference between the input and output values
  let order_give = value.quantity_of(input_value, give_policy_id, give_asset_name) - value.quantity_of(output_value, give_policy_id, give_asset_name)
  let order_give = if give_policy_id == ada_policy_id { order_give - actual_protocol_fee } else { order_give }
  // TODO: is this neccesary? or will one of the checks below *always* fail?
  expect order_give > 0

  // The AMM formula would be
  // let takes =
  //  pool_take * difference * order_give / (
  //    pool_give * 10000 + order_give * difference
  //  )
  // Since we're going to calculate it a few times, we pull out some common terms for reuse
  let pool_take_times_difference = pool_take * difference
  let pool_give_10k = pool_give * 10000

  // Now, we calculate the amount of token the user receives based on the amount they actually gave
  let give_takes_numerator = pool_take_times_difference * order_give
  let give_takes_denominator = pool_give_10k + order_give * difference
  let give_takes = give_takes_numerator / give_takes_denominator
  // Make sure they're getting at least one token!
  expect give_takes > 0

  // If the amount actually given by the user is less than the amount offered by the user,
  // then we need to make sure they would have received the same amount, and this amount is just
  // more efficient.
  // Otherwise, they have to be equal (i.e. the scooper can not force the user to give up more than they offered)
  expect if order_give < order_offer {
    let offer_takes_numerator = pool_take_times_difference * order_offer
    let offer_takes_denominator = pool_give_10k + order_offer * difference
    let offer_takes = offer_takes_numerator / offer_takes_denominator
    offer_takes == give_takes
  } else {
    order_give == order_offer
  }

  // We need to make sure that the user is getting the most efficient swap
  // So we check what they would receive with one less unit of the token they're giving
  // This would be
  // 
  // let takes =
  //   pool_take * difference * (order_give - 1) / (
  //     pool_give * 10000 + (order_give - 1) * difference
  //   )
  //
  //  We can expand this out, algebraically, and simplify to reuse things we've already calculated
  // 
  let one_less =
    (give_takes_numerator - pool_take_times_difference) / (
      give_takes_denominator - difference
    )
  // And that *must* give strictly less takes; this means that the user is getting the most efficient order possible
  expect one_less < give_takes
  
  // And then compute the desired output
  // which is the input value, minus the tokens being swapped, minus the protocol fee
  // plus the asset that we get as part of the swap
  let out_value =
    input_value
      |> value.add(give_policy_id, give_asset_name, -order_give)
      |> value.add(ada_policy_id, ada_asset_name, -actual_protocol_fee)
      |> value.add(take_policy_id, take_asset_name, give_takes)

  (give_takes, order_give, out_value)
}

/// Calculate the new pool state after performing a swap, and validate that the output is correct according to the order
pub fn do_swap(
  pool_state: PoolState,
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
) -> PoolState {
  let Output { value: input_value, .. } = input_utxo
  let Output { value: output_value, address: output_address, datum: output_datum, .. } = output
  // Destructure the pool state
  // TODO: it'd make the code nightmarish, but things would be way more efficient to just always pass these values destructured as parameters...
  let (offer_policy_id, offer_asset_name, offer_amt) = offer
  let PoolState {
    quantity_a,
    quantity_b,
    quantity_lp,
  } = pool_state
  let (a_policy_id, a_asset_name, a_amt) = quantity_a
  let (b_policy_id, b_asset_name, b_amt) = quantity_b

  // Make sure we're paying the result to the correct destination (both the address and the datum),
  // with the correct amount; In the special case where Datum is "Self" (for example for a repeating strategy)
  // use the input datum for validation
  expect when destination is {
    Fixed { address, datum } -> {
      and {
        output_address == address,
        output_datum == datum
      }
    }
    Self -> {
      let Output { address: input_address, datum: input_datum, .. } = input_utxo
      and {
        output_address == input_address,
        output_datum == input_datum
      }
    }
  }

  // There are two symmetric cases, depending on whether you're giving order A or order B
  // If there's a clever way to write this as one branch it might be clearer to read
  if offer_policy_id == a_policy_id && offer_asset_name == a_asset_name {
    // Make sure we're actually swapping against a pool with the correct assets
    // We do this, instead of checking the pool ident, to allow orders to be agnostic and execute against an arbitrary pool
    expect min_received.1st == b_policy_id
    expect min_received.2nd == b_asset_name
    // Compute the actual takes / change
    let (takes, gives, out_value) =
      swap_takes(
        offer_policy_id,
        offer_asset_name,
        b_policy_id,
        b_asset_name,
        a_amt,
        b_amt,
        bid_fees_per_10_thousand,
        actual_protocol_fee,
        offer_amt,
        input_value,
        output_value,
      )

    // Make sure the correct value (including change) carries through to the output
    expect output_value == out_value

    // And check that the min_received (the lowest amount of tokens the user is willing to receive, aka a limit price)
    // is satisfied
    expect takes >= min_received.3rd
    PoolState {
      quantity_a: (a_policy_id, a_asset_name, a_amt + gives),
      quantity_b: (b_policy_id, b_asset_name, b_amt - takes),
      quantity_lp,
    }
  } else if offer_policy_id == b_policy_id && offer_asset_name == b_asset_name {
    // This code is a mirror of the above, so should be scrutinized extra carefully during audit;
    // it's easy to make a mistake when copy-pasting. If there's an easier way to express this symmetry that would be good too
    expect min_received.1st == a_policy_id
    expect min_received.2nd == a_asset_name
    let (takes, gives, out_value) =
      swap_takes(
        offer_policy_id,
        offer_asset_name,
        a_policy_id,
        a_asset_name,
        b_amt,
        a_amt,
        ask_fees_per_10_thousand,
        actual_protocol_fee,
        offer_amt,
        input_value,
        output_value
      )

    // Make sure the correct value (including change) carries through to the output
    expect output_value == out_value
    
    // Check that mintakes is satisfied
    expect takes >= min_received.3rd
    PoolState {
      quantity_a: (a_policy_id, a_asset_name, a_amt - takes),
      quantity_b: (b_policy_id, b_asset_name, b_amt + gives),
      quantity_lp,
    }
  } else {
    // If neither of the assets match what's being offered, fail the transaction, the scooper has incorrectly chosen an order that can't be satisfied
    fail
  }
}
