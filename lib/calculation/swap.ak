use aiken/math
use calculation/stableswap
use cardano/assets.{AssetName, PolicyId, ada_policy_id}
use cardano/transaction.{Output}
use shared.{SingletonValue}
use types/order.{Destination, Fixed, Self}

/// Calculate the new pool state after performing a swap, and validate that the output is correct according to the order
pub fn do_swap(
  // The pool quantities
  pool_policy_a: PolicyId,
  pool_asset_name_a: AssetName,
  pool_quantity_a: Int,
  pool_policy_b: PolicyId,
  pool_asset_name_b: AssetName,
  pool_quantity_b: Int,
  linear_amplification: Int,
  sum_invariant: Int,
  next_sum_invariant: Int,
  /// The full UTXO for this swap
  input_utxo: Output,
  /// Where the results of the swap need to be paid
  destination: Destination,
  /// The liquidity provider fee to charge for bid (A -> B) or ask (B -> A) orders
  lp_bid_basis_points: Int,
  lp_ask_basis_points: Int,
  /// The protocol fee to charge for bid (A -> B) or ask (B -> A) orders
  protocol_bid_basis_points: Int,
  protocol_ask_basis_points: Int,
  actual_protocol_fee: Int,
  /// The amount of value from the UTXO that is "on offer" for the trade
  offer: SingletonValue,
  /// The minimum token that *must* be received for the order to execute; aka a limit price
  min_received: SingletonValue,
  /// The output to compare against
  output: Output,
  // A continuation to call with the updated pool state
  continuation: fn(Int, Int, Int, Int) -> Bool,
) -> Bool {
  let Output { value: input_value, .. } = input_utxo
  let Output {
    value: output_value,
    address: output_address,
    datum: output_datum,
    ..
  } = output
  // Destructure the offer and the min_received
  let (offer_policy_id, offer_asset_name, offer_amt) = offer
  let (min_received_1st, min_received_2nd, min_received_3rd) = min_received

  // Check which direction we're swapping by comparing the asset IDs
  // This also ensures that we're swapping against the right pool.
  // We check the assets, rather than comparing the pool ident, so that orders can be "pool agnostic"
  // By convention we call an order a "bid" if it swaps asset A for asset B,
  // and an "ask" in the other direction
  let (a_to_b, lp_fee_rate, protocol_fee_rate) =
    if offer_policy_id == pool_policy_a && offer_asset_name == pool_asset_name_a {
      expect min_received_1st == pool_policy_b
      expect min_received_2nd == pool_asset_name_b
      (True, lp_bid_basis_points, protocol_bid_basis_points)
    } else if offer_policy_id == pool_policy_b && offer_asset_name == pool_asset_name_b {
      expect min_received_1st == pool_policy_a
      expect min_received_2nd == pool_asset_name_a
      (False, lp_ask_basis_points, protocol_ask_basis_points)
    } else {
      // If neither is true, then this is just the wrong pool! fail the script
      fail @"wrong pool"
    }

  // Make sure we're paying the result to the correct destination (both the address and the datum),
  // with the correct amount; In the special case where Datum is "Self" (for example for a repeating strategy)
  // use the input datum for validation
  expect
    when destination is {
      Fixed { address, datum } -> and {
          output_address == address,
          output_datum == datum,
        }
      Self -> {
        let Output { address: input_address, datum: input_datum, .. } =
          input_utxo
        and {
          output_address == input_address,
          output_datum == input_datum,
        }
      }
    }

  // We look up how much of the offer token is on the input and output UTxO
  // Because orders can contain extra assets for composibility, we consider this the "surplus"
  let given_input =
    assets.quantity_of(input_value, offer_policy_id, offer_asset_name)
  let given_surplus =
    assets.quantity_of(output_value, offer_policy_id, offer_asset_name)

  // We then do a similar thing for the "taken" token, as a result of the swap
  let taken_surplus =
    assets.quantity_of(input_value, min_received_1st, min_received_2nd)
  let taken_output =
    assets.quantity_of(output_value, min_received_1st, min_received_2nd)

  // Now, if one of the assets is ADA, we need to account for the fact
  // that this value isn't taking part in the trade, so we subtract it
  // from the order gives, or add it to the order takes
  let (given_input, taken_surplus) =
    if offer_policy_id == ada_policy_id {
      (given_input - actual_protocol_fee, taken_surplus)
    } else if min_received_1st == ada_policy_id {
      (given_input, taken_surplus - actual_protocol_fee)
    } else {
      (given_input, taken_surplus)
    }

  // The amount the user gave up is the amount on the input, minus the surplus,
  let order_give = given_input - given_surplus
  // And the amount the user received is the amount on the output, minus the surplus that was already on the input
  let order_takes = taken_output - taken_surplus

  // Make sure the amount given is at most the users offer limit, and the amount taken is greater than the users order minimum
  // Note that we check that the order give amount is the minimum of the offer amount and the given input
  // This ensures that the scooper doesn't "under-execute" the swap; we can swap *less* than the offer amount,
  // but only if that's because there are insufficient tokens on the pool
  // NOTE: we *give up* the rounding protection we added in the base pool; this is partly because stableswaps aren't likely to be
  // used with semi-fungible tokens, and partly because users generally complained about the dust they would get on these swaps
  expect and {
      order_give > 0,
      order_give == math.min(offer_amt, given_input),
      order_takes > 0,
      order_takes >= min_received_3rd,
    }

  // We calculate a percentage fee for liquidity providers and the protocol
  // Note that we assume both fees have already been deducted from the output, so we're
  // calculating what the "pure" takes would have been; we do this in a way that is
  // algebraically safe from rounding errors
  // order_takes = swap_result * (1 - fee)
  // swap_result = order_takes / (1 - fee)
  // swap_result = order_takes / (1 - (fee_bp / 10000))
  // swap_result = order_takes / (10000/10000 - (fee_bp / 10000))
  // swap_result = order_takes / ((10000 - fee_bp) / 10000)
  // swap_result = order_takes * 10000 / (10000 - (lp_fee_rate + protocol_fee_rate))
  let total_fee_rate = lp_fee_rate + protocol_fee_rate
  let difference = 10000 - total_fee_rate
  let swap_result = order_takes * 10000 / difference

  let total_fees = swap_result - order_takes
  let lp_fees =
    if total_fee_rate > 0 {
      total_fees * lp_fee_rate / total_fee_rate
    } else {
      0
    }
  let protocol_fees =
    if total_fee_rate > 0 {
      total_fees * protocol_fee_rate / total_fee_rate
    } else {
      0
    }

  // Note: the protocol_fees aren't taken by the user, but are "taken out" of the pool
  let pool_deduction = order_takes + protocol_fees

  // Enforce that the stableswap invariant holds;
  // this ensures that:
  //  - The order_takes was computed by the scooper correctly
  //  - The new sum invariant was computed by the scooper correctly
  if a_to_b {
    expect
      stableswap.swap_invariant(
        pool_quantity_a + order_give,
        pool_quantity_b - pool_deduction,
        lp_fees + 1,
        linear_amplification,
        sum_invariant,
        next_sum_invariant,
      )
    continuation(
      pool_quantity_a + order_give,
      pool_quantity_b - pool_deduction,
      0,
      protocol_fees,
    )
  } else {
    expect
      stableswap.swap_invariant(
        pool_quantity_b + order_give,
        pool_quantity_a - pool_deduction,
        lp_fees,
        linear_amplification,
        sum_invariant,
        next_sum_invariant,
      )
    continuation(
      pool_quantity_a - pool_deduction,
      pool_quantity_b + order_give,
      protocol_fees,
      0,
    )
  }
}
