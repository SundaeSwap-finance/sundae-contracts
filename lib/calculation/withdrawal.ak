use aiken/math
use cardano/assets.{AssetName, PolicyId, ada_asset_name, ada_policy_id}
use cardano/transaction.{Output}
use shared.{SingletonValue}
use types/order.{Destination, Fixed, Self}
use calculation/stableswap

/// Execute a withdrawal order
///
/// This burns LP tokens, and pays out a portion of the pool in proportion to the percentage
/// of the circulating supply of LP tokens that was burned.
pub fn do_withdrawal(
  // The interim pool state against which we should calculate the withdrawal
  pool_policy_a: PolicyId,
  pool_asset_name_a: AssetName,
  pool_quantity_a: Int,
  pool_policy_b: PolicyId,
  pool_asset_name_b: AssetName,
  pool_quantity_b: Int,
  pool_policy_lp: PolicyId,
  pool_asset_name_lp: AssetName,
  pool_quantity_lp: Int,
  linear_amplification: Int,
  next_sum_invariant: Int,
  input_utxo: Output,
  // The incoming UTXO, useful for returning surplus and handling Self destinations
  amount: SingletonValue,
  // The amount to actually withdraw
  destination: Destination,
  // The destination that the LP tokens (and change) should be sent to
  actual_protocol_fee: Int,
  // The protocol fee to deduct
  output: Output,
  // The output to compare the order execution against, to ensure the right quantity was paid out
  // A continuation; a method to call with the new pool state; more efficient than constructing a new object
  continuation: fn(Int, Int, Int) -> Bool,
) -> Bool {
  let (lp_policy, lp_asset_name, amount) = amount
  let Output { value: input_value, .. } = input_utxo

  // Make sure we're withdrawing from the right pool;
  // if we were to provide LP tokens for a different pool, you could withdraw funds that weren't yours
  expect lp_policy == pool_policy_lp
  expect lp_asset_name == pool_asset_name_lp

  // Calculate the amount of LP tokens to burn; because the outcome of a previous order in the chain
  // may be uncertain, we allow some flexibility here, and the amount in the order just specifies a
  // maximum to take. Also note, we specify an amount at all so that you can have things like
  // time-release withdrawals, where you withdraw a small amount across multiple steps in a chain.
  let withdrawn =
    math.min(amount, assets.quantity_of(input_value, lp_policy, lp_asset_name))

  // We calculate the asset_a and asset_b to withdraw, namely the percentage of each asset
  // that corresponds to the percentage of LP token burned.  Note we multiply first to minimize precision loss.
  let withdrawn_a = withdrawn * pool_quantity_a / pool_quantity_lp
  let withdrawn_b = withdrawn * pool_quantity_b / pool_quantity_lp

  // Calculate the remainder that should be directed to the destination:
  // We start with whatever we got in on the input, deduct the LP tokens we're burning
  // the protocol fee, and then add in the amounts that we withdrew
  // OPTIMIZATION: I wonder if there's a more efficient implementation that can do this in just one pass
  let remainder =
    input_value
      |> assets.add(lp_policy, lp_asset_name, -withdrawn)
      |> assets.add(ada_policy_id, ada_asset_name, -actual_protocol_fee)
      |> assets.add(pool_policy_a, pool_asset_name_a, withdrawn_a)
      |> assets.add(pool_policy_b, pool_asset_name_b, withdrawn_b)

  // Make sure we're paying the result to the correct destination (both the address and the datum),
  // with the correct amount; In the special case where Datum is "Self" (for example for a repeating strategy)
  // use the input datum for validation
  expect output.value == remainder
  expect
    when destination is {
      Fixed { address, datum } -> and {
          output.address == address,
          output.datum == datum,
        }
      Self -> {
        let Output { address: input_address, datum: input_datum, .. } =
          input_utxo
        and {
          output.address == input_address,
          output.datum == input_datum,
        }
      }
    }

  // Now, we make sure that the provided next_sum_invariant is correct
  // This is because future orders within this batch might depend on the updated invariant
  let new_pool_a = pool_quantity_a - withdrawn_a
  let new_pool_b = pool_quantity_b - withdrawn_b

  // TODO: do I need to check some relationship between withdrawn, and the delta between sum_invariant and next_sum_invariant?
  expect stableswap.liquidity_invariant(new_pool_a, new_pool_b, linear_amplification, next_sum_invariant)

  // Return the final pool state
  continuation(
    new_pool_a,
    new_pool_b,
    pool_quantity_lp - withdrawn,
  )
}
