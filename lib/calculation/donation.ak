use aiken/transaction.{Output}
use aiken/transaction/value.{ada_policy_id, ada_asset_name, PolicyId, AssetName}
use shared.{SingletonValue}
use types/order.{Destination, Fixed, Self}

/// A donation describes an amount of assets to deposit into the pool, receiving nothing in return (except for the extra change on the UTXO).
/// Because every LP token holder has an entitlement to a percentage of the assets in the pool, the donation is distributed to all LP token holders
/// pro-rata.
/// An end-user might have little use for something like this, but it becomes a powerful primitive for other protocols to create automated
/// incentive programs on chain.  For example, revenue from activity on chain can be donated to the project tokens pool as a liquidity incentive.
// 
// Calculates the new pool state, and whether to consume this output or not
pub fn do_donation(
  /// The pool state as a result of processing all orders before this one
  pool_policy_a: PolicyId,
  pool_asset_name_a: AssetName,
  pool_quantity_a: Int,
  pool_policy_b: PolicyId,
  pool_asset_name_b: AssetName,
  pool_quantity_b: Int,
  /// The total quantity of tokens on this particular input
  input_utxo: Output,
  /// The amounts of each asset being donated
  assets: (SingletonValue, SingletonValue),
  /// The destination (address + datum) that any *change* should be sent
  destination: Destination,
  /// The actual protocol fee that needs to be charged to each user (i.e. baseFee / numOrders + simpleFee)
  /// TODO: this is actually unused, and fixed in a branch
  actual_protocol_fee: Int,
  /// The next output in the list; If there is any change left over from the donation, we would expect this to be that change
  /// If there is no change leftover, this output is ignored and used for the next order
  output: Output,
  // A continuation to call with the new pool balances, and whether to skip an output; more efficient than constructing an object each time
  continuation: fn(Int, Int, Bool) -> Bool
) -> Bool {
  let Output { value: input_value, .. } = input_utxo
  let ((asset_a_policy_id, asset_a_asset_name, asset_a_qty), (asset_b_policy_id, asset_b_asset_name, asset_b_qty)) = assets
  // Make sure we're actually donating the pool assets; this is to prevent setting
  // poolIdent to None, and then filling the pool UTXO with garbage tokens and eventually locking it
  expect asset_a_policy_id == pool_policy_a
  expect asset_a_asset_name == pool_asset_name_a
  expect asset_b_policy_id == pool_policy_b
  expect asset_b_asset_name == pool_asset_name_b
  // Compute however much of the UTXO value is *left over* after deducting the donation amount from it; If nonzero, this will need to be returned to the user
  let remainder =
    input_value
      |> value.add(ada_policy_id, ada_asset_name, -actual_protocol_fee)
      |> value.add(asset_a_policy_id, asset_a_asset_name, -asset_a_qty)
      |> value.add(asset_b_policy_id, asset_b_asset_name, -asset_b_qty)

  let has_remainder = remainder != value.zero()
  // If we have a remainder, then we need to check the details of the output; this awkward structure
  // is so that we can do a few expects without having to return a value right away
  expect or {
    !has_remainder,
    // Make sure we're paying the result to the correct destination (both the address and the datum),
    // with the correct amount; In the special case where Datum is "Self" (for example for a repeating strategy)
    // use the input datum for validation
    and {
      output.value == remainder,
      when destination is {
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
      },
    }
  }
  
  // Continue with the new pool state, which is exactly the old pool state, plus the 
  // quantities of assets donated, plus the protocol fee!
  // We also returned whether we had a remainder or not, so the calling function knows
  // whether to consume the output they gave us or not.
  continuation(
    pool_quantity_a + assets.1st.3rd,
    pool_quantity_b + assets.2nd.3rd,
    has_remainder,
  )
}
