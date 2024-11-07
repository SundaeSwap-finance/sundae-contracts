use aiken/collection/dict.{Dict}
use aiken/crypto.{Blake2b_256, Hash}
use calculation/process.{validate_pool_id}
use calculation/shared.{check_and_set_unique,
  unsafe_fast_index_skip_with_tail} as calculation_shared
use calculation/withdrawal
use cardano/assets.{AssetName, PolicyId, Value}
use cardano/transaction.{Input, Output}
use shared.{AssetClass, Ident, datum_of, is_script, pool_lp_name}
use types/order.{Destination, Order, OrderDatum, SignedStrategyExecution}

/// Construct the initial pool state for processing a set of orders
pub fn condition_pool_input_to_state(
  pool_token_policy: PolicyId,
  assets: (AssetClass, AssetClass),
  protocol_fees: Int,
  identifier: Ident,
  circulating_lp: Int,
  input: Output,
  continuation: fn(
    PolicyId,
    AssetName,
    Int,
    PolicyId,
    AssetName,
    Int,
    PolicyId,
    AssetName,
    Int,
    Int,
  ) ->
    Bool,
) -> Bool {
  let (asset_a, asset_b) = assets
  let (asset_a_policy_id, asset_a_name) = asset_a
  let (asset_b_policy_id, asset_b_name) = asset_b
  // If asset_a is ADA, then we need to not consider the protocol fees as part of this
  // We don't have to check asset_b, because assets are guaranteed to be in lexicographical order.
  let min_utxo =
    if asset_a_policy_id == assets.ada_policy_id {
      protocol_fees
    } else {
      0
    }

  // Then construct the pool state. We include the assets here, instead of just the reserves, so we can check the values of each order
  // TODO: we could potentially save quite a bit by not passing around this object, and passing around a lot of parameters instead...
  continuation(
    asset_a_policy_id,
    asset_a_name,
    assets.quantity_of(input.value, asset_a_policy_id, asset_a_name) - min_utxo,
    asset_b_policy_id,
    asset_b_name,
    assets.quantity_of(input.value, asset_b_policy_id, asset_b_name),
    pool_token_policy,
    pool_lp_name(identifier),
    circulating_lp,
    protocol_fees,
  )
}

/// Process a single order, comparing it to the output to ensure it was executed faithfully, and returning the new pool state
///
/// Most of the parameters here are for performance reasons, to avoid destructuring objects, since thats very expensive
pub fn process_withdrawal_order(
  // The pool state as of the time the order is executed; If we process multiple orders, this gets passed through each time
  pool_policy_a: PolicyId,
  pool_asset_name_a: AssetName,
  pool_quantity_a: Int,
  pool_policy_b: PolicyId,
  pool_asset_name_b: AssetName,
  pool_quantity_b: Int,
  pool_policy_lp: PolicyId,
  pool_asset_name_lp: AssetName,
  pool_quantity_lp: Int,
  // The input being processed
  input: Output,
  // The details of the order to execute, such as whether it's a swap, the limit, etc.
  details: Order,
  // The max protocol fee that *can* be charged from the order; depending on how big the batch size is, some may be returned as a rebate, but this lets the user limit the maximum that gets charged in case the protocol fees in the settings change
  max_protocol_fee: Int,
  // The destination where the result of the order must be sent; useful for chaining transactions, as it lets you specify a datum for the output
  destination: Destination,
  // The base fee, divided among all the participants in the scoop
  amortized_base_fee: Int,
  // The amount to charge for simple vs strategy orders, taken from the settings
  simple_fee: Int,
  // A list of outputs, so we can destructure the next output
  // TODO: we can probably avoid returning the outputs, and just return a boolean
  outputs: List<Output>,
  // A continuation to call with the next pool state and the list of outputs; this is more efficient than constructing an object and tuples
  continuation: fn(Int, Int, Int, List<Output>) -> Bool,
) -> Bool {
  // Returns the updated pool state, the correct list of outputs to resume from, and total fee charged by the order
  expect order.Withdrawal(amount) = details
  expect [output, ..rest_outputs] = outputs
  // Make sure the scooper can only take up to the max fee the user has agreed to
  // (See above)
  let fee = amortized_base_fee + simple_fee
  expect max_protocol_fee >= fee
  // Calculate and validate the result of a withdrawal
  let
    new_a,
    new_b,
    new_lp,
  <-
    withdrawal.do_withdrawal(
      pool_policy_a,
      pool_asset_name_a,
      pool_quantity_a,
      pool_policy_b,
      pool_asset_name_b,
      pool_quantity_b,
      pool_policy_lp,
      pool_asset_name_lp,
      pool_quantity_lp,
      input,
      amount,
      destination,
      fee,
      output,
    )
  continuation(new_a, new_b, new_lp, rest_outputs)
}

/// Recursively process all orders in the correct order
/// There's a lot of parameters here, mostly for efficiency (though with some redundancies being removed in another branch)
pub fn process_withdrawal_orders(
  // The pool identifier we're processing, so we can check the order if it has a specific pool
  this_pool_ident: Ident,
  // The datums in the witness set, in case we need to lookup a non-inline datum
  datums: Dict<Hash<Blake2b_256, Data>, Data>,
  // The initial / current pool state, passed recursively as we process each order
  pool_policy_a: PolicyId,
  pool_asset_name_a: AssetName,
  pool_quantity_a: Int,
  pool_policy_b: PolicyId,
  pool_asset_name_b: AssetName,
  pool_quantity_b: Int,
  pool_policy_lp: PolicyId,
  pool_asset_name_lp: AssetName,
  pool_quantity_lp: Int,
  // The list of remaining indices into the inputs, specifying which orders to process
  input_order: List<(Int, Option<SignedStrategyExecution>, Int)>,
  // The protocol base fee, split across each order
  amortized_base_fee: Int,
  // The simple and strategy fees from the settings datum
  simple_fee: Int,
  // The previous order we processed, to check if we need to restart the loop; TODO: we actually pass +1 from this, and i'm not sure how to explain why we do this...
  prev_index: Int,
  // *all* inputs on the transaction, in case we need to start over from the beginning (i.e. wrap around)
  all_inputs: List<Input>,
  // Just the remaining inputs in the list, in case it's more efficient to keep walking from here
  remaining_inputs: List<Input>,
  // The list of remaining outputs to compare the orders against; usually we pass the `tail` of this list recursively, but in the case of donations with no change, we pass outputs through unchanged
  outputs: List<Output>,
  // A number that, when interpreted as a bit flag, indicates which orders we've already processed; used to check if an order is processed more than once (see InputSorting.md)
  uniqueness_flag: Int,
  // A continuation to call with the final pool state; more efficient than constructing tuples / objects
  continuation: fn(Int, Int, Int) -> Bool,
) -> Bool {
  // Returns the final pool state, and the count of each order type
  // The main "pump" of the recursive loop is the input_order, which is a set of indices into the inputs list
  // specified by the scooper for the order to process each order in.
  // Once we've reached the end of the list, we can return, but otherwise
  when input_order is {
    [] -> continuation(pool_quantity_a, pool_quantity_b, pool_quantity_lp)
    [(idx, _, _), ..rest] -> {
      // First, it's important to check that each order is processed only once;
      // This is quite subtle, so check InputSorting.md for a full explanation
      let next_uniqueness_flag = check_and_set_unique(uniqueness_flag, idx)

      // Then, we identify where to find the inputs; in particular, to avoid "starting from the beginning" every single time
      // when indices are monotonic through the list, we can just continue to advance through the list
      // so, all_inputs will always contain the full list of inputs
      // while remaining_inputs will just contain the ones "after" the last one we processed.
      // So, here, we check if we can continue down this path, or if we need to start from the beginning again
      let next_input_list =
        if idx >= prev_index {
          unsafe_fast_index_skip_with_tail(remaining_inputs, idx - prev_index)
        } else {
          unsafe_fast_index_skip_with_tail(all_inputs, idx)
        }

      expect [input_to_process, ..rest_of_input_list] = next_input_list
      let Input { output: order, .. } = input_to_process

      // It's important that we fail if we ever try to process a UTXO from a wallet address
      // This is a bit unfortunate, because it means we can't support processing orders directly out of a users wallet
      // but is important, because we rely on this to check that every order is processed.
      // If we didn't do this check, a scooper could include a UTXO from their wallet, and leave a *real* order un-processed, and steal those users funds.
      expect is_script(order.address.payment_credential)

      // Find the datum that is associated with this order; we allow that datum to be either inline, or in the witness set,
      // to aid in composibility with other protocols
      // We also check that the datum is in the format we expect;
      // Note: we don't actually check the order address anywhere!! As long as it's a script, and the datum is in the correct format, we're good.
      // This lets us upgrade the order contract, or add other types of orders over time.
      expect Some(datum) = datum_of(datums, order)
      expect datum: OrderDatum = datum
      let OrderDatum { pool_ident, destination, max_protocol_fee, details, .. } =
        datum
      // Make sure we're allowed to process this order (i.e. if the user specified a specific pool, we have to honor that)
      expect validate_pool_id(pool_ident, this_pool_ident)
      // And finally, process this one individual order and compute the next state
      // Note that we get back next_orders here, which is needed if we process a donation that has no change UTXO
      let
        new_a,
        new_b,
        new_lp,
        next_orders,
      <-
        process_withdrawal_order(
          pool_policy_a,
          pool_asset_name_a,
          pool_quantity_a,
          pool_policy_b,
          pool_asset_name_b,
          pool_quantity_b,
          pool_policy_lp,
          pool_asset_name_lp,
          pool_quantity_lp,
          order,
          details,
          max_protocol_fee,
          destination,
          amortized_base_fee,
          simple_fee,
          outputs,
        )

      // And recursively process the rest of the orders
      process_withdrawal_orders(
        this_pool_ident,
        datums,
        pool_policy_a,
        pool_asset_name_a,
        new_a,
        pool_policy_b,
        pool_asset_name_b,
        new_b,
        pool_policy_lp,
        pool_asset_name_lp,
        new_lp,
        rest,
        // This advances to the next element from input_order
        amortized_base_fee,
        simple_fee,
        idx + 1,
        // This is the "previous index" within the input list; TODO: I'm not actually sure why we add 1?
        all_inputs,
        // See the notes above about all_inputs vs remaining_inputs
        rest_of_input_list,
        next_orders,
        next_uniqueness_flag,
        continuation,
      )
    }
  }
}

/// This is responsible for checking that the minting value on the transaction is valid
/// based on the pool state, the policy ID, and the initial incoming datum.
pub fn minted_correct_pool_tokens(
  pool_policy_id: PolicyId,
  mint: Value,
  identifier: Ident,
  circulating_lp: Int,
  quantity_lp: Int,
) -> Bool {
  // Unwrap the silly MintedValue special type
  // Note also we only look at the tokens with this policyID
  // so that we can still mint other tokens
  let minted_tokens = assets.tokens(mint, pool_policy_id)

  // If the initial datum has the same circulating LP as the outcome, then we expect no minted tokens
  // Otherwise, the minted tokens should be exactly the pool LP tokens and nothing else
  // TODO: confirm that the "minting 0 ada" problem doesn't apply here; we have real-world transactions, so I doubt it does, but I want to confirm.
  // TODO: This should allow minting of other tokens, from other policy IDs, perhaps, for composibility?
  if circulating_lp == quantity_lp {
    dict.is_empty(minted_tokens)
  } else {
    dict.to_pairs(minted_tokens) == [
      Pair(pool_lp_name(identifier), quantity_lp - circulating_lp),
    ]
  }
}
