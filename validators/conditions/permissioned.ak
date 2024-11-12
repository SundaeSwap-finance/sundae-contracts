use aiken/cbor.{serialise}
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/crypto.{
  Blake2b_256, Hash, VerificationKeyHash, blake2b_256, verify_ed25519_signature,
}
use aiken/interval.{Finite, Interval, IntervalBound}
use calculation/process.{find_pool_output}
use calculation/shared.{check_and_set_unique,
  unsafe_fast_index_skip_with_tail} as calculation_shared
use cardano/address.{Credential}
use cardano/transaction.{InlineDatum, Input, Output, Transaction, ValidityRange}
use shared.{datum_of, is_script}
use sundae/multisig
use types/conditions/permissioned.{ComplianceToken,
  PermissionedDatum} as permissioned_types
use types/order.{OrderDatum, SignedStrategyExecution}
use types/pool.{Manage, PoolDatum, PoolRedeemer, PoolScoop}

validator permissioned {
  withdraw(
    redeemer: PoolRedeemer,
    _account: Credential,
    transaction: Transaction,
  ) {
    // First, we destructure the transaction right upfront, because field access is O(n),
    // and we want access to these fields with just a single pass over the transaction
    // This will be a common pattern throughout the scripts
    // (in fact, I really want a compiler optimization that detects and lifts repeated field accesses into a destructure)
    let Transaction { inputs, outputs, validity_range, datums, .. } =
      transaction

    // Find the pool output, the output datum, and destructure it to access the fields we need to process the scoop
    let (
      Output { address: pool_output_address, .. },
      PoolDatum { condition_datum: actual_condition_datum_option, .. },
    ) = find_pool_output(outputs)

    // Then, (confusing terminology aside) find the specific pool UTXO being spent by this transaction
    expect Some(Input(_, pool_input)) =
      list.find(
        inputs,
        fn(input) { input.output.address == pool_output_address },
      )

    expect InlineDatum(pool_input_datum) = pool_input.datum

    expect PoolDatum { condition_datum: condition_datum_option, .. } =
      pool_input_datum

    expect Some(condition_datum) = condition_datum_option

    expect PermissionedDatum { whitelisted_oracles } = condition_datum

    // Then, there are two different actions that can be taken against a pool:
    // - Scooping a batch of orders
    // - Withdrawing protocol fees to the treasury
    when redeemer is {
      // In the case of the scoop, the redeemer indicates which scooper is doing the scoop, and the order in which the inputs should be processed
      PoolScoop { input_order, .. } -> and {
          scoop_permission(
            whitelisted_oracles,
            validity_range,
            datums,
            input_order,
            inputs,
          ),
          actual_condition_datum_option == condition_datum_option,
        }
      Manage -> False
    }
  }

  else(_) {
    fail
  }
}

fn scoop_permission(
  oracles: List<VerificationKeyHash>,
  validity_range: ValidityRange,
  datums: Dict<Hash<Blake2b_256, Data>, Data>,
  input_order: List<(Int, Option<SignedStrategyExecution>, Int)>,
  inputs: List<Input>,
) -> Bool {
  let compliance_verified <-
    process_orders(
      oracles,
      validity_range,
      datums,
      input_order,
      0,
      inputs,
      inputs,
      0,
      True,
    )
  compliance_verified
}

/// Process a single order, comparing it to the output to ensure it was executed faithfully, and returning the new pool state
///
/// Most of the parameters here are for performance reasons, to avoid destructuring objects, since thats very expensive
fn process_order(
  // The validity range of the transaction, used to ensure the signed execution is within the correct time window
  validity_range: ValidityRange,
  // The details of the order to execute, such as whether it's a swap, the limit, etc.
  order: OrderDatum,
  // List of oracles that are whitelisted
  oracles: List<VerificationKeyHash>,
) -> Bool {
  // Has an attached compliance token
  expect compliance: ComplianceToken = order.extension

  // The owner of an order is the same as the public key in the token
  expect multisig.Signature(owner_sig) = order.owner

  expect Interval {
    lower_bound: IntervalBound { bound_type: Finite(tx_from), .. },
    upper_bound: IntervalBound { bound_type: Finite(tx_to), .. },
  } = validity_range

  expect Interval {
    lower_bound: IntervalBound { bound_type: Finite(token_from), .. },
    upper_bound: IntervalBound { bound_type: Finite(token_to), .. },
  } = compliance.token.validity_range

  let oracle_key_hash = blake2b_256(compliance.token.oracle_key)

  // The oracle public key is one of those listed in the approved oracles
  expect Some(..) = list.find(oracles, fn(o) { o == oracle_key_hash })
  and {
    owner_sig == compliance.token.user_key,
    // The destination of an order is the same as the one in the token
    order.destination == compliance.token.destination,
    // The order details hash to the same hash from the token
    blake2b_256(serialise(order.details)) == compliance.token.order_hash,
    // The valid range of the transaction entirely contains the valid range from the token
    token_from < tx_from,
    token_to > tx_to,
    // The signature is valid
    verify_ed25519_signature(
      compliance.token.oracle_key,
      serialise(compliance.token),
      compliance.oracle_signature,
    ),
  }
}

/// Recursively process all orders in the correct order
/// There's a lot of parameters here, mostly for efficiency (though with some redundancies being removed in another branch)
fn process_orders(
  // List of oracles that are whitelisted
  oracles: List<VerificationKeyHash>,
  // The transaction valid range, if we end up processing a strategy
  tx_valid_range: ValidityRange,
  // The datums in the witness set, in case we need to lookup a non-inline datum
  datums: Dict<Hash<Blake2b_256, Data>, Data>,
  // The list of remaining indices into the inputs, specifying which orders to process
  input_order: List<(Int, Option<SignedStrategyExecution>, Int)>,
  // The previous order we processed, to check if we need to restart the loop; TODO: we actually pass +1 from this, and i'm not sure how to explain why we do this...
  prev_index: Int,
  // *all* inputs on the transaction, in case we need to start over from the beginning (i.e. wrap around)
  all_inputs: List<Input>,
  // Just the remaining inputs in the list, in case it's more efficient to keep walking from here
  remaining_inputs: List<Input>,
  // A number that, when interpreted as a bit flag, indicates which orders we've already processed; used to check if an order is processed more than once (see InputSorting.md)
  uniqueness_flag: Int,
  // Flag keeping track of compliance verification
  compliance_verified: Bool,
  // A continuation to call with the final pool state; more efficient than constructing tuples / objects
  continuation: fn(Bool) -> Bool,
) -> Bool {
  // Returns the final pool state, and the count of each order type
  // The main "pump" of the recursive loop is the input_order, which is a set of indices into the inputs list
  // specified by the scooper for the order to process each order in.
  // Once we've reached the end of the list, we can return, but otherwise
  when input_order is {
    [] -> continuation(True)
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

      // And finally, process this one individual order and compute the next state
      // Note that we get back next_orders here, which is needed if we process a donation that has no change UTXO
      let order_compliance_verified =
        process_order(tx_valid_range, datum, oracles)

      // And recursively process the rest of the orders
      process_orders(
        oracles,
        tx_valid_range,
        datums,
        rest,
        idx + 1,
        // This is the "previous index" within the input list; TODO: I'm not actually sure why we add 1?
        all_inputs,
        // See the notes above about all_inputs vs remaining_inputs
        rest_of_input_list,
        next_uniqueness_flag,
        order_compliance_verified && compliance_verified,
        continuation,
      )
    }
  }
}
