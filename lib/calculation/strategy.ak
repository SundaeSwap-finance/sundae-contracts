use aiken/cbor
use aiken/dict.{Dict}
use aiken/interval.{Interval, Finite}
use aiken/transaction.{OutputReference, ValidityRange}
use aiken/transaction/credential.{verify_signature, Inline, ScriptCredential, StakeCredential}
use types/order.{StrategyExecution, StrategyAuthorization, Signature, Script, SignedStrategyExecution, Order}

/// Check that the outer interval completely contains the inner interval
fn contains_interval(outer: Interval<Int>, inner: Interval<Int>) -> Bool {
  // Get the lowest point of the inner interval, by factoring in finite / inclusive bounds
  let lowest_inner_point = when inner.lower_bound.bound_type is {
    Finite(low) ->
      if inner.lower_bound.is_inclusive {
        low
      } else {
        low + 1
      }
    _ -> 0
  }
  // Get the highest point of the inner interval, by factoring in finite / inclusive bounds
  let highest_inner_point = when inner.upper_bound.bound_type is {
    Finite(hi) ->
      if inner.upper_bound.is_inclusive {
        hi
      } else {
        hi - 1
      }
    _ -> 99999999999 // TODO: Maybe there's a cleaner way to handle this code
  }
  // Check that the lower bound is satisfied, i.e. <= vs < based on whether the outer bound is inclusive or not
  let lower_bound_contained = when outer.lower_bound.bound_type is {
    Finite(low) ->
      if outer.lower_bound.is_inclusive {
        low <= lowest_inner_point
      } else {
        low < lowest_inner_point
      }
    _ -> True
  }
  // Check that the upper bound is satisfied, i.e. >= vs > based on whether the outer bound is inclusive or not
  let upper_bound_contained = when outer.upper_bound.bound_type is {
    Finite(hi) ->
      if outer.upper_bound.is_inclusive {
        hi >= highest_inner_point
      } else {
        hi > highest_inner_point
      }
    _ -> True
  }
  and {
    lower_bound_contained,
    upper_bound_contained
  }
}

/// Extract the order details for a "strategy" order that was provided by the scooper, and check that it is valid.
/// 
/// In order for a strategy execution to be valid, it must:
/// - Be for the specific order that is being processed
/// - Be executed in a transaction that can only exist within the valid interval with the signature
/// - Have a valid signature
pub fn get_strategy(
  order_tx_ref: OutputReference,
  tx_valid_range: ValidityRange,
  withdrawals: Dict<StakeCredential, Int>,
  auth: StrategyAuthorization,
  strategy: SignedStrategyExecution,
) -> Order {
    let SignedStrategyExecution { strategy, signature } = strategy
    let StrategyExecution { tx_ref, validity_range, details, .. } = strategy
    // We check that the order_tx_ref from the strategy matches the order we're actually processing,
    // to prevent replay / cross-play attacks.
    expect order_tx_ref == tx_ref
    // Make sure the transaction validity range (all the valid points in time the tx could exist)
    // is entirely contained in the *strategy* valid range (all the points where the strategy is valid)
    // This is so the user can impose timing restrictions (for example, maybe they only want their order to execute 20 minutes from now)
    expect contains_interval(validity_range, tx_valid_range)
    when auth is {
      Signature { signer } -> {
        // And finally, use cbor.serialise and check that the signature is valid
        // TODO: is this at risk if cbor.serialise changes? is there a way for us to get the raw bytes of the data?
        let strategy_bytes = cbor.serialise(details)
        expect Some(signature) = signature
        expect verify_signature(signer, strategy_bytes, signature)
        details
      }
      Script { script } -> {
        expect dict.has_key(withdrawals, Inline(ScriptCredential(script)))
        details
      }
    }
}