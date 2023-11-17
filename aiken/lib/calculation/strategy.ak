use aiken/cbor
use aiken/interval.{Interval, Finite}
use aiken/transaction.{OutputReference, ValidityRange}
use aiken/transaction/credential.{verify_signature, VerificationKey}
use types/order.{StrategyExecution, SignedStrategyExecution, Order}

fn contains_interval(outer: Interval<Int>, inner: Interval<Int>) -> Bool {
  let lowest_inner_point = when inner.lower_bound.bound_type is {
    Finite(low) ->
      if inner.lower_bound.is_inclusive {
        low
      } else {
        low + 1
      }
    _ -> 0
  }
  let highest_inner_point = when inner.upper_bound.bound_type is {
    Finite(hi) ->
      if inner.upper_bound.is_inclusive {
        hi
      } else {
        hi - 1
      }
    _ -> 99999999999 // TODO
  }
  let lower_bound_contained = when outer.lower_bound.bound_type is {
    Finite(low) ->
      if outer.lower_bound.is_inclusive {
        low <= lowest_inner_point
      } else {
        low < lowest_inner_point
      }
    _ -> True
  }
  let upper_bound_contained = when outer.upper_bound.bound_type is {
    Finite(hi) ->
      if outer.upper_bound.is_inclusive {
        hi >= highest_inner_point
      } else {
        hi > highest_inner_point
      }
    _ -> True
  }
  lower_bound_contained && upper_bound_contained
}

pub fn get_strategy(
  order_tx_ref: OutputReference,
  tx_valid_range: ValidityRange,
  signer: VerificationKey,
  strategy: SignedStrategyExecution,
) -> Order {
    let SignedStrategyExecution { strategy, signature } = strategy
    let StrategyExecution { tx_ref, validity_range, details } = strategy
    expect order_tx_ref == tx_ref
    // Make sure the transaction validity range (all the valid points in time the tx could exist)
    // is entirely contained in the *strategy* valid range (all the points where the strategy is valid)
    expect contains_interval(validity_range, tx_valid_range)
    let strategy_bytes = cbor.serialise(details)
    expect verify_signature(signer, strategy_bytes, signature)
    details
}