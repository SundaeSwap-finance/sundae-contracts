//// Shared types and functions across all pool calculations

use aiken/list
use aiken/transaction.{Datum, Input, Output}
use aiken/transaction/credential.{Address, ScriptCredential}
use aiken/transaction/value.{Value}
use shared.{Rational, SingletonValue}

/// Represents a payout to a specific 
pub type Payout {
  /// The address to pay out to
  address: Address,
  /// The datum to attach to the payout
  datum: Datum,
  /// The value to pay out
  value: Value,
}

/// An interim pool state
pub type PoolState {
  /// The quantity of token A in the pool
  quantity_a: SingletonValue,
  /// The quantity of token B in the pool
  quantity_b: SingletonValue,
  /// The quantity of LP tokens in the pool
  quantity_lp: SingletonValue,
  /// The fee the pool charges
  fee: Rational,
  /// The rewards accumulated for the scoopers
  rewards: Int,
  /// The payouts that need to be made
  payouts: List<Payout>,
}

fn internal(items: List<Int>, elem: Int, idx: Int) {
  when items is {
    [] -> False
    [head, ..rest] ->
      if head == elem {
        True
      } else if idx == 1 {
        False
      } else {
        internal(rest, elem, idx - 1)
      }
  }
}

pub fn has_in_first_n(items: List<Int>, elem: Int, idx: Int) -> Bool {
  if idx == 0 {
    False
  } else {
    internal(items, elem, idx)
  }
}

test test_has_in_first_n() {
  expect has_in_first_n([1, 2, 3], 1, 3)
  expect has_in_first_n([1, 2, 3], 2, 3)
  expect has_in_first_n([1, 2, 3], 3, 3)
  expect !has_in_first_n([1, 2, 3], 1, 0)
  expect !has_in_first_n([1, 2, 3], 3, 2)
  expect !has_in_first_n([1, 2, 3], 4, 3)
  expect
    !has_in_first_n(
      [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20],
      21,
      20,
    )
  True
}

/// Sort a set of inputs according to an indexing set
pub fn get_sorted_orders(
  input_order: List<Int>,
  inputs: List<Input>,
) -> List<Output> {
  let filtered_orders =
    list.filter(
      inputs,
      fn(i) {
        when i.output.address.payment_credential is {
          ScriptCredential(_) -> True
          _ -> False
        }
      },
    )
  list.indexed_map(
    input_order,
    fn(idx, elem) {
      // make sure we don't secretly process the same order multiple times
      // more efficient than just list.has, since we cut the work in half
      expect !has_in_first_n(input_order, elem, idx)
      // Grab the i'th input
      expect Some(input) = list.at(filtered_orders, elem)
      input.output
    },
  )
}
/// The order to access the inputs
/// The inputs to reorder
