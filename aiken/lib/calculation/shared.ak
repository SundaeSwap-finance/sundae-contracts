//// Shared types and functions across all pool calculations

use aiken/builtin
use shared.{SingletonValue}

/// An interim pool state
pub type PoolState {
  /// The quantity of token A in the pool
  quantity_a: SingletonValue,
  /// The quantity of token B in the pool
  quantity_b: SingletonValue,
  /// The quantity of LP tokens in the pool
  quantity_lp: SingletonValue,
  /// The fee the pool charges
  fees_per_10_thousand: Int,
  /// The rewards accumulated for the scoopers
  rewards: Int,
}

pub fn unsafe_fast_index_skip_with_tail(inputs: List<a>, idx: Int) -> List<a> {
  if idx >= 15 {
    unsafe_fast_index_skip_with_tail(
      inputs
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list,
      idx - 15,
    )
  } else if idx >= 7 {
    unsafe_fast_index_skip_with_tail(
      inputs
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list
        |> builtin.tail_list,
      idx - 7,
    )
  } else {
    unsafe_fast_index_with_tail(inputs, idx)
  }
}

fn unsafe_fast_index_with_tail(inputs: List<a>, idx: Int) -> List<a> {
  if idx == 0 {
    inputs
  } else {
    unsafe_fast_index_with_tail(builtin.tail_list(inputs), idx - 1)
  }
}

pub fn check_and_set_unique(uniqueness_flags: Int, index: Int) -> Int {
  expect index >= 0
  let bit = do_2_exp(index)
  let bit_shifted = 2 * bit

  let flag_set = uniqueness_flags + bit

  expect flag_set % bit_shifted > uniqueness_flags % bit_shifted
  flag_set
}

pub fn do_2_exp(n: Int) -> Int {
  if n <= 0 {
    1
  } else {
    2 * do_2_exp(n - 1)
  }
}
