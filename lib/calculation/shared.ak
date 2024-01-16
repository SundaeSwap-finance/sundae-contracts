//// Shared types and functions across all pool calculations

use aiken/builtin
use aiken/math
use shared.{SingletonValue}

/// An interim pool state
pub type PoolState {
  /// The quantity of token A in the pool
  quantity_a: SingletonValue,
  /// The quantity of token B in the pool
  quantity_b: SingletonValue,
  /// The quantity of LP tokens in the pool
  quantity_lp: SingletonValue,
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
  let bit = small_pow2(index)
  let bit_shifted = 2 * bit

  let flag_set = uniqueness_flags + bit

  expect flag_set % bit_shifted > uniqueness_flags % bit_shifted
  flag_set
}

/// This is a version of pow2 that's optimized for small batch sizes
/// It performs a few more granular loop-unrolls, converging on the small lookup index faster
/// This was presented by TxPipe, and squeezes out one extra escrow over math.pow2 for our typical order sizes
pub fn small_pow2(exponent: Int)  -> Int {
  let single_byte_powers = #[1, 2, 4, 8, 16, 32, 64, 128]
  if exponent < 8 {
    builtin.index_bytearray(single_byte_powers, exponent)
  } else if exponent < 16 {
    // 2^8 * table lookup
    256 * builtin.index_bytearray(single_byte_powers, exponent - 8)
  } else if exponent < 24 {
    // 2^16 * table lookup
    65536 * builtin.index_bytearray(single_byte_powers, exponent - 16)
  } else if exponent < 32 {
    // 2^24 * recurse
    16777216 * builtin.index_bytearray(single_byte_powers, exponent - 24)
  } else if exponent < 40 {
    // 2^32 * recurse
    4294967296 * builtin.index_bytearray(single_byte_powers, exponent - 32)
  } else {
    // Otherwise we can fall back to the built in;
    // currently we can't fit more than 40 orders in a batch, but if
    // the protocol parameters get bumped, we don't want things to start failing!
    // When benchmarking, falling back to the builtin proved to be faster than recursing
    // unsure why that is!
    math.pow2(exponent)
  }
}