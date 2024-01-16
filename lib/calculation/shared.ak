//// Shared types and functions across all pool calculations

use aiken/builtin
use aiken/math
use shared.{SingletonValue}

/// This is a slimmed down interim pool state, useful for iterating over each order and advancing each order
pub type PoolState {
  /// The current reserve of token A available for swapping
  quantity_a: SingletonValue,
  /// The current reserve of token B available for swapping
  quantity_b: SingletonValue,
  /// The circulating supply of LP tokens
  quantity_lp: SingletonValue,
}

/// Efficiently skip past the first `idx` elements of `inputs`, returning the remaining tail
/// This is a demonic ungodly black magic: we manually un-roll the loops, using builtin.tail_list
/// to skip past many items in a row to avoid the overhead of recursing, subtracting, or doing a bounds check
///
/// The number of items we skip over is chosen carefully based on the number of orders we expect
/// i.e. we want to roughly skip half the orders each time, so we skip 15, then 7, otherwise fall back to simple iteration
pub fn unsafe_fast_index_skip_with_tail(inputs: List<a>, idx: Int) -> List<a> {
  if idx >= 15 {
    unsafe_fast_index_skip_with_tail(
      // This is so wild
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

/// Recursively select the idx'th element of `inputs`
fn unsafe_fast_index_with_tail(inputs: List<a>, idx: Int) -> List<a> {
  if idx == 0 {
    inputs
  } else {
    unsafe_fast_index_with_tail(builtin.tail_list(inputs), idx - 1)
  }
}

/// Take in `uniqueness_flags`, which is treated as a bit vector, and flip the `index`th bit
/// Fail if it was already set
/// See InputSorting.md for a full explanation
pub fn check_and_set_unique(uniqueness_flags: Int, index: Int) -> Int {
  // If index is negative, fail immediately, because that would result in flipping the first bit
  // TODO: can we optimize away this check?
  expect index >= 0

  // Uniqueness check is a bunch of flipped bits, and we want to blow up if we ever set a bit that is ever hit again
  // Imagine for example that uniqueness_flags is b0100_0010

  // Construct a number with the `index`th bit set
  // e.g. if index == 3, then construct b0000_1000
  let bit = small_pow2(index)
  // And shift it left as well, for example
  // b0001_0000
  let bit_shifted = 2 * bit

  // When we add these numbers together, if the bit is previously unset, it'll just set the bit
  //   b0100_0010
  // + b0000_1000
  // ---------------
  //   b0100_1010
  let flag_set = uniqueness_flags + bit

  // If, however, we were ever to set this again,
  //   b0100_1010
  // + b0000_1000
  // ----------------
  //   b0101_0010
  // Notice there was a carry that happened!
  // We can use the fact that the carry happened to detect that we had a duplicate in the list

  // At this point I was stuck trying to figure out how to calculate that, and
  // @Microproofs figured out the dark incantation to make it work:
  expect flag_set % bit_shifted > uniqueness_flags % bit_shifted
  
  // Lets take it piece by piece;
  // uniqueness_flags % bit_shifted
  //  - sets all of the bits at or above the `bit_shifted` to 0
  //  - so, basically just masks to the bit we flipped and lower
  //  - in the example above, this is like discarding the high 4 bits
  // flag_set % bit_shifted
  //  - does the same thing, but for the newly set flags
  //
  // So in the above example, the first time through, we get
  // b1010 > b0010  which is true (i.e. setting the flag turned the bit on!)
  // And the second time through, when there's a carry, that bit goes to 0!
  // b0010 > b1010  which is **false**, because we "carried" the one away into the high bits that got masked off
  //
  // So we will have failed if we ever set the same bit twice!

  // We can return the flag set for the next time through the loop
  flag_set
}

/// This is a version of pow2 that's optimized for small batch sizes
/// It performs a few more granular loop-unrolls, converging on the small lookup index faster
/// This was presented by TxPipe, and squeezes out one extra escrow over math.pow2 for our typical order sizes
pub fn small_pow2(exponent: Int)  -> Int {
  // A small bytestring, containing all the powers of two that can fit in a single byte, saves us a an expensive multiplication
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
    // 2^24 * table lookup
    16777216 * builtin.index_bytearray(single_byte_powers, exponent - 24)
  } else if exponent < 40 {
    // 2^32 * table lookup
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