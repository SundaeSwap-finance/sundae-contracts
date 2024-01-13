//// Shared types and functions across all pool calculations

use aiken/builtin
use shared.{SingletonValue}

/// This is a slimmed down interim pool state, useful for iterating over each order and advancing each order
pub type PoolState {
  /// The current reserve of token A available for swapping
  quantity_a: SingletonValue,
  /// The current reserve of token B available for swapping
  quantity_b: SingletonValue,
  /// The circulating supply of LP tokens
  quantity_lp: SingletonValue,

  // TODO: we can probably remove fees_per_10_thousand and protocol_fees, and pass them as parameters instead
  // since the fee rate doesn't change, and the protocol_fees can be computed once at the end
  /// The fee, in basis points; that is, parts per 10,000.  So, 100 == 1%; 30 == 0.3%; etc.
  fees_per_10_thousand: Int,
  /// The protocol_fees collected and set aside in the pool
  protocol_fees: Int,
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
  let bit = do_2_exp(index)
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

/// Calculate 2^n with a simple recursion; TODO: aiken std-lib has a new efficient implementation, see SSW-303
pub fn do_2_exp(n: Int) -> Int {
  if n <= 0 {
    1
  } else {
    2 * do_2_exp(n - 1)
  }
}
