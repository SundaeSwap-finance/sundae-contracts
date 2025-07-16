// The on-chain code must check that all off-chain computations were correctly done. The sum invariant and
// swap result are found using Newton’s method, something that is too expensive to reproduce on-
// chain. Instead, both results raw_swap_result and new_sum_invariant are provided in the redeemer,
// and the on-chain code checks that the provided values correctly solve the equation.

// Performing a swap on a pool without fees would be relatively simple, but because the lp fees
// are added to one side of the pool the amount of assets in the pool increases and the invariant
// changes. When a user swaps assets we first verify that the old invariant holds (using the swap result
// before fees) and afterwards we verify that the new invariant holds (using the swap result after fees).
pub fn swap_invariant(
  new_pool_gives: Int,
  new_pool_takes: Int,
  old_pool_takes: Int,
  raw_swap_result: Int,
  linear_amplification: Int,
  old_sum_invariant: Int,
  new_sum_invariant: Int,
) {
  and {
    exchange_invariant(
      new_pool_gives,
      old_pool_takes,
      raw_swap_result,
      linear_amplification,
      old_sum_invariant,
    )?,
    liquidity_invariant(
      new_pool_gives,
      new_pool_takes,
      linear_amplification,
      new_sum_invariant,
    )?,
  }
}

// This function checks that 𝑦 is the smallest integer such that
// 4𝐴(𝑥+𝑦)+𝐷 ≥ 4𝐴𝐷 + 𝐷^3/4𝑥𝑦
// where:
// • 𝐴 is linear_amplification
// • 𝐷 is old_sum_invariant
// • 𝑥 is (pool_quantity_x + order_give) * precision
// • 𝑦 is pool_quantity_y * precision - raw_swap_result
pub fn exchange_invariant(
  new_pool_gives: Int,
  old_pool_takes: Int,
  raw_swap_result: Int,
  linear_amplification: Int,
  old_sum_invariant: Int,
) {
  let new_pool_takes = old_pool_takes - raw_swap_result

  // 4A(x + y) + D ≥ 4A * D + D^3/4xy
  // 4xy (4A(x + y) + D) ≥ (4xy*4A*D + D^3)
  // 4xy (4A(x + y) + D) - (4xy*4A*D + D^3) ≥ 0
  // 4x(y - 1) (4A(x + y - 1) + D) - (4x(y - 1)*4A*D + D^3) ≥ 0
  // (4xy - 4x) (4A(x + y - 1) + D) - ((4xy - 4x)*4A*D + D^3) ≥ 0
  let d_cubed = old_sum_invariant * old_sum_invariant * old_sum_invariant
  let four_xy = 4 * new_pool_gives * new_pool_takes
  let x_plus_y = new_pool_gives + new_pool_takes
  let four_a = 4 * linear_amplification
  let g1 =
    four_xy * ( four_a * x_plus_y + old_sum_invariant ) - four_xy * four_a * old_sum_invariant - d_cubed
  let g2 =
    ( four_xy - 4 * new_pool_gives ) * (
      four_a * ( x_plus_y - 1 ) + old_sum_invariant
    ) - ( four_xy - 4 * new_pool_gives ) * four_a * old_sum_invariant - d_cubed
  and {
    g1 >= 0,
    g2 < 0,
  }
}

// This function checks that 𝐷 is the biggest integer such that
// 4𝐴(𝑥+𝑦)+𝐷 ≥ 4𝐴𝐷+ 𝐷^3/4𝑥𝑦
// where:
// • 𝐴 is linear_amplification
// • 𝐷 is new_sum_invariant
// • 𝑥 is (pool_quantity_x + order_give) * precision
// • 𝑦 is (pool_quantity_y - swap_result + lp_fees) * precision
pub fn liquidity_invariant(
  new_pool_gives: Int,
  new_pool_takes: Int,
  linear_amplification: Int,
  new_sum_invariant: Int,
) {
  // 4A(x + y)+D >= 4A * D + D^3/4xy
  // 4 * A * 4 * (x*y) * D + D^3 <= (4(x*y) * (4A(x + y) + D))
  // 4 * A * 4 * (x*y) * D + D^3 - (4(x*y) * (4A(x + y) + D)) <= 0
  // 4A * 4xy * D + D^3 - (4xy * 4a * (x + y) + 4xy * D) <= 0
  // 16Axy * D + D^3 - (16Axy * (x + y) + 4xy * D) <= 0
  let four_a = 4 * linear_amplification
  let four_x_y = 4 * new_pool_gives * new_pool_takes
  let d_plus_one = new_sum_invariant + 1
  let d_cubed = new_sum_invariant * new_sum_invariant * new_sum_invariant
  let d_plus_one_cubed = d_plus_one * d_plus_one * d_plus_one
  let x_plus_y = new_pool_gives + new_pool_takes
  let sixteen_a_x_y = four_a * four_x_y
  let sixteen_a_x_y_x_plus_y = sixteen_a_x_y * x_plus_y
  let f1 =
    sixteen_a_x_y * new_sum_invariant + d_cubed - (
      sixteen_a_x_y_x_plus_y + four_x_y * new_sum_invariant
    )
  let f2 =
    sixteen_a_x_y * d_plus_one + d_plus_one_cubed - (
      sixteen_a_x_y_x_plus_y + four_x_y * d_plus_one
    )
  and {
    f1 <= 0,
    f2 > 0,
  }
}
