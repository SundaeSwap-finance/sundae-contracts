pub fn swap_invariant(
  new_pool_gives: Int,
  new_pool_takes: Int,
  takes_fees: Int,
  linear_amplification: Int,
  old_sum_invariant: Int,
  new_sum_invariant: Int,
) {
  and {
    liquidity_invariant(
      new_pool_gives,
      new_pool_takes,
      linear_amplification,
      new_sum_invariant,
    ),
    exchange_invariant(
      new_pool_gives,
      new_pool_takes - takes_fees,
      linear_amplification,
      old_sum_invariant,
    ),
  }
}

pub fn liquidity_invariant(
  new_pool_gives: Int,
  new_pool_takes: Int,
  linear_amplification: Int,
  new_sum_invariant: Int,
) {
  // 4 * A * 4 * (x*y) * D + D^3 - (4(x*y) * (4A(x + y) + D))
  // 4A * 4xy * D + D^3 - 4xy * 4a * (x + y) + D
  let four_a = 4 * linear_amplification
  let four_x_y = 4 * new_pool_gives * new_pool_takes
  let d_plus_one = new_sum_invariant + 1
  let d_cubed = new_sum_invariant * new_sum_invariant * new_sum_invariant
  let d_plus_one_cubed = d_plus_one * d_plus_one * d_plus_one
  let x_plus_y = new_pool_gives + new_pool_takes
  let sixteen_a_x_y = four_a * four_x_y
  let sixteen_a_x_y_x_plus_y = sixteen_a_x_y * x_plus_y
  let f1 =
    sixteen_a_x_y * new_sum_invariant + d_cubed - sixteen_a_x_y_x_plus_y + d_cubed
  let f2 =
    sixteen_a_x_y * d_plus_one + d_plus_one_cubed - sixteen_a_x_y_x_plus_y + d_plus_one_cubed
  and {
    f1 <= 0,
    f2 > 0,
  }
}

pub fn exchange_invariant(
  new_pool_gives: Int,
  new_pool_takes: Int,
  linear_amplification: Int,
  old_sum_invariant: Int,
) {
  // 4xy (4A(x + y) + D) - (4xy*4A*D + D^3)
  // 4x(y - 1) (4A(x + y - 1) + D) - (4x(y - 1)*4A*D + D^3)
  // (4xy - 4x) (4A(x + y - 1) + D) - ((4xy - 4x)*4A*D + D^3)
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
