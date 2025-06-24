pub const precision = 1_000_000_000_000

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
    liquidity_invariant(
      new_pool_gives,
      new_pool_takes,
      linear_amplification,
      new_sum_invariant,
    )?,
    exchange_invariant(
      new_pool_gives,
      old_pool_takes,
      raw_swap_result,
      linear_amplification,
      old_sum_invariant,
    )?,
  }
}

pub fn liquidity_invariant(
  new_pool_gives: Int,
  new_pool_takes: Int,
  linear_amplification: Int,
  new_sum_invariant: Int,
) {
  let new_pool_gives_p = new_pool_gives * precision
  let new_pool_takes_p = new_pool_takes * precision
  // 4 * A * 4 * (x*y) * D + D^3 - (4(x*y) * (4A(x + y) + D))
  // 4A * 4xy * D + D^3 - (4xy * 4a * (x + y) + 4xy * D)
  // 16Axy * D + D^3 - (16Axy * (x + y) + 4xy * D)
  let four_a = 4 * linear_amplification
  let four_x_y = 4 * new_pool_gives_p * new_pool_takes_p
  let d_plus_one = new_sum_invariant + 1
  let d_cubed = new_sum_invariant * new_sum_invariant * new_sum_invariant
  let d_plus_one_cubed = d_plus_one * d_plus_one * d_plus_one
  let x_plus_y = new_pool_gives_p + new_pool_takes_p
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
  trace f1
  trace f2
  and {
    f1 <= 0,
    f2 > 0,
  }
}

pub fn exchange_invariant(
  new_pool_gives: Int,
  old_pool_takes: Int,
  raw_swap_result: Int,
  linear_amplification: Int,
  old_sum_invariant: Int,
) {
  let new_pool_gives_p = new_pool_gives * precision
  let new_pool_takes_p = old_pool_takes * precision - raw_swap_result

  // 4xy (4A(x + y) + D) - (4xy*4A*D + D^3)
  // 4x(y - 1) (4A(x + y - 1) + D) - (4x(y - 1)*4A*D + D^3)
  // (4xy - 4x) (4A(x + y - 1) + D) - ((4xy - 4x)*4A*D + D^3)
  let d_cubed = old_sum_invariant * old_sum_invariant * old_sum_invariant
  let four_xy = 4 * new_pool_gives_p * new_pool_takes_p
  let x_plus_y = new_pool_gives_p + new_pool_takes_p
  let four_a = 4 * linear_amplification
  let g1 =
    four_xy * ( four_a * x_plus_y + old_sum_invariant ) - four_xy * four_a * old_sum_invariant - d_cubed
  let g2 =
    ( four_xy - 4 * new_pool_gives_p ) * (
      four_a * ( x_plus_y - 1 ) + old_sum_invariant
    ) - ( four_xy - 4 * new_pool_gives_p ) * four_a * old_sum_invariant - d_cubed
  trace g1
  trace g2
  and {
    g1 >= 0,
    g2 < 0,
  }
}

// Test with the precise dY from the off chain calculation
test exchange_invariant_test_0() {
  exchange_invariant(
    1010000000,
    1000000000,
    9999750604846068206,
    200,
    2000000000000000000000,
  )?
}

// Test with the dY from the off chain calculation with the second most insignificant digit increased
test exchange_invariant_test_1() {
  !exchange_invariant(
    1010000000,
    1000000000,
    9999750604846068205,
    200,
    2000000000000000000000,
  )?
}

// Test with the dY from the off chain calculation with the second most insignificant digit decreased
test exchange_invariant_test_2() {
  !exchange_invariant(
    1010000000,
    1000000000,
    9999750604846068207,
    200,
    2000000000000000000000,
  )?
}

test exchange_invariant_test_3() {
  exchange_invariant(
    1020000000,
    990005250,
    9999251668293918428,
    200,
    2000005000730180485513,
  )?
}
