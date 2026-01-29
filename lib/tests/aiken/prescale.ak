/// Tests for the prescale functionality in stableswap v2
///
/// Prescale coefficients allow tokens with different decimals or different real-world
/// units (e.g., gold in ounces vs grams) to be used in a stableswap pool while
/// maintaining a target 1:1 ratio in "normalized" units.

use calculation/stableswap.{liquidity_invariant, swap_invariant}
use shared.{calc_precision}

/// Test that the liquidity invariant works with equal prescale factors (1:1)
/// This is the default behavior where no scaling is applied.
test liquidity_invariant_equal_prescale() {
  // Pool with 1B units of each token, A=200
  let reserve_a = 1_000_000_000
  let reserve_b = 1_000_000_000
  let prescale_a = 1
  let prescale_b = 1
  let linear_amplification = 200

  // D = 2 * reserve when balanced with prescale (1, 1)
  // This is approximately 2_000_000_000 * calc_precision
  let sum_invariant = 2_000_000_000_000_000_000_000

  liquidity_invariant(
    reserve_a * prescale_a * calc_precision,
    reserve_b * prescale_b * calc_precision,
    linear_amplification,
    sum_invariant,
  )
}

/// Test that the liquidity invariant works with different prescale factors
/// Simulating a 6-decimal token (like USDC) paired with an 18-decimal token (like DAI)
/// prescale_a = 10^12 to bring 6 decimals up to 18
/// prescale_b = 1 (already at 18 decimals)
test liquidity_invariant_unequal_prescale() {
  // 1M USDC (6 decimals) = 1_000_000 * 10^6 = 1_000_000_000_000 base units
  // But we want it to be equivalent to 1M DAI (18 decimals) = 1_000_000 * 10^18 base units
  // So we store 1_000_000_000_000 USDC base units, and prescale by 10^12
  let reserve_a = 1_000_000_000_000
  // 6 decimal token stored as-is
  let reserve_b = 1_000_000_000_000_000_000_000_000
  // 18 decimal token stored as-is
  let prescale_a = 1_000_000_000_000
  // Bring 6 decimals up to 18
  let prescale_b = 1
  // Already at 18 decimals
  let linear_amplification = 200

  // After prescaling, both are at the same effective scale
  // Scaled reserve_a = 1_000_000_000_000 * 10^12 = 10^24
  // Scaled reserve_b = 10^24
  // D should be approximately 2 * 10^24 * calc_precision
  let sum_invariant = 2_000_000_000_000_000_000_000_000_000_000_000_000

  liquidity_invariant(
    reserve_a * prescale_a * calc_precision,
    reserve_b * prescale_b * calc_precision,
    linear_amplification,
    sum_invariant,
  )
}

/// Test that swap invariant works correctly with prescale
/// This verifies that swaps maintain the correct ratio when prescale is applied
test swap_invariant_with_prescale() {
  // Starting pool state (equal amounts in normalized terms)
  let pool_a_before = 1_000_000_000
  let pool_b_before = 1_000_000_000
  let prescale_a = 1
  let prescale_b = 1
  let linear_amplification = 200

  // Initial sum invariant
  let sum_invariant = 2_000_000_000_000_000_000_000

  // Swap 10M tokens A for tokens B
  let swap_input = 10_000_000
  let pool_a_after = pool_a_before + swap_input
  let pool_b_after = pool_a_before - 9_994_750
  // This is the raw_swap_result that the scooper calculates
  let raw_swap_result = 9999750604846068206

  // New sum invariant after the swap
  let next_sum_invariant = 2_000_005_000_730_180_485_513

  swap_invariant(
    pool_a_after * prescale_a * calc_precision,
    pool_b_after * prescale_b * calc_precision,
    pool_b_before * prescale_b * calc_precision,
    raw_swap_result * prescale_b,
    linear_amplification,
    sum_invariant,
    next_sum_invariant,
  )
}

/// Test prescale factor of 10 vs 1
/// This simulates a more modest decimal difference
test liquidity_invariant_10x_prescale() {
  // Token A has 1 fewer decimal than Token B
  let reserve_a = 100_000_000
  // 8 decimal token
  let reserve_b = 1_000_000_000
  // 9 decimal token
  let prescale_a = 10
  // Scale up by 10
  let prescale_b = 1
  let linear_amplification = 200

  // After prescaling: both effectively 1_000_000_000 in normalized terms
  // D should be approximately 2 * 1_000_000_000 * calc_precision
  let sum_invariant = 2_000_000_000_000_000_000_000

  liquidity_invariant(
    reserve_a * prescale_a * calc_precision,
    reserve_b * prescale_b * calc_precision,
    linear_amplification,
    sum_invariant,
  )
}

/// Test prescale works with asymmetric reserves
/// Even with unequal reserves, prescale should normalize correctly
test liquidity_invariant_asymmetric_with_prescale() {
  // Imbalanced pool: 2B token A, 500M token B (in base units)
  // But token A has 2 fewer decimals, so prescale by 100
  let reserve_a = 20_000_000
  // 2M "real" tokens at 1 decimal = 20M base units
  let reserve_b = 5_000_000_000
  // 5M "real" tokens at 3 decimals = 5B base units
  let prescale_a = 100
  // Scale 1 decimal token up to 3 decimal equivalent
  let prescale_b = 1
  let linear_amplification = 200

  // After prescaling:
  // reserve_a_scaled = 20_000_000 * 100 = 2_000_000_000
  // reserve_b_scaled = 5_000_000_000 * 1 = 5_000_000_000
  // This is an imbalanced pool even after scaling (not 1:1)
  // D adjusts to reflect this imbalance
  let sum_invariant = 6_981_052_296_547_503_073_679

  liquidity_invariant(
    reserve_a * prescale_a * calc_precision,
    reserve_b * prescale_b * calc_precision,
    linear_amplification,
    sum_invariant,
  )
}
