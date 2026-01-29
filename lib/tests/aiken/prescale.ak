use aiken/fuzz
use calculation/stableswap.{liquidity_invariant, swap_invariant}
use shared.{calc_precision, fees_in_legal_range}

/// Tests for the prescale functionality in stableswap v2
///
/// Prescale coefficients allow tokens with different decimals or different real-world
/// units (e.g., gold in ounces vs grams) to be used in a stableswap pool while
/// maintaining a target 1:1 ratio in "normalized" units.
/// Helper function to validate prescale values (mirrors CreatePool validation logic)
/// Both prescale factors must be positive integers
fn is_valid_prescale(prescale_a: Int, prescale_b: Int) -> Bool {
  and {
    prescale_a > 0,
    prescale_b > 0,
  }
}

/// Helper function to validate fee_denominator (mirrors CreatePool validation logic)
/// Fee denominator must be a positive integer
fn is_valid_fee_denominator(fee_denominator: Int) -> Bool {
  fee_denominator > 0
}

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

/// Test prescale works with asymmetric raw reserves that become balanced after prescaling
/// This verifies that prescale correctly normalizes different decimal tokens to 1:1
test liquidity_invariant_asymmetric_with_prescale() {
  // Token A: 500M base units with prescale 10 → 5B normalized
  // Token B: 5B base units with prescale 1 → 5B normalized
  // After prescaling, both are equal, so D = 2 * 5B * calc_precision
  let reserve_a = 500_000_000
  let reserve_b = 5_000_000_000
  let prescale_a = 10
  let prescale_b = 1
  let linear_amplification = 200

  // After prescaling:
  // reserve_a_scaled = 500_000_000 * 10 = 5_000_000_000
  // reserve_b_scaled = 5_000_000_000 * 1 = 5_000_000_000
  // D = 2 * 5_000_000_000 * calc_precision = 10_000_000_000_000_000_000_000
  let sum_invariant = 10_000_000_000_000_000_000_000

  liquidity_invariant(
    reserve_a * prescale_a * calc_precision,
    reserve_b * prescale_b * calc_precision,
    linear_amplification,
    sum_invariant,
  )
}

// ============================================================================
// CreatePool Validation Tests
// These tests verify the validation logic that should be enforced when creating
// a new pool with prescale and fee_denominator values.
// ============================================================================

/// Test that valid prescale values are accepted
test prescale_validation_valid() {
  and {
    // Default prescale (1, 1) is valid
    is_valid_prescale(1, 1),
    // Asymmetric prescale is valid
    is_valid_prescale(1_000_000_000_000, 1),
    // Both sides scaled is valid
    is_valid_prescale(100, 10),
    // Very large prescale is valid
    is_valid_prescale(1_000_000_000_000_000_000, 1_000_000_000_000_000_000),
  }
}

/// Test that invalid prescale values are rejected
/// This is critical for CreatePool validation - prescale of 0 would cause division by zero
test prescale_validation_invalid() {
  and {
    // prescale_a = 0 is invalid
    !is_valid_prescale(0, 1),
    // prescale_b = 0 is invalid
    !is_valid_prescale(1, 0),
    // Both zero is invalid
    !is_valid_prescale(0, 0),
    // Negative prescale_a is invalid
    !is_valid_prescale(-1, 1),
    // Negative prescale_b is invalid
    !is_valid_prescale(1, -1),
    // Both negative is invalid
    !is_valid_prescale(-1, -1),
  }
}

/// Test that valid fee_denominator values are accepted
test fee_denominator_validation_valid() {
  and {
    // Default basis points (10,000) is valid
    is_valid_fee_denominator(10_000),
    // Parts per million is valid
    is_valid_fee_denominator(1_000_000),
    // Parts per billion is valid
    is_valid_fee_denominator(1_000_000_000),
    // Minimal valid denominator (1) is valid
    is_valid_fee_denominator(1),
  }
}

/// Test that invalid fee_denominator values are rejected
/// This is critical for CreatePool validation - fee_denominator of 0 would cause division by zero
test fee_denominator_validation_invalid() {
  and {
    // fee_denominator = 0 is invalid
    !is_valid_fee_denominator(0),
    // Negative fee_denominator is invalid
    !is_valid_fee_denominator(-1),
    // Very negative is invalid
    !is_valid_fee_denominator(-10_000),
  }
}

/// Test that very large prescale values don't cause overflow issues
/// This is important for tokens with extreme decimal differences
test liquidity_invariant_extreme_prescale() {
  // Simulating a hypothetical case with very large prescale
  // Token A: Very few decimals (like 0), Token B: Many decimals (like 18)
  let reserve_a = 1_000_000
  // 1M base units
  let reserve_b = 1_000_000_000_000_000_000_000_000
  // 10^24 base units
  let prescale_a = 1_000_000_000_000_000_000
  // Scale by 10^18
  let prescale_b = 1
  let linear_amplification = 200

  // After prescaling:
  // reserve_a_scaled = 1_000_000 * 10^18 = 10^24
  // reserve_b_scaled = 10^24 * 1 = 10^24
  // D = 2 * 10^24 * calc_precision
  let sum_invariant = 2_000_000_000_000_000_000_000_000_000_000_000_000

  liquidity_invariant(
    reserve_a * prescale_a * calc_precision,
    reserve_b * prescale_b * calc_precision,
    linear_amplification,
    sum_invariant,
  )
}

// ============================================================================
// Property-Based (Fuzz) Tests
// These tests verify properties hold for randomly generated inputs
// ============================================================================

/// Fuzzer for a pair of positive prescale values
fn positive_prescale_pair() -> Fuzzer<(Int, Int)> {
  fuzz.both(
    fuzz.int_between(1, 1_000_000_000_000),
    fuzz.int_between(1, 1_000_000_000_000),
  )
}

/// Fuzzer for prescale pair where first is non-positive
fn prescale_pair_a_nonpositive() -> Fuzzer<(Int, Int)> {
  fuzz.both(fuzz.int_at_most(0), fuzz.int_between(1, 1_000_000))
}

/// Fuzzer for prescale pair where second is non-positive
fn prescale_pair_b_nonpositive() -> Fuzzer<(Int, Int)> {
  fuzz.both(fuzz.int_between(1, 1_000_000), fuzz.int_at_most(0))
}

/// Fuzzer for fee and denominator where fee is within legal range
fn legal_fee_pair() -> Fuzzer<(Int, Int)> {
  fuzz.both(fuzz.int_between(0, 1_000_000), fuzz.int_between(1, 1_000_000))
    |> fuzz.map(fn(pair) { (pair.1st % ( pair.2nd + 1 ), pair.2nd) })
}

/// Fuzzer for fee and denominator where fee exceeds denominator
fn excessive_fee_pair() -> Fuzzer<(Int, Int)> {
  fuzz.both(fuzz.int_between(1, 1_000_000), fuzz.int_between(1, 1_000_000))
    |> fuzz.map(fn(pair) { (pair.2nd + pair.1st, pair.2nd) })
}

/// Fuzzer for balanced pool parameters (reserve, prescale, amplification)
fn balanced_pool_params() -> Fuzzer<(Int, Int, Int)> {
  fuzz.tuple3(
    fuzz.int_between(1_000_000, 1_000_000_000),
    fuzz.int_between(1, 1_000_000),
    fuzz.int_between(1, 1000),
  )
}

/// Fuzzer for asymmetric pool parameters (base_reserve, scale_factor, amplification)
fn asymmetric_pool_params() -> Fuzzer<(Int, Int, Int)> {
  fuzz.tuple3(
    fuzz.int_between(1_000_000, 100_000_000),
    fuzz.int_between(1, 1000),
    fuzz.int_between(10, 500),
  )
}

/// Property: Any positive prescale values should be valid
test prop_prescale_positive_is_valid(pair via positive_prescale_pair()) {
  is_valid_prescale(pair.1st, pair.2nd)
}

/// Property: Any non-positive prescale_a should be invalid
test prop_prescale_a_nonpositive_is_invalid(
  pair via prescale_pair_a_nonpositive(),
) {
  !is_valid_prescale(pair.1st, pair.2nd)
}

/// Property: Any non-positive prescale_b should be invalid
test prop_prescale_b_nonpositive_is_invalid(
  pair via prescale_pair_b_nonpositive(),
) {
  !is_valid_prescale(pair.1st, pair.2nd)
}

/// Property: Any positive fee_denominator should be valid
test prop_fee_denominator_positive_is_valid(
  fee_denominator via fuzz.int_between(1, 1_000_000_000),
) {
  is_valid_fee_denominator(fee_denominator)
}

/// Property: Any non-positive fee_denominator should be invalid
test prop_fee_denominator_nonpositive_is_invalid(
  fee_denominator via fuzz.int_at_most(0),
) {
  !is_valid_fee_denominator(fee_denominator)
}

/// Property: Fees between 0 and fee_denominator (inclusive) should be in legal range
test prop_fees_in_range_are_legal(pair via legal_fee_pair()) {
  fees_in_legal_range(pair.1st, pair.2nd)
}

/// Property: Fees above fee_denominator should be illegal (> 100%)
test prop_fees_above_denominator_are_illegal(pair via excessive_fee_pair()) {
  !fees_in_legal_range(pair.1st, pair.2nd)
}

/// Property: Negative fees should always be illegal
test prop_negative_fees_are_illegal(
  pair via fuzz.both(
    fuzz.int_between(-1_000_000, -1),
    fuzz.int_between(1, 1_000_000),
  ),
) {
  !fees_in_legal_range(pair.1st, pair.2nd)
}

/// Property: For a balanced pool (equal reserves after prescaling), D = 2 * reserve * calc_precision
test prop_liquidity_invariant_balanced_pool(params via balanced_pool_params()) {
  let (reserve, prescale, amplification) = params
  let scaled_reserve = reserve * prescale * calc_precision
  let sum_invariant = 2 * scaled_reserve

  liquidity_invariant(
    scaled_reserve,
    scaled_reserve,
    amplification,
    sum_invariant,
  )
}

/// Property: Prescale correctly normalizes asymmetric raw reserves
test prop_prescale_normalizes_asymmetric_reserves(
  params via asymmetric_pool_params(),
) {
  let (base_reserve, scale_factor, amplification) = params

  // Token A: smaller raw reserve, higher prescale
  let reserve_a = base_reserve
  let prescale_a = scale_factor
  // Token B: larger raw reserve, prescale of 1
  let reserve_b = base_reserve * scale_factor
  let prescale_b = 1

  let scaled_a = reserve_a * prescale_a * calc_precision
  let scaled_b = reserve_b * prescale_b * calc_precision
  let sum_invariant = scaled_a + scaled_b

  // Both scaled values should be equal
  expect scaled_a == scaled_b

  liquidity_invariant(scaled_a, scaled_b, amplification, sum_invariant)
}
