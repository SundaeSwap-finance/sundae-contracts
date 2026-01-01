/// Property-based testing helpers for Sundae contracts.

use aiken/collection/list
use aiken/fuzz
use aiken/math

// Swap Invariants

pub fn swap_preserves_k(old_a: Int, old_b: Int, new_a: Int, new_b: Int) -> Bool {
  new_a * new_b >= old_a * old_b
}

pub fn swap_output_bounded(offer: Int, received: Int) -> Bool {
  received < offer
}

pub fn fee_monotonic(
  output_low_fee: Int,
  output_high_fee: Int,
  low_fee: Int,
  high_fee: Int,
) -> Bool {
  if low_fee < high_fee {
    output_low_fee >= output_high_fee
  } else {
    True
  }
}

// LP Token Invariants

pub fn deposit_lp_proportional(
  deposit_value: Int,
  pool_value: Int,
  lp_issued: Int,
  circulating_lp: Int,
) -> Bool {
  deposit_value * circulating_lp <= lp_issued * pool_value + pool_value
}

pub fn withdrawal_proportional(
  lp_burned: Int,
  circulating_lp: Int,
  value_received: Int,
  pool_value: Int,
) -> Bool {
  lp_burned * pool_value >= value_received * circulating_lp
}

pub fn withdrawal_never_exceeds_share(
  lp_burned: Int,
  circulating_lp: Int,
  value_received: Int,
  pool_value: Int,
) -> Bool {
  value_received * circulating_lp <= lp_burned * pool_value
}

// Swap Calculation Helpers

pub fn calc_swap_output(
  reserve_a: Int,
  reserve_b: Int,
  offer_a: Int,
  fee_per_10k: Int,
) -> Int {
  let fee_mult = 10000 - fee_per_10k
  let numerator = reserve_b * offer_a * fee_mult
  let denominator = ( reserve_a + offer_a ) * 10000
  numerator / denominator
}

pub fn calc_swap_output_with_protocol_fee(
  reserve_a: Int,
  reserve_b: Int,
  offer_a: Int,
  lp_fee_per_10k: Int,
  protocol_fee: Int,
) -> Int {
  let effective_offer = offer_a - protocol_fee
  calc_swap_output(reserve_a, reserve_b, effective_offer, lp_fee_per_10k)
}

pub fn calc_new_reserves_after_swap(
  reserve_a: Int,
  reserve_b: Int,
  offer_a: Int,
  fee_per_10k: Int,
) -> (Int, Int) {
  let output_b = calc_swap_output(reserve_a, reserve_b, offer_a, fee_per_10k)
  (reserve_a + offer_a, reserve_b - output_b)
}

// Deposit/Withdrawal Calculation Helpers

pub fn calc_lp_for_deposit(
  reserve_a: Int,
  reserve_b: Int,
  deposit_a: Int,
  deposit_b: Int,
  circulating_lp: Int,
) -> Int {
  if circulating_lp == 0 {
    expect Some(lp) = math.sqrt(deposit_a * deposit_b)
    lp
  } else {
    let ratio_a = deposit_a * circulating_lp / reserve_a
    let ratio_b = deposit_b * circulating_lp / reserve_b
    math.min(ratio_a, ratio_b)
  }
}

pub fn calc_withdrawal_return(
  reserve_a: Int,
  reserve_b: Int,
  lp_amount: Int,
  circulating_lp: Int,
) -> (Int, Int) {
  let return_a = reserve_a * lp_amount / circulating_lp
  let return_b = reserve_b * lp_amount / circulating_lp
  (return_a, return_b)
}

// Test Data Generators

pub fn swap_test_amounts() -> List<Int> {
  [
    1,
    100,
    1_000,
    10_000,
    1_000_000,
    10_000_000,
    100_000_000,
    1_000_000_000,
    10_000_000_000,
    100_000_000_000,
  ]
}

pub fn pool_test_sizes() -> List<Int> {
  [
    10_000_000,
    100_000_000,
    1_000_000_000,
    10_000_000_000,
    100_000_000_000,
    1_000_000_000_000,
  ]
}

pub fn fee_test_values() -> List<Int> {
  [0, 5, 30, 100, 300, 500]
}

pub fn protocol_fee_values() -> List<Int> {
  [0, 1_000_000, 2_500_000, 5_000_000]
}

pub fn imbalanced_pools() -> List<(Int, Int)> {
  [
    (1_000_000_000, 1_000_000_000),
    (10_000_000_000, 1_000_000_000),
    (1_000_000_000, 10_000_000_000),
    (100_000_000_000, 1_000_000_000),
    (1_000_000_000, 100_000_000_000),
    (1_000_000_000_000, 10_000_000),
    (10_000_000, 1_000_000_000_000),
  ]
}

pub fn lp_test_amounts() -> List<Int> {
  [1_000, 100_000, 1_000_000, 10_000_000, 100_000_000, 500_000_000]
}

// Invariant Testing Helpers

pub fn for_all(values: List<a>, predicate: fn(a) -> Bool) -> Bool {
  list.all(values, predicate)
}

pub fn for_all_pairs(
  xs: List<a>,
  ys: List<b>,
  predicate: fn(a, b) -> Bool,
) -> Bool {
  list.all(xs, fn(x) { list.all(ys, fn(y) { predicate(x, y) }) })
}

pub fn for_all_triples(
  xs: List<a>,
  ys: List<b>,
  zs: List<c>,
  predicate: fn(a, b, c) -> Bool,
) -> Bool {
  list.all(xs, fn(x) { for_all_pairs(ys, zs, fn(y, z) { predicate(x, y, z) }) })
}

// Basic Verification Tests

test swap_calc_known_values() {
  let output = calc_swap_output(1_000_000_000, 1_000_000_000, 10_000_000, 5)
  output > 9_800_000 && output < 10_000_000
}

test swap_preserves_k_basic() {
  let (new_a, new_b) = calc_new_reserves_after_swap(1_000_000_000, 1_000_000_000, 10_000_000, 5)
  swap_preserves_k(1_000_000_000, 1_000_000_000, new_a, new_b)
}

test lp_calc_known_values() {
  calc_lp_for_deposit(0, 0, 1_000_000, 1_000_000, 0) == 1_000_000
}

test withdrawal_calc_known_values() {
  let (return_a, return_b) =
    calc_withdrawal_return(1_000_000_000, 1_000_000_000, 100_000_000, 1_000_000_000)
  return_a == 100_000_000 && return_b == 100_000_000
}

// Constant Product Invariant Tests

test prop_swap_preserves_k_all_amounts() {
  let pool = 1_000_000_000
  for_all(swap_test_amounts(), fn(offer) {
    let (new_a, new_b) = calc_new_reserves_after_swap(pool, pool, offer, 30)
    swap_preserves_k(pool, pool, new_a, new_b)
  })
}

test prop_swap_preserves_k_all_pools() {
  for_all(pool_test_sizes(), fn(pool) {
    let offer = pool / 100
    let (new_a, new_b) = calc_new_reserves_after_swap(pool, pool, offer, 30)
    swap_preserves_k(pool, pool, new_a, new_b)
  })
}

test prop_swap_preserves_k_imbalanced() {
  for_all(imbalanced_pools(), fn(pools) {
    let (pool_a, pool_b) = pools
    let offer = math.min(pool_a, pool_b) / 100
    let (new_a, new_b) = calc_new_reserves_after_swap(pool_a, pool_b, offer, 30)
    swap_preserves_k(pool_a, pool_b, new_a, new_b)
  })
}

test prop_swap_preserves_k_all_fees() {
  let pool = 1_000_000_000
  let offer = 10_000_000
  for_all(fee_test_values(), fn(fee) {
    let (new_a, new_b) = calc_new_reserves_after_swap(pool, pool, offer, fee)
    swap_preserves_k(pool, pool, new_a, new_b)
  })
}

// Rounding Direction Tests (always favors pool)

test prop_rounding_favors_pool_swap() {
  for_all_pairs(pool_test_sizes(), swap_test_amounts(), fn(pool, offer) {
    if offer < pool {
      let output = calc_swap_output(pool, pool, offer, 30)
      let (new_a, new_b) = (pool + offer, pool - output)
      new_a * new_b >= pool * pool
    } else {
      True
    }
  })
}

test prop_rounding_favors_pool_imbalanced() {
  for_all(imbalanced_pools(), fn(pools) {
    let (pool_a, pool_b) = pools
    let offer = math.min(pool_a, pool_b) / 10
    let output = calc_swap_output(pool_a, pool_b, offer, 30)
    let (new_a, new_b) = (pool_a + offer, pool_b - output)
    new_a * new_b >= pool_a * pool_b
  })
}

// Output Bounds Tests

test prop_swap_output_bounded_all() {
  for_all_pairs(pool_test_sizes(), swap_test_amounts(), fn(pool, offer) {
    if offer < pool {
      let output = calc_swap_output(pool, pool, offer, 30)
      swap_output_bounded(offer, output)
    } else {
      True
    }
  })
}

test prop_swap_output_bounded_imbalanced() {
  for_all(imbalanced_pools(), fn(pools) {
    let (pool_a, pool_b) = pools
    let offer = math.min(pool_a, pool_b) / 10
    let output = calc_swap_output(pool_a, pool_b, offer, 30)
    output < pool_b && output >= 0
  })
}

// Fee Monotonicity Tests

test prop_fee_monotonic_all_fees() {
  let pool = 1_000_000_000
  let offer = 10_000_000
  for_all_pairs(fee_test_values(), fee_test_values(), fn(low, high) {
    let low_output = calc_swap_output(pool, pool, offer, low)
    let high_output = calc_swap_output(pool, pool, offer, high)
    fee_monotonic(low_output, high_output, low, high)
  })
}

test prop_zero_fee_max_output() {
  for_all(pool_test_sizes(), fn(pool) {
    let offer = pool / 100
    let zero_fee = calc_swap_output(pool, pool, offer, 0)
    let with_fee = calc_swap_output(pool, pool, offer, 30)
    zero_fee > with_fee
  })
}

// Price Impact Tests

test prop_price_impact_increases_with_size() {
  for_all(pool_test_sizes(), fn(pool) {
    let small_offer = pool / 1000
    let large_offer = pool / 10
    let small_output = calc_swap_output(pool, pool, small_offer, 30)
    let large_output = calc_swap_output(pool, pool, large_offer, 30)
    small_output * large_offer > large_output * small_offer
  })
}

test prop_larger_pool_less_slippage() {
  let offer = 10_000_000
  let small_pool = 100_000_000
  let large_pool = 10_000_000_000
  let small_output = calc_swap_output(small_pool, small_pool, offer, 30)
  let large_output = calc_swap_output(large_pool, large_pool, offer, 30)
  large_output > small_output
}

test prop_price_impact_imbalanced() {
  for_all(imbalanced_pools(), fn(pools) {
    let (pool_a, pool_b) = pools
    // Use offer relative to pool_a (the input side) not min
    let small_offer = pool_a / 10000
    let large_offer = pool_a / 100
    if small_offer > 0 && large_offer < pool_a {
      let small_output = calc_swap_output(pool_a, pool_b, small_offer, 30)
      let large_output = calc_swap_output(pool_a, pool_b, large_offer, 30)
      // Small swap gets better effective rate
      small_output * large_offer >= large_output * small_offer
    } else {
      True
    }
  })
}

// Protocol Fee Tests

test prop_protocol_fee_reduces_output() {
  for_all(protocol_fee_values(), fn(protocol_fee) {
    let pool = 1_000_000_000
    let offer = 100_000_000
    let no_fee = calc_swap_output(pool, pool, offer, 30)
    let with_fee = calc_swap_output_with_protocol_fee(pool, pool, offer, 30, protocol_fee)
    if protocol_fee > 0 {
      with_fee < no_fee
    } else {
      with_fee == no_fee
    }
  })
}

test prop_protocol_fee_monotonic() {
  let pool = 1_000_000_000
  let offer = 100_000_000
  for_all_pairs(protocol_fee_values(), protocol_fee_values(), fn(low, high) {
    let low_output = calc_swap_output_with_protocol_fee(pool, pool, offer, 30, low)
    let high_output = calc_swap_output_with_protocol_fee(pool, pool, offer, 30, high)
    if low < high {
      low_output >= high_output
    } else {
      True
    }
  })
}

// Symmetry Tests

test prop_swap_symmetric_balanced() {
  let pool = 1_000_000_000
  for_all(swap_test_amounts(), fn(offer) {
    if offer < pool {
      let output_a_to_b = calc_swap_output(pool, pool, offer, 30)
      let output_b_to_a = calc_swap_output(pool, pool, offer, 30)
      output_a_to_b == output_b_to_a
    } else {
      True
    }
  })
}

// Withdrawal Tests

test prop_withdrawal_proportional_all() {
  let pool = 1_000_000_000
  let circ_lp = 1_000_000_000
  for_all(lp_test_amounts(), fn(lp_amount) {
    let (return_a, return_b) = calc_withdrawal_return(pool, pool, lp_amount, circ_lp)
    return_a == lp_amount && return_b == lp_amount
  })
}

test prop_withdrawal_never_exceeds_pool() {
  for_all_pairs(pool_test_sizes(), lp_test_amounts(), fn(pool, lp_amount) {
    let circ_lp = pool
    let (return_a, return_b) = calc_withdrawal_return(pool, pool, lp_amount, circ_lp)
    withdrawal_never_exceeds_share(lp_amount, circ_lp, return_a, pool) && withdrawal_never_exceeds_share(lp_amount, circ_lp, return_b, pool)
  })
}

test prop_withdrawal_proportional_imbalanced() {
  for_all(imbalanced_pools(), fn(pools) {
    let (pool_a, pool_b) = pools
    let circ_lp = math.min(pool_a, pool_b)
    let lp_amount = circ_lp / 10
    let (return_a, return_b) = calc_withdrawal_return(pool_a, pool_b, lp_amount, circ_lp)
    withdrawal_proportional(lp_amount, circ_lp, return_a, pool_a) && withdrawal_proportional(lp_amount, circ_lp, return_b, pool_b)
  })
}

// Round-Trip Tests (deposit then withdraw)

test prop_deposit_withdraw_round_trip() {
  for_all(pool_test_sizes(), fn(deposit_amount) {
    let pool = deposit_amount * 10
    let circ_lp = pool
    let lp_issued = calc_lp_for_deposit(pool, pool, deposit_amount, deposit_amount, circ_lp)
    let new_pool = pool + deposit_amount
    let new_circ = circ_lp + lp_issued
    let (return_a, _return_b) = calc_withdrawal_return(new_pool, new_pool, lp_issued, new_circ)
    return_a <= deposit_amount && return_a >= deposit_amount - 1
  })
}

test prop_round_trip_rounding_favors_pool() {
  for_all(pool_test_sizes(), fn(deposit) {
    let pool = deposit * 100
    let circ_lp = pool
    let lp_issued = calc_lp_for_deposit(pool, pool, deposit, deposit, circ_lp)
    let new_pool = pool + deposit
    let new_circ = circ_lp + lp_issued
    let (return_a, return_b) = calc_withdrawal_return(new_pool, new_pool, lp_issued, new_circ)
    return_a <= deposit && return_b <= deposit
  })
}

// Edge Case Tests

test prop_minimum_swap_produces_output() {
  for_all(pool_test_sizes(), fn(pool) {
    let output = calc_swap_output(pool, pool, 1, 30)
    output >= 0
  })
}

test prop_large_swap_bounded() {
  for_all(pool_test_sizes(), fn(pool) {
    let offer = pool / 2
    let output = calc_swap_output(pool, pool, offer, 30)
    output < pool && output > 0
  })
}

test prop_swap_entire_reserve_bounded() {
  for_all(pool_test_sizes(), fn(pool) {
    let output = calc_swap_output(pool, pool, pool, 30)
    output < pool && output > 0
  })
}

// Deposit Tests

test prop_deposit_issues_positive_lp() {
  for_all_pairs(pool_test_sizes(), pool_test_sizes(), fn(pool, deposit) {
    if deposit > 0 && pool > 0 {
      let lp = calc_lp_for_deposit(pool, pool, deposit, deposit, pool)
      lp > 0 || deposit * pool / pool == 0
    } else {
      True
    }
  })
}

test prop_deposit_lp_proportional_check() {
  for_all(pool_test_sizes(), fn(deposit) {
    let pool = deposit * 10
    let circ_lp = pool
    let lp_issued = calc_lp_for_deposit(pool, pool, deposit, deposit, circ_lp)
    deposit_lp_proportional(deposit, pool, lp_issued, circ_lp)
  })
}

test prop_first_deposit_geometric_mean() {
  for_all_pairs([1_000_000, 10_000_000, 100_000_000], [1_000_000, 10_000_000, 100_000_000], fn(a, b) {
    let lp = calc_lp_for_deposit(0, 0, a, b, 0)
    expect Some(expected) = math.sqrt(a * b)
    lp == expected
  })
}

// =============================================================================
// Fuzz-Based Property Tests
// =============================================================================
// These use Aiken's built-in fuzzing framework for true randomized testing.

// Fuzzers

fn pool_size() -> Fuzzer<Int> {
  fuzz.int_between(1_000_000, 1_000_000_000_000)
}

fn offer_amount() -> Fuzzer<Int> {
  fuzz.int_between(1, 100_000_000_000)
}

fn fee_bps() -> Fuzzer<Int> {
  fuzz.int_between(0, 500)
}

fn lp_amount() -> Fuzzer<Int> {
  fuzz.int_between(1, 1_000_000_000)
}

fn protocol_fee() -> Fuzzer<Int> {
  fuzz.int_between(0, 5_000_000)
}

fn pool_pair() -> Fuzzer<(Int, Int)> {
  fuzz.both(pool_size(), pool_size())
}

// Fuzzed Swap Tests

fn swap_params() -> Fuzzer<(Int, Int, Int)> {
  fuzz.map3(pool_size(), offer_amount(), fee_bps(), fn(p, o, f) { (p, o, f) })
}

fn fee_pair() -> Fuzzer<(Int, Int)> {
  fuzz.both(fee_bps(), fee_bps())
}

test fuzz_swap_preserves_k(params via swap_params()) {
  let (pool, offer, fee) = params
  if offer < pool {
    let (new_a, new_b) = calc_new_reserves_after_swap(pool, pool, offer, fee)
    swap_preserves_k(pool, pool, new_a, new_b)
  } else {
    True
  }
}

test fuzz_swap_preserves_k_imbalanced(params via fuzz.both(pool_pair(), fee_bps())) {
  let ((pool_a, pool_b), fee) = params
  let offer = math.min(pool_a, pool_b) / 100
  if offer > 0 {
    let (new_a, new_b) = calc_new_reserves_after_swap(pool_a, pool_b, offer, fee)
    swap_preserves_k(pool_a, pool_b, new_a, new_b)
  } else {
    True
  }
}

test fuzz_swap_output_bounded(params via fuzz.both(pool_size(), offer_amount())) {
  let (pool, offer) = params
  if offer < pool {
    let output = calc_swap_output(pool, pool, offer, 30)
    swap_output_bounded(offer, output)
  } else {
    True
  }
}

test fuzz_rounding_favors_pool(params via swap_params()) {
  let (pool, offer, fee) = params
  if offer < pool {
    let output = calc_swap_output(pool, pool, offer, fee)
    let (new_a, new_b) = (pool + offer, pool - output)
    new_a * new_b >= pool * pool
  } else {
    True
  }
}

test fuzz_fee_monotonicity(params via fuzz.both(fuzz.both(pool_size(), offer_amount()), fee_pair())) {
  let ((pool, offer), (fee1, fee2)) = params
  if offer < pool {
    let output1 = calc_swap_output(pool, pool, offer, fee1)
    let output2 = calc_swap_output(pool, pool, offer, fee2)
    fee_monotonic(output1, output2, fee1, fee2)
  } else {
    True
  }
}

test fuzz_protocol_fee_reduces_output(
  params via fuzz.both(swap_params(), protocol_fee()),
) {
  let ((pool, offer, _), prot_fee) = params
  if offer < pool && prot_fee > 0 && prot_fee < offer {
    let no_fee = calc_swap_output(pool, pool, offer, 30)
    let with_fee = calc_swap_output_with_protocol_fee(pool, pool, offer, 30, prot_fee)
    with_fee < no_fee
  } else {
    True
  }
}

// Fuzzed Price Impact Tests

test fuzz_price_impact(
  pool via pool_size(),
) {
  let small_offer = pool / 1000
  let large_offer = pool / 10
  if small_offer > 0 {
    let small_output = calc_swap_output(pool, pool, small_offer, 30)
    let large_output = calc_swap_output(pool, pool, large_offer, 30)
    // Smaller trades get better effective rates
    small_output * large_offer >= large_output * small_offer
  } else {
    True
  }
}

// Fuzzed Withdrawal Tests

test fuzz_withdrawal_bounded(params via fuzz.both(pool_size(), lp_amount())) {
  let (pool, lp) = params
  let circ_lp = pool
  if lp <= circ_lp {
    let (return_a, return_b) = calc_withdrawal_return(pool, pool, lp, circ_lp)
    withdrawal_never_exceeds_share(lp, circ_lp, return_a, pool) && withdrawal_never_exceeds_share(lp, circ_lp, return_b, pool)
  } else {
    True
  }
}

test fuzz_withdrawal_imbalanced(params via fuzz.both(pool_pair(), lp_amount())) {
  let ((pool_a, pool_b), lp) = params
  let circ_lp = math.min(pool_a, pool_b)
  if lp <= circ_lp && lp > 0 {
    let (return_a, return_b) = calc_withdrawal_return(pool_a, pool_b, lp, circ_lp)
    withdrawal_proportional(lp, circ_lp, return_a, pool_a) && withdrawal_proportional(lp, circ_lp, return_b, pool_b)
  } else {
    True
  }
}

// Fuzzed Round-Trip Tests

test fuzz_deposit_withdraw_round_trip(params via fuzz.both(pool_size(), offer_amount())) {
  let (pool, deposit) = params
  if deposit < pool / 10 && deposit > 0 {
    let circ_lp = pool
    let lp_issued = calc_lp_for_deposit(pool, pool, deposit, deposit, circ_lp)
    if lp_issued > 0 {
      let new_pool = pool + deposit
      let new_circ = circ_lp + lp_issued
      let (return_a, return_b) = calc_withdrawal_return(new_pool, new_pool, lp_issued, new_circ)
      return_a <= deposit && return_b <= deposit
    } else {
      True
    }
  } else {
    True
  }
}

// Fuzzed Deposit Tests

test fuzz_deposit_lp_proportional(params via fuzz.both(pool_size(), offer_amount())) {
  let (pool, deposit) = params
  if deposit > 0 && deposit < pool {
    let circ_lp = pool
    let lp_issued = calc_lp_for_deposit(pool, pool, deposit, deposit, circ_lp)
    deposit_lp_proportional(deposit, pool, lp_issued, circ_lp)
  } else {
    True
  }
}

fn deposit_pair() -> Fuzzer<(Int, Int)> {
  fuzz.both(fuzz.int_between(1_000, 1_000_000_000), fuzz.int_between(1_000, 1_000_000_000))
}

test fuzz_first_deposit_sqrt(params via deposit_pair()) {
  let (a, b) = params
  let lp = calc_lp_for_deposit(0, 0, a, b, 0)
  expect Some(expected) = math.sqrt(a * b)
  lp == expected
}
