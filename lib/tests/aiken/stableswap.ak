use calculation/stableswap.{exchange_invariant, liquidity_invariant}
use shared.{calc_precision}

test liquidity_invariant_test_0() {
  liquidity_invariant(
    20_000_000 * calc_precision,
    20_000_000 * calc_precision,
    200,
    40000000000000000000,
  )?
}

// Test with the precise dY from the off chain calculation
test exchange_invariant_test_0() {
  exchange_invariant(
    1010000000 * calc_precision,
    1000000000 * calc_precision,
    9999750604846068206,
    200,
    2000000000000000000000,
  )?
}

// Test with the dY from the off chain calculation with the second most insignificant digit increased
test exchange_invariant_test_1() {
  !exchange_invariant(
    1010000000 * calc_precision,
    1000000000 * calc_precision,
    9999750604846068205,
    200,
    2000000000000000000000,
  )?
}

// Test with the dY from the off chain calculation with the second most insignificant digit decreased
test exchange_invariant_test_2() {
  !exchange_invariant(
    1010000000 * calc_precision,
    1000000000 * calc_precision,
    9999750604846068207,
    200,
    2000000000000000000000,
  )?
}

test exchange_invariant_test_3() {
  exchange_invariant(
    1020000000 * calc_precision,
    990005250 * calc_precision,
    9999251668293918428,
    200,
    2000005000730180485513,
  )?
}
