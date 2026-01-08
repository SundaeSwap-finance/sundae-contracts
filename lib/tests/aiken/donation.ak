/// Donation calculation tests - rewritten using test fixtures

use calculation/donation.{do_donation}
use calculation/shared.{PoolState}
use cardano/assets.{merge}
use cardano/transaction.{InlineDatum, NoDatum, Output}
use tests/fixtures/addresses.{user_address}
use tests/fixtures/constants.{rberry_asset_name, rberry_policy}
use tests/fixtures/datums.{
  default_swap_order, with_details, with_pool_ident, with_self_destination,
}
use tests/fixtures/values.{lovelace, rberry}
use types/order as types_order

// ============================================
// Test Fixtures
// ============================================

const protocol_fee = 2_500_000

/// Standard 1B/1B pool
fn test_pool_state() -> PoolState {
  PoolState {
    quantity_a: (#"", #"", 1_000_000_000),
    quantity_b: (rberry_policy, rberry_asset_name, 1_000_000_000),
    quantity_lp: (#"99999999", #"4c50", 1_000_000_000),
  }
}

// ============================================
// Tests
// ============================================

/// Test donation with no remainder (exact amounts)
test donation_no_remainder() {
  let pool = test_pool_state()

  // Donate 1M ADA + 1M RBERRY (balanced, no remainder)
  let donation_assets =
    ((#"", #"", 1_000_000), (rberry_policy, rberry_asset_name, 1_000_000))

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(types_order.Donation { assets: donation_assets })

  // Input has the donation amount + protocol fee
  let input =
    Output {
      address: user_address(),
      value: lovelace(3_500_000) |> merge(rberry(1_000_000)),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // No remainder output needed - donation ignores this when has_remainder is false
  let output =
    Output {
      address: user_address(),
      value: lovelace(999_999_999_999_999_999),
      datum: NoDatum,
      reference_script: None,
    }

  let new_a, new_b, has_remainder <-
    do_donation(
      pool.quantity_a.1st,
      pool.quantity_a.2nd,
      pool.quantity_a.3rd,
      pool.quantity_b.1st,
      pool.quantity_b.2nd,
      pool.quantity_b.3rd,
      input,
      donation_assets,
      order.destination,
      protocol_fee,
      output,
    )

  expect !has_remainder
  expect new_a == 1_001_000_000
  expect new_b == 1_001_000_000
  True
}

/// Test donation with remainder (unbalanced donation)
test donation_with_remainder() {
  let pool = test_pool_state()

  // Donate only RBERRY (0 ADA, 1M RBERRY) - remainder expected
  let donation_assets =
    ((#"", #"", 0), (rberry_policy, rberry_asset_name, 1_000_000))

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(types_order.Donation { assets: donation_assets })

  let input =
    Output {
      address: user_address(),
      value: lovelace(3_500_000) |> merge(rberry(1_000_000)),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // Remainder: leftover ADA after protocol fee
  let output =
    Output {
      address: user_address(),
      value: lovelace(1_000_000),
      datum: NoDatum,
      reference_script: None,
    }

  let new_a, new_b, has_remainder <-
    do_donation(
      pool.quantity_a.1st,
      pool.quantity_a.2nd,
      pool.quantity_a.3rd,
      pool.quantity_b.1st,
      pool.quantity_b.2nd,
      pool.quantity_b.3rd,
      input,
      donation_assets,
      order.destination,
      protocol_fee,
      output,
    )

  expect has_remainder
  expect new_a == 1_000_000_000
  // No ADA donated
  expect new_b == 1_001_000_000
  True
}

/// Test donation with Self destination (remainder returns to same address/datum)
test donation_self_destination_with_remainder() {
  let pool = test_pool_state()

  // Donate only RBERRY - remainder will go back to Self
  let donation_assets =
    ((#"", #"", 0), (rberry_policy, rberry_asset_name, 1_000_000))

  // Create order with Self destination
  let self_order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(types_order.Donation { assets: donation_assets })
      |> with_self_destination()

  let input =
    Output {
      address: user_address(),
      value: lovelace(3_500_000) |> merge(rberry(1_000_000)),
      datum: InlineDatum(self_order),
      reference_script: None,
    }

  // Remainder: returns to SAME address with SAME datum
  let output =
    Output {
      address: user_address(),
      value: lovelace(1_000_000),
      datum: InlineDatum(self_order),
      reference_script: None,
    }

  let new_a, new_b, has_remainder <-
    do_donation(
      pool.quantity_a.1st,
      pool.quantity_a.2nd,
      pool.quantity_a.3rd,
      pool.quantity_b.1st,
      pool.quantity_b.2nd,
      pool.quantity_b.3rd,
      input,
      donation_assets,
      self_order.destination,
      protocol_fee,
      output,
    )

  expect has_remainder
  expect new_a == 1_000_000_000
  expect new_b == 1_001_000_000
  True
}

/// Test donation fails when remainder output is incorrect
test donation_wrong_remainder_fails() fail {
  let pool = test_pool_state()

  let donation_assets =
    ((#"", #"", 0), (rberry_policy, rberry_asset_name, 1_000_000))

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(types_order.Donation { assets: donation_assets })

  let input =
    Output {
      address: user_address(),
      value: lovelace(3_500_000) |> merge(rberry(1_000_000)),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // Wrong remainder amount
  let output =
    Output {
      address: user_address(),
      value: lovelace(123),
      datum: NoDatum,
      reference_script: None,
    }

  let new_a, new_b, has_remainder <-
    do_donation(
      pool.quantity_a.1st,
      pool.quantity_a.2nd,
      pool.quantity_a.3rd,
      pool.quantity_b.1st,
      pool.quantity_b.2nd,
      pool.quantity_b.3rd,
      input,
      donation_assets,
      order.destination,
      protocol_fee,
      output,
    )

  expect has_remainder
  expect new_a == 1_000_000_000
  expect new_b == 1_001_000_000
  True
}
