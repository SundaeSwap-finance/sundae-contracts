/// Swap calculation tests - rewritten using test fixtures
///
/// Compare with swap.ak to see the improvement in readability

use calculation/shared.{PoolState}
use calculation/swap.{do_swap}
use cardano/assets.{merge}
use cardano/transaction.{InlineDatum, NoDatum, Output}
use tests/fixtures/addresses.{user_address}
use tests/fixtures/values.{lovelace, rberry}
use tests/fixtures/constants.{rberry_asset_name, rberry_policy}
use tests/fixtures/datums.{default_swap_order, with_details, with_pool_ident}
use types/order.{Swap}

// ============================================
// Test Fixtures
// ============================================

/// Standard pool state for swap tests: 1B ADA / 1B RBERRY
fn test_pool_state() -> PoolState {
  PoolState {
    quantity_a: (#"", #"", 1_000_000_000),
    quantity_b: (rberry_policy, rberry_asset_name, 1_000_000_000),
    quantity_lp: (#"99999999", #"4c50", 1_000_000_000),
  }
}

/// Standard protocol fee for tests
const protocol_fee = 2_500_000

/// Standard LP fee (0.05%)
const lp_fee = 5

// ============================================
// Tests
// ============================================

/// Test that swap fails when min_received is set too high
test swap_min_received_too_high() fail {
  let pool = test_pool_state()

  // Order: swap 10 ADA for RBERRY, but demand more than possible
  let swap_offer = (#"", #"", 10_000_000)
  let swap_min = (rberry_policy, rberry_asset_name, 10_000_000)

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Swap { offer: swap_offer, min_received: swap_min })

  // Input: order value (10 ADA + 2.5 ADA protocol fee + 2 ADA for output)
  let input =
    Output {
      address: user_address(),
      value: lovelace(14_500_000),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // Output: what the user would receive (actual swap gives ~9.896M RBERRY)
  let output =
    Output {
      address: user_address(),
      value: lovelace(2_000_000) |> merge(rberry(9_896_088)),
      datum: NoDatum,
      reference_script: None,
    }

  // Execute swap - should fail because min_received (10M) > actual (9.896M)
  let new_a, new_b <-
    do_swap(
      pool.quantity_a.1st,
      pool.quantity_a.2nd,
      pool.quantity_a.3rd,
      pool.quantity_b.1st,
      pool.quantity_b.2nd,
      pool.quantity_b.3rd,
      input,
      order.destination,
      lp_fee,
      lp_fee,
      protocol_fee,
      swap_offer,
      swap_min,
      output,
    )

  expect new_a == 1_000_000_000 + 10_000_000
  expect new_b == 1_000_000_000 - 9_896_088
  True
}

/// Test swap with asymmetric bid/ask fees (higher ask fee)
test swap_with_asymmetric_fees() {
  let pool = test_pool_state()

  // Order: swap 10M RBERRY for ADA (selling RBERRY, so ask fee applies)
  let swap_offer = (rberry_policy, rberry_asset_name, 10_000_000)
  let swap_min = (#"", #"", 0)

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Swap { offer: swap_offer, min_received: swap_min })

  // Input: RBERRY + protocol fee
  let input =
    Output {
      address: user_address(),
      value: lovelace(4_500_000) |> merge(rberry(10_000_000)),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // Output: ADA received (less than with symmetric fees due to 1% ask fee)
  let output =
    Output {
      address: user_address(),
      value: lovelace(11_802_950),
      datum: NoDatum,
      reference_script: None,
    }

  let bid_fee = 5
  // 0.05%
  let ask_fee = 100
  // 1.00%

  let new_a, new_b <-
    do_swap(
      pool.quantity_a.1st,
      pool.quantity_a.2nd,
      pool.quantity_a.3rd,
      pool.quantity_b.1st,
      pool.quantity_b.2nd,
      pool.quantity_b.3rd,
      input,
      order.destination,
      bid_fee,
      ask_fee,
      protocol_fee,
      swap_offer,
      swap_min,
      output,
    )

  // With 0.05% bid fee, user would get 9,896,088 ADA
  // With 1.00% ask fee, user gets less: 9,802,950 ADA
  expect new_a == 1_000_000_000 - 9_802_950
  expect new_b == 1_000_000_000 + 10_000_000
  True
}
