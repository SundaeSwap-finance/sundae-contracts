/// Withdrawal calculation tests - rewritten using test fixtures

use calculation/shared.{PoolState}
use calculation/withdrawal.{do_withdrawal}
use cardano/assets.{add, merge}
use cardano/transaction.{InlineDatum, NoDatum, Output}
use mocktail/virgin_address.{mock_pub_key_address}
use tests/fixtures/addresses.{user_address}
use tests/fixtures/constants.{rberry_asset_name, rberry_policy}
use tests/fixtures/datums.{default_swap_order, with_details, with_pool_ident}
use tests/fixtures/values.{lovelace, rberry}
use types/order.{Withdrawal}

// ============================================
// Test Fixtures
// ============================================

const protocol_fee = 2_500_000

const lp_policy = #"99999999999999999999999999999999999999999999999999999999"

const lp_name = "LP"

/// Pool with 1.01B of each asset (after some deposits)
fn test_pool_state() -> PoolState {
  PoolState {
    quantity_a: (#"", #"", 1_010_000_000),
    quantity_b: (rberry_policy, rberry_asset_name, 1_010_000_000),
    quantity_lp: (lp_policy, lp_name, 1_010_000_000),
  }
}

// ============================================
// Tests
// ============================================

/// Test successful withdrawal
test withdrawal_success() {
  let pool = test_pool_state()

  // Withdraw 10M LP tokens
  let amount = (lp_policy, lp_name, 10_000_000)

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Withdrawal { amount })

  // Input: LP tokens + protocol fee buffer
  let input =
    Output {
      address: user_address(),
      value: lovelace(4_500_000) |> add(lp_policy, lp_name, 10_000_000),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // Output: proportional share of pool (10M each of ADA and RBERRY)
  let output =
    Output {
      address: user_address(),
      value: lovelace(2_000_000 + 10_000_000) |> merge(rberry(10_000_000)),
      datum: NoDatum,
      reference_script: None,
    }

  let new_a, new_b, new_lp <-
    do_withdrawal(
      pool.quantity_a.1st,
      pool.quantity_a.2nd,
      pool.quantity_a.3rd,
      pool.quantity_b.1st,
      pool.quantity_b.2nd,
      pool.quantity_b.3rd,
      pool.quantity_lp.1st,
      pool.quantity_lp.2nd,
      pool.quantity_lp.3rd,
      input,
      amount,
      order.destination,
      protocol_fee,
      output,
    )

  // Pool should have 1B of each after withdrawal
  expect new_a == 1_000_000_000
  expect new_b == 1_000_000_000
  expect new_lp == 1_000_000_000
  True
}

/// Test withdrawal fails with incorrect output value
test withdrawal_wrong_value_fails() fail {
  let pool = test_pool_state()
  let amount = (lp_policy, lp_name, 10_000_000)

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Withdrawal { amount })

  let input =
    Output {
      address: user_address(),
      value: lovelace(4_500_000) |> add(lp_policy, lp_name, 10_000_000),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // Wrong output value - missing the withdrawn assets
  let output =
    Output {
      address: user_address(),
      value: lovelace(2_000_000),
      datum: NoDatum,
      reference_script: None,
    }

  let new_a, new_b, new_lp <-
    do_withdrawal(
      pool.quantity_a.1st,
      pool.quantity_a.2nd,
      pool.quantity_a.3rd,
      pool.quantity_b.1st,
      pool.quantity_b.2nd,
      pool.quantity_b.3rd,
      pool.quantity_lp.1st,
      pool.quantity_lp.2nd,
      pool.quantity_lp.3rd,
      input,
      amount,
      order.destination,
      protocol_fee,
      output,
    )

  expect new_a == 1_000_000_000
  expect new_b == 1_000_000_000
  expect new_lp == 1_000_000_000
  True
}

/// Test withdrawal fails when paid to wrong address
test withdrawal_wrong_address_fails() fail {
  let pool = test_pool_state()
  let amount = (lp_policy, lp_name, 10_000_000)

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Withdrawal { amount })

  let input =
    Output {
      address: user_address(),
      value: lovelace(4_500_000) |> add(lp_policy, lp_name, 10_000_000),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // Correct value but wrong destination address
  let wrong_address = mock_pub_key_address(99, None)
  let output =
    Output {
      address: wrong_address,
      value: lovelace(2_000_000 + 10_000_000) |> merge(rberry(10_000_000)),
      datum: NoDatum,
      reference_script: None,
    }

  let new_a, new_b, new_lp <-
    do_withdrawal(
      pool.quantity_a.1st,
      pool.quantity_a.2nd,
      pool.quantity_a.3rd,
      pool.quantity_b.1st,
      pool.quantity_b.2nd,
      pool.quantity_b.3rd,
      pool.quantity_lp.1st,
      pool.quantity_lp.2nd,
      pool.quantity_lp.3rd,
      input,
      amount,
      order.destination,
      protocol_fee,
      output,
    )

  expect new_a == 1_000_000_000
  expect new_b == 1_000_000_000
  expect new_lp == 1_000_000_000
  True
}
