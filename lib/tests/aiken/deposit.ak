/// Deposit calculation tests - rewritten using test fixtures

use aiken/math
use calculation/deposit.{do_deposit}
use calculation/shared.{PoolState}
use cardano/assets.{from_asset, merge}
use cardano/transaction.{InlineDatum, NoDatum, Output}
use tests/fixtures/addresses.{user_address}
use tests/fixtures/constants.{rberry_asset_name, rberry_policy}
use tests/fixtures/datums.{default_swap_order, with_details, with_pool_ident}
use tests/fixtures/values.{lovelace, rberry}
use types/order.{Deposit}

// ============================================
// Test Fixtures
// ============================================

/// Standard protocol fee
const protocol_fee = 2_500_000

/// LP token identifiers for tests
const lp_policy = #"99999999999999999999999999999999999999999999999999999999"

const lp_name = "LP"

/// Create a pool state with given quantities
fn pool_state(qty_a: Int, qty_b: Int) -> PoolState {
  expect Some(qty_lp) = math.sqrt(qty_a * qty_b)
  PoolState {
    quantity_a: (#"", #"", qty_a),
    quantity_b: (rberry_policy, rberry_asset_name, qty_b),
    quantity_lp: (lp_policy, lp_name, qty_lp),
  }
}

// ============================================
// Tests
// ============================================

/// Test successful deposit with balanced assets
test deposit_balanced() {
  // Pool: 1B ADA / 1B RBERRY
  let pool = pool_state(1_000_000_000, 1_000_000_000)

  // Deposit: 100M ADA + 50M RBERRY (will use 50M of each due to ratio)
  let deposit_assets =
    ((#"", #"", 200_000_000), (rberry_policy, rberry_asset_name, 500_000_000))

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Deposit { assets: deposit_assets })

  // User has 100M ADA + 50M RBERRY in order
  let input =
    Output {
      address: user_address(),
      value: lovelace(100_000_000) |> merge(rberry(50_000_000)),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // User receives: remainder + LP tokens (50M LP for 50M:50M deposit)
  let output =
    Output {
      address: user_address(),
      value: lovelace(47_500_000)
        |> merge(from_asset(lp_policy, lp_name, 50_000_000)),
      datum: NoDatum,
      reference_script: None,
    }

  let _, _, _ <-
    do_deposit(
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
      deposit_assets,
      order.destination,
      protocol_fee,
      output,
    )
  True
}

/// Test deposit fails when LP tokens issued would be zero
test deposit_too_small_fails() fail {
  // Pool with very large imbalance
  let pool = pool_state(661_278_808_416, 188_253_559_159_646)

  // Tiny deposit relative to pool size
  let deposit_assets =
    ((#"", #"", 151_171_875), (rberry_policy, rberry_asset_name, 150))

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Deposit { assets: deposit_assets })

  let input =
    Output {
      address: user_address(),
      value: lovelace(153_000_000) |> merge(rberry(150)),
      datum: InlineDatum(order),
      reference_script: None,
    }

  let output =
    Output {
      address: user_address(),
      value: lovelace(152_000_000),
      datum: NoDatum,
      reference_script: None,
    }

  let _, _, _ <-
    do_deposit(
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
      deposit_assets,
      order.destination,
      protocol_fee,
      output,
    )
  True
}
