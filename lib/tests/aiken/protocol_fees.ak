/// Protocol fee cut tests
///
/// Tests for the protocol's cut of LP fees during swaps.
/// The protocol_fee_basis_points field determines what percentage of LP fees
/// go to the protocol vs staying in the pool for LPs.

use calculation/swap.{do_swap}
use cardano/assets.{merge}
use cardano/transaction.{InlineDatum, NoDatum, Output}
use tests/fixtures/addresses.{user_address}
use tests/fixtures/constants.{rberry_asset_name, rberry_policy}
use tests/fixtures/datums.{default_swap_order, with_details, with_pool_ident}
use tests/fixtures/values.{lovelace, rberry}
use types/order.{Swap}

// ============================================
// Constants
// ============================================

/// Standard protocol fee (ADA charged per order)
const protocol_fee = 2_500_000

/// Pool reserves
const pool_reserve = 1_000_000_000

// ============================================
// Protocol Fee Cut Tests
// ============================================

/// Test swap with 10% protocol cut of LP fees (bid direction: ADA -> Token)
/// With 0.05% LP fee and 10% protocol cut:
/// - User swaps 10M ADA for tokens
/// - LP fee ~= 10M * 0.0005 = 5000 ADA worth
/// - Protocol cut = 5000 * 0.10 = 500 ADA equivalent
test swap_protocol_fee_cut_bid_10_percent() {
  // Setup: 10% protocol cut (1000 basis points = 10%)
  let lp_fee = 5
  // 0.05%
  let protocol_cut = 1000
  // 10% of LP fee

  let swap_offer = (#"", #"", 10_000_000)
  let swap_min = (rberry_policy, rberry_asset_name, 0)

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Swap { offer: swap_offer, min_received: swap_min })

  let input =
    Output {
      address: user_address(),
      value: lovelace(14_500_000),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // User receives tokens (same as without protocol cut - they don't pay more)
  let output =
    Output {
      address: user_address(),
      value: lovelace(2_000_000) |> merge(rberry(9_896_088)),
      datum: NoDatum,
      reference_script: None,
    }

  let new_a, new_b, delta_fee_a, delta_fee_b <-
    do_swap(
      #"",
      #"",
      pool_reserve,
      rberry_policy,
      rberry_asset_name,
      pool_reserve,
      input,
      order.destination,
      lp_fee,
      lp_fee,
      protocol_cut,
      protocol_cut,
      protocol_fee,
      swap_offer,
      swap_min,
      output,
    )

  // The delta_fee_a should be the protocol's cut of the LP fee
  // gives = 10_000_000, lp_fee = 5, protocol_cut = 1000
  // protocol_fee_cut = 10_000_000 * 5 * 1000 / 100_000_000 = 500
  expect delta_fee_a == 500
  expect delta_fee_b == 0

  // Pool A reserves increase by (gives - protocol_cut)
  // new_a = pool_reserve + gives - protocol_fee_cut = 1B + 10M - 500
  expect new_a == pool_reserve + 10_000_000 - 500
  expect new_b == pool_reserve - 9_896_088
  True
}

/// Test swap with 10% protocol cut in ask direction (Token -> ADA)
/// Protocol cut should be taken from asset B (the token) in this case
test swap_protocol_fee_cut_ask_10_percent() {
  let lp_fee = 5
  // 0.05%
  let protocol_cut = 1000
  // 10% of LP fee

  let swap_offer = (rberry_policy, rberry_asset_name, 10_000_000)
  let swap_min = (#"", #"", 0)

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Swap { offer: swap_offer, min_received: swap_min })

  // Input: user has tokens + ADA for protocol fee
  let input =
    Output {
      address: user_address(),
      value: lovelace(4_500_000) |> merge(rberry(10_000_000)),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // Output: user receives ADA (swap result + leftover from input minus protocol fee)
  // takes = 10M * 1B / (1B + 10M * 0.9995) = 9,896,088 ADA
  // User gets: 9,896,088 ADA from swap + 2M min ADA
  let output =
    Output {
      address: user_address(),
      value: lovelace(2_000_000 + 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let new_a, new_b, delta_fee_a, delta_fee_b <-
    do_swap(
      #"",
      #"",
      pool_reserve,
      rberry_policy,
      rberry_asset_name,
      pool_reserve,
      input,
      order.destination,
      lp_fee,
      lp_fee,
      protocol_cut,
      protocol_cut,
      protocol_fee,
      swap_offer,
      swap_min,
      output,
    )

  // For ask direction, protocol cut comes from asset B (token)
  // gives = 10_000_000 tokens, lp_fee = 5, protocol_cut = 1000
  // protocol_fee_cut = 10_000_000 * 5 * 1000 / 100_000_000 = 500
  expect delta_fee_a == 0
  expect delta_fee_b == 500

  // Pool B reserves increase by (gives - protocol_cut)
  expect new_b == pool_reserve + 10_000_000 - 500
  expect new_a == pool_reserve - 9_896_088
  True
}

/// Test swap with zero protocol cut - all fees stay with LPs
test swap_zero_protocol_fee_cut() {
  let lp_fee = 5
  let protocol_cut = 0
  // No protocol cut

  let swap_offer = (#"", #"", 10_000_000)
  let swap_min = (rberry_policy, rberry_asset_name, 0)

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Swap { offer: swap_offer, min_received: swap_min })

  let input =
    Output {
      address: user_address(),
      value: lovelace(14_500_000),
      datum: InlineDatum(order),
      reference_script: None,
    }

  let output =
    Output {
      address: user_address(),
      value: lovelace(2_000_000) |> merge(rberry(9_896_088)),
      datum: NoDatum,
      reference_script: None,
    }

  let new_a, new_b, delta_fee_a, delta_fee_b <-
    do_swap(
      #"",
      #"",
      pool_reserve,
      rberry_policy,
      rberry_asset_name,
      pool_reserve,
      input,
      order.destination,
      lp_fee,
      lp_fee,
      protocol_cut,
      protocol_cut,
      protocol_fee,
      swap_offer,
      swap_min,
      output,
    )

  // No protocol cut means no fee deltas
  expect delta_fee_a == 0
  expect delta_fee_b == 0

  // Full amount goes to pool reserves
  expect new_a == pool_reserve + 10_000_000
  expect new_b == pool_reserve - 9_896_088
  True
}

/// Test swap with 100% protocol cut - all LP fees go to protocol
test swap_max_protocol_fee_cut() {
  let lp_fee = 5
  // 0.05%
  let protocol_cut = 10000
  // 100% of LP fee goes to protocol

  let swap_offer = (#"", #"", 10_000_000)
  let swap_min = (rberry_policy, rberry_asset_name, 0)

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Swap { offer: swap_offer, min_received: swap_min })

  let input =
    Output {
      address: user_address(),
      value: lovelace(14_500_000),
      datum: InlineDatum(order),
      reference_script: None,
    }

  let output =
    Output {
      address: user_address(),
      value: lovelace(2_000_000) |> merge(rberry(9_896_088)),
      datum: NoDatum,
      reference_script: None,
    }

  let new_a, new_b, delta_fee_a, delta_fee_b <-
    do_swap(
      #"",
      #"",
      pool_reserve,
      rberry_policy,
      rberry_asset_name,
      pool_reserve,
      input,
      order.destination,
      lp_fee,
      lp_fee,
      protocol_cut,
      protocol_cut,
      protocol_fee,
      swap_offer,
      swap_min,
      output,
    )

  // 100% protocol cut: gives * lp_fee * 10000 / 100_000_000 = gives * lp_fee / 10000
  // = 10_000_000 * 5 / 10000 = 5000
  expect delta_fee_a == 5000
  expect delta_fee_b == 0

  // Pool reserves reduced by the full LP fee
  expect new_a == pool_reserve + 10_000_000 - 5000
  expect new_b == pool_reserve - 9_896_088
  True
}

/// Test swap with asymmetric protocol fees (different bid vs ask cuts)
test swap_asymmetric_protocol_fee_cuts() {
  let lp_fee = 100
  // 1% LP fee
  let protocol_bid_cut = 500
  // 5% of LP fee on bids
  let protocol_ask_cut = 2000
  // 20% of LP fee on asks

  // Bid direction swap (ADA -> Token)
  let swap_offer = (#"", #"", 10_000_000)
  let swap_min = (rberry_policy, rberry_asset_name, 0)

  let order =
    default_swap_order()
      |> with_pool_ident(None)
      |> with_details(Swap { offer: swap_offer, min_received: swap_min })

  let input =
    Output {
      address: user_address(),
      value: lovelace(14_500_000),
      datum: InlineDatum(order),
      reference_script: None,
    }

  // With 1% LP fee, user receives less tokens
  // takes = 10M * 1B / (1B + 10M * 0.99) = ~9,802,950
  let output =
    Output {
      address: user_address(),
      value: lovelace(2_000_000) |> merge(rberry(9_802_950)),
      datum: NoDatum,
      reference_script: None,
    }

  let new_a, new_b, delta_fee_a, delta_fee_b <-
    do_swap(
      #"",
      #"",
      pool_reserve,
      rberry_policy,
      rberry_asset_name,
      pool_reserve,
      input,
      order.destination,
      lp_fee,
      lp_fee,
      protocol_bid_cut,
      protocol_ask_cut,
      protocol_fee,
      swap_offer,
      swap_min,
      output,
    )

  // Bid direction uses protocol_bid_cut (5%)
  // protocol_fee_cut = 10_000_000 * 100 * 500 / 100_000_000 = 5000
  expect delta_fee_a == 5000
  expect delta_fee_b == 0
  expect new_a == pool_reserve + 10_000_000 - 5000
  expect new_b == pool_reserve - 9_802_950
  True
}

