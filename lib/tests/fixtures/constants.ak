/// Central constants used across all test files.
/// This consolidates what was previously scattered across multiple test helper files.

// ============================================
// Script Hashes
// ============================================

pub const settings_policy_id =
  #"00000000000000000000000000000000000000000000000000000000"

pub const pool_script_hash =
  #"00000000000000000000000000000000000000000000000000000001"

pub const order_script_hash =
  #"00000000000000000000000000000000000000000000000000000002"

pub const stake_script_hash =
  #"00000000000000000000000000000000000000000000000000000003"

pub const manage_stake_script_hash =
  #"00000000000000000000000000000000000000000000000000000003"

// ============================================
// Test Keys
// ============================================

pub const scooper =
  #"00000000000000000000000000000000000000000000000000000004"

pub const not_a_scooper =
  #"11111111111111111111111111111111111111111111111111111114"

/// Extra scooper that can be added to a pool's extra_scoopers list
pub const extra_scooper =
  #"22222222222222222222222222222222222222222222222222222224"

pub const payment_key =
  #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"

pub const stake_key =
  #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

pub const treasury_admin_key =
  #"17bbd2d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

pub const random_hash =
  #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"

pub const other_hash =
  #"01010101010101010101010101010101010101010101010101010101"

// ============================================
// Test Tokens
// ============================================

pub const rberry_policy =
  #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"

pub const rberry_asset_name = #"524245525259"

/// Tuple form for convenience: (policy, name)
pub const rberry = (#"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77", #"524245525259")

pub const sundae_policy =
  #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"

pub const sundae_asset_name = #"53554e444145"

/// Tuple form for convenience: (policy, name)
pub const sundae = (#"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77", #"53554e444145")

pub const other_policy =
  #"fafafafafafafafafafafafafafafafafafafafafafafafafafafafa"

pub const other_asset_name = #"fafafafa"

pub const empty_asset_name = #""

// ============================================
// Identifiers
// ============================================

pub const pool_ident =
  #"00000000000000000000000000000000000000000000000000000003"

// ============================================
// Common Amounts
// ============================================

/// Minimum ADA for outputs (2 ADA)
pub const min_ada = 2_000_000

/// Standard protocol fee (2.5 ADA)
pub const protocol_fee = 2_500_000

/// One billion lovelace (1000 ADA) - common reserve amount
pub const billion = 1_000_000_000

/// One million lovelace (1 ADA)
pub const million = 1_000_000

/// Default LP fee in basis points (0.05% = 5/10000)
pub const default_lp_fee = 5

/// High LP fee for testing (1% = 100/10000)
pub const high_lp_fee = 100

// ============================================
// Treasury/Admin Keys
// ============================================

pub const settings_admin_key =
  #"6313a1d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

pub const metadata_admin_key =
  #"7b143ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"

pub const example_treasury_admin =
  #"17bbd2d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

pub const example_treasury_address =
  #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"
