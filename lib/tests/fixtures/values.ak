/// Asset and Value fixtures for testing.
/// Provides helpers for constructing common token values.

use cardano/assets.{Value, add, from_asset, from_lovelace, zero}
use shared.{pool_lp_name, pool_nft_name, pool_ref_name}
use tests/fixtures/constants.{
  other_asset_name, other_policy, pool_ident, rberry_asset_name, rberry_policy,
  settings_policy_id, sundae_asset_name, sundae_policy,
}
use types/settings.{settings_nft_name}

// ============================================
// ADA helpers
// ============================================

/// Standard ADA amount (1 ADA = 1,000,000 lovelace)
pub const ada = 1_000_000

/// Create a value with only ADA
pub fn lovelace(amount: Int) -> Value {
  from_lovelace(amount)
}

// ============================================
// Token value constructors
// ============================================

/// Create a value with RBERRY tokens
pub fn rberry(amount: Int) -> Value {
  from_asset(rberry_policy, rberry_asset_name, amount)
}

/// Create a value with SUNDAE tokens
pub fn sundae(amount: Int) -> Value {
  from_asset(sundae_policy, sundae_asset_name, amount)
}

/// Create a value with an "other" test token
pub fn other_token(amount: Int) -> Value {
  from_asset(other_policy, other_asset_name, amount)
}

/// Create a value with a custom token
pub fn token(policy: ByteArray, name: ByteArray, amount: Int) -> Value {
  from_asset(policy, name, amount)
}

// ============================================
// Pool token constructors
// ============================================

/// Pool NFT value (the CIP-68 222 token held by the pool)
pub fn pool_nft() -> Value {
  pool_nft_for(pool_ident)
}

/// Pool NFT for a specific pool identifier
pub fn pool_nft_for(ident: ByteArray) -> Value {
  from_asset(settings_policy_id, pool_nft_name(ident), 1)
}

/// Pool LP token value
pub fn pool_lp(amount: Int) -> Value {
  pool_lp_for(pool_ident, amount)
}

/// Pool LP token for a specific pool identifier
pub fn pool_lp_for(ident: ByteArray, amount: Int) -> Value {
  from_asset(settings_policy_id, pool_lp_name(ident), amount)
}

/// Pool reference token (CIP-68 100 for metadata)
pub fn pool_ref() -> Value {
  pool_ref_for(pool_ident)
}

/// Pool reference token for a specific pool identifier
pub fn pool_ref_for(ident: ByteArray) -> Value {
  from_asset(settings_policy_id, pool_ref_name(ident), 1)
}

// ============================================
// Settings token
// ============================================

/// The settings NFT that authenticates the settings UTXO
pub fn settings_nft() -> Value {
  from_asset(settings_policy_id, settings_nft_name, 1)
}

// ============================================
// Common pool values
// ============================================

/// A typical ADA/RBERRY pool value
/// Includes: pool NFT + 1B lovelace + 1B RBERRY
pub fn ada_rberry_pool_value() -> Value {
  zero
    |> add(settings_policy_id, pool_nft_name(pool_ident), 1)
    |> add(#"", #"", 1_000_000_000)
    |> add(rberry_policy, rberry_asset_name, 1_000_000_000)
}

/// A typical ADA/RBERRY pool value with protocol fees
pub fn ada_rberry_pool_value_with_fees(protocol_fees: Int) -> Value {
  zero
    |> add(settings_policy_id, pool_nft_name(pool_ident), 1)
    |> add(#"", #"", 1_000_000_000 + protocol_fees)
    |> add(rberry_policy, rberry_asset_name, 1_000_000_000)
}

// ============================================
// Common order values
// ============================================

/// A swap order value: 10 ADA (includes 2.5 ADA for fees)
pub fn swap_order_value() -> Value {
  from_lovelace(12_500_000)
}

/// A swap order offering RBERRY
pub fn rberry_swap_order_value(rberry_amount: Int) -> Value {
  from_lovelace(2_500_000) |> add(rberry_policy, rberry_asset_name, rberry_amount)
}

/// A deposit order value
pub fn deposit_order_value(ada_amount: Int, rberry_amount: Int) -> Value {
  from_lovelace(ada_amount + 2_500_000)
    |> add(rberry_policy, rberry_asset_name, rberry_amount)
}

/// A withdrawal order value (LP tokens + fee)
pub fn withdrawal_order_value(lp_amount: Int) -> Value {
  from_lovelace(2_500_000)
    |> add(settings_policy_id, pool_lp_name(pool_ident), lp_amount)
}
