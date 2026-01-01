/// Address fixtures for testing.
/// Provides consistent mock addresses for pools, orders, users, and treasury.

use cardano/address.{Address, Inline, Script, VerificationKey}
use mocktail/virgin_address.{
  mock_pub_key_address, mock_pub_key_stake_cred, mock_script_address,
}
use tests/fixtures/constants.{
  order_script_hash, payment_key, pool_script_hash, stake_key, stake_script_hash,
  treasury_admin_key,
}

// ============================================
// Script Addresses
// ============================================

/// The pool script address (no stake credential)
pub fn pool_address() -> Address {
  Address {
    payment_credential: Script(pool_script_hash),
    stake_credential: None,
  }
}

/// The pool script address with stake credential attached
pub fn pool_address_with_stake() -> Address {
  Address {
    payment_credential: Script(pool_script_hash),
    stake_credential: Some(Inline(Script(stake_script_hash))),
  }
}

/// The order script address
pub fn order_address() -> Address {
  Address {
    payment_credential: Script(order_script_hash),
    stake_credential: None,
  }
}

// ============================================
// Wallet Addresses
// ============================================

/// A typical user wallet address (payment key only)
pub fn user_address() -> Address {
  Address {
    payment_credential: VerificationKey(payment_key),
    stake_credential: None,
  }
}

/// A user wallet address with staking
pub fn user_address_with_stake() -> Address {
  Address {
    payment_credential: VerificationKey(payment_key),
    stake_credential: Some(Inline(VerificationKey(stake_key))),
  }
}

/// The treasury address for fee withdrawals
pub fn treasury_address() -> Address {
  Address {
    payment_credential: VerificationKey(treasury_admin_key),
    stake_credential: None,
  }
}

// ============================================
// Mocktail-based addresses (for variation)
// ============================================

/// Create a numbered user address (useful for multiple users in a test)
pub fn user_address_n(n: Int) -> Address {
  mock_pub_key_address(n, None)
}

/// Create a numbered user address with stake credential
pub fn user_address_n_with_stake(n: Int) -> Address {
  mock_pub_key_address(n, Some(mock_pub_key_stake_cred(n)))
}

/// Create a numbered script address (useful for multiple scripts in a test)
pub fn script_address_n(n: Int) -> Address {
  mock_script_address(n, None)
}
