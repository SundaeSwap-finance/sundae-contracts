/// Datum fixtures and fluent builders for testing.
/// Provides default datums and builder functions for pool, order, and settings.

use cardano/transaction.{NoDatum}
use shared.{pool_lp_name}
use sundae/multisig.{MultisigScript, Signature}
use tests/fixtures/addresses.{treasury_address, user_address}
use tests/fixtures/constants
use tests/fixtures/constants.{payment_key, rberry, scooper, treasury_admin_key}
use types/order.{
  Deposit, Destination, Donation, Fixed, Order, OrderDatum, Self, Swap,
  Withdrawal,
}
use types/pool.{PoolDatum}
use types/settings.{SettingsDatum}

// ============================================
// Pool Datum Builders
// ============================================

/// Create a default ADA/RBERRY pool datum with sensible test defaults
pub fn default_pool_datum() -> PoolDatum {
  PoolDatum {
    identifier: constants.pool_ident,
    assets: ((#"", #""), rberry),
    circulating_lp: 1_000_000_000,
    lp_fee_basis_points: (5, 5),
    protocol_fee_basis_points: (0, 0),
    fee_manager: None,
    staking_admin: None,
    extra_scoopers: [],
    market_open: 0,
    protocol_fees: (2_000_000, 0, 0),
  }
}

/// Set the pool identifier
pub fn with_identifier(datum: PoolDatum, ident: ByteArray) -> PoolDatum {
  PoolDatum { ..datum, identifier: ident }
}

/// Set the pool assets
pub fn with_assets(
  datum: PoolDatum,
  asset_a: (ByteArray, ByteArray),
  asset_b: (ByteArray, ByteArray),
) -> PoolDatum {
  PoolDatum { ..datum, assets: (asset_a, asset_b) }
}

/// Set LP fees (bid and ask)
pub fn with_lp_fees(datum: PoolDatum, bid: Int, ask: Int) -> PoolDatum {
  PoolDatum { ..datum, lp_fee_basis_points: (bid, ask) }
}

/// Set the circulating LP supply
pub fn with_circulating_lp(datum: PoolDatum, amount: Int) -> PoolDatum {
  PoolDatum { ..datum, circulating_lp: amount }
}

/// Set protocol fees collected (ada, asset_a, asset_b)
pub fn with_protocol_fees(
  datum: PoolDatum,
  ada: Int,
  asset_a: Int,
  asset_b: Int,
) -> PoolDatum {
  PoolDatum { ..datum, protocol_fees: (ada, asset_a, asset_b) }
}

/// Set the fee manager
pub fn with_fee_manager(
  datum: PoolDatum,
  manager: Option<MultisigScript>,
) -> PoolDatum {
  PoolDatum { ..datum, fee_manager: manager }
}

/// Set the market open timestamp
pub fn with_market_open(datum: PoolDatum, timestamp: Int) -> PoolDatum {
  PoolDatum { ..datum, market_open: timestamp }
}

/// Set the protocol fee basis points (protocol's cut of LP fees)
pub fn with_protocol_fee_basis_points(
  datum: PoolDatum,
  bid: Int,
  ask: Int,
) -> PoolDatum {
  PoolDatum { ..datum, protocol_fee_basis_points: (bid, ask) }
}

/// Set the staking admin
pub fn with_staking_admin(
  datum: PoolDatum,
  admin: Option<MultisigScript>,
) -> PoolDatum {
  PoolDatum { ..datum, staking_admin: admin }
}

/// Set extra scoopers for this pool
pub fn with_extra_scoopers(
  datum: PoolDatum,
  scoopers: List<ByteArray>,
) -> PoolDatum {
  PoolDatum { ..datum, extra_scoopers: scoopers }
}

// ============================================
// Order Datum Builders
// ============================================

/// Create a default swap order (ADA -> RBERRY)
pub fn default_swap_order() -> OrderDatum {
  let pool_ident = Some(constants.pool_ident)
  let owner = Signature { key_hash: payment_key }
  let destination = Fixed { address: user_address(), datum: NoDatum }
  let details =
    Swap {
      offer: (#"", #"", 10_000_000),
      min_received: (rberry.1st, rberry.2nd, 9_500_000),
    }
  OrderDatum {
    pool_ident,
    owner,
    max_protocol_fee: 2_500_000,
    destination,
    details,
    extension: Void,
  }
}

/// Create a swap order with custom amounts
pub fn swap_order(
  offer_policy: ByteArray,
  offer_name: ByteArray,
  offer_amount: Int,
  min_policy: ByteArray,
  min_name: ByteArray,
  min_amount: Int,
) -> OrderDatum {
  let pool_ident = Some(constants.pool_ident)
  let owner = Signature { key_hash: payment_key }
  let destination = Fixed { address: user_address(), datum: NoDatum }
  let details =
    Swap {
      offer: (offer_policy, offer_name, offer_amount),
      min_received: (min_policy, min_name, min_amount),
    }
  OrderDatum {
    pool_ident,
    owner,
    max_protocol_fee: 2_500_000,
    destination,
    details,
    extension: Void,
  }
}

/// Create a deposit order
pub fn deposit_order(ada_amount: Int, token_amount: Int) -> OrderDatum {
  let pool_ident = Some(constants.pool_ident)
  let owner = Signature { key_hash: payment_key }
  let destination = Fixed { address: user_address(), datum: NoDatum }
  let details =
    Deposit {
      assets: ((#"", #"", ada_amount), (rberry.1st, rberry.2nd, token_amount)),
    }
  OrderDatum {
    pool_ident,
    owner,
    max_protocol_fee: 2_500_000,
    destination,
    details,
    extension: Void,
  }
}

/// Create a withdrawal order
pub fn withdrawal_order(lp_amount: Int) -> OrderDatum {
  let pool_ident = Some(constants.pool_ident)
  let owner = Signature { key_hash: payment_key }
  let destination = Fixed { address: user_address(), datum: NoDatum }
  let amount =
    (
      constants.settings_policy_id,
      pool_lp_name(constants.pool_ident),
      lp_amount,
    )
  let details = Withdrawal { amount }
  OrderDatum {
    pool_ident,
    owner,
    max_protocol_fee: 2_500_000,
    destination,
    details,
    extension: Void,
  }
}

/// Create a donation order
pub fn donation_order(ada_amount: Int, token_amount: Int) -> OrderDatum {
  let pool_ident = Some(constants.pool_ident)
  let owner = Signature { key_hash: payment_key }
  let destination = Fixed { address: user_address(), datum: NoDatum }
  let details =
    Donation {
      assets: ((#"", #"", ada_amount), (rberry.1st, rberry.2nd, token_amount)),
    }
  OrderDatum {
    pool_ident,
    owner,
    max_protocol_fee: 2_500_000,
    destination,
    details,
    extension: Void,
  }
}

/// Set the order owner
pub fn with_owner(datum: OrderDatum, owner: MultisigScript) -> OrderDatum {
  OrderDatum { ..datum, owner: owner }
}

/// Set the pool identifier for the order
pub fn with_pool_ident(datum: OrderDatum, ident: Option<ByteArray>) -> OrderDatum {
  OrderDatum { ..datum, pool_ident: ident }
}

/// Set the max protocol fee
pub fn with_max_protocol_fee(datum: OrderDatum, fee: Int) -> OrderDatum {
  OrderDatum { ..datum, max_protocol_fee: fee }
}

/// Set the destination
pub fn with_destination(datum: OrderDatum, dest: Destination) -> OrderDatum {
  OrderDatum { ..datum, destination: dest }
}

/// Set to use self-destination (result goes back to source)
pub fn with_self_destination(datum: OrderDatum) -> OrderDatum {
  OrderDatum { ..datum, destination: Self }
}

/// Set the order details
pub fn with_details(datum: OrderDatum, details: Order) -> OrderDatum {
  OrderDatum { ..datum, details: details }
}

// ============================================
// Settings Datum Builders
// ============================================

/// Create a default settings datum for testing
pub fn default_settings_datum() -> SettingsDatum {
  SettingsDatum {
    settings_admin: Signature { key_hash: treasury_admin_key },
    metadata_admin: treasury_address(),
    treasury_admin: Signature { key_hash: treasury_admin_key },
    treasury_address: treasury_address(),
    treasury_allowance: (1, 10),
    authorized_scoopers: Some([scooper]),
    authorized_staking_keys: [],
    base_fee: 1_000_000,
    simple_fee: 1_500_000,
    strategy_fee: 2_000_000,
    pool_creation_fee: 0,
    extensions: [],
  }
}

/// Set the authorized scoopers
pub fn with_scoopers(
  datum: SettingsDatum,
  scoopers: Option<List<ByteArray>>,
) -> SettingsDatum {
  SettingsDatum { ..datum, authorized_scoopers: scoopers }
}

/// Set the base fee
pub fn with_base_fee(datum: SettingsDatum, fee: Int) -> SettingsDatum {
  SettingsDatum { ..datum, base_fee: fee }
}

/// Set the simple fee
pub fn with_simple_fee(datum: SettingsDatum, fee: Int) -> SettingsDatum {
  SettingsDatum { ..datum, simple_fee: fee }
}

/// Set all fees at once
pub fn with_fees(
  datum: SettingsDatum,
  base: Int,
  simple: Int,
  strategy: Int,
) -> SettingsDatum {
  SettingsDatum {
    ..datum,
    base_fee: base,
    simple_fee: simple,
    strategy_fee: strategy,
  }
}

/// Set the settings admin
pub fn with_settings_admin(
  datum: SettingsDatum,
  admin: MultisigScript,
) -> SettingsDatum {
  SettingsDatum { ..datum, settings_admin: admin }
}

/// Set the treasury admin
pub fn with_treasury_admin(
  datum: SettingsDatum,
  admin: MultisigScript,
) -> SettingsDatum {
  SettingsDatum { ..datum, treasury_admin: admin }
}
