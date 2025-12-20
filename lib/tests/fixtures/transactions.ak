/// Transaction builder fixtures for pool validator tests.
/// Provides fluent builders for constructing test transactions with minimal boilerplate.

use aiken/collection/list
use aiken/interval
use cardano/address.{Address, Inline, Script, VerificationKey}
use cardano/assets.{add, from_lovelace}
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, Transaction}
use mocktail/virgin_output_reference.{mock_tx_hash, mock_utxo_ref}
use shared.{pool_lp_name, pool_nft_name}
use sundae/multisig.{Signature}
use tests/fixtures/addresses.{
  order_address, pool_address, treasury_address, user_address,
}
use tests/fixtures/constants.{
  pool_ident, pool_script_hash, rberry_asset_name, rberry_policy,
  settings_policy_id, scooper,
}
use tests/fixtures/datums.{default_swap_order}
use types/order.{Deposit, OrderDatum, Self, Swap, Withdrawal}
use types/pool.{PoolDatum, PoolRedeemer, PoolScoop}
use types/settings.{SettingsDatum, settings_nft_name}

// ============================================
// Common Test Values
// ============================================

/// Minimum ADA for outputs
pub const min_ada = 2_000_000

/// Standard protocol fee
pub const protocol_fee = 2_500_000

/// One billion (common reserve amount)
pub const billion = 1_000_000_000

/// Default LP fee (0.05%)
pub const default_lp_fee = 5

// ============================================
// Scoop Transaction Builder
// ============================================

/// Builder for constructing scoop transactions
pub type ScoopBuilder {
  /// Pool reserves (ADA, token)
  pool_reserves: (Int, Int),
  /// Pool datum to use (or None for default)
  pool_datum: Option<PoolDatum>,
  /// Protocol fees in pool
  protocol_fees: Int,
  /// List of orders: (offer_amount, min_received, is_ada_to_token)
  orders: List<(Int, Int, Bool)>,
  /// Extra signatories
  signatories: List<ByteArray>,
  /// Transaction fee
  tx_fee: Int,
}

/// Create a new scoop transaction builder with defaults
pub fn scoop() -> ScoopBuilder {
  ScoopBuilder {
    pool_reserves: (billion, billion),
    pool_datum: None,
    protocol_fees: min_ada,
    orders: [],
    signatories: [scooper],
    tx_fee: 1_000_000,
  }
}

/// Set pool reserves (ADA amount, token amount)
pub fn with_pool_reserves(
  builder: ScoopBuilder,
  ada: Int,
  token: Int,
) -> ScoopBuilder {
  ScoopBuilder { ..builder, pool_reserves: (ada, token) }
}

/// Set custom pool datum
pub fn with_pool_datum(builder: ScoopBuilder, datum: PoolDatum) -> ScoopBuilder {
  ScoopBuilder { ..builder, pool_datum: Some(datum) }
}

/// Set protocol fees in pool
pub fn with_protocol_fees(builder: ScoopBuilder, fees: Int) -> ScoopBuilder {
  ScoopBuilder { ..builder, protocol_fees: fees }
}

/// Add a swap order (ADA -> Token)
pub fn with_swap_ada_to_token(
  builder: ScoopBuilder,
  offer: Int,
  min_received: Int,
) -> ScoopBuilder {
  ScoopBuilder {
    ..builder,
    orders: list.concat(builder.orders, [(offer, min_received, True)]),
  }
}

/// Add a swap order (Token -> ADA)
pub fn with_swap_token_to_ada(
  builder: ScoopBuilder,
  offer: Int,
  min_received: Int,
) -> ScoopBuilder {
  ScoopBuilder {
    ..builder,
    orders: list.concat(builder.orders, [(offer, min_received, False)]),
  }
}

/// Set transaction fee
pub fn with_tx_fee(builder: ScoopBuilder, fee: Int) -> ScoopBuilder {
  ScoopBuilder { ..builder, tx_fee: fee }
}

/// Set signatories (defaults to [scooper])
pub fn with_signatories(
  builder: ScoopBuilder,
  sigs: List<ByteArray>,
) -> ScoopBuilder {
  ScoopBuilder { ..builder, signatories: sigs }
}

// ============================================
// Input/Output Builders
// ============================================

/// Build a pool input with given reserves
pub fn pool_input(
  ada_reserve: Int,
  token_reserve: Int,
  protocol_fees: Int,
  datum: PoolDatum,
) -> Input {
  let pool_nft = pool_nft_name(pool_ident)
  Input {
    output_reference: mock_utxo_ref(0, 0),
    output: Output {
      address: pool_address(),
      value: from_lovelace(ada_reserve + protocol_fees)
        |> add(rberry_policy, rberry_asset_name, token_reserve)
        |> add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

/// Build an order input for an ADA->Token swap
pub fn swap_order_input(index: Int, offer: Int, min_received: Int) -> Input {
  let order_datum =
    OrderDatum {
      ..default_swap_order(),
      details: Swap {
        offer: (#"", #"", offer),
        min_received: (rberry_policy, rberry_asset_name, min_received),
      },
    }
  Input {
    output_reference: mock_utxo_ref(0, index),
    output: Output {
      address: order_address(),
      value: from_lovelace(protocol_fee + min_ada + offer),
      datum: InlineDatum(order_datum),
      reference_script: None,
    },
  }
}

/// Build an order input for a Token->ADA swap
pub fn swap_order_input_token_to_ada(
  index: Int,
  offer: Int,
  min_received: Int,
) -> Input {
  let order_datum =
    OrderDatum {
      ..default_swap_order(),
      details: Swap {
        offer: (rberry_policy, rberry_asset_name, offer),
        min_received: (#"", #"", min_received),
      },
    }
  Input {
    output_reference: mock_utxo_ref(0, index),
    output: Output {
      address: order_address(),
      value: from_lovelace(protocol_fee + min_ada)
        |> add(rberry_policy, rberry_asset_name, offer),
      datum: InlineDatum(order_datum),
      reference_script: None,
    },
  }
}

/// Build an order input for a deposit (ADA + RBERRY)
pub fn deposit_order_input(
  index: Int,
  ada_amount: Int,
  token_amount: Int,
) -> Input {
  let order_datum =
    OrderDatum {
      ..default_swap_order(),
      details: Deposit {
        assets: (
          (#"", #"", ada_amount),
          (rberry_policy, rberry_asset_name, token_amount),
        ),
      },
    }
  Input {
    output_reference: mock_utxo_ref(0, index),
    output: Output {
      address: order_address(),
      value: from_lovelace(protocol_fee + min_ada + ada_amount)
        |> add(rberry_policy, rberry_asset_name, token_amount),
      datum: InlineDatum(order_datum),
      reference_script: None,
    },
  }
}

/// Build an order input with Self destination (for repeating orders)
pub fn self_swap_order_input(index: Int, offer: Int, min_received: Int) -> Input {
  let order_datum =
    OrderDatum {
      ..default_swap_order(),
      destination: Self,
      details: Swap {
        offer: (#"", #"", offer),
        min_received: (rberry_policy, rberry_asset_name, min_received),
      },
    }
  Input {
    output_reference: mock_utxo_ref(0, index),
    output: Output {
      address: order_address(),
      value: from_lovelace(protocol_fee + min_ada + offer),
      datum: InlineDatum(order_datum),
      reference_script: None,
    },
  }
}

/// Build a withdrawal order input
pub fn withdrawal_order_input(index: Int, lp_amount: Int) -> Input {
  let lp_name = pool_lp_name(pool_ident)
  let order_datum =
    OrderDatum {
      ..default_swap_order(),
      details: Withdrawal {
        amount: (pool_script_hash, lp_name, lp_amount),
      },
    }
  Input {
    output_reference: mock_utxo_ref(0, index),
    output: Output {
      address: order_address(),
      value: from_lovelace(protocol_fee + min_ada)
        |> add(pool_script_hash, lp_name, lp_amount),
      datum: InlineDatum(order_datum),
      reference_script: None,
    },
  }
}

/// Build a pool output after processing orders
pub fn pool_output(
  ada_reserve: Int,
  token_reserve: Int,
  protocol_fees: Int,
  datum: PoolDatum,
) -> Output {
  let pool_nft = pool_nft_name(pool_ident)
  Output {
    address: pool_address(),
    value: from_lovelace(ada_reserve + protocol_fees)
      |> add(rberry_policy, rberry_asset_name, token_reserve)
      |> add(pool_script_hash, pool_nft, 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

/// Build a user payout output (ADA + optional RBERRY)
pub fn user_output(ada: Int, token: Int) -> Output {
  let value =
    if token > 0 {
      from_lovelace(ada) |> add(rberry_policy, rberry_asset_name, token)
    } else {
      from_lovelace(ada)
    }
  Output {
    address: user_address(),
    value: value,
    datum: NoDatum,
    reference_script: None,
  }
}

/// Build a user payout for a withdrawal (ADA + RBERRY, no LP remainder)
pub fn user_withdrawal_output(ada: Int, token: Int) -> Output {
  Output {
    address: user_address(),
    value: from_lovelace(ada) |> add(rberry_policy, rberry_asset_name, token),
    datum: NoDatum,
    reference_script: None,
  }
}

/// Build a user payout with LP tokens and remainder RBERRY
pub fn user_output_with_lp(ada: Int, lp_amount: Int, rberry_remainder: Int) -> Output {
  let lp_name = pool_lp_name(pool_ident)
  let value =
    from_lovelace(ada)
      |> add(pool_script_hash, lp_name, lp_amount)
      |> add(rberry_policy, rberry_asset_name, rberry_remainder)
  Output {
    address: user_address(),
    value: value,
    datum: NoDatum,
    reference_script: None,
  }
}

/// Build a self-destination output (goes back to order address with same datum)
pub fn self_output(input: Input, ada: Int, token: Int) -> Output {
  let value =
    if token > 0 {
      from_lovelace(ada) |> add(rberry_policy, rberry_asset_name, token)
    } else {
      from_lovelace(ada)
    }
  Output {
    address: input.output.address,
    value: value,
    datum: input.output.datum,
    reference_script: None,
  }
}

/// Build a treasury output for fee withdrawal
pub fn treasury_output(amount: Int) -> Output {
  Output {
    address: treasury_address(),
    value: from_lovelace(amount),
    datum: InlineDatum(Void),
    reference_script: None,
  }
}

// ============================================
// Settings Input Builder
// ============================================

/// Build a settings reference input
pub fn settings_ref_input(scoopers: List<ByteArray>) -> Input {
  let settings_datum = mk_settings_datum(scoopers)
  Input {
    output_reference: mock_utxo_ref(1, 0),
    output: Output {
      address: Address {
        payment_credential: Script(settings_policy_id),
        stake_credential: None,
      },
      value: from_lovelace(min_ada)
        |> add(settings_policy_id, settings_nft_name, 1),
      datum: InlineDatum(settings_datum),
      reference_script: None,
    },
  }
}

/// Create a settings datum with given scoopers
fn mk_settings_datum(scoopers: List<ByteArray>) -> SettingsDatum {
  let settings_admin_key =
    #"6313a1d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"
  let metadata_admin_key =
    #"7b143ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"
  let treasury_admin_key =
    #"17bbd2d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"
  SettingsDatum {
    settings_admin: Signature { key_hash: settings_admin_key },
    metadata_admin: Address(VerificationKey(metadata_admin_key), None),
    treasury_admin: Signature { key_hash: treasury_admin_key },
    treasury_address: treasury_address(),
    treasury_allowance: (1, 10),
    authorized_scoopers: Some(scoopers),
    authorized_staking_keys: [VerificationKey(settings_admin_key)],
    base_fee: 0,
    simple_fee: protocol_fee,
    strategy_fee: 5_000_000,
    pool_creation_fee: 0,
    extensions: [],
  }
}

// ============================================
// Redeemer Builders
// ============================================

/// Build a PoolScoop redeemer for given order count
pub fn scoop_redeemer(order_count: Int) -> PoolRedeemer {
  let input_order =
    list.range(1, order_count)
      |> list.map(fn(i) { (i, None, 0) })
  PoolScoop { signatory_index: 0, scooper_index: 0, input_order: input_order }
}

// ============================================
// Full Transaction Builders
// ============================================

/// Build a complete scoop transaction
/// Returns (transaction, pool_datum, pool_input_ref)
pub fn build_scoop_tx(
  pool_in: Input,
  orders: List<Input>,
  pool_out: Output,
  user_outputs: List<Output>,
  signatories: List<ByteArray>,
) -> Transaction {
  let all_outputs = [pool_out, ..user_outputs]
  Transaction {
    ..transaction.placeholder,
    inputs: [pool_in, ..orders],
    reference_inputs: [settings_ref_input(signatories)],
    outputs: all_outputs,
    fee: 1_000_000,
    validity_range: interval.between(1, 2),
    extra_signatories: signatories,
    id: mock_tx_hash(1),
  }
}

// ============================================
// Withdraw Fees Helpers
// ============================================

/// Build a pool input with staking credential for manage operations
pub fn pool_input_with_stake(
  ada_reserve: Int,
  token_reserve: Int,
  protocol_fees: Int,
  datum: PoolDatum,
  stake_key: ByteArray,
) -> Input {
  let pool_nft = pool_nft_name(pool_ident)
  Input {
    output_reference: mock_utxo_ref(0, 0),
    output: Output {
      address: Address {
        payment_credential: Script(pool_script_hash),
        stake_credential: Some(Inline(VerificationKey(stake_key))),
      },
      value: from_lovelace(ada_reserve + protocol_fees)
        |> add(rberry_policy, rberry_asset_name, token_reserve)
        |> add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

/// Build a pool output with staking credential for manage operations
pub fn pool_output_with_stake(
  ada_reserve: Int,
  token_reserve: Int,
  protocol_fees: Int,
  datum: PoolDatum,
  stake_key: ByteArray,
) -> Output {
  let pool_nft = pool_nft_name(pool_ident)
  Output {
    address: Address {
      payment_credential: Script(pool_script_hash),
      stake_credential: Some(Inline(VerificationKey(stake_key))),
    },
    value: from_lovelace(ada_reserve + protocol_fees)
      |> add(rberry_policy, rberry_asset_name, token_reserve)
      |> add(pool_script_hash, pool_nft, 1),
    datum: InlineDatum(datum),
    reference_script: None,
  }
}

// ============================================
// Test Assertion Helpers
// ============================================

/// Calculate expected swap output using constant product formula
/// Returns (ada_delta, token_delta) where negative means taken from pool
pub fn calc_swap_output(
  pool_ada: Int,
  pool_token: Int,
  offer_ada: Int,
  lp_fee_per_10k: Int,
) -> Int {
  // Simple constant product: (x + dx) * (y - dy) = x * y
  // dy = y * dx / (x + dx) * (1 - fee)
  let fee_multiplier = 10000 - lp_fee_per_10k
  let numerator = pool_token * offer_ada * fee_multiplier
  let denominator = ( pool_ada + offer_ada ) * 10000
  numerator / denominator
}
