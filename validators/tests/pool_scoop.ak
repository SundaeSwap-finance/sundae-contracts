/// Pool scoop tests using mocktail fixtures
///
/// Tests pool validator scooping operations: swaps, deposits, withdrawals.
/// Naming: pool_scoop_<scenario>_<success|fails>

use aiken/interval
use cardano/address.{Address, Inline, Script, VerificationKey}
use cardano/assets.{add, from_lovelace, zero}
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, Transaction}
use mocktail/virgin_output_reference.{mock_tx_hash, mock_utxo_ref}
use pool as pool_validator
use shared.{pool_lp_name, pool_nft_name}
use tests/fixtures/addresses.{order_address, pool_address, user_address}
use tests/fixtures/constants.{
  billion, extra_scooper, manage_stake_script_hash, min_ada, not_a_scooper,
  other_hash, pool_ident, pool_script_hash, protocol_fee, rberry_asset_name,
  rberry_policy, scooper, settings_policy_id, stake_key,
}
use tests/fixtures/datums.{
  default_pool_datum, default_swap_order, with_extra_scoopers,
}
use tests/fixtures/transactions.{
  deposit_order_input, pool_input, pool_output, scoop_redeemer,
  self_swap_order_input, settings_ref_input, swap_order_input,
  swap_order_input_token_to_ada, user_output, user_output_with_lp,
  user_withdrawal_output, withdrawal_order_input,
}
use types/order.{OrderDatum, Swap}
use types/pool.{PoolDatum, PoolScoop}

// ============================================
// HELPER: Build a simple scoop transaction
// ============================================

fn simple_scoop_tx(
  pool_in: Input,
  order_ins: List<Input>,
  pool_out: Output,
  user_outs: List<Output>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [pool_in, ..order_ins],
    reference_inputs: [settings_ref_input([scooper])],
    outputs: [pool_out, ..user_outs],
    fee: 1_000_000,
    validity_range: interval.between(1, 2),
    extra_signatories: [scooper],
    id: mock_tx_hash(1),
  }
}

// ============================================
// COMPACT SCOOP TESTS
// ============================================

test pool_scoop_two_swaps_success() {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + 2 * protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)
  let o2 = swap_order_input(2, 10_000_000, 0)

  // Second swap gets less due to price impact from first
  let p_out =
    pool_output(
      billion + 20_000_000,
      billion - 19_598_183,
      min_ada + 2 * protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada, 9_896_088)
  let u2 = user_output(min_ada, 9_702_095)

  let tx = simple_scoop_tx(p_in, [o1, o2], p_out, [u1, u2])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(2),
    p_in.output_reference,
    tx,
  )
}

test pool_scoop_market_closed_fails() fail {
  let datum = PoolDatum { ..default_pool_datum(), market_open: 999_999_999_999 }
  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }
  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_896_088,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada, 9_896_088)

  let tx = simple_scoop_tx(p_in, [o1], p_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

test pool_scoop_zero_offer_fails() fail {
  let datum = default_pool_datum()
  let p_in = pool_input(billion, billion, min_ada, datum)

  let order_datum =
    OrderDatum {
      ..default_swap_order(),
      details: Swap {
        offer: (#"", #"", 0),
        min_received: (rberry_policy, rberry_asset_name, 0),
      },
    }
  let o1 =
    Input {
      output_reference: mock_utxo_ref(0, 1),
      output: Output {
        address: order_address(),
        value: from_lovelace(protocol_fee + min_ada),
        datum: InlineDatum(order_datum),
        reference_script: None,
      },
    }

  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }
  let p_out = pool_output(billion, billion, min_ada + protocol_fee, datum_out)
  let u1 = user_output(min_ada, 0)

  let tx = simple_scoop_tx(p_in, [o1], p_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

test pool_scoop_wrong_destination_fails() fail {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_896_088,
      min_ada + protocol_fee,
      datum_out,
    )

  // Pay to order_address instead of user_address
  let wrong_out =
    Output {
      address: order_address(),
      value: from_lovelace(min_ada) |> add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let tx = simple_scoop_tx(p_in, [o1], p_out, [wrong_out])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

test pool_scoop_nft_stolen_fails() fail {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  // Pool output missing NFT (stolen)
  let bad_pool_out =
    Output {
      address: pool_address(),
      value: from_lovelace(billion + 10_000_000 + min_ada + protocol_fee)
        |> add(rberry_policy, rberry_asset_name, billion - 9_896_088),
      datum: InlineDatum(datum_out),
      reference_script: None,
    }
  let u1 = user_output(min_ada, 9_896_088)

  let tx = simple_scoop_tx(p_in, [o1], bad_pool_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

test pool_scoop_protocol_fees_wrong_fails() fail {
  let datum = default_pool_datum()
  // Protocol fees not incremented (should add protocol_fee)
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  let p_out =
    pool_output(billion + 10_000_000, billion - 9_896_088, min_ada, datum_out)
  let u1 = user_output(min_ada, 9_896_088)

  let tx = simple_scoop_tx(p_in, [o1], p_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

test pool_scoop_unauthorized_scooper_fails() fail {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_896_088,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada, 9_896_088)

  // Signed by not_a_scooper but settings only authorizes scooper
  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [p_in, o1],
      reference_inputs: [settings_ref_input([scooper])],
      outputs: [p_out, u1],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [not_a_scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

test pool_scoop_high_lp_fee_success() {
  // Pool with 1% fee (100bp) instead of default 0.05% (5bp)
  let datum = PoolDatum { ..default_pool_datum(), lp_fee_basis_points: (100, 100) }
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  // Higher fee means less output: 9,802,950 vs 9,896,088 at 0.05%
  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_802_950,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada, 9_802_950)

  let tx = simple_scoop_tx(p_in, [o1], p_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

// ============================================
// SINGLE ORDER TESTS
// ============================================

test pool_scoop_single_swap_success() {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_896_088,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada, 9_896_088)

  let tx = simple_scoop_tx(p_in, [o1], p_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

test pool_scoop_token_to_ada_success() {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input_token_to_ada(1, 10_000_000, 0)

  let p_out =
    pool_output(
      billion - 9_896_088,
      billion + 10_000_000,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada + 9_896_088, 0)

  let tx = simple_scoop_tx(p_in, [o1], p_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

// ============================================
// MIXED ORDER TESTS
// ============================================

test pool_scoop_swap_deposit_success() {
  let datum = default_pool_datum()
  let datum_out =
    PoolDatum {
      ..datum,
      circulating_lp: 1_009_900_990,
      protocol_fees: (7_000_000, 0, 0),
    }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)
  let o2 = deposit_order_input(2, 10_000_000, 10_000_000)

  let p_out =
    Output {
      address: pool_address(),
      value: from_lovelace(1_027_000_000)
        |> add(rberry_policy, rberry_asset_name, 999_906_922)
        |> add(pool_script_hash, pool_nft_name(pool_ident), 1),
      datum: InlineDatum(datum_out),
      reference_script: None,
    }

  let u1 = user_output(min_ada, 9_896_088)
  let u2 = user_output_with_lp(min_ada, 9_900_990, 196_990)

  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [p_in, o1, o2],
      reference_inputs: [settings_ref_input([scooper])],
      outputs: [p_out, u1, u2],
      mint: zero |> add(pool_script_hash, pool_lp_name(pool_ident), 9_900_990),
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    PoolScoop {
      signatory_index: 0,
      scooper_index: 0,
      input_order: [(1, None, 0), (2, None, 0)],
    },
    p_in.output_reference,
    tx,
  )
}

// ============================================
// SELF-DESTINATION TESTS
// ============================================

test pool_scoop_self_destination_success() {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = self_swap_order_input(1, 10_000_000, 0)

  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_896_088,
      min_ada + protocol_fee,
      datum_out,
    )

  // Self destination returns to order address with same datum
  let u1 =
    Output {
      address: order_address(),
      value: from_lovelace(min_ada) |> add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: o1.output.datum,
      reference_script: None,
    }

  let tx = simple_scoop_tx(p_in, [o1], p_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

// ============================================
// POOL OUTPUT VALIDATION TESTS
// ============================================

test pool_scoop_output_to_wallet_fails() fail {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  // Pool output sent to wallet instead of script
  let bad_pool_out =
    Output {
      address: user_address(),
      value: from_lovelace(billion + 10_000_000 + min_ada + protocol_fee)
        |> add(rberry_policy, rberry_asset_name, billion - 9_896_088)
        |> add(pool_script_hash, pool_nft_name(pool_ident), 1),
      datum: InlineDatum(datum_out),
      reference_script: None,
    }
  let u1 = user_output(min_ada, 9_896_088)

  let tx = simple_scoop_tx(p_in, [o1], bad_pool_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

test pool_scoop_output_wrong_script_fails() fail {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  // Pool output sent to different script
  let bad_pool_out =
    Output {
      address: Address {
        payment_credential: Script(other_hash),
        stake_credential: None,
      },
      value: from_lovelace(billion + 10_000_000 + min_ada + protocol_fee)
        |> add(rberry_policy, rberry_asset_name, billion - 9_896_088)
        |> add(pool_script_hash, pool_nft_name(pool_ident), 1),
      datum: InlineDatum(datum_out),
      reference_script: None,
    }
  let u1 = user_output(min_ada, 9_896_088)

  let tx = simple_scoop_tx(p_in, [o1], bad_pool_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

test pool_scoop_staking_credential_changed_fails() fail {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  // Pool input has stake_key as staking credential
  let p_in =
    Input {
      output_reference: mock_utxo_ref(0, 0),
      output: Output {
        address: Address {
          payment_credential: Script(pool_script_hash),
          stake_credential: Some(Inline(VerificationKey(stake_key))),
        },
        value: from_lovelace(billion + min_ada)
          |> add(rberry_policy, rberry_asset_name, billion)
          |> add(pool_script_hash, pool_nft_name(pool_ident), 1),
        datum: InlineDatum(datum),
        reference_script: None,
      },
    }
  let o1 = swap_order_input(1, 10_000_000, 0)

  // Pool output has different staking credential (other_hash)
  let bad_pool_out =
    Output {
      address: Address {
        payment_credential: Script(pool_script_hash),
        stake_credential: Some(Inline(VerificationKey(other_hash))),
      },
      value: from_lovelace(billion + 10_000_000 + min_ada + protocol_fee)
        |> add(rberry_policy, rberry_asset_name, billion - 9_896_088)
        |> add(pool_script_hash, pool_nft_name(pool_ident), 1),
      datum: InlineDatum(datum_out),
      reference_script: None,
    }
  let u1 = user_output(min_ada, 9_896_088)

  let tx = simple_scoop_tx(p_in, [o1], bad_pool_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

/// Scoop fails when user payouts are swapped (order 1 gets order 2's output)
test pool_scoop_payouts_swapped_fails() fail {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + 2 * protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)
  let o2 = swap_order_input(2, 10_000_000, 0)

  let p_out =
    pool_output(
      billion + 20_000_000,
      billion - 19_598_183,
      min_ada + 2 * protocol_fee,
      datum_out,
    )

  // Wrong: payouts are swapped between orders
  let u1 = user_output(min_ada, 9_702_095)
  let u2 = user_output(min_ada, 9_896_088)

  let tx = simple_scoop_tx(p_in, [o1, o2], p_out, [u1, u2])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(2),
    p_in.output_reference,
    tx,
  )
}

// ============================================
// WITHDRAWAL TESTS
// ============================================

test pool_scoop_withdrawal_success() {
  let datum = default_pool_datum()
  let datum_out =
    PoolDatum {
      ..datum,
      circulating_lp: 990_000_000,
      protocol_fees: (min_ada + protocol_fee, 0, 0),
    }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = withdrawal_order_input(1, 10_000_000)

  // 10M LP = 1% of pool = 10M of each asset
  let p_out =
    pool_output(
      billion - 10_000_000,
      billion - 10_000_000,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_withdrawal_output(min_ada + 10_000_000, 10_000_000)

  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [p_in, o1],
      reference_inputs: [settings_ref_input([scooper])],
      outputs: [p_out, u1],
      mint: zero |> add(pool_script_hash, pool_lp_name(pool_ident), -10_000_000),
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

test pool_scoop_swap_withdrawal_success() {
  let datum = default_pool_datum()
  let datum_out =
    PoolDatum {
      ..datum,
      circulating_lp: 990_000_000,
      protocol_fees: (min_ada + 2 * protocol_fee, 0, 0),
    }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)
  let o2 = withdrawal_order_input(2, 10_000_000)

  // Withdrawal happens after swap, so proportions reflect post-swap state
  let p_out =
    pool_output(
      billion + 10_000_000 - 10_100_000,
      billion - 9_896_088 - 9_901_039,
      min_ada + 2 * protocol_fee,
      datum_out,
    )

  let u1 = user_output(min_ada, 9_896_088)
  let u2 = user_withdrawal_output(min_ada + 10_100_000, 9_901_039)

  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [p_in, o1, o2],
      reference_inputs: [settings_ref_input([scooper])],
      outputs: [p_out, u1, u2],
      mint: zero |> add(pool_script_hash, pool_lp_name(pool_ident), -10_000_000),
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    PoolScoop {
      signatory_index: 0,
      scooper_index: 0,
      input_order: [(1, None, 0), (2, None, 0)],
    },
    p_in.output_reference,
    tx,
  )
}

// ============================================
// SLIPPAGE PROTECTION TESTS
// ============================================

/// Swap fails when output is below min_received (slippage protection)
test pool_scoop_min_received_not_met_fails() fail {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)

  // Order requires minimum 9_900_000 RBERRY but swap only yields 9_896_088
  let order_datum =
    OrderDatum {
      ..default_swap_order(),
      details: Swap {
        offer: (#"", #"", 10_000_000),
        min_received: (rberry_policy, rberry_asset_name, 9_900_000),
      },
    }
  let o1 =
    Input {
      output_reference: mock_utxo_ref(0, 1),
      output: Output {
        address: order_address(),
        value: from_lovelace(protocol_fee + min_ada + 10_000_000),
        datum: InlineDatum(order_datum),
        reference_script: None,
      },
    }

  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_896_088,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada, 9_896_088)

  let tx = simple_scoop_tx(p_in, [o1], p_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

/// Swap succeeds when output exactly meets min_received
test pool_scoop_min_received_exact_success() {
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)

  // Order requires exactly what the swap yields
  let order_datum =
    OrderDatum {
      ..default_swap_order(),
      details: Swap {
        offer: (#"", #"", 10_000_000),
        min_received: (rberry_policy, rberry_asset_name, 9_896_088),
      },
    }
  let o1 =
    Input {
      output_reference: mock_utxo_ref(0, 1),
      output: Output {
        address: order_address(),
        value: from_lovelace(protocol_fee + min_ada + 10_000_000),
        datum: InlineDatum(order_datum),
        reference_script: None,
      },
    }

  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_896_088,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada, 9_896_088)

  let tx = simple_scoop_tx(p_in, [o1], p_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

// ============================================
// EXTRA SCOOPERS TESTS
// ============================================

/// Extra scooper (not in global list) can scoop a pool that has them listed
test pool_scoop_extra_scooper_success() {
  // Pool with extra_scooper in its extra_scoopers list
  let datum =
    default_pool_datum()
      |> with_extra_scoopers([extra_scooper])
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_896_088,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada, 9_896_088)

  // Signed by extra_scooper, settings only has regular scooper
  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [p_in, o1],
      reference_inputs: [settings_ref_input([scooper])],
      outputs: [p_out, u1],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [extra_scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    PoolScoop { signatory_index: 0, scooper_index: -1, input_order: [(1, None, 0)] },
    p_in.output_reference,
    tx,
  )
}

/// Extra scooper cannot scoop a pool that doesn't have them listed
test pool_scoop_extra_scooper_not_in_pool_fails() fail {
  // Pool with NO extra_scoopers (only global list matters)
  let datum = default_pool_datum()
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_896_088,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada, 9_896_088)

  // Signed by extra_scooper but neither in global list nor in pool's extra_scoopers
  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [p_in, o1],
      reference_inputs: [settings_ref_input([scooper])],
      outputs: [p_out, u1],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [extra_scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    PoolScoop { signatory_index: 0, scooper_index: -1, input_order: [(1, None, 0)] },
    p_in.output_reference,
    tx,
  )
}

/// Global scooper still works when pool has extra_scoopers
test pool_scoop_global_scooper_with_extras_present_success() {
  // Pool has extra_scoopers but we use global scooper
  let datum =
    default_pool_datum()
      |> with_extra_scoopers([extra_scooper])
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_896_088,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada, 9_896_088)

  // Signed by global scooper (in settings list)
  let tx = simple_scoop_tx(p_in, [o1], p_out, [u1])

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    scoop_redeemer(1),
    p_in.output_reference,
    tx,
  )
}

/// Unauthorized scooper fails even when pool has extra_scoopers (but not this one)
test pool_scoop_unauthorized_fails_with_extras() fail {
  // Pool has extra_scoopers but not_a_scooper is not in either list
  let datum =
    default_pool_datum()
      |> with_extra_scoopers([extra_scooper])
  let datum_out = PoolDatum { ..datum, protocol_fees: (min_ada + protocol_fee, 0, 0) }

  let p_in = pool_input(billion, billion, min_ada, datum)
  let o1 = swap_order_input(1, 10_000_000, 0)

  let p_out =
    pool_output(
      billion + 10_000_000,
      billion - 9_896_088,
      min_ada + protocol_fee,
      datum_out,
    )
  let u1 = user_output(min_ada, 9_896_088)

  // Signed by not_a_scooper (not in global or extra list)
  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [p_in, o1],
      reference_inputs: [settings_ref_input([scooper])],
      outputs: [p_out, u1],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [not_a_scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(datum),
    PoolScoop { signatory_index: 0, scooper_index: -1, input_order: [(1, None, 0)] },
    p_in.output_reference,
    tx,
  )
}

