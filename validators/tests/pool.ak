/// Pool validator tests using mocktail pattern
///
/// Each test builds its transaction directly for clarity and maintainability.

use aiken/builtin
use aiken/crypto
use aiken/interval
use aiken/primitive/bytearray
use cardano/address.{
  Address, Inline, Script, VerificationKey, from_script,
  from_verification_key, with_delegation_key,
}
use cardano/assets.{ada_asset_name, ada_policy_id}
use cardano/script_context.{ScriptContext}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Spend, Transaction,
  Withdraw,
}
use calculation/shared as calc_shared
use mocktail/virgin_output_reference.{mock_utxo_ref}
use pool as pool_validator
use shared.{pool_lp_name, pool_nft_name, pool_token_names}
use sundae/multisig
use tests/examples/ex_settings.{
  example_settings_admin, example_treasury_address, example_treasury_admin,
  mk_valid_settings_datum,
}
use tests/examples/ex_shared.{compare_redeemer, compare_stake, mk_tx_hash}
use tests/fixtures/constants.{
  manage_stake_script_hash, not_a_scooper, order_script_hash, other_asset_name,
  other_hash, other_policy, payment_key, pool_ident, pool_script_hash,
  random_hash, rberry_asset_name, rberry_policy, scooper, settings_policy_id,
  stake_key,
}
use tx_util/builder.{
  add_asset_to_tx_output, add_tx_input, add_tx_output, add_tx_ref_input,
  build_txn_context, insert_redeemer, insert_withdrawal, mint_assets,
  new_tx_input, new_tx_output, with_asset_of_tx_input,
}
use types/order.{Deposit, Fixed, OrderDatum, Self, Swap}
use types/pool.{
  BurnPool, CreatePool, Manage, PoolDatum, PoolScoop,
  UpdatePoolFees, WithdrawFees,
}
use types/settings.{settings_nft_name}

// ============================================
// Shared Helpers
// ============================================

const settings_admin = #"6313a1d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

const metadata_admin = #"7b143ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"

fn mock_tx_hash(n: Int) -> ByteArray {
  mock_utxo_ref(0, n).transaction_id
}

fn mk_output_reference(n: Int) -> OutputReference {
  OutputReference { transaction_id: mock_tx_hash(0), output_index: n }
}

fn wallet_address(hash: ByteArray) -> Address {
  Address { payment_credential: VerificationKey(hash), stake_credential: None }
}

fn script_address(hash: ByteArray) -> Address {
  Address { payment_credential: Script(hash), stake_credential: None }
}

fn mk_settings_input(scoopers_list: List<ByteArray>, ix: Int) -> Input {
  let settings_address = script_address(settings_policy_id)
  let settings_datum = mk_valid_settings_datum(scoopers_list)
  Input {
    output_reference: mk_output_reference(ix),
    output: Output {
      address: settings_address,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(settings_policy_id, settings_nft_name, 1),
      datum: InlineDatum(settings_datum),
      reference_script: None,
    },
  }
}

fn base_pool_datum() -> PoolDatum {
  PoolDatum {
    identifier: pool_ident,
    assets: ((ada_policy_id, ada_asset_name), (rberry_policy, rberry_asset_name)),
    circulating_lp: 1_000_000_000,
    bid_fees_per_10_thousand: 5,
    ask_fees_per_10_thousand: 5,
    fee_manager: None,
    market_open: 0,
    protocol_fees: 2_000_000,
  }
}

fn base_order_datum() -> OrderDatum {
  OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(payment_key),
    max_protocol_fee: 2_500_000,
    destination: Fixed { address: wallet_address(payment_key), datum: NoDatum },
    details: Swap {
      offer: (ada_policy_id, ada_asset_name, 10_000_000),
      min_received: (rberry_policy, rberry_asset_name, 0),
    },
    extension: builtin.i_data(0),
  }
}

// ============================================
// SCOOP TESTS
// ============================================

/// Test basic swap+swap scoop succeeds
test scoop_swap_swap_success() {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let pool_output =
    Output {
      address: pool_address,
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_401_817)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  let order1_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_702_095),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test swap with surplus value succeeds
test scoop_swap_with_surplus_success() {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  // Order 1 has extra 10M lovelace
  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(24_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let pool_output =
    Output {
      address: pool_address,
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_401_817)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  // Order 1 gets extra 10M back
  let order1_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(12_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_702_095),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test pool validator ignores fee amount
test scoop_ignores_transaction_fee() {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let pool_output =
    Output {
      address: pool_address,
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_401_817)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  let order1_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_702_095),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test higher swap fees work correctly
test scoop_with_high_swap_fees() {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum =
    PoolDatum {
      ..base_pool_datum(),
      bid_fees_per_10_thousand: 100,
      ask_fees_per_10_thousand: 100,
    }
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let pool_output =
    Output {
      address: pool_address,
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_585_372)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  let order1_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_802_950),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_611_678),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test Self destination for strategy orders
test scoop_with_self_destination() {
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_datum = OrderDatum { ..base_order_datum(), destination: Self }
  let order2_datum = OrderDatum { ..base_order_datum(), destination: Self }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(order1_datum),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(order2_datum),
        reference_script: None,
      },
    }

  let pool_output =
    Output {
      address: pool_address,
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_401_817)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  // Self destination: output goes back to order address with order datum
  let order1_out =
    Output {
      address: order_address,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: InlineDatum(order1_datum),
      reference_script: None,
    }

  let order2_out =
    Output {
      address: order_address,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_702_095),
      datum: InlineDatum(order2_datum),
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test scoop fails when output goes to wrong destination
test scoop_wrong_destination_fails() fail {
  let wrong_addr =
    wallet_address(#"12000000000000000000000000000000000000000000000000000000")
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let pool_output =
    Output {
      address: pool_address,
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_401_817)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  // Wrong destination
  let order1_out =
    Output {
      address: wrong_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: wrong_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_702_095),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test scoop fails when payouts are swapped between orders
test scoop_payouts_swapped_fails() fail {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let pool_output =
    Output {
      address: pool_address,
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_401_817)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  // Swapped payouts - order1 gets order2's amount and vice versa
  let order1_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_702_095),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test scoop fails when pool output missing NFT
test scoop_missing_pool_nft_fails() fail {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  // Missing pool NFT
  let pool_output =
    Output {
      address: pool_address,
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_401_817),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  let order1_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_702_095),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test scoop fails when pool output goes to wallet address
test scoop_pool_output_to_wallet_fails() fail {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  // Pool output to wallet address
  let pool_output =
    Output {
      address: from_verification_key(random_hash),
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_401_817)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  let order1_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_702_095),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test scoop fails when pool output goes to wrong script
test scoop_pool_output_wrong_script_fails() fail {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  // Pool output to wrong script
  let pool_output =
    Output {
      address: from_script(random_hash),
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_401_817)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  let order1_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_702_095),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test scoop fails when pool staking credential changes
test scoop_pool_staking_credential_change_fails() fail {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let order_address = script_address(order_script_hash)

  let pool_input_address =
    from_script(pool_script_hash)
      |> with_delegation_key(random_hash)
  let pool_output_address =
    from_script(pool_script_hash)
      |> with_delegation_key(other_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_input_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  // Staking credential changed
  let pool_output =
    Output {
      address: pool_output_address,
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_401_817)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  let order1_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_702_095),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test scoop fails when scooper not in settings
test scoop_unauthorized_scooper_fails() fail {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum = PoolDatum { ..pool_datum, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(base_order_datum()),
        reference_script: None,
      },
    }

  let pool_output =
    Output {
      address: pool_address,
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 980_401_817)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  let order1_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_702_095),
      datum: NoDatum,
      reference_script: None,
    }

  // Settings with different authorized scooper
  let settings_input = mk_settings_input([not_a_scooper], 1)

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [settings_input],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

/// Test swap + deposit in same batch
test scoop_swap_deposit_success() {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  let pool_datum = base_pool_datum()
  let pool_out_datum =
    PoolDatum { ..pool_datum, circulating_lp: 1_009_900_990, protocol_fees: 7_000_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(1_002_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 1_000_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  // Order 1: Swap
  let order_datum_1 = base_order_datum()

  // Order 2: Deposit
  let order_datum_2 =
    OrderDatum {
      ..base_order_datum(),
      details: Deposit {
        assets: (
          (ada_policy_id, ada_asset_name, 10_000_000),
          (rberry_policy, rberry_asset_name, 10_000_000),
        ),
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000),
        datum: InlineDatum(order_datum_1),
        reference_script: None,
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(14_500_000)
          |> assets.add(rberry_policy, rberry_asset_name, 10_000_000),
        datum: InlineDatum(order_datum_2),
        reference_script: None,
      },
    }

  let pool_output =
    Output {
      address: pool_address,
      value: assets.from_lovelace(1_027_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 999_906_922)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  let order1_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(rberry_policy, rberry_asset_name, 9_896_088),
      datum: NoDatum,
      reference_script: None,
    }

  let order2_out =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(pool_script_hash, pool_lp_name(pool_ident), 9_900_990)
        |> assets.add(rberry_policy, rberry_asset_name, 196_990),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, order1_out, order2_out],
      fee: 1_000_000,
      mint: assets.zero
        |> assets.add(pool_script_hash, pool_lp_name(pool_ident), 9_900_990),
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

// ============================================
// WITHDRAW FEES TESTS
// ============================================

/// Test basic fee withdrawal succeeds
test withdraw_fees_success() {
  let pool_nft = pool_nft_name(pool_ident)
  let protocol_fees = 2_000_000
  let withdraw_amount = 100

  let pool_address =
    Address {
      payment_credential: Script(pool_script_hash),
      stake_credential: Some(Inline(VerificationKey(example_settings_admin))),
    }

  let pool_datum = base_pool_datum()
  let pool_out_datum =
    PoolDatum { ..pool_datum, protocol_fees: protocol_fees - withdraw_amount }

  let normal_input =
    new_tx_input(mk_tx_hash(0), wallet_address(payment_key), 1_000_000, NoDatum)

  let pool_input =
    new_tx_input(
      mk_tx_hash(1),
      script_address(pool_script_hash),
      1_000_000_000 + protocol_fees,
      InlineDatum(pool_datum),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> with_asset_of_tx_input(assets.from_asset(pool_script_hash, pool_nft, 1))

  let pool_output =
    new_tx_output(pool_address, 0, InlineDatum(pool_out_datum))
      |> add_asset_to_tx_output(
          assets.from_lovelace(1_000_000_000 + protocol_fees - withdraw_amount),
        )
      |> add_asset_to_tx_output(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> add_asset_to_tx_output(assets.from_asset(pool_script_hash, pool_nft, 1))

  let treasury_output =
    new_tx_output(
      Address(VerificationKey(example_treasury_address), None),
      0,
      InlineDatum(Void),
    )
      |> add_asset_to_tx_output(assets.from_lovelace(withdraw_amount))

  let settings_input =
    new_tx_input(
      mk_tx_hash(1),
      script_address(settings_policy_id),
      1,
      InlineDatum(mk_valid_settings_datum([])),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(settings_policy_id, settings_nft_name, 1),
        )

  let withdraw_fees_redeemer =
    WithdrawFees { amount: withdraw_amount, treasury_output: 1, pool_input: 1 }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_ref_input(settings_input)
      |> add_tx_input(pool_input)
      |> add_tx_input(normal_input)
      |> add_tx_output(treasury_output)
      |> add_tx_output(pool_output)
      |> insert_withdrawal(Script(manage_stake_script_hash), 0, compare_stake)
      |> insert_redeemer(
          Spend(OutputReference { transaction_id: mk_tx_hash(1), output_index: 0 }),
          Manage,
          compare_redeemer,
        )
      |> insert_redeemer(
          Withdraw(Script(manage_stake_script_hash)),
          withdraw_fees_redeemer,
          compare_redeemer,
        )
      |> builder.add_signatory(example_treasury_admin)
      |> builder.spend(pool_input.output_reference)

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    Manage,
    pool_input.output_reference,
    ctx.transaction,
  ) && pool_validator.manage.else(
    settings_policy_id,
    ScriptContext { ..ctx, redeemer: withdraw_fees_redeemer },
  )
}

/// Test fee withdrawal fails with wrong pool input index
test withdraw_fees_wrong_input_fails() fail {
  let pool_nft = pool_nft_name(pool_ident)
  let protocol_fees = 2_000_000
  let withdraw_amount = 100

  let pool_address =
    Address {
      payment_credential: Script(pool_script_hash),
      stake_credential: Some(Inline(VerificationKey(example_settings_admin))),
    }

  let pool_datum = base_pool_datum()
  let pool_out_datum =
    PoolDatum { ..pool_datum, protocol_fees: protocol_fees - withdraw_amount }

  let normal_input =
    new_tx_input(mk_tx_hash(0), wallet_address(payment_key), 1_000_000, NoDatum)

  let pool_input =
    new_tx_input(
      mk_tx_hash(1),
      script_address(pool_script_hash),
      1_000_000_000 + protocol_fees,
      InlineDatum(pool_datum),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> with_asset_of_tx_input(assets.from_asset(pool_script_hash, pool_nft, 1))

  let pool_output =
    new_tx_output(pool_address, 0, InlineDatum(pool_out_datum))
      |> add_asset_to_tx_output(
          assets.from_lovelace(1_000_000_000 + protocol_fees - withdraw_amount),
        )
      |> add_asset_to_tx_output(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> add_asset_to_tx_output(assets.from_asset(pool_script_hash, pool_nft, 1))

  let treasury_output =
    new_tx_output(
      Address(VerificationKey(example_treasury_address), None),
      0,
      InlineDatum(Void),
    )
      |> add_asset_to_tx_output(assets.from_lovelace(withdraw_amount))

  let settings_input =
    new_tx_input(
      mk_tx_hash(1),
      script_address(settings_policy_id),
      1,
      InlineDatum(mk_valid_settings_datum([])),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(settings_policy_id, settings_nft_name, 1),
        )

  // Wrong pool input index (0 instead of 1)
  let withdraw_fees_redeemer =
    WithdrawFees { amount: withdraw_amount, treasury_output: 1, pool_input: 0 }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_ref_input(settings_input)
      |> add_tx_input(pool_input)
      |> add_tx_input(normal_input)
      |> add_tx_output(treasury_output)
      |> add_tx_output(pool_output)
      |> insert_withdrawal(Script(manage_stake_script_hash), 0, compare_stake)
      |> insert_redeemer(
          Spend(OutputReference { transaction_id: mk_tx_hash(1), output_index: 0 }),
          Manage,
          compare_redeemer,
        )
      |> insert_redeemer(
          Withdraw(Script(manage_stake_script_hash)),
          withdraw_fees_redeemer,
          compare_redeemer,
        )
      |> builder.add_signatory(example_treasury_admin)
      |> builder.spend(pool_input.output_reference)

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    Manage,
    pool_input.output_reference,
    ctx.transaction,
  ) && pool_validator.manage.else(
    settings_policy_id,
    ScriptContext { ..ctx, redeemer: withdraw_fees_redeemer },
  )
}

/// Test overdrawing fees fails
test withdraw_fees_overdraw_fails() fail {
  let pool_nft = pool_nft_name(pool_ident)
  let protocol_fees = 2_000_000
  let withdraw_amount = 5_000_000

  let pool_address =
    Address {
      payment_credential: Script(pool_script_hash),
      stake_credential: Some(Inline(VerificationKey(example_settings_admin))),
    }

  let pool_datum = base_pool_datum()
  let pool_out_datum =
    PoolDatum { ..pool_datum, protocol_fees: protocol_fees - withdraw_amount }

  let normal_input =
    new_tx_input(mk_tx_hash(0), wallet_address(payment_key), 1_000_000, NoDatum)

  let pool_input =
    new_tx_input(
      mk_tx_hash(1),
      script_address(pool_script_hash),
      1_000_000_000 + protocol_fees,
      InlineDatum(pool_datum),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> with_asset_of_tx_input(assets.from_asset(pool_script_hash, pool_nft, 1))

  let pool_output =
    new_tx_output(pool_address, 0, InlineDatum(pool_out_datum))
      |> add_asset_to_tx_output(
          assets.from_lovelace(1_000_000_000 + protocol_fees - withdraw_amount),
        )
      |> add_asset_to_tx_output(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> add_asset_to_tx_output(assets.from_asset(pool_script_hash, pool_nft, 1))

  let treasury_output =
    new_tx_output(
      Address(VerificationKey(example_treasury_address), None),
      0,
      InlineDatum(Void),
    )
      |> add_asset_to_tx_output(assets.from_lovelace(withdraw_amount))

  let settings_input =
    new_tx_input(
      mk_tx_hash(1),
      script_address(settings_policy_id),
      1,
      InlineDatum(mk_valid_settings_datum([])),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(settings_policy_id, settings_nft_name, 1),
        )

  let withdraw_fees_redeemer =
    WithdrawFees { amount: withdraw_amount, treasury_output: 1, pool_input: 1 }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_ref_input(settings_input)
      |> add_tx_input(pool_input)
      |> add_tx_input(normal_input)
      |> add_tx_output(treasury_output)
      |> add_tx_output(pool_output)
      |> insert_withdrawal(Script(manage_stake_script_hash), 0, compare_stake)
      |> insert_redeemer(
          Spend(OutputReference { transaction_id: mk_tx_hash(1), output_index: 0 }),
          Manage,
          compare_redeemer,
        )
      |> insert_redeemer(
          Withdraw(Script(manage_stake_script_hash)),
          withdraw_fees_redeemer,
          compare_redeemer,
        )
      |> builder.add_signatory(example_treasury_admin)
      |> builder.spend(pool_input.output_reference)

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    Manage,
    pool_input.output_reference,
    ctx.transaction,
  ) && pool_validator.manage.else(
    settings_policy_id,
    ScriptContext { ..ctx, redeemer: withdraw_fees_redeemer },
  )
}

// ============================================
// UPDATE POOL FEES TESTS
// ============================================

/// Test updating pool fees succeeds
test update_pool_fees_success() {
  let pool_nft = pool_nft_name(pool_ident)

  let pool_address =
    Address {
      payment_credential: Script(pool_script_hash),
      stake_credential: Some(Inline(VerificationKey(example_settings_admin))),
    }

  let pool_datum =
    PoolDatum { ..base_pool_datum(), fee_manager: Some(multisig.Signature(pool_script_hash)) }

  let pool_out_datum =
    PoolDatum { ..pool_datum, bid_fees_per_10_thousand: 10, ask_fees_per_10_thousand: 10 }

  let pool_rider = 2_000_000

  let pool_input =
    new_tx_input(
      mk_tx_hash(0),
      pool_address,
      1_000_000_000 + pool_rider,
      InlineDatum(pool_datum),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> with_asset_of_tx_input(assets.from_asset(pool_script_hash, pool_nft, 1))

  let pool_output =
    new_tx_output(pool_address, 0, InlineDatum(pool_out_datum))
      |> add_asset_to_tx_output(assets.from_lovelace(1_000_000_000 + pool_rider))
      |> add_asset_to_tx_output(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> add_asset_to_tx_output(assets.from_asset(pool_script_hash, pool_nft, 1))

  let settings_input =
    new_tx_input(
      mk_tx_hash(1),
      script_address(settings_policy_id),
      1,
      InlineDatum(mk_valid_settings_datum([])),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(settings_policy_id, settings_nft_name, 1),
        )

  let update_fees_redeemer = UpdatePoolFees { pool_input: 0 }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_ref_input(settings_input)
      |> add_tx_input(pool_input)
      |> add_tx_output(pool_output)
      |> builder.add_signatory(pool_script_hash)
      |> builder.spend(pool_input.output_reference)

  let ctx = ScriptContext { ..ctx, redeemer: update_fees_redeemer }

  pool_validator.manage.else(settings_policy_id, ctx)
}

/// Test setting fees > 100% fails
test update_pool_fees_too_high_fails() fail {
  let pool_nft = pool_nft_name(pool_ident)

  let pool_address =
    Address {
      payment_credential: Script(pool_script_hash),
      stake_credential: Some(Inline(VerificationKey(example_settings_admin))),
    }

  let pool_datum =
    PoolDatum { ..base_pool_datum(), fee_manager: Some(multisig.Signature(pool_script_hash)) }

  // Fees > 100%
  let pool_out_datum =
    PoolDatum {
      ..pool_datum,
      bid_fees_per_10_thousand: 10001,
      ask_fees_per_10_thousand: 10001,
    }

  let pool_rider = 2_000_000

  let pool_input =
    new_tx_input(
      mk_tx_hash(0),
      pool_address,
      1_000_000_000 + pool_rider,
      InlineDatum(pool_datum),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> with_asset_of_tx_input(assets.from_asset(pool_script_hash, pool_nft, 1))

  let pool_output =
    new_tx_output(pool_address, 0, InlineDatum(pool_out_datum))
      |> add_asset_to_tx_output(assets.from_lovelace(1_000_000_000 + pool_rider))
      |> add_asset_to_tx_output(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> add_asset_to_tx_output(assets.from_asset(pool_script_hash, pool_nft, 1))

  let settings_input =
    new_tx_input(
      mk_tx_hash(1),
      script_address(settings_policy_id),
      1,
      InlineDatum(mk_valid_settings_datum([])),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(settings_policy_id, settings_nft_name, 1),
        )

  let update_fees_redeemer = UpdatePoolFees { pool_input: 0 }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_ref_input(settings_input)
      |> add_tx_input(pool_input)
      |> add_tx_output(pool_output)
      |> builder.add_signatory(pool_script_hash)
      |> builder.spend(pool_input.output_reference)

  let ctx = ScriptContext { ..ctx, redeemer: update_fees_redeemer }

  pool_validator.manage.else(settings_policy_id, ctx)
}

/// Test updating fees without fee manager fails
test update_pool_fees_no_manager_fails() fail {
  let pool_nft = pool_nft_name(pool_ident)

  let pool_address =
    Address {
      payment_credential: Script(pool_script_hash),
      stake_credential: Some(Inline(VerificationKey(example_settings_admin))),
    }

  // No fee manager
  let pool_datum = PoolDatum { ..base_pool_datum(), fee_manager: None }

  let pool_out_datum =
    PoolDatum { ..pool_datum, bid_fees_per_10_thousand: 10, ask_fees_per_10_thousand: 10 }

  let pool_rider = 2_000_000

  let pool_input =
    new_tx_input(
      mk_tx_hash(0),
      pool_address,
      1_000_000_000 + pool_rider,
      InlineDatum(pool_datum),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> with_asset_of_tx_input(assets.from_asset(pool_script_hash, pool_nft, 1))

  let pool_output =
    new_tx_output(pool_address, 0, InlineDatum(pool_out_datum))
      |> add_asset_to_tx_output(assets.from_lovelace(1_000_000_000 + pool_rider))
      |> add_asset_to_tx_output(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> add_asset_to_tx_output(assets.from_asset(pool_script_hash, pool_nft, 1))

  let settings_input =
    new_tx_input(
      mk_tx_hash(1),
      script_address(settings_policy_id),
      1,
      InlineDatum(mk_valid_settings_datum([])),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(settings_policy_id, settings_nft_name, 1),
        )

  let update_fees_redeemer = UpdatePoolFees { pool_input: 0 }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_ref_input(settings_input)
      |> add_tx_input(pool_input)
      |> add_tx_output(pool_output)
      |> builder.add_signatory(pool_script_hash)
      |> builder.spend(pool_input.output_reference)

  let ctx = ScriptContext { ..ctx, redeemer: update_fees_redeemer }

  pool_validator.manage.else(settings_policy_id, ctx)
}

/// Test updating fees with script-based fee manager
test update_pool_fees_with_script_manager() {
  let pool_nft = pool_nft_name(pool_ident)
  let script_hash = #"1234"

  let pool_address =
    Address {
      payment_credential: Script(pool_script_hash),
      stake_credential: Some(Inline(VerificationKey(example_settings_admin))),
    }

  // Script-based fee manager
  let pool_datum =
    PoolDatum { ..base_pool_datum(), fee_manager: Some(multisig.Script(script_hash)) }

  let pool_out_datum =
    PoolDatum { ..pool_datum, bid_fees_per_10_thousand: 10, ask_fees_per_10_thousand: 10 }

  let pool_rider = 2_000_000

  let pool_input =
    new_tx_input(
      mk_tx_hash(0),
      pool_address,
      1_000_000_000 + pool_rider,
      InlineDatum(pool_datum),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> with_asset_of_tx_input(assets.from_asset(pool_script_hash, pool_nft, 1))

  let pool_output =
    new_tx_output(pool_address, 0, InlineDatum(pool_out_datum))
      |> add_asset_to_tx_output(assets.from_lovelace(1_000_000_000 + pool_rider))
      |> add_asset_to_tx_output(
          assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000),
        )
      |> add_asset_to_tx_output(assets.from_asset(pool_script_hash, pool_nft, 1))

  let settings_input =
    new_tx_input(
      mk_tx_hash(1),
      script_address(settings_policy_id),
      1,
      InlineDatum(mk_valid_settings_datum([])),
    )
      |> with_asset_of_tx_input(
          assets.from_asset(settings_policy_id, settings_nft_name, 1),
        )

  let update_fees_redeemer = UpdatePoolFees { pool_input: 0 }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_ref_input(settings_input)
      |> add_tx_input(pool_input)
      |> add_tx_output(pool_output)
      |> builder.add_signatory(pool_script_hash)
      |> builder.spend(pool_input.output_reference)

  // Add script withdrawal for multisig
  let ctx =
    ScriptContext {
      ..ctx,
      transaction: Transaction {
        ..ctx.transaction,
        withdrawals: [Pair(Script(script_hash), 100)],
      },
      redeemer: update_fees_redeemer,
    }

  pool_validator.manage.else(settings_policy_id, ctx)
}

// ============================================
// MINT POOL TESTS
// ============================================

fn pool_ident_from_input(tx_input: Input) -> ByteArray {
  tx_input.output_reference.transaction_id
    |> bytearray.concat(#"23")
    |> bytearray.concat(
        pool_validator.int_to_ident(tx_input.output_reference.output_index),
      )
    |> crypto.blake2b_256
    |> bytearray.drop(4)
}

fn funds_input() -> Input {
  Input {
    output_reference: mk_output_reference(0),
    output: Output {
      address: wallet_address(payment_key),
      value: assets.from_lovelace(100_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

/// Test basic pool creation
test mint_pool_success() {
  let pool_address =
    script_address(pool_script_hash)
      |> with_delegation_key(settings_admin)
  let user_address = wallet_address(payment_key)
  let settings_input = mk_settings_input([], 1)

  let input = funds_input()
  let pool_id = pool_ident_from_input(input)
  let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) =
    pool_token_names(pool_id)

  let pool_datum =
    PoolDatum {
      identifier: pool_id,
      assets: ((ada_policy_id, ada_asset_name), (rberry_policy, rberry_asset_name)),
      circulating_lp: 1_000_000_000,
      bid_fees_per_10_thousand: 5,
      ask_fees_per_10_thousand: 5,
      fee_manager: None,
      market_open: 0,
      protocol_fees: 2_000_000,
    }

  let pool_output_val =
    assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000)
      |> assets.add(pool_script_hash, new_pool_nft_token, 1)
      |> assets.merge(assets.from_lovelace(1_002_000_000))

  let pool_output =
    new_tx_output(pool_address, 0, InlineDatum(pool_datum))
      |> add_asset_to_tx_output(pool_output_val)

  let lp_output_val =
    assets.from_asset(pool_script_hash, pool_lp_name(pool_id), 1_000_000_000)
      |> assets.merge(assets.from_lovelace(2_000_000))

  let lp_output =
    new_tx_output(user_address, 0, NoDatum)
      |> add_asset_to_tx_output(lp_output_val)

  let ref_output_val =
    assets.from_asset(pool_script_hash, new_pool_ref_token, 1)
      |> assets.merge(assets.from_lovelace(2_000_000))

  let metadata_address = wallet_address(metadata_admin)
  let ref_output =
    new_tx_output(metadata_address, 0, InlineDatum(Void))
      |> add_asset_to_tx_output(ref_output_val)

  let pool_mint_redeemer =
    CreatePool {
      assets: ((ada_policy_id, ada_asset_name), (rberry_policy, rberry_asset_name)),
      pool_output: 0,
      metadata_output: 2,
    }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> mint_assets(
          pool_script_hash,
          assets.zero
            |> assets.add(pool_script_hash, new_pool_lp_token, 1_000_000_000)
            |> assets.add(pool_script_hash, new_pool_nft_token, 1)
            |> assets.add(pool_script_hash, new_pool_ref_token, 1),
        )
      |> add_tx_input(input)
      |> add_tx_ref_input(settings_input)
      |> add_tx_output(ref_output)
      |> add_tx_output(lp_output)
      |> add_tx_output(pool_output)

  pool_validator.pool.mint(
    manage_stake_script_hash,
    settings_policy_id,
    pool_mint_redeemer,
    pool_script_hash,
    ctx.transaction,
  )
}

/// Test creating pool with exotic token pair (non-ADA)
test mint_pool_exotic_pair() {
  let pool_address =
    script_address(pool_script_hash)
      |> with_delegation_key(settings_admin)
  let user_address = wallet_address(payment_key)
  let settings_input = mk_settings_input([], 1)

  let input = funds_input()
  let pool_id = pool_ident_from_input(input)
  let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) =
    pool_token_names(pool_id)

  let pool_datum =
    PoolDatum {
      identifier: pool_id,
      assets: ((#"01", #"54"), (#"02", #"55")),
      circulating_lp: 1_000_000_000,
      bid_fees_per_10_thousand: 5,
      ask_fees_per_10_thousand: 5,
      fee_manager: None,
      market_open: 0,
      protocol_fees: 2_000_000,
    }

  let pool_output_val =
    assets.from_asset(#"01", #"54", 1_000_000_000)
      |> assets.add(#"02", #"55", 1_000_000_000)
      |> assets.add(pool_script_hash, new_pool_nft_token, 1)
      |> assets.add(#"", #"", 2_000_000)

  let pool_output =
    new_tx_output(pool_address, 0, InlineDatum(pool_datum))
      |> add_asset_to_tx_output(pool_output_val)

  let lp_output_val =
    assets.from_asset(pool_script_hash, pool_lp_name(pool_id), 1_000_000_000)
      |> assets.merge(assets.from_lovelace(2_000_000))

  let lp_output =
    new_tx_output(user_address, 0, NoDatum)
      |> add_asset_to_tx_output(lp_output_val)

  let ref_output_val =
    assets.from_asset(pool_script_hash, new_pool_ref_token, 1)
      |> assets.merge(assets.from_lovelace(2_000_000))

  let metadata_address = wallet_address(metadata_admin)
  let ref_output =
    new_tx_output(metadata_address, 0, InlineDatum(Void))
      |> add_asset_to_tx_output(ref_output_val)

  let pool_mint_redeemer =
    CreatePool { assets: ((#"01", #"54"), (#"02", #"55")), pool_output: 0, metadata_output: 2 }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> mint_assets(
          pool_script_hash,
          assets.zero
            |> assets.add(pool_script_hash, new_pool_lp_token, 1_000_000_000)
            |> assets.add(pool_script_hash, new_pool_nft_token, 1)
            |> assets.add(pool_script_hash, new_pool_ref_token, 1),
        )
      |> add_tx_input(input)
      |> add_tx_ref_input(settings_input)
      |> add_tx_output(ref_output)
      |> add_tx_output(lp_output)
      |> add_tx_output(pool_output)

  pool_validator.pool.mint(
    manage_stake_script_hash,
    settings_policy_id,
    pool_mint_redeemer,
    pool_script_hash,
    ctx.transaction,
  )
}

/// Test pool creation fails with wrong output address
test mint_pool_wrong_address_fails() fail {
  let wrong_address =
    from_verification_key(random_hash)
      |> with_delegation_key(stake_key)
  let user_address = wallet_address(payment_key)
  let settings_input = mk_settings_input([], 1)

  let input = funds_input()
  let pool_id = pool_ident_from_input(input)
  let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) =
    pool_token_names(pool_id)

  let pool_datum =
    PoolDatum {
      identifier: pool_id,
      assets: ((ada_policy_id, ada_asset_name), (rberry_policy, rberry_asset_name)),
      circulating_lp: 1_000_000_000,
      bid_fees_per_10_thousand: 5,
      ask_fees_per_10_thousand: 5,
      fee_manager: None,
      market_open: 0,
      protocol_fees: 2_000_000,
    }

  let pool_output_val =
    assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000)
      |> assets.add(pool_script_hash, new_pool_nft_token, 1)
      |> assets.merge(assets.from_lovelace(1_002_000_000))

  // Wrong address
  let pool_output =
    new_tx_output(wrong_address, 0, InlineDatum(pool_datum))
      |> add_asset_to_tx_output(pool_output_val)

  let lp_output_val =
    assets.from_asset(pool_script_hash, pool_lp_name(pool_id), 1_000_000_000)
      |> assets.merge(assets.from_lovelace(2_000_000))

  let lp_output =
    new_tx_output(user_address, 0, NoDatum)
      |> add_asset_to_tx_output(lp_output_val)

  let ref_output_val =
    assets.from_asset(pool_script_hash, new_pool_ref_token, 1)
      |> assets.merge(assets.from_lovelace(2_000_000))

  let metadata_address = wallet_address(metadata_admin)
  let ref_output =
    new_tx_output(metadata_address, 0, InlineDatum(Void))
      |> add_asset_to_tx_output(ref_output_val)

  let pool_mint_redeemer =
    CreatePool {
      assets: ((ada_policy_id, ada_asset_name), (rberry_policy, rberry_asset_name)),
      pool_output: 0,
      metadata_output: 2,
    }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> mint_assets(
          pool_script_hash,
          assets.zero
            |> assets.add(pool_script_hash, new_pool_lp_token, 1_000_000_000)
            |> assets.add(pool_script_hash, new_pool_nft_token, 1)
            |> assets.add(pool_script_hash, new_pool_ref_token, 1),
        )
      |> add_tx_input(input)
      |> add_tx_ref_input(settings_input)
      |> add_tx_output(ref_output)
      |> add_tx_output(lp_output)
      |> add_tx_output(pool_output)

  pool_validator.pool.mint(
    manage_stake_script_hash,
    settings_policy_id,
    pool_mint_redeemer,
    pool_script_hash,
    ctx.transaction,
  )
}

/// Test pool creation fails with non-void metadata datum
test mint_pool_nonvoid_datum_fails() fail {
  let pool_address =
    script_address(pool_script_hash)
      |> with_delegation_key(settings_admin)
  let user_address = wallet_address(payment_key)
  let settings_input = mk_settings_input([], 1)

  let input = funds_input()
  let pool_id = pool_ident_from_input(input)
  let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) =
    pool_token_names(pool_id)

  let pool_datum =
    PoolDatum {
      identifier: pool_id,
      assets: ((ada_policy_id, ada_asset_name), (rberry_policy, rberry_asset_name)),
      circulating_lp: 1_000_000_000,
      bid_fees_per_10_thousand: 5,
      ask_fees_per_10_thousand: 5,
      fee_manager: None,
      market_open: 0,
      protocol_fees: 2_000_000,
    }

  let pool_output_val =
    assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000)
      |> assets.add(pool_script_hash, new_pool_nft_token, 1)
      |> assets.merge(assets.from_lovelace(1_002_000_000))

  let pool_output =
    new_tx_output(pool_address, 0, InlineDatum(pool_datum))
      |> add_asset_to_tx_output(pool_output_val)

  let lp_output_val =
    assets.from_asset(pool_script_hash, pool_lp_name(pool_id), 1_000_000_000)
      |> assets.merge(assets.from_lovelace(2_000_000))

  let lp_output =
    new_tx_output(user_address, 0, NoDatum)
      |> add_asset_to_tx_output(lp_output_val)

  let ref_output_val =
    assets.from_asset(pool_script_hash, new_pool_ref_token, 1)
      |> assets.merge(assets.from_lovelace(2_000_000))

  let metadata_address = wallet_address(metadata_admin)
  // Non-void datum
  let ref_output =
    new_tx_output(metadata_address, 0, InlineDatum("Evil data"))
      |> add_asset_to_tx_output(ref_output_val)

  let pool_mint_redeemer =
    CreatePool {
      assets: ((ada_policy_id, ada_asset_name), (rberry_policy, rberry_asset_name)),
      pool_output: 0,
      metadata_output: 2,
    }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> mint_assets(
          pool_script_hash,
          assets.zero
            |> assets.add(pool_script_hash, new_pool_lp_token, 1_000_000_000)
            |> assets.add(pool_script_hash, new_pool_nft_token, 1)
            |> assets.add(pool_script_hash, new_pool_ref_token, 1),
        )
      |> add_tx_input(input)
      |> add_tx_ref_input(settings_input)
      |> add_tx_output(ref_output)
      |> add_tx_output(lp_output)
      |> add_tx_output(pool_output)

  pool_validator.pool.mint(
    manage_stake_script_hash,
    settings_policy_id,
    pool_mint_redeemer,
    pool_script_hash,
    ctx.transaction,
  )
}

/// Test minting multiple pool NFTs fails
test mint_pool_multiple_nfts_fails() fail {
  let pool_address =
    script_address(pool_script_hash)
      |> with_delegation_key(settings_admin)
  let user_address = wallet_address(payment_key)
  let settings_input = mk_settings_input([], 1)

  let input = funds_input()
  let pool_id = pool_ident_from_input(input)
  let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) =
    pool_token_names(pool_id)

  let pool_datum =
    PoolDatum {
      identifier: pool_id,
      assets: ((ada_policy_id, ada_asset_name), (rberry_policy, rberry_asset_name)),
      circulating_lp: 1_000_000_000,
      bid_fees_per_10_thousand: 5,
      ask_fees_per_10_thousand: 5,
      fee_manager: None,
      market_open: 0,
      protocol_fees: 2_000_000,
    }

  // Multiple NFTs
  let pool_output_val =
    assets.from_asset(rberry_policy, rberry_asset_name, 1_000_000_000)
      |> assets.add(pool_script_hash, new_pool_nft_token, 2)
      |> assets.merge(assets.from_lovelace(1_002_000_000))

  let pool_output =
    new_tx_output(pool_address, 0, InlineDatum(pool_datum))
      |> add_asset_to_tx_output(pool_output_val)

  let lp_output_val =
    assets.from_asset(pool_script_hash, pool_lp_name(pool_id), 1_000_000_000)
      |> assets.merge(assets.from_lovelace(2_000_000))

  let lp_output =
    new_tx_output(user_address, 0, NoDatum)
      |> add_asset_to_tx_output(lp_output_val)

  let ref_output_val =
    assets.from_asset(pool_script_hash, new_pool_ref_token, 1)
      |> assets.merge(assets.from_lovelace(2_000_000))

  let metadata_address = wallet_address(metadata_admin)
  let ref_output =
    new_tx_output(metadata_address, 0, InlineDatum(Void))
      |> add_asset_to_tx_output(ref_output_val)

  let pool_mint_redeemer =
    CreatePool {
      assets: ((ada_policy_id, ada_asset_name), (rberry_policy, rberry_asset_name)),
      pool_output: 0,
      metadata_output: 2,
    }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> mint_assets(
          pool_script_hash,
          assets.zero
            |> assets.add(pool_script_hash, new_pool_lp_token, 1_000_000_000)
            |> assets.add(pool_script_hash, new_pool_nft_token, 2)
            |> assets.add(pool_script_hash, new_pool_ref_token, 1),
        )
      |> add_tx_input(input)
      |> add_tx_ref_input(settings_input)
      |> add_tx_output(ref_output)
      |> add_tx_output(lp_output)
      |> add_tx_output(pool_output)

  pool_validator.pool.mint(
    manage_stake_script_hash,
    settings_policy_id,
    pool_mint_redeemer,
    pool_script_hash,
    ctx.transaction,
  )
}

// ============================================
// BURN POOL TESTS
// ============================================

/// Test burning empty pool
test burn_pool_success() {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)

  let pool_datum =
    PoolDatum {
      ..base_pool_datum(),
      circulating_lp: 0,
    }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(2_000_000)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let settings_input = mk_settings_input([scooper], 1)

  let change_output =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000),
      datum: NoDatum,
      reference_script: None,
    }

  let (_, pool_nft_token, _) = pool_token_names(pool_ident)

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> mint_assets(
          pool_script_hash,
          assets.zero
            |> assets.add(pool_script_hash, pool_nft_token, -1),
        )
      |> add_tx_input(pool_input)
      |> add_tx_ref_input(settings_input)
      |> add_tx_output(change_output)

  pool_validator.pool.mint(
    manage_stake_script_hash,
    settings_policy_id,
    BurnPool { identifier: pool_ident },
    pool_script_hash,
    ctx.transaction,
  )
}

// ============================================
// POOL VALUE HELPER TESTS
// ============================================

/// Test has_expected_pool_value with ADA/token pair
test pool_value_ada_token_pair() {
  let (_, pool_nft_token, pool_lp_token) = pool_token_names(pool_ident)
  let pool_value =
    assets.from_lovelace(102_000_000)
      |> assets.add(rberry_policy, rberry_asset_name, 100_000_000)
      |> assets.add(pool_script_hash, pool_nft_token, 1)

  let outcome =
    calc_shared.PoolState {
      quantity_lp: (pool_script_hash, pool_lp_token, 99),
      quantity_a: (ada_policy_id, ada_asset_name, 100_000_000),
      quantity_b: (rberry_policy, rberry_asset_name, 100_000_000),
    }

  pool_validator.has_expected_pool_value(
    pool_script_hash,
    pool_ident,
    pool_value,
    outcome.quantity_a.1st,
    outcome.quantity_a.2nd,
    outcome.quantity_a.3rd,
    outcome.quantity_b.1st,
    outcome.quantity_b.2nd,
    outcome.quantity_b.3rd,
    99,
    2_000_000,
  )
}

/// Test has_expected_pool_value with token/token pair
test pool_value_token_token_pair() {
  let (_, pool_nft_token, pool_lp_token) = pool_token_names(pool_ident)
  let pool_value =
    assets.from_lovelace(2_000_000)
      |> assets.add(other_policy, other_asset_name, 100_000_000)
      |> assets.add(rberry_policy, rberry_asset_name, 100_000_000)
      |> assets.add(pool_script_hash, pool_nft_token, 1)

  let outcome =
    calc_shared.PoolState {
      quantity_lp: (pool_script_hash, pool_lp_token, 99),
      quantity_a: (other_policy, other_asset_name, 100_000_000),
      quantity_b: (rberry_policy, rberry_asset_name, 100_000_000),
    }

  pool_validator.has_expected_pool_value(
    pool_script_hash,
    pool_ident,
    pool_value,
    outcome.quantity_a.1st,
    outcome.quantity_a.2nd,
    outcome.quantity_a.3rd,
    outcome.quantity_b.1st,
    outcome.quantity_b.2nd,
    outcome.quantity_b.3rd,
    99,
    2_000_000,
  )
}

/// Test has_expected_pool_value with empty pool
test pool_value_empty_pool() {
  let (_, pool_nft_token, pool_lp_token) = pool_token_names(pool_ident)
  let pool_value =
    assets.from_lovelace(3_000_000)
      |> assets.add(pool_script_hash, pool_nft_token, 1)

  let outcome =
    calc_shared.PoolState {
      quantity_lp: (pool_script_hash, pool_lp_token, 0),
      quantity_a: (other_policy, other_asset_name, 0),
      quantity_b: (rberry_policy, rberry_asset_name, 0),
    }

  pool_validator.has_expected_pool_value(
    pool_script_hash,
    pool_ident,
    pool_value,
    outcome.quantity_a.1st,
    outcome.quantity_a.2nd,
    outcome.quantity_a.3rd,
    outcome.quantity_b.1st,
    outcome.quantity_b.2nd,
    outcome.quantity_b.3rd,
    0,
    3_000_000,
  )
}

// ============================================
// EDGE CASE TESTS
// ============================================

/// Test swap with edge case rounding where amounts are very small relative to pool size
/// Two swaps of 799M ADA each against a pool with 20B ADA and only 50 rberry tokens
test scoop_swap_with_over_rounding() {
  let user_addr = wallet_address(payment_key)
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let order_address = script_address(order_script_hash)

  // Specific amounts that test edge case rounding
  let amt_1 = 408_367_450
  let amt_2 = 425_387_016

  let pool_datum =
    PoolDatum {
      ..base_pool_datum(),
      protocol_fees: 2_000_000,
    }
  let pool_out_datum =
    PoolDatum { ..pool_datum, protocol_fees: 2_000_000 + 2_500_000 + 2_500_000 }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(20_000_000_000 + 2_000_000)
          |> assets.add(rberry_policy, rberry_asset_name, 50)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let order1_datum =
    OrderDatum {
      ..base_order_datum(),
      details: Swap {
        offer: (ada_policy_id, ada_asset_name, 799_000_000),
        min_received: (rberry_policy, rberry_asset_name, 0),
      },
    }

  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(4_500_000 + 799_000_000),
        datum: InlineDatum(order1_datum),
        reference_script: None,
      },
    }

  let order2_datum =
    OrderDatum {
      ..base_order_datum(),
      details: Swap {
        offer: (ada_policy_id, ada_asset_name, 799_000_000),
        min_received: (rberry_policy, rberry_asset_name, 0),
      },
    }

  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(4_500_000 + 799_000_000),
        datum: InlineDatum(order2_datum),
        reference_script: None,
      },
    }

  // Pool output with updated reserves
  let pool_output =
    Output {
      address: pool_address,
      value: assets.from_lovelace(
        amt_1 + amt_2 + 20_000_000_000 + 2_000_000 + 2_500_000 + 2_500_000,
      )
        |> assets.add(rberry_policy, rberry_asset_name, 48)
        |> assets.add(pool_script_hash, pool_nft, 1),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }

  // User outputs - each gets 1 rberry token, with ADA change
  let user1_output =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000 + 799_000_000 - amt_1)
        |> assets.add(rberry_policy, rberry_asset_name, 1),
      datum: NoDatum,
      reference_script: None,
    }

  let user2_output =
    Output {
      address: user_addr,
      value: assets.from_lovelace(2_000_000 + 799_000_000 - amt_2)
        |> assets.add(rberry_policy, rberry_asset_name, 1),
      datum: NoDatum,
      reference_script: None,
    }

  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [mk_settings_input([scooper], 1)],
      outputs: [pool_output, user1_output, user2_output],
      fee: 1_000_000,
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      id: mock_tx_hash(1),
    }

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [(1, None, 0), (2, None, 0)] },
    pool_input.output_reference,
    transaction,
  )
}

// ============================================
// POOL EVAPORATION TESTS
// ============================================

/// Test burning an empty pool while withdrawing all protocol fees
test evaporate_pool_success() {
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let protocol_fees = 18_000_000
  let withdraw_amount = 18_000_000

  let pool_datum =
    PoolDatum {
      ..base_pool_datum(),
      circulating_lp: 0,
      protocol_fees: protocol_fees,
    }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(protocol_fees)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let settings_input = mk_settings_input([scooper], 1)

  let treasury_output =
    Output {
      address: Address(VerificationKey(example_treasury_address), None),
      value: assets.from_lovelace(withdraw_amount),
      datum: InlineDatum(Void),
      reference_script: None,
    }

  let (_, pool_nft_token, _) = pool_token_names(pool_ident)

  let withdraw_fees_redeemer =
    WithdrawFees { amount: withdraw_amount, treasury_output: 0, pool_input: 0 }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> mint_assets(
          pool_script_hash,
          assets.zero
            |> assets.add(pool_script_hash, pool_nft_token, -1),
        )
      |> add_tx_input(pool_input)
      |> add_tx_ref_input(settings_input)
      |> add_tx_output(treasury_output)
      |> insert_withdrawal(Script(manage_stake_script_hash), 0, compare_stake)
      |> insert_redeemer(
          Spend(mk_output_reference(0)),
          Manage,
          compare_redeemer,
        )
      |> insert_redeemer(
          Withdraw(Script(manage_stake_script_hash)),
          withdraw_fees_redeemer,
          compare_redeemer,
        )
      |> builder.add_signatory(example_treasury_admin)
      |> builder.spend(pool_input.output_reference)

  pool_validator.manage.else(
    settings_policy_id,
    ScriptContext { ..ctx, redeemer: withdraw_fees_redeemer },
  )
}

/// Test that pool evaporation fails if not withdrawing full protocol fees
test evaporate_pool_insufficient_withdrawal_fails() fail {
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let protocol_fees = 18_000_000
  // Withdraw less than full protocol fees - should fail
  let withdraw_amount = 17_000_000

  let pool_datum =
    PoolDatum {
      ..base_pool_datum(),
      circulating_lp: 0,
      protocol_fees: protocol_fees,
    }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(protocol_fees)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let settings_input = mk_settings_input([scooper], 1)

  let treasury_output =
    Output {
      address: Address(VerificationKey(example_treasury_address), None),
      value: assets.from_lovelace(withdraw_amount),
      datum: NoDatum,
      reference_script: None,
    }

  let (_, pool_nft_token, _) = pool_token_names(pool_ident)

  let withdraw_fees_redeemer =
    WithdrawFees { amount: withdraw_amount, treasury_output: 0, pool_input: 0 }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> mint_assets(
          pool_script_hash,
          assets.zero
            |> assets.add(pool_script_hash, pool_nft_token, -1),
        )
      |> add_tx_input(pool_input)
      |> add_tx_ref_input(settings_input)
      |> add_tx_output(treasury_output)
      |> insert_withdrawal(Script(manage_stake_script_hash), 0, compare_stake)
      |> insert_redeemer(
          Spend(mk_output_reference(0)),
          Manage,
          compare_redeemer,
        )
      |> insert_redeemer(
          Withdraw(Script(manage_stake_script_hash)),
          withdraw_fees_redeemer,
          compare_redeemer,
        )
      |> builder.add_signatory(example_treasury_admin)
      |> builder.spend(pool_input.output_reference)

  pool_validator.manage.else(
    settings_policy_id,
    ScriptContext { ..ctx, redeemer: withdraw_fees_redeemer },
  )
}

/// Test pool evaporation via pool.spend with Manage redeemer
test evaporate_pool_via_spend() {
  let pool_nft = pool_nft_name(pool_ident)
  let pool_address = script_address(pool_script_hash)
  let protocol_fees = 18_000_000
  let withdraw_amount = 18_000_000

  let pool_datum =
    PoolDatum {
      ..base_pool_datum(),
      circulating_lp: 0,
      protocol_fees: protocol_fees,
    }

  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: assets.from_lovelace(protocol_fees)
          |> assets.add(pool_script_hash, pool_nft, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }

  let settings_input = mk_settings_input([scooper], 1)

  let treasury_output =
    Output {
      address: Address(VerificationKey(example_treasury_address), None),
      value: assets.from_lovelace(withdraw_amount),
      datum: NoDatum,
      reference_script: None,
    }

  let (_, pool_nft_token, _) = pool_token_names(pool_ident)

  let withdraw_fees_redeemer =
    WithdrawFees { amount: withdraw_amount, treasury_output: 0, pool_input: 0 }

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> mint_assets(
          pool_script_hash,
          assets.zero
            |> assets.add(pool_script_hash, pool_nft_token, -1),
        )
      |> add_tx_input(pool_input)
      |> add_tx_ref_input(settings_input)
      |> add_tx_output(treasury_output)
      |> insert_withdrawal(Script(manage_stake_script_hash), 0, compare_stake)
      |> insert_redeemer(
          Spend(mk_output_reference(0)),
          Manage,
          compare_redeemer,
        )
      |> insert_redeemer(
          Withdraw(Script(manage_stake_script_hash)),
          withdraw_fees_redeemer,
          compare_redeemer,
        )
      |> builder.add_signatory(example_treasury_admin)
      |> builder.spend(pool_input.output_reference)

  pool_validator.pool.spend(
    manage_stake_script_hash,
    settings_policy_id,
    Some(pool_datum),
    Manage,
    pool_input.output_reference,
    ctx.transaction,
  )
}
