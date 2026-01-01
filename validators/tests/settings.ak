use aiken/interval
use cardano/address.{Address, VerificationKey, from_script}
use cardano/assets
use cardano/transaction.{InlineDatum, NoDatum, OutputReference}
use settings as settings_validator
use sundae/multisig
use tests/fixtures/constants.{payment_key, settings_policy_id, stake_key}
use tx_util/builder.{
  add_asset_to_tx_output, add_tx_input, add_tx_output, build_txn_context,
  mint_assets, new_tx_input, new_tx_output, with_asset_of_tx_input,
}
use types/settings.{
  SettingsAdminUpdate, SettingsDatum, SettingsRedeemer, TreasuryAdminUpdate,
  settings_nft_name,
}

const settings_admin = #"6313a1d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

const metadata_admin = #"7b143ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"

const treasury_admin = #"17bbd2d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

const new_treasury_address = #"aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd"

const protocol_boot_utxo = OutputReference { transaction_id: #"00", output_index: 0 }

fn base_settings_datum() -> SettingsDatum {
  SettingsDatum {
    settings_admin: multisig.Signature(settings_admin),
    metadata_admin: Address(VerificationKey(metadata_admin), None),
    treasury_admin: multisig.Signature(treasury_admin),
    treasury_address: Address(VerificationKey(payment_key), None),
    treasury_allowance: (1, 10),
    authorized_scoopers: Some([]),
    authorized_staking_keys: [VerificationKey(stake_key)],
    base_fee: 0,
    simple_fee: 2_500_000,
    strategy_fee: 5_000_000,
    pool_creation_fee: 0,
    extensions: Void,
  }
}

fn settings_address() {
  from_script(settings_policy_id)
}

fn settings_nft() {
  assets.from_asset(settings_policy_id, settings_nft_name, 1)
}

fn settings_input(datum: SettingsDatum) {
  new_tx_input(#"01", settings_address(), 2_000_000, InlineDatum(datum))
    |> with_asset_of_tx_input(settings_nft())
}

fn settings_output(datum: SettingsDatum, lovelace: Int) {
  new_tx_output(settings_address(), lovelace, InlineDatum(datum))
    |> add_asset_to_tx_output(settings_nft())
}

fn run_settings_spend(
  input_datum: SettingsDatum,
  output_datum: SettingsDatum,
  output_lovelace: Int,
  signer: ByteArray,
  redeemer: SettingsRedeemer,
) -> Bool {
  let input = settings_input(input_datum)
  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_input(input)
      |> add_tx_output(settings_output(output_datum, output_lovelace))
      |> builder.add_signatory(signer)
      |> builder.spend(input.output_reference)

  settings_validator.settings.spend(
    protocol_boot_utxo,
    Some(input_datum),
    redeemer,
    input.output_reference,
    ctx.transaction,
  )
}

// Mint Tests

fn test_mint_settings(nft_count: Int) -> Bool {
  let mint_value =
    assets.from_asset(settings_policy_id, settings_nft_name, nft_count)

  let datum = base_settings_datum()
  let output =
    new_tx_output(settings_address(), 2_000_000, InlineDatum(datum))
      |> add_asset_to_tx_output(mint_value)

  let boot_input =
    new_tx_input(#"00", from_script(#"00"), 2_000_000, NoDatum)
      |> with_asset_of_tx_input(assets.from_asset(#"00", "boot utxo name", 1))

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> mint_assets(settings_policy_id, mint_value)
      |> add_tx_input(boot_input)
      |> add_tx_output(output)

  settings_validator.settings.mint(
    protocol_boot_utxo,
    Void,
    settings_policy_id,
    ctx.transaction,
  )
}

test mint_settings_success() {
  test_mint_settings(1)
}

test mint_settings_multiple_fails() fail {
  test_mint_settings(2)
}

// SettingsAdminUpdate Spend Tests

test spend_settings_change_ada_rider() {
  let datum = base_settings_datum()
  run_settings_spend(datum, datum, 2_000_001, settings_admin, SettingsAdminUpdate)
}

test spend_settings_change_value_fails() fail {
  let datum = base_settings_datum()
  let input = settings_input(datum)

  let junk_policy = "junk policy"
  let junk_input =
    new_tx_input(#"99", from_script(junk_policy), 1, InlineDatum(Void))
      |> with_asset_of_tx_input(assets.from_asset(junk_policy, "junk", 1))
  let junk_output =
    new_tx_output(from_script(junk_policy), 1, InlineDatum(Void))
      |> add_asset_to_tx_output(assets.from_asset(junk_policy, "junk", 1))

  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_input(input)
      |> add_tx_input(junk_input)
      |> add_tx_output(settings_output(datum, 2_000_001))
      |> add_tx_output(junk_output)
      |> builder.add_signatory(settings_admin)
      |> builder.spend(input.output_reference)

  settings_validator.settings.spend(
    protocol_boot_utxo,
    Some(datum),
    SettingsAdminUpdate,
    input.output_reference,
    ctx.transaction,
  )
}

// TreasuryAdminUpdate Tests

test treasury_admin_update_address_success() {
  let input_datum = base_settings_datum()
  let output_datum =
    SettingsDatum {
      ..input_datum,
      treasury_address: Address(VerificationKey(new_treasury_address), None),
    }
  run_settings_spend(
    input_datum,
    output_datum,
    2_000_000,
    treasury_admin,
    TreasuryAdminUpdate,
  )
}

test treasury_admin_update_allowance_success() {
  let input_datum = base_settings_datum()
  let output_datum = SettingsDatum { ..input_datum, treasury_allowance: (2, 10) }
  run_settings_spend(
    input_datum,
    output_datum,
    2_000_000,
    treasury_admin,
    TreasuryAdminUpdate,
  )
}

test treasury_admin_update_staking_keys_success() {
  let input_datum = base_settings_datum()
  let new_key = #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
  let output_datum =
    SettingsDatum {
      ..input_datum,
      authorized_staking_keys: [
        VerificationKey(stake_key),
        VerificationKey(new_key),
      ],
    }
  run_settings_spend(
    input_datum,
    output_datum,
    2_000_000,
    treasury_admin,
    TreasuryAdminUpdate,
  )
}

test treasury_admin_cannot_change_settings_admin_fails() fail {
  let input_datum = base_settings_datum()
  let output_datum =
    SettingsDatum {
      ..input_datum,
      settings_admin: multisig.Signature(new_treasury_address),
    }
  run_settings_spend(
    input_datum,
    output_datum,
    2_000_000,
    treasury_admin,
    TreasuryAdminUpdate,
  )
}

test treasury_admin_cannot_change_simple_fee_fails() fail {
  let input_datum = base_settings_datum()
  let output_datum = SettingsDatum { ..input_datum, simple_fee: 5_000_000 }
  run_settings_spend(
    input_datum,
    output_datum,
    2_000_000,
    treasury_admin,
    TreasuryAdminUpdate,
  )
}

test treasury_admin_update_wrong_signature_fails() fail {
  let input_datum = base_settings_datum()
  let output_datum =
    SettingsDatum {
      ..input_datum,
      treasury_address: Address(VerificationKey(new_treasury_address), None),
    }
  // Wrong signer
  run_settings_spend(
    input_datum,
    output_datum,
    2_000_000,
    settings_admin,
    TreasuryAdminUpdate,
  )
}
