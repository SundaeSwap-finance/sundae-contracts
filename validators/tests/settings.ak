use aiken/interval
use aiken/transaction.{InlineDatum, NoDatum, OutputReference, TransactionId}
use aiken/transaction/credential.{VerificationKeyCredential, Address, from_script}
use aiken/transaction/value
use sundae/multisig
use types/settings.{SettingsDatum, SettingsAdminUpdate, settings_nft_name}
use tx_util/builder.{
  build_txn_context,
  add_tx_input,
  add_tx_output,
  new_tx_output,
  new_tx_input,
  with_asset_of_tx_input,
  add_asset_to_tx_output,
  mint_assets
}
use tests/examples/ex_settings.{example_settings_admin, example_metadata_admin, example_treasury_admin}
use settings as settings_validator
use tests/constants

fn test_mint_settings(settings_nfts_count: Int) {
  let settings_nft = value.to_minted_value(value.from_asset(constants.settings_policy_id, settings_nft_name, settings_nfts_count))

  let settings_datum = mk_valid_settings_datum([]) // Some([]) for authorized_scoopers means no one can scoop

  let settings_output = new_tx_output(
    from_script(constants.settings_policy_id),
    2_000_000,
    InlineDatum(settings_datum)
  ) |> add_asset_to_tx_output(value.from_asset(constants.settings_policy_id, settings_nft_name, settings_nfts_count))

  let protocol_boot_utxo = OutputReference { transaction_id: TransactionId { hash: #"00"}, output_index: 0 }
  let protocol_boot_utxo_policy = #"00"

  let protocol_boot_utxo_input = new_tx_input(
    protocol_boot_utxo.transaction_id.hash,
    from_script(protocol_boot_utxo_policy),
    2_000_000,
    NoDatum,
  ) |> with_asset_of_tx_input(value.from_asset(protocol_boot_utxo_policy, "boot utxo name", 1))
  let ctx = 
    interval.between(1, 2)
      |> build_txn_context()
      |> mint_assets(constants.settings_policy_id, settings_nft)
      |> add_tx_input(protocol_boot_utxo_input)
      |> add_tx_output(settings_output)


  let minted = settings_validator.mint(protocol_boot_utxo, Void, ctx)
  minted
}

test mint_invalid_settings_multiple_nft() fail {
  test_mint_settings(2)
}

test mint_valid_settings() {
  test_mint_settings(1)
}

fn mk_valid_settings_datum(
  scoopers: List<ByteArray>,
) -> SettingsDatum {
  SettingsDatum {
    settings_admin: multisig.Signature(example_settings_admin),
    metadata_admin: Address(
      VerificationKeyCredential(example_metadata_admin),
      None,
    ),
    treasury_admin: multisig.Signature(example_treasury_admin),
    treasury_address: Address(
      VerificationKeyCredential(constants.payment_key),
      None,
    ),
    treasury_allowance: (1, 10),
    authorized_scoopers: Some(scoopers),
    authorized_staking_keys: [
      VerificationKeyCredential(constants.stake_key),
    ],
    base_fee: 0,
    simple_fee: 2_500_000,
    strategy_fee: 5_000_000,
    pool_creation_fee: 0,
    extensions: Void,
  }
}

// Test that we can spend the settings NFT and do a no-op change, except for a new ada lovelace rider value
test test_spend_settings_change_ada_rider() {
  let first_settings_utxo = OutputReference { transaction_id: TransactionId { hash: #"01"}, output_index: 0 }
  let first_settings_rider = 2_000_000
  let first_settings = new_tx_input(
    first_settings_utxo.transaction_id.hash,
    from_script(constants.settings_policy_id),
    first_settings_rider,
    InlineDatum(mk_valid_settings_datum([])),
  ) |> with_asset_of_tx_input(value.from_asset(constants.settings_policy_id, settings_nft_name, 1))

  let protocol_boot_utxo = OutputReference { transaction_id: TransactionId { hash: #"00"}, output_index: 0 }
  let second_settings_datum = mk_valid_settings_datum([])
  
  let second_settings_redeemer = SettingsAdminUpdate

  let second_settings_output = new_tx_output(
        from_script(constants.settings_policy_id),
        first_settings_rider+1,
        InlineDatum(second_settings_datum)
      ) |> add_asset_to_tx_output(value.from_asset(constants.settings_policy_id, settings_nft_name, 1))
  let ctx = 
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_input(first_settings)
      |> add_tx_output(second_settings_output)
      |> builder.add_signatory(example_settings_admin)
      |> builder.spend(first_settings.output_reference)
  
  settings_validator.spend(protocol_boot_utxo, second_settings_datum, second_settings_redeemer, ctx)
}

// Test that we can spend the settings NFT and CAN'T change a non-ADA value while changing rider
test test_spend_settings_change_value() fail {
  let first_settings_utxo = OutputReference { transaction_id: TransactionId { hash: #"01"}, output_index: 0 }
  let first_settings_rider = 2_000_000
  let first_settings = new_tx_input(
    first_settings_utxo.transaction_id.hash,
    from_script(constants.settings_policy_id),
    first_settings_rider,
    InlineDatum(mk_valid_settings_datum([])),
  ) |> with_asset_of_tx_input(value.from_asset(constants.settings_policy_id, settings_nft_name, 1))

  let protocol_boot_utxo = OutputReference { transaction_id: TransactionId { hash: #"00"}, output_index: 0 }
  let second_settings_datum = mk_valid_settings_datum([])
  
  let second_settings_redeemer = SettingsAdminUpdate

  let second_settings_output = new_tx_output(
        from_script(constants.settings_policy_id),
        first_settings_rider+1,
        InlineDatum(second_settings_datum)
      ) |> add_asset_to_tx_output(value.from_asset(constants.settings_policy_id, settings_nft_name, 1))

  let junk_policy = "junk policy"
  let junk_name = "junk name"
  let junk_utxo = OutputReference { transaction_id: TransactionId { hash: #"99"}, output_index: 0 }
  let junk_input = new_tx_input(
        junk_utxo.transaction_id.hash,
        from_script(junk_policy),
        1,
        InlineDatum(Void)) |> with_asset_of_tx_input(value.from_asset(junk_policy, junk_name, 1))
  
  let junk_output = new_tx_output(
        from_script(junk_policy),
        1,
        InlineDatum(Void)
      ) |> add_asset_to_tx_output(value.from_asset(junk_policy, junk_name, 1))
  let ctx = 
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_input(first_settings)
      |> add_tx_input(junk_input)
      |> add_tx_output(second_settings_output)
      |> add_tx_output(junk_output)
      |> builder.add_signatory(example_settings_admin)
      |> builder.spend(first_settings.output_reference)
  
  let second_settings = settings_validator.spend(protocol_boot_utxo, second_settings_datum, second_settings_redeemer, ctx)
  second_settings
}