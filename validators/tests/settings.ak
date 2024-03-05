use aiken/interval
use aiken/transaction.{InlineDatum, NoDatum, OutputReference, TransactionId}
use aiken/transaction/credential.{VerificationKeyCredential, Address, from_script}
use aiken/transaction/value
use sundae/multisig
use types/settings.{SettingsDatum, settings_nft_name}
use tx_util/builder.{
  build_txn_context,
  mint_assets,
  add_tx_input,
  add_tx_output,
  new_tx_output,
  new_tx_input,
  with_asset_of_tx_input,
  add_asset_to_tx_output,
}
use settings as settings_validator

fn test_mint_settings(settings_nfts_count: Int) {
  let settings_nft_policy = #"00"

  let settings_nft = value.to_minted_value(value.from_asset(settings_nft_policy, settings_nft_name, settings_nfts_count))

  let settings_datum = mk_valid_settings_datum([]) // Some([]) for authorized_scoopers means no one can scoop

  let settings_output = new_tx_output(
    from_script(settings_nft_policy),
    2_000_000,
    InlineDatum(settings_datum)
  ) |> add_asset_to_tx_output(value.from_asset(settings_nft_policy, settings_nft_name, settings_nfts_count))

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
      |> mint_assets(settings_nft_policy, settings_nft)
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
    settings_admin: multisig.Signature(
      #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
    ),
    metadata_admin: Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    ),
    treasury_admin: multisig.Signature(
      #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
    ),
    treasury_address: Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    ),
    treasury_allowance: (1, 10),
    authorized_scoopers: Some(scoopers),
    authorized_staking_keys: [
      VerificationKeyCredential(
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"
      ),
    ],
    base_fee: 0,
    simple_fee: 2_500_000,
    strategy_fee: 5_000_000,
    pool_creation_fee: 0,
    extensions: Void,
  }
}