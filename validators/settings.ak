use aiken/list
use aiken/interval
use aiken/bytearray
use aiken/transaction.{InlineDatum, NoDatum, Spend, Mint, ScriptContext, OutputReference, Transaction, TransactionId}
use aiken/transaction/credential.{VerificationKeyCredential, Address, from_script}
use aiken/transaction/value
use sundae/multisig
use types/settings.{SettingsDatum, SettingsRedeemer, SettingsAdminUpdate, TreasuryAdminUpdate, settings_nft_name}
use tx_util/builder.{
  build_txn_context,
  mint_assets,
  add_tx_input,
  add_tx_output,
  new_tx_output,
  new_tx_input,
  with_asset_of_tx_input,
}
use shared.{spent_output}

/// The settings validator lets the settings and treasury admins update global settings for the protocol
///
/// It is parameterized by the protocol_boot_utxo, a constant to make it an NFT by the usual trick.
validator(protocol_boot_utxo: OutputReference) {
  fn spend(d: SettingsDatum, redeemer: SettingsRedeemer, ctx: ScriptContext) {
    // Find our own input so we know the datum / our own address
    let own_input = spent_output(ctx)
    let own_address = own_input.address

    // Then find our continuing output;
    // We can assume the first output is the settings output
    // This is safe because the ledger doesn't reorder the outputs,
    // and if someone constructs it out of order, it'll just fail because the address / value will change
    // since the NFT is unique
    expect Some(own_output) = list.head(ctx.transaction.outputs)
    expect own_output.address == own_address
    // It's also important to enforce that this is an inline datum;
    // setting a datum hash could cause a bunch of off-chain infrastructure to break
    expect InlineDatum(output_datum) = own_output.datum
    expect output_datum: SettingsDatum = output_datum

    // We check that the value on the input and output are equal, to ensure the settings NFT gets paid back into the output
    // TODO: should we allow the ADA to change, just in case the minUTXO changes? that would allow us to add or reclaim
    // ADA if minUTXO changed
    let value_not_changed =
      own_output.value == own_input.value

    // Make sure we don't mint anything, otherwise someone might mint another settings token
    let no_mint =
      value.from_minted_value(ctx.transaction.mint) == value.zero()

    // Then, depending on which admin is doing the updating, different settings are updatable
    when redeemer is {
      // In the case of the settings admin
      SettingsAdminUpdate -> {
        // First, make sure the settings admin credentials (for example, a multisig) are satisfied
        // TODO: We could also require a signature from the new admin too, as a guard against accidentally bricking the protocol
        // TODO: We could also require a signature from the new treasury admin, if we're updating the treasury admin;
        // for example, this would ensure we don't set the treasury admin
        // so large that it exceeds execution units?
        let signed_by_admin =
          multisig.satisfied(
            d.settings_admin,
            ctx.transaction.extra_signatories,
            ctx.transaction.validity_range,
          )

        // Settings admin can change any datum fields except for these
        // TODO: This code is kind of hard to read: it's checking that some fields are *unchanged*
        // but the way we think about admin controls is in what they *are* allowed to change
        // one way to make this clearer is to construct an "allowed datum", by using
        // fields allowed to change from the output, and fields not allowed to change from the input
        // then comparing it to the output
        let datum_updated_legally =
            output_datum.authorized_staking_keys == d.authorized_staking_keys
            && output_datum.treasury_address == d.treasury_address
            && output_datum.treasury_allowance == d.treasury_allowance
        // TODO: move base_fee, simple_fee, and strategy_fee to the treasury admin instead, maybe?
        // TODO: enforce maximum scooper keys to some large N, to prevent locking the datum?

        and {
          signed_by_admin,
          datum_updated_legally,
          value_not_changed,
          no_mint,
        }
      }
      TreasuryAdminUpdate -> {
        let signed_by_admin =
          multisig.satisfied(
            d.treasury_admin,
            ctx.transaction.extra_signatories,
            ctx.transaction.validity_range,
          )

        // Treasury admin can change any datum fields except for these
        // i.e. can change the treasury address and treasury allowance
        // TODO: similar to above, and even more eggregiously here, it's confusing to parse this
        // because we normally think about what the admin *can* change, not what they *can't* change
        let datum_updated_legally =
            output_datum.authorized_staking_keys == d.authorized_staking_keys
            && output_datum.settings_admin == d.settings_admin
            && output_datum.metadata_admin == d.metadata_admin
            && output_datum.treasury_admin == d.treasury_admin
            && output_datum.authorized_scoopers == d.authorized_scoopers
            && output_datum.base_fee == d.base_fee
            && output_datum.simple_fee == d.simple_fee
            && output_datum.strategy_fee == d.strategy_fee
            && output_datum.extensions == d.extensions
        // TODO: enforce maximum staking keys to some large N, to prevent locking the Datum

        and {
          signed_by_admin,
          datum_updated_legally,
          value_not_changed,
          no_mint,
        }
      }
    }
  }

  // Let us mint the settings NFT exactly once, by checking that one of the inputs is the protocol_boot_utxo
  fn mint(_r: Data, ctx: ScriptContext) {
    expect Mint(own_policy_id) = ctx.purpose

    // Check that we mint *only* one token, and it's exactly our own policy id, and the settings NFT name
    // This ensures that we don't sneakily mint a second NFT for "reasons"
    let mints_exactly_one_settings_nft =
      value.from_minted_value(ctx.transaction.mint) ==
        value.from_asset(own_policy_id, settings_nft_name, 1)

    // And, like mentioned above, ensure that this is a true NFT
    let spends_protocol_boot_utxo =
      list.any(ctx.transaction.inputs, fn(input) {
        input.output_reference == protocol_boot_utxo
      })
    and {
      mints_exactly_one_settings_nft,
      spends_protocol_boot_utxo,
    }
  }
}


fn test_mint_settings(settings_nfts_count: Int) {
  let settings_nft_name = "settings"
  let settings_nft_policy = #"00"

  let settings_nft = value.to_minted_value(value.from_asset(settings_nft_policy, settings_nft_name, settings_nfts_count))

  let settings_datum = mk_valid_settings_datum([]) // Some([]) for authorized_scoopers means no one can scoop

  let settings_output = new_tx_output(
    from_script(settings_nft_policy),
    2_000_000,
    InlineDatum(settings_datum)
  )

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


  let minted = mint(protocol_boot_utxo, Void, ctx)
  minted
}

test mint_invalid_settings_multiple_nft() {
  !test_mint_settings(2)
}

test mint_valid_settings() {
  test_mint_settings(1)
}

//TODO: import?
fn mk_tx_hash(n: Int) -> TransactionId {
  expect n < 256
  let h =
    bytearray.push(
      #"00000000000000000000000000000000000000000000000000000000000000",
      n,
    )
  TransactionId { hash: h }
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