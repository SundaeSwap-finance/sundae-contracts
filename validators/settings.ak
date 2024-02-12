use aiken/list
use aiken/transaction.{InlineDatum, Mint, ScriptContext, OutputReference}
use aiken/transaction/credential.{ScriptCredential}
use aiken/transaction/value
use sundae/multisig
use types/settings.{SettingsDatum, SettingsRedeemer, SettingsAdminUpdate, TreasuryAdminUpdate, settings_nft_name}
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

    let expected_mint = value.from_asset(own_policy_id, settings_nft_name, 1)
    // Check that we mint *only* one token, and it's exactly our own policy id, and the settings NFT name
    // This ensures that we don't sneakily mint a second NFT for "reasons"
    let mints_exactly_one_settings_nft =
      value.from_minted_value(ctx.transaction.mint) == expected_mint

    // And, like mentioned above, ensure that this is a true NFT
    let spends_protocol_boot_utxo =
      list.any(ctx.transaction.inputs, fn(input) {
        input.output_reference == protocol_boot_utxo
      })

    // Make sure the output value contains no extra tokens, and is paid to the settings script itself
    expect [settings_output] = list.filter(ctx.transaction.outputs, fn(output) {
      value.without_lovelace(output.value) == expected_mint
    })
    let pays_to_settings_script = settings_output.address.payment_credential == ScriptCredential(own_policy_id)
    // Make sure the datum is an inline datum, and has a well-formed datum
    expect InlineDatum(settings_datum) = settings_output.datum
    expect _: SettingsDatum = settings_datum

    // TODO: require a signature from the 3 initial admins (or at least the settings admin) to prevent bricking?

    and {
      mints_exactly_one_settings_nft,
      spends_protocol_boot_utxo,
      pays_to_settings_script,
    }
  }
}
