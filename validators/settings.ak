use aiken/list
use aiken/transaction.{InlineDatum, Spend, Mint, ScriptContext, OutputReference}
use aiken/transaction/value
use sundae/multisig
use types/settings.{SettingsDatum, SettingsMintRedeemer, SettingsRedeemer, SettingsAdminUpdate, TreasuryAdminUpdate, settings_nft_name}

validator(protocol_boot_utxo: OutputReference) {
  fn spend(d: SettingsDatum, redeemer: SettingsRedeemer, ctx: ScriptContext) {
    expect Spend(own_utxo_ref) = ctx.purpose
    expect Some(own_input) = ctx.transaction.inputs
      |> transaction.find_input(own_utxo_ref)
    let own_address = own_input.output.address
    // Assume the first output is the settings output
    // This is safe because we check that the value is unchanged, so if the outputs
    // are in a different order, that check will fail
    expect Some(own_output) = list.head(ctx.transaction.outputs)
    expect own_output.address == own_address
    expect InlineDatum(output_datum) = own_output.datum
    expect output_datum: SettingsDatum = output_datum

    // TODO: allow the ADA to change, in case the minUTXO changes, we don't want to lock the UTXO
    let value_not_changed =
      own_output.value == own_input.output.value

    let no_mint =
      value.from_minted_value(ctx.transaction.mint) == value.zero()

    when redeemer is {
      SettingsAdminUpdate -> {
        // TODO: require signature from the new admin too, to prevent dead-locking the protocol
        // TODO: also check for signature from the new treasury admin, if changed, to prevent locking the datum
        // i.e. that ensures we're within execution units limit
        let signed_by_admin =
          multisig.satisfied(
            d.settings_admin,
            ctx.transaction.extra_signatories,
            ctx.transaction.validity_range,
          )

        // Settings admin can change any datum fields except for these
        let datum_updated_legally =
            output_datum.authorized_staking_keys == d.authorized_staking_keys
            && output_datum.treasury_address == d.treasury_address
            && output_datum.treasury_allowance == d.treasury_allowance
        // TODO: move base_fee, simple_fee, and strategy_fee to the treasury admin instead
        // TODO: enforce maximum scooper keys to some large N, to prevent locking the datum

        signed_by_admin
          && datum_updated_legally
          && value_not_changed
          && no_mint
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
        // TODO: with a big list of fields like this, it's hard to understand and check correctness
        // instead, we could construct an "allowed datum" from the fields that *are* allowed to change, with a spread from the old datum
        // and compare that
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

        signed_by_admin
          && datum_updated_legally
          && value_not_changed
          && no_mint
      }
    }
  }

  // TODO: do we need a SettingsMintRedeemer? perhaps we make this Data
  fn mint(_r: SettingsMintRedeemer, ctx: ScriptContext) {
    expect Mint(own_policy_id) = ctx.purpose
    let mints_exactly_one_settings_nft =
      value.from_minted_value(ctx.transaction.mint) ==
        value.from_asset(own_policy_id, settings_nft_name, 1)

    let spends_protocol_boot_utxo =
      list.any(ctx.transaction.inputs, fn(input) {
        input.output_reference == protocol_boot_utxo
      })

    mints_exactly_one_settings_nft
      && spends_protocol_boot_utxo
  }
}
