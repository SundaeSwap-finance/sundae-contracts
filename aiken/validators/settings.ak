use aiken/list
use aiken/transaction.{InlineDatum, Spend, Mint, ScriptContext, OutputReference}
use aiken/transaction/credential.{ScriptCredential}
use aiken/transaction/value
use shared
use types/settings.{SettingsDatum, SettingsMintRedeemer, SettingsRedeemer}

validator(protocol_boot_utxo: OutputReference) {
  fn spend(d: SettingsDatum, _r: SettingsRedeemer, ctx: ScriptContext) {
    expect Spend(own_utxo_ref) = ctx.purpose
    expect Some(own_input) = ctx.transaction.inputs
      |> transaction.find_input(own_utxo_ref)
    let own_address = own_input.output.address
    expect ScriptCredential(own_script_hash) = own_address.payment_credential

    // Assume the first output is the settings output
    expect Some(own_output) = list.head(ctx.transaction.outputs)
    expect own_output.address == own_address
    expect InlineDatum(output_datum) = own_output.datum
    expect output_datum: SettingsDatum = output_datum

    let token_present_in_output =
      value.quantity_of(
        own_output.value,
        own_script_hash,
        shared.settings_nft_name) == 1

    let value_not_changed =
      own_output.value == own_input.output.value

    let datum_updated_legally =
      d.pool_script_hash == #""
        || output_datum == d

    let no_mint =
      value.from_minted_value(ctx.transaction.mint) == value.zero()

    token_present_in_output
      && value_not_changed
      && datum_updated_legally
      && no_mint
  }

  fn mint(_r: SettingsMintRedeemer, ctx: ScriptContext) {
    expect Mint(own_policy_id) = ctx.purpose
    let mints_exactly_one_settings_nft =
      value.from_minted_value(ctx.transaction.mint) ==
        value.from_asset(own_policy_id, shared.settings_nft_name, 1)

    let spends_protocol_boot_utxo =
      list.any(ctx.transaction.inputs, fn(input) {
        input.output_reference == protocol_boot_utxo
      })

    mints_exactly_one_settings_nft
      && spends_protocol_boot_utxo
  }
}
