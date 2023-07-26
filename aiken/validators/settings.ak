use aiken/list
use aiken/transaction.{Mint, ScriptContext, OutputReference}
use aiken/transaction/value
use shared
use types/settings.{SettingsDatum, SettingsMintRedeemer, SettingsRedeemer}

validator(protocol_boot_utxo: OutputReference) {
  fn spend(_d: SettingsDatum, _r: SettingsRedeemer, _ctx: ScriptContext) {
    True
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
