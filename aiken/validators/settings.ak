use aiken/transaction.{ScriptContext}
use types/settings.{SettingsDatum, SettingsMintRedeemer, SettingsRedeemer}

validator {
  fn spend(_d: SettingsDatum, _r: SettingsRedeemer, _ctx: ScriptContext) {
    True
  }

  fn mint(_r: SettingsMintRedeemer, _ctx: ScriptContext) {
    True
  }
}
