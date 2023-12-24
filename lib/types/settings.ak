use aiken/transaction/credential.{Address, VerificationKey}
use sundae/multisig.{MultisigScript}
use shared.{Rational}
use aiken/transaction.{
  InlineDatum, Input, Output,
}
use aiken/builtin
use aiken/transaction/value.{AssetName, PolicyId}
use aiken/dict

pub type SettingsDatum {
  settings_admin: MultisigScript, // Who can update the updatable settings?
  metadata_admin: Address, // Who or what governs the evolution of CIP-68 token metadata
  treasury_admin: MultisigScript, // Who can withdraw to the treasury and control delegation
  treasury_address: Address, // Where to withdraw treasury funds
  treasury_allowance: Rational, // When withdrawing, what percentage can be released for administrative purposes?
  authorized_scoopers: Option<List<VerificationKey>>,
  authorized_staking_keys: List<VerificationKey>,

  base_fee: Int,
  simple_fee: Int,
  strategy_fee: Int,

  // TODO: split into settings_extensions and treasury_extensions
  // so we can permission things separately
  extensions: Data,
}

pub type SettingsRedeemer {
  SettingsAdminUpdate
  TreasuryAdminUpdate
}

pub type SettingsMintRedeemer {
  BootProtocol
}

pub const settings_nft_name: AssetName = "settings"

pub fn find_settings_datum(reference_inputs: List<Input>, settings_policy_id: PolicyId) -> SettingsDatum {
  let settings_output = builtin.head_list(reference_inputs).output
  let settings_value_list = dict.to_list(value.to_dict(settings_output.value))
  expect [_, (found_policy_id, settings_dict)] = settings_value_list
  expect found_policy_id == settings_policy_id
  expect dict.to_list(settings_dict) == [(settings_nft_name, 1)]
  expect InlineDatum(settings_datum) = settings_output.datum
  expect settings_datum: SettingsDatum = settings_datum
  settings_datum
}
