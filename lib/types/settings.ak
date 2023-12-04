use aiken/bytearray
use aiken/cbor
use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction/credential.{Address, Script, VerificationKey, VerificationKeyCredential}
use sundae/multisig.{MultisigScript}
use shared.{Rational}
use aiken/transaction.{
  InlineDatum, Input, Output,
}
use aiken/builtin
use aiken/transaction/value.{AssetName, PolicyId}
use aiken/dict

pub type SettingsDatum {
  pool_script_hash: Hash<Blake2b_224, Script>,
  settings_admin: MultisigScript, // Who can update the updatable settings?
  metadata_admin: Address, // Who or what governs the evolution of CIP-68 token metadata
  treasury_admin: MultisigScript, // Who can withdraw to the treasury and control delegation
  treasury_address: Address, // Where to withdraw treasury funds
  treasury_allowance: Rational, // When withdrawing, what percentage can be released for administrative purposes?
  authorized_scoopers: List<VerificationKey>,
  authorized_staking_keys: List<VerificationKey>,

  base_fee: Int,
  simple_fee: Int,
  strategy_fee: Int,

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

test example_settings_datum() {
  trace bytearray.to_hex(
    cbor.serialise(
      SettingsDatum {
        pool_script_hash: #"596a9815c479a00f894a5a61f64d1f508635dd4e809b29ce1c4afadc",
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
        authorized_scoopers: [
          #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
        ],
        authorized_staking_keys: [
          #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
        ],
        base_fee: 1_000_000,
        simple_fee: 100_000,
        strategy_fee: 200_000,
        extensions: Void,
      },
    ),
  )
  True
}
