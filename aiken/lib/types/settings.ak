use aiken/bytearray
use aiken/cbor
use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction/credential.{Address, Script, VerificationKey, VerificationKeyCredential}
use sundae/multisig.{MultisigScript}
use shared.{Rational}

pub type SettingsDatum {
  pool_script_hash: Hash<Blake2b_224, Script>,
  settings_admin: MultisigScript, // Who can update the updatable settings?
  treasury_admin: MultisigScript, // Who can withdraw to the treasury
  treasury_address: Address, // Where to withdraw treasury funds
  treasury_allowance: Rational, // When withdrawing, what percentage can be released for administrative purposes?
  authorized_scoopers: List<VerificationKey>,
  authorized_staking_keys: List<VerificationKey>,
}

pub type SettingsRedeemer {
  FactorySetPoolScriptInfo
}

pub type SettingsMintRedeemer {
  BootProtocol
}

test example_settings_datum() {
  trace bytearray.to_hex(
    cbor.serialise(
      SettingsDatum {
        pool_script_hash: #"596a9815c479a00f894a5a61f64d1f508635dd4e809b29ce1c4afadc",
        settings_admin: multisig.Signature(
          #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
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
      },
    ),
  )
  True
}
