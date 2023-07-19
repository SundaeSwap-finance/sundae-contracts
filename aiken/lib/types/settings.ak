use aiken/bytearray
use aiken/cbor
use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction/credential.{Script, VerificationKey}
use aiken/transaction/value.{PolicyId}
use sundae/multisig.{MultisigScript}

pub type SettingsDatum {
  pool_script_hash: Hash<Blake2b_224, Script>,
  pool_policy_id: PolicyId,
  settings_admin: MultisigScript,
  rewards_admin: MultisigScript,
  authorized_scoopers: List<VerificationKey>,
  authorized_staking_keys: List<VerificationKey>,
}

test example_settings_datum() {
  trace bytearray.to_hex(
    cbor.serialise(
      SettingsDatum {
        pool_script_hash: #"596a9815c479a00f894a5a61f64d1f508635dd4e809b29ce1c4afadc",
        pool_policy_id: #"596a9815c479a00f894a5a61f64d1f508635dd4e809b29ce1c4afadc",
        settings_admin: multisig.Signature(
          #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
        ),
        rewards_admin: multisig.Signature(
          #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
        ),
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
