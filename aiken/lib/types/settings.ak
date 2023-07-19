use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction/credential.{Script, VerificationKey}
use aiken/transaction/value.{PolicyId}
use sundae/multisig.{MultisigScript}

pub type SettingsDatum {
  pool_script_hash: Hash<Blake2b_224, Script>,
  pool_currency_symbol: PolicyId,
  settings_admin: MultisigScript,
  rewards_admin: MultisigScript,
  authorized_scoopers: List<VerificationKey>,
  authorized_staking_keys: List<VerificationKey>,
}
