use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction/credential.{PoolId, Script, VerificationKey}
use shared.{AssetClass}
use sundae/multisig.{MultisigScript}

pub type SettingsDatum {
  pool_script_hash: Hash<Blake2b_224, Script>,
  pool_currency_symbol: AssetClass,
  settings_admin: MultisigScript,
  rewards_admin: MultisigScript,
  authorized_scoopers: List<VerificationKey>,
  authorized_pools: List<PoolId>,
}
