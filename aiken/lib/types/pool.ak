use aiken/bytearray
use aiken/time.{PosixTime}
use aiken/transaction/value.{AssetName, PolicyId}
use shared.{AssetClass, Ident, Rational}

pub type PoolDatum {
  identifier: Ident,
  assets: (AssetClass, AssetClass),
  circulating_lp: Int,
  fees: Rational,
  market_open: PosixTime,
  rewards: Int,
}

pub type PoolRedeemer {
  pool_input: Int,
  settings_input: Int,
  signatory_index: Int,
  scooper_index: Int,
  input_order: List<Int>,
}

pub type PoolMintRedeemer {
  CreatePool { assets: (AssetClass, AssetClass) }
  MintLP { identifier: Ident }
}

pub fn pool_token(policy_id: PolicyId, ident: Ident) -> AssetClass {
  (policy_id, bytearray.concat("p", ident))
}

pub fn pool_token_name(ident: Ident) -> AssetName {
  bytearray.concat("p", ident)
}

pub fn lp_token(policy_id: PolicyId, ident: Ident) -> AssetClass {
  (policy_id, bytearray.concat("l", ident))
}

pub fn lp_token_name(ident: Ident) -> AssetName {
  bytearray.concat("l", ident)
}
