use aiken/time.{PosixTime}
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
  input_order: List<Int>,
}

pub type PoolMintRedeemer {
  CreatePool { assets: (AssetClass, AssetClass) }
  MintLP { identifier: Ident }
}
