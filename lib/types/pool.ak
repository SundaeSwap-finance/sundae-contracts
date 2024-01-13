use aiken/time.{PosixTime}
use shared.{AssetClass, Ident}
use types/order.{SignedStrategyExecution}

pub type PoolDatum {
  identifier: Ident,
  assets: (AssetClass, AssetClass),
  circulating_lp: Int,
  fees_per_10_thousand: (Int, Int),
  market_open: PosixTime,
  fee_finalized: PosixTime,
  protocol_fees: Int,
}

pub type PoolRedeemer {
  PoolScoop {
    signatory_index: Int,
    scooper_index: Int,
    // TODO: remove this, and update all our off-chain tx building code
    amortized_base_fee: Int,
    input_order: List<(Int, Option<SignedStrategyExecution>)>,
  }
  WithdrawFees {
    amount: Int,
    treasury_output: Int,
  }
}


pub type PoolMintRedeemer {
  MintLP { identifier: Ident }
  CreatePool { assets: (AssetClass, AssetClass), pool_output: Int, metadata_output: Int }
}
