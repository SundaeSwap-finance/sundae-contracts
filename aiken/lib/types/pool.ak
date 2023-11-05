use aiken/bytearray
use aiken/cbor
use aiken/time.{PosixTime}
use shared.{AssetClass, Ident}

pub type PoolDatum {
  identifier: Ident,
  assets: (AssetClass, AssetClass),
  circulating_lp: Int,
  fees_per_10_thousand: (Int, Int),
  market_open: PosixTime,
  fee_finalized: PosixTime,
  protocol_fees: Int,
}

test example_pool_datum() {
  trace bytearray.to_hex(
    cbor.serialise(
      PoolDatum {
        identifier: "123",
        assets: (
          (#"", #""),
          (
            #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77",
            #"53554e444145",
          ),
        ),
        circulating_lp: 20229488080013,
        fees_per_10_thousand: (2000, 500),
        market_open: 100,
        fee_finalized: 1000,
        protocol_fees: 10000000,
      },
    ),
  )
  True
}

pub type PoolRedeemer {
  PoolScoop {
    signatory_index: Int,
    scooper_index: Int,
    input_order: List<Int>,
  }
  WithdrawFees {
    amount: Int,
    treasury_output: Int,
  }
}

test example_pool_scoop_redeemer() {
  trace bytearray.to_hex(
    cbor.serialise(
      PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [1] },
    ),
  )
  True
}

test example_pool_withdraw_fees_redeemer() {
  trace bytearray.to_hex(
    cbor.serialise(
      WithdrawFees { amount: 100, treasury_output: 1 },
    ),
  )
  True
}

pub type PoolMintRedeemer {
  MintLP { identifier: Ident }
  CreatePool { assets: (AssetClass, AssetClass), pool_output: Int, metadata_output: Int }
}

test example_pool_mint_redeemer() {
  trace bytearray.to_hex(
    cbor.serialise(
      CreatePool {
        assets: (
          (#"", #""),
          (
            #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77",
            #"53554e444145",
          ),
        ),
        pool_output: 0,
        metadata_output: 1,
      },
    ),
  )
  True
}
