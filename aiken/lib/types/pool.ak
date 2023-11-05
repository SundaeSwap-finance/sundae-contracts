use aiken/bytearray
use aiken/cbor
use aiken/time.{PosixTime}
use aiken/transaction/value.{AssetName, PolicyId}
use shared.{AssetClass, Ident}

pub type PoolDatum {
  identifier: Ident,
  assets: (AssetClass, AssetClass),
  circulating_lp: Int,
  fees_per_10_thousand: Int,
  market_open: PosixTime,
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
        fees_per_10_thousand: 3,
        market_open: 100,
        protocol_fees: 10000000,
      },
    ),
  )
  True
}

pub type PoolRedeemer {
  signatory_index: Int,
  scooper_index: Int,
  input_order: List<Int>,
}

test example_pool_redeemer() {
  trace bytearray.to_hex(
    cbor.serialise(
      PoolRedeemer { signatory_index: 0, scooper_index: 0, input_order: [1] },
    ),
  )
  True
}

pub type PoolMintRedeemer {
  MintLP { identifier: Ident }
  CreatePool { assets: (AssetClass, AssetClass), pool_output: Int }
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
      },
    ),
  )
  True
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
