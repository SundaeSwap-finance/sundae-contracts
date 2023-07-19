use aiken/bytearray
use aiken/cbor
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
        fees: (3, 1000),
        market_open: 100,
        rewards: 10000000,
      },
    ),
  )
  True
}

pub type PoolRedeemer {
  settings_input: Int,
  signatory_index: Int,
  scooper_index: Int,
  input_order: List<Int>,
}

test example_pool_redeemer() {
  trace bytearray.to_hex(
    cbor.serialise(
      PoolRedeemer {
        settings_input: 0,
        signatory_index: 0,
        scooper_index: 0,
        input_order: [1],
      },
    ),
  )
  True
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
