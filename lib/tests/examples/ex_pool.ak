use tests/examples/ex_shared.{print_example}
use types/pool.{
  CreatePool, ManageRedeemer, PoolDatum, PoolMintRedeemer, PoolRedeemer,
  PoolScoop, WithdrawFees,
}

fn mk_pool_datum() -> PoolDatum {
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
    bid_fees_per_10_thousand: 2000,
    ask_fees_per_10_thousand: 2000,
    fee_manager: None,
    market_open: 100,
    protocol_fees: 10000000,
  }
}

test example_pool_datum() {
  print_example(mk_pool_datum())
}

fn mk_pool_scoop() -> PoolRedeemer {
  PoolScoop {
    signatory_index: 0,
    scooper_index: 0,
    input_order: [(1, None, 0)],
  }
}

test example_pool_scoop_redeemer() {
  print_example(mk_pool_scoop())
}

pub fn mk_withdraw_fees_redeemer() -> ManageRedeemer {
  WithdrawFees { amount: 100, treasury_output: 1, pool_input: 0 }
}

test example_pool_withdraw_fees_redeemer() {
  print_example(mk_withdraw_fees_redeemer())
}

fn mk_pool_mint_redeemer() -> PoolMintRedeemer {
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
  }
}

test example_pool_mint_redeemer() {
  print_example(mk_pool_mint_redeemer())
}
