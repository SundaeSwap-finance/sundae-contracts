use aiken/cbor
use types/order.{Destination, OrderDatum, Swap, Deposit, Withdrawal, Strategy, StrategyExecution, SignedStrategyExecution, Signature, Scoop, Cancel}
use aiken/transaction.{NoDatum, OutputReference, TransactionId}
use aiken/interval
use sundae/multisig
use tests/examples/ex_shared.{print_example, wallet_address}

fn mk_swap() -> OrderDatum {
  let addr = wallet_address(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513")
  let dest = Destination { address: addr, datum: NoDatum }
  let swap =
    Swap(
      (#"", #"", 10000000),
      (
        #"d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf",
        #"44554d4d59",
        0,
      ),
    )
  OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2500000,
    destination: dest,
    details: swap,
    extension: Void,
  }
}

test example_swap() {
  print_example(mk_swap())
}

fn mk_deposit() -> OrderDatum {
  let addr = wallet_address(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513")
  let dest = Destination { address: addr, datum: NoDatum }
  let deposit =
    Deposit((
      (#"", #"", 10000000),
      (
        #"d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf",
        #"44554d4d59",
        10000000,
      ),
    ))
  OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2500000,
    destination: dest,
    details: deposit,
    extension: Void,
  }
}

test example_deposit() {
  print_example(mk_deposit())
}

fn mk_withdrawal() {
  let addr = wallet_address(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513")
  let dest = Destination { address: addr, datum: NoDatum }
  let withdrawal =
    Withdrawal(
      (
        #"d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf",
        #"6c",
        10000000
      ),
    )
  OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2500000,
    destination: dest,
    details: withdrawal,
    extension: Void,
  }
}

test example_withdrawal() {
  print_example(mk_withdrawal())
}


fn mk_strategy() {
  let addr = wallet_address(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513")
  let dest = Destination { address: addr, datum: NoDatum }
  let strategy = Strategy(Signature(#"d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf"))
  OrderDatum {
    pool_ident: Some(#"fc2c4a6ae8048b0b5affc169dfd496a7ace7d08288c476d9d7a5804e"),
    owner: multisig.Signature(
      #"c279a3fb3b4e62bbc78e288783b58045d4ae82a18867d8352d02775a",
    ),
    max_protocol_fee: 1000000,
    destination: dest,
    details: strategy,
    extension: Void,
  }
}

test example_strategy() {
  print_example(mk_strategy())
}

fn mk_strategy_execution() {
  let tx_ref = OutputReference(TransactionId(#"797831ec63153a84ae1393bd5fea14196684f1dd12d6485e93cfe373d142e0d3"), 1)
  let valid_range = interval.between(1000, 10000)
  StrategyExecution {
    tx_ref: tx_ref,
    validity_range: valid_range,
    details: mk_swap().details,
    extensions: Void,
  }
}

test example_strategy_execution() {
  print_example(mk_strategy_execution())
}

fn mk_signed_strategy_execution() {
  let sig = #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"
  SignedStrategyExecution {
    strategy: mk_strategy_execution(),
    signature: Some(sig),
  }
}

test example_signed_strategy_execution() {
  print_example(mk_signed_strategy_execution())
}

test example_cancel_redeemer() {
  print_example(cbor.serialise(Cancel))
}
test example_scoop_redeemer() {
  print_example(cbor.serialise(Scoop))
}
