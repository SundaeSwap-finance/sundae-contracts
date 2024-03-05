use aiken/cbor
use types/order.{Destination, Fixed, Self, OrderDatum, Swap, Deposit, Withdrawal, Scoop, Cancel}
use aiken/transaction.{NoDatum}
use sundae/multisig
use tests/examples/ex_shared.{print_example, wallet_address}

fn mk_swap() -> OrderDatum {
  let addr = wallet_address(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513")
  let dest = Fixed { address: addr, datum: NoDatum }
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
  let dest = Fixed { address: addr, datum: NoDatum }
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
  let dest = Fixed { address: addr, datum: NoDatum }
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

test example_cancel_redeemer() {
  print_example(cbor.serialise(Cancel))
}
test example_scoop_redeemer() {
  print_example(cbor.serialise(Scoop))
}
