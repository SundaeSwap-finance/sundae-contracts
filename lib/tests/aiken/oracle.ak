
use aiken/transaction.{NoDatum, InlineDatum, Output}
use aiken/transaction/credential.{Address, ScriptCredential}
use aiken/transaction/value
use calculation/oracle.{check_oracle}
use types/order.{OrderDatum, Destination}
use sundae/multisig

test oracle() {
  let addr =
    Address(
      ScriptCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let input_value =
    value.from_lovelace(4_500_000)
  let order = OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2_500_000,
    destination: Destination {
      address: addr,
      datum: NoDatum,
    },
    details: order.Oracle,
    extension: Void,
  }
  let output = Output {
    address: addr,
    value: value.from_lovelace(2_000_000),
    datum: NoDatum,
    reference_script: None,
  }
  let ok = check_oracle(input_value, order, 2_500_000, output)
  ok
}

test oracle_bad_datum() {
  let addr =
    Address(
      ScriptCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let input_value =
    value.from_lovelace(4_500_000)
  let order = OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2_500_000,
    destination: Destination {
      address: addr,
      datum: NoDatum,
    },
    details: order.Oracle,
    extension: Void,
  }
  let output = Output {
    address: addr,
    value: value.from_lovelace(2_000_000),
    datum: InlineDatum(Void),
    reference_script: None,
  }
  let ok = check_oracle(input_value, order, 2_500_000, output)
  !ok
}
