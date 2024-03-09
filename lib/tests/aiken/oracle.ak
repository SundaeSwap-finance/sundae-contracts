
use aiken/transaction.{NoDatum, InlineDatum, Output}
use aiken/transaction/credential.{Address, ScriptCredential}
use aiken/transaction/value
use calculation/oracle.{check_oracle}
use types/order.{Fixed}

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
  let destination = Fixed {
        address: addr,
        datum: NoDatum,
    }
  let input = Output {
    address: addr,
    value: input_value,
    datum: InlineDatum(Void),
    reference_script: None,
  }
  let output = Output {
    address: addr,
    value: value.from_lovelace(2_000_000),
    datum: NoDatum,
    reference_script: None,
  }
  check_oracle(input, destination, 2_500_000, output)
}

test oracle_bad_datum() fail {
  let addr =
    Address(
      ScriptCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let input_value =
    value.from_lovelace(4_500_000)
  let destination = Fixed {
      address: addr,
      datum: NoDatum,
    }
  let input = Output {
    address: addr,
    value: input_value,
    datum: InlineDatum(Void),
    reference_script: None,
  }
  let output = Output {
    address: addr,
    value: value.from_lovelace(2_000_000),
    datum: InlineDatum(Void),
    reference_script: None,
  }
  check_oracle(input, destination, 2_500_000, output)
}
