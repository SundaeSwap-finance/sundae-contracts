use aiken/transaction.{NoDatum, InlineDatum, Output}
use aiken/transaction/credential.{Address, ScriptCredential}
use aiken/transaction/value.{Value, ada_policy_id, ada_asset_name}
use sundae/multisig
use types/order.{Destination, OrderDatum}
use types/oracle.{OracleDatum}

pub fn check_oracle(
  input_value: Value,
  order: OrderDatum,
  actual_protocol_fee: Int,
  output: Output,
) -> Bool {
  // Make sure all of the funds from the input make it into the oracle output
  // Note that the accuracy of the oracle is handled by the mint policy, since it has easier
  // access to the final state of the order
  let remainder = value.add(input_value, ada_policy_id, ada_asset_name, -actual_protocol_fee)

  and {
    output.address == order.destination.address,
    output.datum == order.destination.datum,
    output.value == remainder
  }
}

test oracle() {
  let addr =
    Address(
      ScriptCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let ada = (#"", #"")
  let rberry = (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
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
  let ada = (#"", #"")
  let rberry = (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
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
