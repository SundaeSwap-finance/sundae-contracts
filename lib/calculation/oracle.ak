use aiken/transaction.{Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{Value, ada_policy_id, ada_asset_name}
use types/order.{OrderDatum}

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

  // TODO
  True
}

test oracle() {
  // TODO
  True
}
