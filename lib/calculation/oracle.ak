use aiken/transaction.{Output}
use aiken/transaction/value.{ada_policy_id, ada_asset_name}
use types/order.{Destination, Fixed, Self}

pub fn check_oracle(
  input: Output,
  destination: Destination,
  actual_protocol_fee: Int,
  output: Output,
) -> Bool {
  // Make sure all of the funds from the input make it into the oracle output
  // Note that the accuracy of the oracle is handled by the mint policy, since it has easier
  // access to the final state of the order
  // TODO: this needs the token minted
  let remainder = value.add(input.value, ada_policy_id, ada_asset_name, -actual_protocol_fee)

  and {
    output.value == remainder,
    when destination is {
      Fixed(address, datum) -> and {
        output.address == address,
        output.datum == datum,
      }
      Self -> and {
        output.address == input.address,
        output.datum == input.datum,
      }
    }
  }
}