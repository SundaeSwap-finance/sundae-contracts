use aiken/transaction.{Output}
use aiken/transaction/value.{ada_policy_id, ada_asset_name, PolicyId, AssetName}
use types/order.{Destination, Fixed, Self}

pub fn check_record(
  input: Output,
  destination: Destination,
  actual_protocol_fee: Int,
  output: Output,
  policy: (PolicyId, AssetName),
) -> Bool {
  // Make sure all of the funds from the input make it into the oracle output
  // Note that the accuracy of the oracle is handled by the mint policy, since it has easier
  // access to the final state of the order
  let remainder = input.value
    |> value.add(ada_policy_id, ada_asset_name, -actual_protocol_fee)
    |> value.add(policy.1st, policy.2nd, 1)

  and {
    output.value == remainder,
    when destination is {
      // The datum of the destination can be used by the oracle minting policy
      // but we don't check it here
      // E.g. for a standard sundae oracle, the datum encodes the owner, and
      // then the oracle minting policy checks that the scooper minted an oracle
      // with the correct owner
      Fixed(address, _) -> output.address == address
      // It doesn't make sense for an oracle to chain into an oracle
      Self -> False
    }
  }
}
