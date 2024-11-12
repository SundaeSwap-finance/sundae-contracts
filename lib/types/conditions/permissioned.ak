use aiken/crypto.{
  Blake2b_256, Hash, Signature, VerificationKey, VerificationKeyHash,
}
use cardano/address.{Address}
use cardano/transaction.{ValidityRange}
use shared.{Ident}

pub type ComplianceToken {
  // The users DID Identifier
  did: Ident,
  //The users public key
  user_key: VerificationKey,
  //The destination address
  destination: Address,
  //Blake2b-256 hash of the cbor serialized details from the order
  order_hash: Hash<Blake2b_256, Data>,
  //A valid range
  validity_range: ValidityRange,
  //The public key of the oracle
  oracle_key: VerificationKey,
  //A signature from the compliance oracle for fields 1-6
  oracle_signature: Signature,
}

pub type PermissionedDatum {
  whitelisted_oracles: List<VerificationKeyHash>,
}
