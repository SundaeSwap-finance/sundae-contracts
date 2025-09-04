use aiken/crypto.{
  Blake2b_256, Hash, Signature, VerificationKey, VerificationKeyHash,
}
use cardano/transaction.{ValidityRange}
use shared.{Ident}
use types/order.{Destination}

pub type ComplianceToken {
  token: TokenData,
  //A signature from the compliance oracle for serialized TokenData
  oracle_signature: Signature,
}

pub type TokenData {
  // The users DID Identifier
  did: Ident,
  //The users public key
  user_key: VerificationKeyHash,
  //The destination address
  destination: Destination,
  //Blake2b-256 hash of the cbor serialized details from the order
  order_hash: Hash<Blake2b_256, Data>,
  //A valid range
  validity_range: ValidityRange,
  //The public key of the oracle
  oracle_key: VerificationKey,
}

pub type PermissionedDatum {
  whitelisted_oracles: List<VerificationKeyHash>,
}
