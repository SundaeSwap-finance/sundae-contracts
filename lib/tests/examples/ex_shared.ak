use aiken/bytearray
use aiken/transaction.{OutputReference, TransactionId}
use aiken/transaction/credential.{Address, ScriptCredential, VerificationKeyCredential}

pub fn mk_tx_hash(n: Int) -> TransactionId {
  expect n < 256
  let h =
    bytearray.push(
      #"00000000000000000000000000000000000000000000000000000000000000",
      n,
    )
  TransactionId { hash: h }
}

pub fn mk_output_reference(n: Int) -> OutputReference {
  OutputReference { transaction_id: mk_tx_hash(0), output_index: n }
}

pub fn script_address(hash: ByteArray) -> Address {
  Address { payment_credential: ScriptCredential(hash), stake_credential: None }
}

pub fn wallet_address(hash: ByteArray) -> Address {
  Address(VerificationKeyCredential(hash), None)
}