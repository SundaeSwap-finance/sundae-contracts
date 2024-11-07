use aiken/cbor
use aiken/primitive/bytearray
use aiken/primitive/int
use cardano/address.{Address, Credential, Script, VerificationKey}
use cardano/transaction.{
  Mint, OutputReference, ScriptPurpose, Spend, TransactionId, Withdraw,
}

pub const examples_enabled: Int = 1

pub fn print_example(d: Data) -> Bool {
  if examples_enabled == 1 {
    trace bytearray.to_hex(cbor.serialise(d))
    True
  } else {
    True
  }
}

pub fn mk_tx_hash(n: Int) -> TransactionId {
  expect n < 256
  bytearray.push(
    #"00000000000000000000000000000000000000000000000000000000000000",
    n,
  )
}

pub fn mk_output_reference(n: Int) -> OutputReference {
  OutputReference { transaction_id: mk_tx_hash(0), output_index: n }
}

pub fn script_address(hash: ByteArray) -> Address {
  Address { payment_credential: Script(hash), stake_credential: None }
}

pub fn wallet_address(hash: ByteArray) -> Address {
  Address(VerificationKey(hash), None)
}

pub fn compare_stake(left: Credential, right: Credential) -> Ordering {
  let left =
    when left is {
      Script(x) -> x
      VerificationKey(x) -> x
    }
  let right =
    when right is {
      Script(x) -> x
      VerificationKey(x) -> x
    }
  bytearray.compare(left, right)
}

pub fn compare_redeemer(left: ScriptPurpose, right: ScriptPurpose) -> Ordering {
  when (left, right) is {
    (Mint(p_a), Mint(p_b)) -> bytearray.compare(p_a, p_b)
    (Spend(OutputReference(tx_a, idx_a)), Spend(OutputReference(tx_b, idx_b))) ->
      when bytearray.compare(tx_a, tx_b) is {
        Less -> Less
        Greater -> Greater
        Equal -> int.compare(idx_a, idx_b)
      }
    (Withdraw(stake_a), Withdraw(stake_b)) -> compare_stake(stake_a, stake_b)
    (Mint(_), _) -> Less
    (Spend(_), _) -> Less
    (Withdraw(_), _) -> Less
    // FIXME: What about new script purposes introcued in Conway (Vote & Propose)?
    (_, _) -> Greater
  }
}
