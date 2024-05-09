use aiken/bytearray
use aiken/cbor
use aiken/int
use aiken/transaction.{
  Mint, OutputReference, ScriptPurpose, Spend, TransactionId, WithdrawFrom,
}
use aiken/transaction/credential.{
  Address, Inline, ScriptCredential, StakeCredential, VerificationKeyCredential,
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

pub fn compare_stake(left: StakeCredential, right: StakeCredential) -> Ordering {
  let left =
    when left is {
      Inline(ScriptCredential(x)) -> x
      Inline(VerificationKeyCredential(x)) -> x
      _ -> fail
    }
  let right =
    when right is {
      Inline(ScriptCredential(x)) -> x
      Inline(VerificationKeyCredential(x)) -> x
      _ -> fail
    }
  bytearray.compare(left, right)
}

pub fn compare_redeemer(left: ScriptPurpose, right: ScriptPurpose) -> Ordering {
  when (left, right) is {
    (Mint(p_a), Mint(p_b)) -> bytearray.compare(p_a, p_b)
    (
      Spend(OutputReference(TransactionId(tx_a), idx_a)),
      Spend(OutputReference(TransactionId(tx_b), idx_b)),
    ) ->
      when bytearray.compare(tx_a, tx_b) is {
        Less -> Less
        Greater -> Greater
        Equal -> int.compare(idx_a, idx_b)
      }
    (WithdrawFrom(stake_a), WithdrawFrom(stake_b)) ->
      compare_stake(stake_a, stake_b)
    (Mint(_), _) -> Less
    (Spend(_), _) -> Less
    (WithdrawFrom(_), _) -> Less
    (_, _) -> Greater
  }
}
