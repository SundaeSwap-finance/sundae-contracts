use aiken/interval
use cardano/address.{Address, Credential, Script, VerificationKey}
use cardano/assets
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, Transaction}
use mocktail/virgin_address.{mock_script_address}
use mocktail/virgin_output_reference.{mock_utxo_ref}
use order as order_validator
use sundae/multisig
use tests/fixtures/constants.{other_hash, payment_key, random_hash, stake_script_hash}
use types/order.{Cancel, Destination, OrderDatum, Scoop, Swap}

fn order_address() {
  mock_script_address(0, None)
}

fn scoop_tx(withdrawals: Pairs<Credential, Int>) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [
      Input {
        output_reference: mock_utxo_ref(0, 1),
        output: Output {
          address: order_address(),
          value: assets.from_lovelace(2_000_000),
          datum: InlineDatum(Void),
          reference_script: None,
        },
      },
    ],
    fee: 1_000_000,
    withdrawals: withdrawals,
    validity_range: interval.between(1, 2),
    id: mock_utxo_ref(0, 1).transaction_id,
  }
}

fn run_scoop(withdrawals: Pairs<Credential, Int>) -> Bool {
  let order_datum: Data = Void
  order_validator.order.spend(
    stake_script_hash,
    Some(order_datum),
    Scoop,
    mock_utxo_ref(0, 1),
    scoop_tx(withdrawals),
  )
}

// Scoop tests

test scoop_order_success() {
  run_scoop([Pair(Script(stake_script_hash), 0)])
}

test scoop_order_with_extra_withdrawals() {
  run_scoop([
    Pair(Script(random_hash), 100),
    Pair(Script(other_hash), 500),
    Pair(Script(stake_script_hash), 0),
  ])
}

test scoop_order_missing_stake_withdrawal_fails() fail {
  run_scoop([Pair(Script(random_hash), 100), Pair(Script(other_hash), 500)])
}

/// Test scoop with VerificationKey withdrawals (covers the _ -> acc branch)
test scoop_order_with_verification_key_withdrawals() {
  run_scoop([
    Pair(VerificationKey(random_hash), 100),
    Pair(VerificationKey(other_hash), 500),
    Pair(Script(stake_script_hash), 0),
  ])
}

// Cancel tests

fn order_datum(owner: multisig.MultisigScript) -> OrderDatum {
  OrderDatum {
    pool_ident: None,
    owner: owner,
    max_protocol_fee: 2_500_000,
    destination: Destination.Fixed {
      address: Address(VerificationKey(payment_key), None),
      datum: NoDatum,
    },
    details: Swap {
      offer: (#"", #"", 1_000_000),
      min_received: (#"aabb", #"abcd", 100),
    },
    extension: Void,
  }
}

fn cancel_tx(
  signatories: List<ByteArray>,
  validity: interval.Interval<Int>,
  withdrawals: Pairs<Credential, Int>,
) -> Transaction {
  Transaction {
    ..transaction.placeholder,
    inputs: [
      Input {
        output_reference: mock_utxo_ref(0, 1),
        output: Output {
          address: order_address(),
          value: assets.from_lovelace(5_000_000),
          datum: InlineDatum(Void),
          reference_script: None,
        },
      },
    ],
    fee: 1_000_000,
    extra_signatories: signatories,
    validity_range: validity,
    withdrawals: withdrawals,
    id: mock_utxo_ref(0, 1).transaction_id,
  }
}

fn run_cancel(
  owner: multisig.MultisigScript,
  signatories: List<ByteArray>,
  validity: interval.Interval<Int>,
  withdrawals: Pairs<Credential, Int>,
) -> Bool {
  let datum: Data = order_datum(owner)
  order_validator.order.spend(
    stake_script_hash,
    Some(datum),
    Cancel,
    mock_utxo_ref(0, 1),
    cancel_tx(signatories, validity, withdrawals),
  )
}

test cancel_order_with_signature_success() {
  let owner = multisig.Signature { key_hash: payment_key }
  run_cancel(owner, [payment_key], interval.between(1, 10), [])
}

test cancel_order_missing_signature_fails() fail {
  let owner = multisig.Signature { key_hash: payment_key }
  run_cancel(owner, [other_hash], interval.between(1, 10), [])
}

test cancel_order_multisig_allof_success() {
  let owner =
    multisig.AllOf {
      scripts: [
        multisig.Signature { key_hash: payment_key },
        multisig.Signature { key_hash: other_hash },
      ],
    }
  run_cancel(owner, [payment_key, other_hash], interval.between(1, 10), [])
}

test cancel_order_multisig_allof_partial_fails() fail {
  let owner =
    multisig.AllOf {
      scripts: [
        multisig.Signature { key_hash: payment_key },
        multisig.Signature { key_hash: other_hash },
      ],
    }
  run_cancel(owner, [payment_key], interval.between(1, 10), [])
}

test cancel_order_multisig_anyof_success() {
  let owner =
    multisig.AnyOf {
      scripts: [
        multisig.Signature { key_hash: payment_key },
        multisig.Signature { key_hash: other_hash },
      ],
    }
  run_cancel(owner, [other_hash], interval.between(1, 10), [])
}

test cancel_order_with_script_owner_success() {
  let owner = multisig.Script { script_hash: random_hash }
  run_cancel(owner, [], interval.between(1, 10), [Pair(Script(random_hash), 0)])
}

test cancel_order_script_owner_missing_withdrawal_fails() fail {
  let owner = multisig.Script { script_hash: random_hash }
  run_cancel(owner, [], interval.between(1, 10), [Pair(Script(other_hash), 0)])
}

test cancel_order_no_datum_fails() fail {
  order_validator.order.spend(
    stake_script_hash,
    None,
    Cancel,
    mock_utxo_ref(0, 1),
    cancel_tx([payment_key], interval.between(1, 10), []),
  )
}

test cancel_order_before_time_success() {
  let owner = multisig.Before { time: 100 }
  run_cancel(owner, [], interval.between(1, 50), [])
}

test cancel_order_after_time_not_met_fails() fail {
  let owner = multisig.After { time: 100 }
  run_cancel(owner, [], interval.between(1, 50), [])
}
