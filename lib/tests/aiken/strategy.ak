/// Strategy execution tests - using fixtures framework
///
/// Tests for deferred strategy order execution with validity windows and authorization.

use aiken/cbor
use aiken/interval
use cardano/address.{Credential, Script}
use cardano/transaction.{OutputReference}
use calculation/strategy.{get_strategy}
use tests/fixtures/constants.{other_hash}
use types/order.{
  Order, Script as ScriptAuth, Signature as SignatureAuth,
  SignedStrategyExecution, StrategyAuthorization, StrategyExecution, Swap,
}

// ============================================
// Test Constants
// ============================================

/// Script hash for strategy authorization tests
const test_script = other_hash

/// Different script hash for failure tests
const other_script = #"02020202020202020202020202020202020202020202020202020202"

/// Public key for signature tests (64 bytes for ed25519 extended)
const test_pubkey = #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f993065136af53ff4"

// ============================================
// Test Fixtures
// ============================================

fn order_ref() -> OutputReference {
  OutputReference {
    transaction_id: #"0000000000000000000000000000000000000000000000000000000000000001",
    output_index: 0,
  }
}

fn other_order_ref() -> OutputReference {
  OutputReference {
    transaction_id: #"0000000000000000000000000000000000000000000000000000000000000002",
    output_index: 0,
  }
}

fn swap_details() -> Order {
  Swap {
    offer: (#"", #"", 10_000_000),
    min_received: (#"cafe", #"beef", 9_000_000),
  }
}

fn execution(
  tx_ref: OutputReference,
  validity: interval.Interval<Int>,
) -> StrategyExecution {
  StrategyExecution {
    tx_ref: tx_ref,
    validity_range: validity,
    details: swap_details(),
    extensions: Void,
  }
}

fn run_strategy(
  tx_ref: OutputReference,
  tx_range: interval.Interval<Int>,
  withdrawals: Pairs<Credential, Int>,
  auth: StrategyAuthorization,
  exec: StrategyExecution,
  sig: Option<ByteArray>,
) -> Bool {
  let sse = SignedStrategyExecution { execution: exec, signature: sig }
  get_strategy(tx_ref, tx_range, withdrawals, auth, sse) == swap_details()
}

// Script authorization tests

test strategy_script_auth_success() {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution(order_ref(), interval.between(50, 300)),
    None,
  )
}

test strategy_script_auth_missing_fails() fail {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [],
    ScriptAuth { script: test_script },
    execution(order_ref(), interval.between(50, 300)),
    None,
  )
}

test strategy_script_auth_wrong_script_fails() fail {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [Pair(Script(other_script), 0)],
    ScriptAuth { script: test_script },
    execution(order_ref(), interval.between(50, 300)),
    None,
  )
}

// Replay prevention (tx_ref must match)

test strategy_wrong_tx_ref_fails() fail {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution(other_order_ref(), interval.between(50, 300)),
    None,
  )
}

// Validity range containment tests

test strategy_tx_before_validity_fails() fail {
  run_strategy(
    order_ref(),
    interval.between(50, 200),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution(order_ref(), interval.between(100, 300)),
    None,
  )
}

test strategy_tx_after_validity_fails() fail {
  run_strategy(
    order_ref(),
    interval.between(100, 400),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution(order_ref(), interval.between(50, 300)),
    None,
  )
}

test strategy_exact_validity_match_success() {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution(order_ref(), interval.between(100, 200)),
    None,
  )
}

test strategy_tx_inside_validity_success() {
  run_strategy(
    order_ref(),
    interval.between(150, 250),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution(order_ref(), interval.between(100, 300)),
    None,
  )
}

// Signature authorization tests

test strategy_signature_auth_missing_signature_fails() fail {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [],
    SignatureAuth { signer: test_pubkey },
    execution(order_ref(), interval.between(50, 300)),
    None,
  )
}

test strategy_signature_auth_invalid_signature_fails() fail {
  let fake_sig =
    #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [],
    SignatureAuth { signer: test_pubkey },
    execution(order_ref(), interval.between(50, 300)),
    Some(fake_sig),
  )
}

// Interval edge cases

test strategy_exclusive_tx_validity_success() {
  run_strategy(
    order_ref(),
    interval.entirely_between(99, 201),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution(order_ref(), interval.between(50, 300)),
    None,
  )
}

test strategy_exclusive_tx_validity_too_wide_fails() fail {
  run_strategy(
    order_ref(),
    interval.entirely_between(49, 151),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution(order_ref(), interval.between(100, 200)),
    None,
  )
}

test strategy_tx_after_infinite_fails() fail {
  run_strategy(
    order_ref(),
    interval.after(100),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution(order_ref(), interval.between(50, 300)),
    None,
  )
}

test strategy_tx_before_infinite_fails() fail {
  run_strategy(
    order_ref(),
    interval.before(200),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution(order_ref(), interval.between(50, 300)),
    None,
  )
}

fn execution_with_interval(validity: interval.Interval<Int>) -> StrategyExecution {
  StrategyExecution {
    tx_ref: order_ref(),
    validity_range: validity,
    details: swap_details(),
    extensions: Void,
  }
}

test strategy_exclusive_strategy_bounds_success() {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution_with_interval(interval.entirely_between(49, 301)),
    None,
  )
}

test strategy_exclusive_strategy_bounds_too_tight_fails() fail {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution_with_interval(interval.entirely_between(100, 200)),
    None,
  )
}

test strategy_infinite_lower_bound_success() {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution_with_interval(interval.before(300)),
    None,
  )
}

test strategy_infinite_upper_bound_success() {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution_with_interval(interval.after(50)),
    None,
  )
}

test strategy_everything_interval_success() {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [Pair(Script(test_script), 0)],
    ScriptAuth { script: test_script },
    execution_with_interval(interval.everything),
    None,
  )
}

// Valid signature test vector

test print_strategy_cbor() {
  let exec = execution(order_ref(), interval.between(50, 300))
  trace cbor.diagnostic(cbor.serialise(exec))
  True
}

const sig_pubkey = #"8a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c"

const sig_signature = #"cafa9126b7b01dc5a44ed97fe0145b4c04855ca06bd33001abc70f649c0d4fb8b846d394c9e603955a56ce33d22b1ca6b462426a907d205f1d5c23f36b1c0308"

test strategy_signature_auth_valid_signature_success() {
  run_strategy(
    order_ref(),
    interval.between(100, 200),
    [],
    SignatureAuth { signer: sig_pubkey },
    execution(order_ref(), interval.between(50, 300)),
    Some(sig_signature),
  )
}
