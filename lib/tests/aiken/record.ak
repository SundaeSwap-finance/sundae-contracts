/// Oracle record calculation tests - rewritten using test fixtures

use aiken/interval
use calculation/record.{check_record}
use cardano/address.{Address}
use cardano/assets.{add}
use cardano/transaction.{InlineDatum, NoDatum, Output}
use mocktail/virgin_address.{mock_pub_key_address, mock_script_address}
use shared
use sundae/multisig
use tests/fixtures/values.{lovelace}
use types/oracle.{OracleDatum}
use types/order.{Fixed, Self}

// ============================================
// Test Fixtures
// ============================================

const protocol_fee = 2_500_000

const oracle_policy_id =
  #"00000000000000000000000000000000000000000000000000000000"

fn oracle_name() -> ByteArray {
  shared.oracle_sft_name()
}

/// Script address for order/oracle destination
fn script_dest() -> Address {
  mock_script_address(0, None)
}

/// Default oracle datum for tests
fn test_oracle_datum() -> OracleDatum {
  OracleDatum {
    owner: multisig.AnyOf([]),
    valid_range: interval.between(0, 1),
    pool_ident: #"00",
    reserve_a: (#"00", #"00", 1_000_000),
    reserve_b: (#"00", #"00", 1_000_000),
    circulating_lp: (#"00", #"00", 1_000_000),
  }
}

// ============================================
// Tests
// ============================================

/// Test successful oracle record creation
test record_success() {
  let addr = script_dest()
  let destination = Fixed { address: addr, datum: NoDatum }

  let input =
    Output {
      address: addr,
      value: lovelace(4_500_000),
      datum: InlineDatum(Void),
      reference_script: None,
    }

  let output =
    Output {
      address: addr,
      value: lovelace(2_000_000) |> add(oracle_policy_id, oracle_name(), 1),
      datum: InlineDatum(test_oracle_datum()),
      reference_script: None,
    }

  check_record(
    input,
    destination,
    protocol_fee,
    output,
    (oracle_policy_id, oracle_name()),
  )
}

/// Test record fails when output goes to wrong destination
test record_wrong_destination_fails() fail {
  let addr = script_dest()
  let wrong_addr = mock_pub_key_address(99, None)
  let destination = Fixed { address: addr, datum: NoDatum }

  let input =
    Output {
      address: addr,
      value: lovelace(4_500_000),
      datum: InlineDatum(Void),
      reference_script: None,
    }

  // Output goes to wrong address
  let output =
    Output {
      address: wrong_addr,
      value: lovelace(2_000_000) |> add(oracle_policy_id, oracle_name(), 1),
      datum: InlineDatum(test_oracle_datum()),
      reference_script: None,
    }

  check_record(
    input,
    destination,
    protocol_fee,
    output,
    (oracle_policy_id, oracle_name()),
  )
}

/// Test record fails when oracle token is missing
test record_missing_token_fails() fail {
  let addr = script_dest()
  let destination = Fixed { address: addr, datum: NoDatum }

  let input =
    Output {
      address: addr,
      value: lovelace(4_500_000),
      datum: InlineDatum(Void),
      reference_script: None,
    }

  // Output missing oracle token
  let output =
    Output {
      address: addr,
      value: lovelace(2_000_000),
      datum: InlineDatum(test_oracle_datum()),
      reference_script: None,
    }

  check_record(
    input,
    destination,
    protocol_fee,
    output,
    (oracle_policy_id, oracle_name()),
  )
}

/// Test record fails when input already has oracle token (bad datum)
test record_input_has_token_fails() fail {
  let addr = script_dest()
  let destination = Fixed { address: addr, datum: NoDatum }

  // Input already has the oracle token - this shouldn't be allowed
  let input =
    Output {
      address: addr,
      value: lovelace(4_500_000) |> add(oracle_policy_id, oracle_name(), 1),
      datum: InlineDatum(Void),
      reference_script: None,
    }

  let output =
    Output {
      address: addr,
      value: lovelace(2_000_000) |> add(oracle_policy_id, oracle_name(), 1),
      datum: InlineDatum(test_oracle_datum()),
      reference_script: None,
    }

  check_record(
    input,
    destination,
    protocol_fee,
    output,
    (oracle_policy_id, oracle_name()),
  )
}

/// Test record fails with Self destination (oracles shouldn't chain into themselves)
test record_self_destination_fails() fail {
  let addr = script_dest()

  let input =
    Output {
      address: addr,
      value: lovelace(4_500_000),
      datum: InlineDatum(Void),
      reference_script: None,
    }

  // Even with correct value, Self destination should fail
  let output =
    Output {
      address: addr,
      value: lovelace(2_000_000) |> add(oracle_policy_id, oracle_name(), 1),
      datum: InlineDatum(test_oracle_datum()),
      reference_script: None,
    }

  check_record(
    input,
    Self,
    protocol_fee,
    output,
    (oracle_policy_id, oracle_name()),
  )
}
