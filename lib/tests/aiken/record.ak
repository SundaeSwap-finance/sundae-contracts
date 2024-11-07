use aiken/interval
use calculation/record.{check_record}
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets
use cardano/transaction.{InlineDatum, NoDatum, Output}
use shared
use sundae/multisig
use types/oracle.{OracleDatum} as types_oracle
use types/order.{Fixed}

test record() {
  let oracle_policy_id =
    #"00000000000000000000000000000000000000000000000000000000"
  let oracle_name = shared.oracle_sft_name()
  let addr =
    Address(
      Script(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"),
      None,
    )
  let input_value = assets.from_lovelace(4_500_000)
  let destination = Fixed { address: addr, datum: NoDatum }
  let input =
    Output {
      address: addr,
      value: input_value,
      datum: InlineDatum(Void),
      reference_script: None,
    }
  let output =
    Output {
      address: addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(oracle_policy_id, oracle_name, 1),
      datum: InlineDatum(
        OracleDatum {
          owner: multisig.AnyOf([]),
          valid_range: interval.between(0, 1),
          pool_ident: #"00",
          reserve_a: (#"00", #"00", 1_000_000),
          reserve_b: (#"00", #"00", 1_000_000),
          circulating_lp: (#"00", #"00", 1_000_000),
        },
      ),
      reference_script: None,
    }
  check_record(
    input,
    destination,
    2_500_000,
    output,
    (oracle_policy_id, oracle_name),
  )
}

test record_must_pay_to_destination() fail {
  let oracle_policy_id =
    #"00000000000000000000000000000000000000000000000000000000"
  let oracle_name = shared.oracle_sft_name()
  let addr =
    Address(
      Script(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"),
      None,
    )
  let input_value = assets.from_lovelace(4_500_000)
  let destination = Fixed { address: addr, datum: NoDatum }
  let input =
    Output {
      address: addr,
      value: input_value,
      datum: InlineDatum(Void),
      reference_script: None,
    }
  let output =
    Output {
      address: Address(VerificationKey(#"00"), None),
      value: assets.from_lovelace(2_000_000)
        |> assets.add(oracle_policy_id, oracle_name, 1),
      datum: InlineDatum(
        OracleDatum {
          owner: multisig.AnyOf([]),
          valid_range: interval.between(0, 1),
          pool_ident: #"00",
          reserve_a: (#"00", #"00", 1_000_000),
          reserve_b: (#"00", #"00", 1_000_000),
          circulating_lp: (#"00", #"00", 1_000_000),
        },
      ),
      reference_script: None,
    }
  check_record(
    input,
    destination,
    2_500_000,
    output,
    (oracle_policy_id, oracle_name),
  )
}

test record_must_have_token() fail {
  let oracle_policy_id =
    #"00000000000000000000000000000000000000000000000000000000"
  let oracle_name = shared.oracle_sft_name()
  let addr =
    Address(
      Script(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"),
      None,
    )
  let input_value = assets.from_lovelace(4_500_000)
  let destination = Fixed { address: addr, datum: NoDatum }
  let input =
    Output {
      address: addr,
      value: input_value,
      datum: InlineDatum(Void),
      reference_script: None,
    }
  let output =
    Output {
      address: addr,
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(
        OracleDatum {
          owner: multisig.AnyOf([]),
          valid_range: interval.between(0, 1),
          pool_ident: #"00",
          reserve_a: (#"00", #"00", 1_000_000),
          reserve_b: (#"00", #"00", 1_000_000),
          circulating_lp: (#"00", #"00", 1_000_000),
        },
      ),
      reference_script: None,
    }
  check_record(
    input,
    destination,
    2_500_000,
    output,
    (oracle_policy_id, oracle_name),
  )
}

test record_bad_datum() fail {
  let oracle_policy_id =
    #"00000000000000000000000000000000000000000000000000000000"
  let oracle_name = shared.oracle_sft_name()
  let addr =
    Address(
      Script(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"),
      None,
    )
  let input_value =
    assets.from_lovelace(4_500_000)
      |> assets.add(oracle_policy_id, oracle_name, 1)
  let destination = Fixed { address: addr, datum: NoDatum }
  let input =
    Output {
      address: addr,
      value: input_value,
      datum: InlineDatum(Void),
      reference_script: None,
    }
  let output =
    Output {
      address: addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(oracle_policy_id, oracle_name, 1),
      datum: InlineDatum(
        OracleDatum {
          owner: multisig.AnyOf([]),
          valid_range: interval.between(0, 1),
          pool_ident: #"00",
          reserve_a: (#"00", #"00", 1_000_000),
          reserve_b: (#"00", #"00", 1_000_000),
          circulating_lp: (#"00", #"00", 1_000_000),
        },
      ),
      reference_script: None,
    }
  check_record(
    input,
    destination,
    2_500_000,
    output,
    (oracle_policy_id, oracle_name),
  )
}
