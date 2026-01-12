use aiken/interval
use cardano/address.{Address, Script, VerificationKey}
use cardano/assets.{add, from_lovelace}
use cardano/script_context.{ScriptContext, Spending, Withdrawing}
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, OutputReference, Spend}
use shared.{pool_lp_name, pool_nft_name}
use stake as stake_validator
use tests/fixtures/constants.{
  payment_key, pool_ident, pool_script_hash, stake_script_hash,
}
use types/pool.{DirectScoop, PoolScoop}

fn pool_address() -> Address {
  Address(Script(pool_script_hash), None)
}

fn user_address() -> Address {
  Address(VerificationKey(payment_key), None)
}

fn pool_output_with_nft() -> Output {
  Output {
    address: pool_address(),
    value: from_lovelace(2_000_000)
      |> add(pool_script_hash, pool_nft_name(pool_ident), 1),
    datum: InlineDatum(Void),
    reference_script: None,
  }
}

fn pool_output_with_lp_only() -> Output {
  Output {
    address: pool_address(),
    value: from_lovelace(2_000_000)
      |> add(pool_script_hash, pool_lp_name(pool_ident), 1_000_000),
    datum: InlineDatum(Void),
    reference_script: None,
  }
}

fn output_with_wrong_policy() -> Output {
  let wrong_policy = #"aaaabbbbccccddddeeeeffffaaaabbbbccccddddeeeeffffaaaabbbb"
  Output {
    address: pool_address(),
    value: from_lovelace(2_000_000)
      |> add(wrong_policy, pool_nft_name(pool_ident), 1),
    datum: InlineDatum(Void),
    reference_script: None,
  }
}

fn output_ada_only() -> Output {
  Output {
    address: user_address(),
    value: from_lovelace(2_000_000),
    datum: NoDatum,
    reference_script: None,
  }
}

fn pool_input() -> Input {
  Input {
    output_reference: OutputReference { transaction_id: #"0000", output_index: 0 },
    output: pool_output_with_nft(),
  }
}

fn run_stake(outputs: List<Output>) -> Bool {
  let withdrawals = [Pair(Script(stake_script_hash), 0)]
  let pool_out_ref = OutputReference { transaction_id: #"0000", output_index: 0 }
  let pool_scoop_redeemer: Data = PoolScoop { signatory_index: 0, scooper_index: 0, input_order: [] }
  let tx =
    transaction.Transaction {
      ..transaction.placeholder,
      inputs: [pool_input()],
      outputs: outputs,
      withdrawals: withdrawals,
      redeemers: [Pair(Spend(pool_out_ref), pool_scoop_redeemer)],
      validity_range: interval.between(1, 2),
    }

  let ctx =
    ScriptContext {
      transaction: tx,
      redeemer: Void,
      info: Withdrawing(Script(stake_script_hash)),
    }

  stake_validator.stake.else(pool_script_hash, ctx)
}

// Success cases

test stake_withdraw_with_pool_nft_success() {
  run_stake([pool_output_with_nft()])
}

test stake_withdraw_with_multiple_tokens_success() {
  let output =
    Output {
      address: pool_address(),
      value: from_lovelace(5_000_000)
        |> add(pool_script_hash, pool_nft_name(pool_ident), 1)
        |> add(pool_script_hash, pool_lp_name(pool_ident), 1_000_000),
      datum: InlineDatum(Void),
      reference_script: None,
    }
  run_stake([output])
}

// Failure cases

test stake_withdraw_no_outputs_fails() fail {
  run_stake([])
}

test stake_withdraw_no_pool_tokens_fails() fail {
  run_stake([output_ada_only()])
}

test stake_withdraw_wrong_policy_fails() fail {
  run_stake([output_with_wrong_policy()])
}

test stake_withdraw_lp_only_fails() fail {
  run_stake([pool_output_with_lp_only()])
}

test stake_withdraw_nft_in_second_output_fails() fail {
  run_stake([output_ada_only(), pool_output_with_nft()])
}

test stake_non_withdrawing_context_fails() fail {
  let tx =
    transaction.Transaction {
      ..transaction.placeholder,
      outputs: [pool_output_with_nft()],
      validity_range: interval.between(1, 2),
    }

  let ctx =
    ScriptContext {
      transaction: tx,
      redeemer: Void,
      info: Spending {
        output: OutputReference { transaction_id: #"abcd", output_index: 0 },
        datum: None,
      },
    }

  stake_validator.stake.else(pool_script_hash, ctx)
}

// DirectScoop should be rejected - orders cannot be processed with DirectScoop
// This prevents an attack where orders could be "authorized" but not actually processed
test stake_direct_scoop_rejected() fail {
  let withdrawals = [Pair(Script(stake_script_hash), 0)]
  let pool_out_ref = OutputReference { transaction_id: #"0000", output_index: 0 }
  // Use DirectScoop instead of PoolScoop
  let direct_scoop_redeemer: Data = DirectScoop { signatory_index: 0, scooper_index: 0 }
  let tx =
    transaction.Transaction {
      ..transaction.placeholder,
      inputs: [pool_input()],
      outputs: [pool_output_with_nft()],
      withdrawals: withdrawals,
      redeemers: [Pair(Spend(pool_out_ref), direct_scoop_redeemer)],
      validity_range: interval.between(1, 2),
    }

  let ctx =
    ScriptContext {
      transaction: tx,
      redeemer: Void,
      info: Withdrawing(Script(stake_script_hash)),
    }

  stake_validator.stake.else(pool_script_hash, ctx)
}
