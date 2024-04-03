use aiken/dict
use aiken/interval
use aiken/transaction.{
  InlineDatum, Input, Output,
  ScriptContext, Spend, Transaction,
}
use aiken/transaction/credential.{
  Inline, ScriptCredential,
}
use aiken/transaction/value
use tests/examples/ex_shared.{
  mk_output_reference, mk_tx_hash, script_address, compare_stake,
}
use order as order_validator
use tests/constants
use types/order.{Scoop}

test scoop_order() {
  let order_address = script_address(constants.order_script_hash)
  let order_datum = Void // Not needed by scoop
  let order_redeemer = Scoop
  let order_input =
    Input {
      output_reference: mk_output_reference(1),
      output: Output {
        address: order_address,
        value: value.from_lovelace(2_000_000),
        datum: InlineDatum(order_datum),
        reference_script: None,
      },
    }
  let ctx =
    ScriptContext {
      transaction: Transaction {
        inputs: [order_input],
        reference_inputs: [],
        outputs: [],
        fee: value.from_lovelace(1_000_000),
        mint: value.to_minted_value(value.from_lovelace(0)),
        certificates: [],
        withdrawals: dict.from_ascending_list(
          [(Inline(ScriptCredential(constants.stake_script_hash)), 0)],
          compare_stake,
        ),
        validity_range: interval.between(1, 2),
        extra_signatories: [],
        redeemers: dict.new(),
        datums: dict.new(),
        id: mk_tx_hash(1),
      },
      purpose: Spend(order_input.output_reference),
    }
  let result = order_validator.spend(constants.stake_script_hash, order_datum, order_redeemer, ctx)
  result
}
