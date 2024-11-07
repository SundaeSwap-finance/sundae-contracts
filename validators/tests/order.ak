use aiken/interval
use cardano/address.{Credential, Script}
use cardano/assets
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use order as order_validator
use tests/constants
use tests/examples/ex_shared.{mk_output_reference, mk_tx_hash, script_address}
use types/order.{Scoop}

test scoop_order_test() {
  scoop_order([Pair(Script(constants.stake_script_hash), 0)])
}

test scoop_order_extra_withdrawals() {
  scoop_order(
    [
      Pair(Script(constants.random_hash), 100),
      Pair(Script(constants.other_hash), 500),
      Pair(Script(constants.stake_script_hash), 0),
    ],
  )
}

test scoop_order_missing_stake_script_withdrawal() fail {
  scoop_order(
    [
      Pair(Script(constants.random_hash), 100),
      Pair(Script(constants.other_hash), 500),
    ],
  )
}

fn scoop_order(withdrawals: Pairs<Credential, Int>) {
  let order_address = script_address(constants.order_script_hash)
  let order_datum: Data = Void
  // Not needed by scoop
  let order_redeemer = Scoop
  let order_input =
    Input {
      output_reference: mk_output_reference(1),
      output: Output {
        address: order_address,
        value: assets.from_lovelace(2_000_000),
        datum: InlineDatum(order_datum),
        reference_script: None,
      },
    }
  let transaction =
    Transaction {
      ..transaction.placeholder,
      inputs: [order_input],
      fee: 1_000_000,
      withdrawals: withdrawals,
      validity_range: interval.between(1, 2),
      id: mk_tx_hash(1),
    }
  let result =
    order_validator.order.spend(
      constants.stake_script_hash,
      Some(order_datum),
      order_redeemer,
      order_input.output_reference,
      transaction,
    )
  result
}
