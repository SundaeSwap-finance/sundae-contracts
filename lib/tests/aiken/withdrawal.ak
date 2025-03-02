use aiken/option
use calculation/shared.{PoolState} as calc_shared
use calculation/withdrawal.{do_withdrawal}
use cardano/address.{Address, VerificationKey}
use cardano/assets.{Value}
use cardano/transaction.{InlineDatum, NoDatum, Output}
use sundae/multisig
use types/order.{Fixed, OrderDatum}

type WithdrawalTestOptions {
  edit_withdrawal_value: Option<Value>,
  edit_withdrawal_destination: Option<Address>,
}

fn default_withdrawal_test_options() {
  WithdrawalTestOptions {
    edit_withdrawal_value: None,
    edit_withdrawal_destination: None,
  }
}

test withdrawal_ok() {
  let options = default_withdrawal_test_options()
  withdrawal_test(options)
}

test withdrawal_wrong_output_value() fail {
  let options =
    WithdrawalTestOptions {
      ..default_withdrawal_test_options(),
      edit_withdrawal_value: Some(assets.from_lovelace(2_000_000)),
    }
  withdrawal_test(options)
}

test withdrawal_wrong_payout_address() fail {
  let burn_addr =
    Address(
      VerificationKey(
        #"77777777777777777777777777777777777777777777777777777777",
      ),
      None,
    )
  let options =
    WithdrawalTestOptions {
      ..default_withdrawal_test_options(),
      edit_withdrawal_destination: Some(burn_addr),
    }
  withdrawal_test(options)
}

fn withdrawal_test(options: WithdrawalTestOptions) {
  let addr =
    Address(
      VerificationKey(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let rberry =
    (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  let pool_state =
    PoolState {
      quantity_a: (#"", #"", 1_010_000_000),
      quantity_b: (rberry.1st, rberry.2nd, 1_010_000_000),
      quantity_lp: (lp.1st, lp.2nd, 1_010_000_000),
    }
  let input_value =
    assets.from_lovelace(4_500_000)
      |> assets.add(lp.1st, lp.2nd, 10_000_000)
  let amount = (lp.1st, lp.2nd, 10_000_000)
  let order =
    OrderDatum {
      pool_ident: None,
      owner: multisig.Signature(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      max_protocol_fee: 2_500_000,
      destination: Fixed { address: addr, datum: NoDatum },
      details: order.Withdrawal { amount },
      extension: Void,
    }
  let output =
    Output {
      address: option.or_else(options.edit_withdrawal_destination, addr),
      value: option.or_else(
        options.edit_withdrawal_value,
        assets.from_lovelace(2_000_000)
          |> assets.add(#"", #"", 10_000_000)
          |> assets.add(rberry.1st, rberry.2nd, 10_000_000),
      ),
      datum: NoDatum,
      reference_script: None,
    }
  let input =
    Output {
      address: addr,
      value: input_value,
      datum: InlineDatum(order),
      reference_script: None,
    }
  let
    new_a,
    new_b,
    new_lp,
  <-
    do_withdrawal(
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd,
      pool_state.quantity_a.3rd,
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd,
      pool_state.quantity_b.3rd,
      pool_state.quantity_lp.1st,
      pool_state.quantity_lp.2nd,
      pool_state.quantity_lp.3rd,
      0,
      pool_state.quantity_lp.3rd,
      input,
      amount,
      order.destination,
      2_500_000,
      output,
    )
  expect new_a == 1_000_000_000
  expect new_b == 1_000_000_000
  expect new_lp == 1000000000
  True
}
