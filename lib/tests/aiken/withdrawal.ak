use aiken/option
use aiken/transaction.{NoDatum, InlineDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{Value}
use calculation/shared.{PoolState} as calc_shared
use sundae/multisig
use types/order.{Fixed, OrderDatum}
use calculation/withdrawal.{do_withdrawal}

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

!test withdrawal_wrong_output_value() {
  let options = WithdrawalTestOptions {
    ..default_withdrawal_test_options(),
    edit_withdrawal_value: Some(value.from_lovelace(2_000_000))
  }
  withdrawal_test(options)
}

!test withdrawal_wrong_payout_address() {
  let burn_addr =
    Address(
      VerificationKeyCredential(
        #"77777777777777777777777777777777777777777777777777777777",
      ),
      None,
    )
  let options = WithdrawalTestOptions {
    ..default_withdrawal_test_options(),
    edit_withdrawal_destination: Some(burn_addr),
  }
  withdrawal_test(options)
}

fn withdrawal_test(options: WithdrawalTestOptions) {
  let addr =
    Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let rberry = (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  let pool_state = PoolState {
    quantity_a: (#"", #"", 1_010_000_000),
    quantity_b: (rberry.1st, rberry.2nd, 1_010_000_000),
    quantity_lp: (lp.1st, lp.2nd, 1_010_000_000),
  }
  let input_value =
    value.from_lovelace(4_500_000)
      |> value.add(lp.1st, lp.2nd, 10_000_000)
  let amount = (lp.1st, lp.2nd, 10_000_000)
  let order = OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2_500_000,
    destination: Fixed {
      address: addr,
      datum: NoDatum,
    },
    details: order.Withdrawal {
      amount
    },
    extension: Void,
  }
  let output = Output {
    address: option.or_else(options.edit_withdrawal_destination, addr),
    value: option.or_else(
      options.edit_withdrawal_value,
      value.from_lovelace(2_000_000)
        |> value.add(#"", #"", 10_000_000)
        |> value.add(rberry.1st, rberry.2nd, 10_000_000)),
    datum: NoDatum,
    reference_script: None,
  }
  let input = Output {
    address: addr,
    value: input_value,
    datum: InlineDatum(order),
    reference_script: None,
  }
  let final_pool_state = do_withdrawal(pool_state, input, amount, order.destination, 2_500_000, output)
  expect final_pool_state.quantity_a.3rd == 1_000_000_000
  expect final_pool_state.quantity_b.3rd == 1_000_000_000
  True
}
