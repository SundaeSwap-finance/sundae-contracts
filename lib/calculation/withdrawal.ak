use aiken/option
use aiken/math
use aiken/transaction.{NoDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{Value, ada_policy_id, ada_asset_name}
use calculation/shared.{PoolState} as calc_shared
use sundae/multisig
use shared.{SingletonValue}
use types/order.{Destination, OrderDatum}

pub fn do_withdrawal(
  pool_state: PoolState,
  input_value: Value,
  amount: SingletonValue,
  destination: Destination,
  actual_protocol_fee: Int,
  output: Output,
) -> PoolState {
  let (lp_policy, lp_asset_name, amount) = amount

  expect lp_policy == pool_state.quantity_lp.1st
  expect lp_asset_name == pool_state.quantity_lp.2nd
  let withdrawn =
    math.min(amount, value.quantity_of(input_value, lp_policy, lp_asset_name))
  let withdrawn_a =
    withdrawn * pool_state.quantity_a.3rd / pool_state.quantity_lp.3rd
  let withdrawn_b =
    withdrawn * pool_state.quantity_b.3rd / pool_state.quantity_lp.3rd
  let remainder =
    input_value
      |> value.add(lp_policy, lp_asset_name, -withdrawn)
      |> value.add(ada_policy_id, ada_asset_name, -actual_protocol_fee)
      |> value.add(
          pool_state.quantity_a.1st,
          pool_state.quantity_a.2nd,
          withdrawn_a,
        )
      |> value.add(
          pool_state.quantity_b.1st,
          pool_state.quantity_b.2nd,
          withdrawn_b,
        )

  // Check that the payout is satisfied by the next output
  expect output.address == destination.address
  expect output.datum == destination.datum
  expect output.value == remainder
  PoolState {
    quantity_a: (
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd,
      pool_state.quantity_a.3rd - withdrawn_a,
    ),
    quantity_b: (
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd,
      pool_state.quantity_b.3rd - withdrawn_b,
    ),
    quantity_lp: (
      pool_state.quantity_lp.1st,
      pool_state.quantity_lp.2nd,
      pool_state.quantity_lp.3rd - withdrawn,
    ),
    fees_per_10_thousand: pool_state.fees_per_10_thousand,
    protocol_fees: pool_state.protocol_fees + actual_protocol_fee,
  }
}

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
    fees_per_10_thousand: 5,
    protocol_fees: 2_000_000,
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
    destination: Destination {
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
  let final_pool_state = do_withdrawal(pool_state, input_value, amount, order.destination, 2_500_000, output)
  expect final_pool_state.quantity_a.3rd == 1_000_000_000
  expect final_pool_state.quantity_b.3rd == 1_000_000_000
  expect final_pool_state.protocol_fees == 4_500_000
  True
}
