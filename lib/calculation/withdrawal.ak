use aiken/option
use aiken/math
use aiken/transaction.{NoDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{Value, PolicyId, AssetName, ada_policy_id, ada_asset_name}
use sundae/multisig
use types/order.{Destination, OrderDatum}

pub fn do_withdrawal(
  // pool_state: PoolState,
  asset_a_policy: PolicyId,
  asset_a_name: AssetName,
  asset_b_policy: PolicyId,
  asset_b_name: AssetName,
  pool_lp_policy: PolicyId,
  pool_lp_name: AssetName,
  asset_a_reserve: Int,
  asset_b_reserve: Int,
  circulating_lp: Int,

  input_value: Value,
  order: OrderDatum,
  actual_protocol_fee: Int,
  output: Output,
) -> (
  // PoolState
  Int, // new_asset_a_reserve
  Int, // new_asset_b_reserve
  Int, // new_circulating_lp
) {
  expect order.Withdrawal { amount } = order.details
  let (lp_policy, lp_asset_name, amount) = amount

  expect lp_policy == pool_lp_policy
  expect lp_asset_name == pool_lp_name
  let withdrawn =
    math.min(amount, value.quantity_of(input_value, lp_policy, lp_asset_name))
  let withdrawn_a = withdrawn * asset_a_reserve / circulating_lp
  let withdrawn_b = withdrawn * asset_b_reserve / circulating_lp
  let remainder =
    input_value
      |> value.add(lp_policy, lp_asset_name, -withdrawn)
      |> value.add(ada_policy_id, ada_asset_name, -actual_protocol_fee)
      |> value.add(
          asset_a_policy,
          asset_a_name,
          withdrawn_a,
        )
      |> value.add(
          asset_b_policy,
          asset_b_name,
          withdrawn_b,
        )

  // Check that the payout is satisfied by the next output
  expect output.address == order.destination.address
  expect output.datum == order.destination.datum
  expect output.value == remainder
  (
    asset_a_reserve - withdrawn_a,
    asset_b_reserve - withdrawn_b,
    circulating_lp - withdrawn,
  )
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
  let input_value =
    value.from_lovelace(4_500_000)
      |> value.add(lp.1st, lp.2nd, 10_000_000)
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
      amount: (lp.1st, lp.2nd, 10_000_000),
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
  let (new_reserve_a, new_reserve_b, new_lp) = do_withdrawal(
    #"",
    #"",
    rberry.1st,
    rberry.2nd,
    lp.1st,
    lp.2nd,
    1_010_000_000,
    1_010_000_000,
    1_010_000_000,
    input_value,
    order,
    2_500_000,
    output
  )
  expect new_reserve_a == 1_000_000_000
  expect new_reserve_b == 1_000_000_000
  expect new_lp == 1_000_000_000
  True
}
