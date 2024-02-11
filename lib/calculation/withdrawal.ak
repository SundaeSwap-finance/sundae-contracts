use aiken/option
use aiken/math
use aiken/transaction.{NoDatum, InlineDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{Value, ada_policy_id, ada_asset_name}
use calculation/shared.{PoolState} as calc_shared
use sundae/multisig
use shared.{SingletonValue}
use types/order.{Destination, Fixed, Self, OrderDatum}

/// Execute a withdrawal order
///
/// This burns LP tokens, and pays out a portion of the pool in proportion to the percentage
/// of the circulating supply of LP tokens that was burned.
pub fn do_withdrawal(
  pool_state: PoolState,     // The interim pool state against which we should calculate the withdrawal
  input_utxo: Output,        // The incoming UTXO, useful for returning surplus and handling Self destinations
  amount: SingletonValue,    // The amount to actually withdraw
  destination: Destination,  // The destination that the LP tokens (and change) should be sent to
  actual_protocol_fee: Int,  // The protocol fee to deduct
  output: Output,            // The output to compare the order execution against, to ensure the right quantity was paid out
) -> PoolState {
  let (lp_policy, lp_asset_name, amount) = amount
  let Output { address: input_address, datum: input_datum, value: input_value, .. } = input_utxo

  // Make sure we're withdrawing from the right pool;
  // if we were to provide LP tokens for a different pool, you could withdraw funds that weren't yours
  expect lp_policy == pool_state.quantity_lp.1st
  expect lp_asset_name == pool_state.quantity_lp.2nd
  // Calculate the amount of LP tokens to burn; because the outcome of a previous order in the chain
  // may be uncertain, we allow some flexibility here, and the amount in the order just specifies a
  // maximum to take. Also note, we specify an amount at all so that you can have things like
  // time-release withdrawals, where you withdraw a small amount across multiple steps in a chain.
  let withdrawn =
    math.min(amount, value.quantity_of(input_value, lp_policy, lp_asset_name))

  // We calculate the asset_a and asset_b to withdraw, namely the percentage of each asset
  // that corresponds to the percentage of LP token burned.  Note we multiply first to minimize precision loss.
  // OPTIMIZATION: we can probably destructure the SingletonValues once and reuse, but optimizing
  // deposits/withdrawals isn't as critical as optimizing swaps.
  let withdrawn_a =
    withdrawn * pool_state.quantity_a.3rd / pool_state.quantity_lp.3rd
  let withdrawn_b =
    withdrawn * pool_state.quantity_b.3rd / pool_state.quantity_lp.3rd

  // Calculate the remainder that should be directed to the destination:
  // We start with whatever we got in on the input, deduct the LP tokens we're burning
  // the protocol fee, and then add in the amounts that we withdrew
  // OPTIMIZATION: I wonder if there's a more efficient implementation that can do this in just one pass
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

  
  // Make sure we're paying the result to the correct destination (both the address and the datum),
  // with the correct amount; In the special case where Datum is "Self" (for example for a repeating strategy)
  // use the input datum for validation
  expect output.value == remainder
  expect when destination is {
    Fixed { address, datum } -> {
      and {
        output.address == address,
        output.datum == datum
      }
    }
    Self -> {
      and {
        output.address == input_address,
        output.datum == input_datum
      }
    }
  }

  // Return the final pool state
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
