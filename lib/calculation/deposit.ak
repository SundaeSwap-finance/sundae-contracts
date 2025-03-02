use aiken/math
use calculation/shared.{PoolState} as calc_shared
use calculation/stableswap
use cardano/address.{Address, VerificationKey}
use cardano/assets.{AssetName, PolicyId, ada_asset_name, ada_policy_id}
use cardano/transaction.{InlineDatum, NoDatum, Output}
use shared.{SingletonValue}
use sundae/multisig
use types/order.{Destination, Fixed, OrderDatum, Self}

/// Calculate the result of depositing some amount of tokens into the pool
///
/// Should increment the pool state by the appropriate amount, and check that the correct
/// LP tokens are distributed to the destination.
///
pub fn do_deposit(
  pool_policy_a: PolicyId,
  pool_asset_name_a: AssetName,
  pool_quantity_a: Int,
  pool_policy_b: PolicyId,
  pool_asset_name_b: AssetName,
  pool_quantity_b: Int,
  pool_policy_lp: PolicyId,
  pool_asset_name_lp: AssetName,
  pool_quantity_lp: Int,
  linear_amplification: Int,
  sum_invariant: Int,
  next_sum_invariant: Int,
  input_utxo: Output,
  assets: (SingletonValue, SingletonValue),
  destination: Destination,
  actual_protocol_fee: Int,
  output: Output,
  continuation: fn(Int, Int, Int) -> Bool,
) -> Bool {
  let (order_a, order_b) = assets
  let Output { value: input_value, .. } = input_utxo

  // Policy ID and token name of the assets must match the pool, otherwise someone
  // could load the pool with junk tokens and freeze the pool.
  expect order_a.1st == pool_policy_a
  expect order_a.2nd == pool_asset_name_a
  expect order_b.1st == pool_policy_b
  expect order_b.2nd == pool_asset_name_b

  // Fetch the input and output quantities for A and B
  let input_a = assets.quantity_of(input_value, order_a.1st, order_a.2nd)
  let surplus_a = assets.quantity_of(output.value, order_a.1st, order_a.2nd)
  let input_b = assets.quantity_of(input_value, order_b.1st, order_b.2nd)
  let surplus_b = assets.quantity_of(output.value, order_b.1st, order_b.2nd)

  // The difference between these amounts is the amount the user actually deposits into the pool
  let deposited_a = surplus_a - input_a
  let deposited_b = surplus_b - input_b

  // Check that the deposited amounts are ok; specifically:
  // - we ensure that it's positive, so we don't accidentally withdraw tokens
  // - we ensure that it's less than the users maximum deposit amount, so we can compose with other protocols
  // - we ensure that the scooper doesn't under or over execute the deposit by ensuring we deposit the lesser of:
  //   - the users maximum deposit amount, so we can compose with other protocols
  //   - the amount of tokens on the input, to ensure the scooper doesn't under-execute the order
  expect and {
      deposited_a > 0,
      deposited_a == math.min(order_a.3rd, input_a),
      deposited_b > 0,
      deposited_b == math.min(order_b.3rd, input_b),
    }

  // Check the stableswap liquidity invariant; this ensures that either we provided a balanced amount of tokens,
  // or we paid liquidity provider fees on whatever portion was "swapped"
  let new_pool_a = pool_quantity_a + deposited_a
  let new_pool_b = pool_quantity_b + deposited_b
  expect
    stableswap.liquidity_invariant(
      new_pool_a,
      new_pool_b,
      linear_amplification,
      next_sum_invariant,
    )

  // We can then calculate how much LP tokens this user should get;
  // Because we check that `next_sum_invariant` is correct above,
  // The LP tokens the user receives is a percentage share of the existing LP tokens
  // where the percentage is the percent increase in the sum invariant!
  // We calculate this here in a precision-safe manner.
  let delta_d = next_sum_invariant - sum_invariant
  let issued_lp_tokens = delta_d * pool_quantity_lp / sum_invariant

  // Make sure we don't ever allow this to round to zero, which would just eat some of the users assets
  expect issued_lp_tokens > 0

  // Calculate what assets we expect on at the destination;
  // i.e. it should be whatever assets were on the inputs, minus the amount that was deposited, minus the fee,
  // plus the relevant LP tokens
  let out_value =
    input_value
      |> assets.add(order_a.1st, order_a.2nd, -deposited_a)
      |> assets.add(order_b.1st, order_b.2nd, -deposited_b)
      |> assets.add(ada_policy_id, ada_asset_name, -actual_protocol_fee)
      |> assets.add(pool_policy_lp, pool_asset_name_lp, issued_lp_tokens)

  // Make sure we're paying the result to the correct destination (both the address and the datum),
  // with the correct amount; In the special case where Datum is "Self" (for example for a repeating strategy)
  // use the input datum for validation
  expect output.value == out_value
  expect
    when destination is {
      Fixed { address, datum } -> and {
          output.address == address,
          output.datum == datum,
        }
      Self -> {
        let Output { address: input_address, datum: input_datum, .. } =
          input_utxo
        and {
          output.address == input_address,
          output.datum == input_datum,
        }
      }
    }

  // And construct the final pool state
  continuation(new_pool_a, new_pool_b, pool_quantity_lp + issued_lp_tokens)
}

test deposit_test() {
  let addr =
    Address(
      VerificationKey(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let ada = (#"", #"")
  let rberry =
    (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  let pool_state =
    PoolState {
      quantity_a: (#"", #"", 1_000_000_000),
      quantity_b: (rberry.1st, rberry.2nd, 1_000_000_000),
      quantity_lp: (lp.1st, lp.2nd, 1_000_000_000),
    }
  let input_value =
    assets.from_lovelace(14_500_000)
      |> assets.add(rberry.1st, rberry.2nd, 10_000_000)
  let assets =
    ((ada.1st, ada.2nd, 10_000_000), (rberry.1st, rberry.2nd, 10_000_000))
  let order =
    OrderDatum {
      pool_ident: None,
      owner: multisig.Signature(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      max_protocol_fee: 2_500_000,
      destination: Fixed { address: addr, datum: NoDatum },
      details: order.Deposit { assets },
      extension: Void,
    }
  let output =
    Output {
      address: addr,
      value: assets.from_lovelace(2_000_000)
        |> assets.add(lp.1st, lp.2nd, 10_000_000),
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
    do_deposit(
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
      pool_state.quantity_lp.3rd,
      input,
      assets,
      order.destination,
      2_500_000,
      output,
    )
  expect new_a == 1_010_000_000
  expect new_b == 1_010_000_000
  expect new_lp == 1_000_000_000 + 10_000_000
  True
}
