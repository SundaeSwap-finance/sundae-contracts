use aiken/math
use aiken/transaction.{InlineDatum, NoDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{AssetName, PolicyId, ada_asset_name, ada_policy_id}
use calculation/shared.{PoolState} as calc_shared
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
  input_utxo: Output,
  assets: (SingletonValue, SingletonValue),
  destination: Destination,
  actual_protocol_fee: Int,
  output: Output,
  continuation: fn(Int, Int, Int) -> Bool,
) -> Bool {
  let (asset_a, asset_b) = assets
  let Output { value: input_value, .. } = input_utxo

  // Policy ID and token name of the assets must match the pool, otherwise someone
  // could load the pool with junk tokens and freeze the pool.
  expect asset_a.1st == pool_policy_a
  expect asset_a.2nd == pool_asset_name_a
  expect asset_b.1st == pool_policy_b
  expect asset_b.2nd == pool_asset_name_b

  // A deposit is permitted to have lower funds than the datum claims, so that
  // we can compose it as a step in a chain where the exact amount will be unknown.
  // So we take either the datum value or the actual amount on the utxo, whichever is
  // less.
  let user_gives_a = {
    // However, we need to leave some amount of ADA behind, to return as a deposit and the fee
    // to ensure that the change can be returned to the user
    // TODO: perhaps we should just let hte minUTXO fail, and go with what the user entered?
    // that way we save on ex units, and don't need to worry about a weird constant
    let extra_ada =
      if asset_a.1st == "" {
        // TODO: public constant for the deposit
        2_000_000 + actual_protocol_fee
      } else {
        0
      }
    math.min(
      asset_a.3rd,
      value.quantity_of(input_value, asset_a.1st, asset_a.2nd) - extra_ada,
    )
  }
  // Because we subtract off the deposit and the actual protocol fee, this could end up negative
  // in which case we should fail
  expect user_gives_a > 0
  // Similarly, take *up to* the the datum amount of assetB
  let user_gives_b =
    math.min(
      asset_b.3rd,
      value.quantity_of(input_value, asset_b.1st, asset_b.2nd),
    )

  // The ratio of a to b might have shifted since the user created their order amount
  // So some small amount of a or b might be returned to the user
  // So, calculate how much "b" do we have, in units of asset A, so we can check which is greater
  let b_in_units_of_a = user_gives_b * pool_quantity_a / pool_quantity_b

  // Amount that user actually deposits in the pool, after giving back change.
  let (deposited_a, deposited_b) =
    // If we have more b than a, then we can only take up to an equivalent amount of b, and return the rest
    // otherwise if we have more a than b, we can return some amount of `a` to the user instead
    if b_in_units_of_a > user_gives_a {
      // Make sure to do ceiling division here, to round in favor fo the protocol
      // That is, when in doubt, take up to one more token from the user than the
      // LP tokens we issue would entail
      let give_b = ( pool_quantity_b * user_gives_a - 1 ) / pool_quantity_a + 1
      (user_gives_a, give_b)
    } else {
      (b_in_units_of_a, user_gives_b)
    }

  // We can then calculate how much LP tokens this user should get;
  // This maintains the invariant that the minted LP tokens, as a percentage of the new circulating supply,
  // is equal to the percentage of assets the user deposited in the pool.
  // We can choose either asset A or asset B to compute this, since presumably, they're depositing equal value of each
  // 
  // issued_lp_tokens / (pool_state.quantity_lp.3rd + issued_lp_tokens) = deposited_a / (pool_state.quantity_a.3rd + deposited_a)
  // 
  // Solving for `issued_lp_tokens` gives:
  let issued_lp_tokens = deposited_a * pool_quantity_lp / pool_quantity_a

  // Make sure we don't ever allow this to round to zero, which would just eat some of the users assets
  expect issued_lp_tokens > 0

  // Calculate what assets we expect on at the destination;
  // i.e. it should be whatever assets were on the inputs, minus the amount that was deposited, minus the fee,
  // plus the relevant LP tokens
  let out_value =
    input_value
      |> value.add(asset_a.1st, asset_a.2nd, -deposited_a)
      |> value.add(asset_b.1st, asset_b.2nd, -deposited_b)
      |> value.add(ada_policy_id, ada_asset_name, -actual_protocol_fee)
      |> value.add(pool_policy_lp, pool_asset_name_lp, issued_lp_tokens)

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
  continuation(
    pool_quantity_a + deposited_a,
    pool_quantity_b + deposited_b,
    pool_quantity_lp + issued_lp_tokens,
  )
}

test deposit_test() {
  let addr =
    Address(
      VerificationKeyCredential(
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
    value.from_lovelace(14_500_000)
      |> value.add(rberry.1st, rberry.2nd, 10_000_000)
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
      value: value.from_lovelace(2_000_000)
        |> value.add(lp.1st, lp.2nd, 10_000_000),
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
