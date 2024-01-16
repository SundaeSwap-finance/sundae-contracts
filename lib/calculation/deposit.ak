use aiken/math
use aiken/transaction.{NoDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{Value, ada_policy_id, ada_asset_name}
use calculation/shared.{PoolState} as calc_shared
use sundae/multisig
use shared.{SingletonValue}
use types/order.{Destination, OrderDatum}

pub fn do_deposit(
  pool_state: PoolState,
  input_value: Value,
  assets: (SingletonValue, SingletonValue),
  destination: Destination,
  actual_protocol_fee: Int,
  output: Output,
) -> PoolState {
  let (asset_a, asset_b) = assets

  // Policy ID and token name of the assets must match the pool.
  expect asset_a.1st == pool_state.quantity_a.1st
  expect asset_a.2nd == pool_state.quantity_a.2nd
  expect asset_b.1st == pool_state.quantity_b.1st
  expect asset_b.2nd == pool_state.quantity_b.2nd

  // A deposit is permitted to have lower funds than the datum claims. So we
  // take either the datum value or the actual amount on the utxo, whichever is
  // less.
  let user_gives_a = {
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
  let user_gives_b =
    math.min(
      asset_b.3rd,
      value.quantity_of(input_value, asset_a.1st, asset_a.2nd),
    )

  let b_in_units_of_a =
    user_gives_b * pool_state.quantity_a.3rd / pool_state.quantity_b.3rd

  // Amount that user actually deposits in the pool, after giving back change.
  let (final_a, final_b) =
    if b_in_units_of_a > user_gives_a {
      let change =
        pool_state.quantity_b.3rd
          * (b_in_units_of_a - user_gives_a)
          / pool_state.quantity_a.3rd
      (user_gives_a, user_gives_b - change)
    } else {
      let change = user_gives_a - b_in_units_of_a
      (user_gives_a - change, user_gives_b)
    }

  let issued_lp_tokens =
    final_a * pool_state.quantity_lp.3rd / pool_state.quantity_a.3rd

  let out_value =
    input_value
      |> value.add(asset_a.1st, asset_a.2nd, -final_a)
      |> value.add(asset_b.1st, asset_b.2nd, -final_b)
      |> value.add(ada_policy_id, ada_asset_name, -actual_protocol_fee)
      |> value.add(
          pool_state.quantity_lp.1st,
          pool_state.quantity_lp.2nd,
          issued_lp_tokens,
        )

  expect output.address == destination.address
  expect output.datum == destination.datum
  expect output.value == out_value
  PoolState {
    quantity_a: (
      pool_state.quantity_a.1st,
      pool_state.quantity_a.2nd,
      pool_state.quantity_a.3rd + final_a,
    ),
    quantity_b: (
      pool_state.quantity_b.1st,
      pool_state.quantity_b.2nd,
      pool_state.quantity_b.3rd + final_b,
    ),
    quantity_lp: (
      pool_state.quantity_lp.1st,
      pool_state.quantity_lp.2nd,
      pool_state.quantity_lp.3rd + issued_lp_tokens,
    ),
    fees_per_10_thousand: pool_state.fees_per_10_thousand,
    protocol_fees: pool_state.protocol_fees + actual_protocol_fee,
  }
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
  let rberry = (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  let pool_state = PoolState {
    quantity_a: (#"", #"", 1_000_000_000),
    quantity_b: (rberry.1st, rberry.2nd, 1_000_000_000),
    quantity_lp: (lp.1st, lp.2nd, 1_000_000_000),
    fees_per_10_thousand: 5,
    protocol_fees: 2_000_000,
  }
  let input_value =
    value.from_lovelace(14_500_000)
      |> value.add(rberry.1st, rberry.2nd, 10_000_000)
  let assets = ((ada.1st, ada.2nd, 10_000_000), (rberry.1st, rberry.2nd, 10_000_000))
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
    details: order.Deposit {
      assets: assets,
    },
    extension: Void,
  }
  let output = Output {
    address: addr,
    value: value.from_lovelace(2_000_000)
      |> value.add(lp.1st, lp.2nd, 10_000_000),
    datum: NoDatum,
    reference_script: None,
  }
  let final_pool_state = do_deposit(pool_state, input_value, assets, order.destination, 2_500_000, output)
  expect final_pool_state.quantity_a.3rd == 1_010_000_000
  expect final_pool_state.quantity_b.3rd == 1_010_000_000
  expect final_pool_state.protocol_fees == 4_500_000
  True
}
