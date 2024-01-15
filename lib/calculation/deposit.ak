use aiken/math
use aiken/transaction.{NoDatum, Output}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{Value, PolicyId, AssetName, ada_policy_id, ada_asset_name}
use sundae/multisig
use types/order.{Destination, Deposit, OrderDatum}

pub fn do_deposit(
  // pool_state: PoolState,
  asset_a_policy: PolicyId,
  asset_a_name: AssetName,
  asset_b_policy: PolicyId,
  asset_b_name: AssetName,
  lp_policy: PolicyId,
  lp_name: AssetName,
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
  expect order.Deposit { assets } = order.details
  let (asset_a, asset_b) = assets

  // Policy ID and token name of the assets must match the pool.
  expect asset_a.1st == asset_a_policy
  expect asset_a.2nd == asset_a_name
  expect asset_b.1st == asset_b_policy
  expect asset_b.2nd == asset_b_name

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

  let b_in_units_of_a = user_gives_b * asset_a_reserve / asset_b_reserve

  // Amount that user actually deposits in the pool, after giving back change.
  let (final_a, final_b) =
    if b_in_units_of_a > user_gives_a {
      let change =
        asset_b_reserve
          * (b_in_units_of_a - user_gives_a)
          / asset_a_reserve
      (user_gives_a, user_gives_b - change)
    } else {
      let change = user_gives_a - b_in_units_of_a
      (user_gives_a - change, user_gives_b)
    }

  let issued_lp_tokens = final_a * circulating_lp / asset_a_reserve

  let out_value =
    input_value
      |> value.add(asset_a.1st, asset_a.2nd, -final_a)
      |> value.add(asset_b.1st, asset_b.2nd, -final_b)
      |> value.add(ada_policy_id, ada_asset_name, -actual_protocol_fee)
      |> value.add(
          lp_policy,
          lp_name,
          issued_lp_tokens,
        )

  expect output.address == order.destination.address
  expect output.datum == order.destination.datum
  expect output.value == out_value
  (
    asset_a_reserve + final_a,
    asset_b_reserve + final_b,
    circulating_lp + issued_lp_tokens,
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
  let rberry = (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  let input_value =
    value.from_lovelace(14_500_000)
      |> value.add(rberry.1st, rberry.2nd, 10_000_000)
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
      assets: ((ada.1st, ada.2nd, 10_000_000), (rberry.1st, rberry.2nd, 10_000_000)),
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
  let (new_reserve_a, new_reserve_b, new_circulating_lp) = do_deposit(
    #"",
    #"",
    rberry.1st,
    rberry.2nd,
    lp.1st,
    lp.2nd,
    1_000_000_000,
    1_000_000_000,
    1_000_000_000,
    input_value,
    order,
    2_500_000,
    output,
  )
  expect new_reserve_a == 1_010_000_000
  expect new_reserve_b == 1_010_000_000
  expect new_circulating_lp == 1010000000
  True
}
