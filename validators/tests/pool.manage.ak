use aiken/collection/list.{for_each}
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/fuzz.{and_then, constant}
use aiken/fuzz/scenario.{Label, Scenario}
use calculation/process.{pool_input_to_asset_reserves}
use calculation/stableswap.{liquidity_invariant, precision}
use cardano/address.{Credential}
use cardano/assets.{
  AssetName, Lovelace, PolicyId, Value, ada_asset_name, ada_policy_id,
}
use cardano/fuzz as cardano
use cardano/transaction.{InlineDatum, Input, Output, ScriptPurpose, Transaction}
use pool as handlers
use shared.{Ident}
use sundae/multisig.{MultisigScript}
use tests/constants
use tests/examples/ex_settings.{mk_valid_settings_input}
use types/pool.{ManageRedeemer, StablePoolDatum}

const settings_policy_id: PolicyId =
  #"00000000000000000000000000000000000000000000000000000000"

const pool_script_hash: ScriptHash =
  #"fffffffffffffffffffffffffffffffffffffffffffffffffffffff1"

const pool_identifier: Ident =
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffff11"

const validator_hash: ScriptHash =
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

fn run_as(txs: List<Transaction>) {
  let default_handler =
    handlers.manage.else(settings_policy_id, _)
  scenario.run(
    txs,
    validator_hash,
    scenario.into_mint_handler(default_handler),
    scenario.into_spend_handler(default_handler),
    scenario.into_withdraw_handler(default_handler),
    scenario.into_publish_handler(default_handler),
    scenario.into_vote_handler(default_handler),
  )
}

type State {
  labels: List<Label>,
  done: Bool,
}

fn default_state() -> State {
  State { labels: [], done: False }
}

test prop_pool_manage_ok(ok via scenario.ok(default_state(), step)) {
  run_as(ok)
  for_each(ok, post_conditions)
}

fn post_conditions(scenario: Transaction) {
  expect Some(Pair(_, data)) =
    list.find(
      scenario.redeemers,
      fn(Pair(key, _)) {
        when key is {
          ScriptPurpose.Withdraw(..) -> True
          _ -> False
        }
      },
    )

  expect redeemer: ManageRedeemer = data

  let (is_withdraw, is_update_fees, is_adjust_amplification) =
    when redeemer is {
      ManageRedeemer.WithdrawFees { .. } -> (True, False, False)
      ManageRedeemer.UpdatePoolFees { .. } -> (False, True, False)
      ManageRedeemer.AdjustLinearAmplification { .. } -> (False, False, True)
    }
  []
    |> scenario.classify(@"· action = withdraw", is_withdraw)
    |> scenario.classify(@"· action = update fees", is_update_fees)
    |> scenario.classify(
        @"· action = adjust amplification",
        is_adjust_amplification,
      )
    |> for_each(fuzz.label)
}

fn step(st: State) -> Fuzzer<Scenario<State>> {
  if st.done {
    constant(Scenario.Done)
  } else {
    let action <- and_then(manage_redeemer())

    let (st, pool_input, pool_datum) <-
      and_then(scenario_pool_input_baseline(st, action))

    let (st, settings_input) <- and_then(scenario_settings_input_baseline(st))

    let withdrawals <- and_then(scenario_withdrawals_baseline())

    let (st, redeemers) <- and_then(scenario_redeemers_baseline(st, action))

    let (st, pool_output) <-
      and_then(scenario_pool_output_baseline(st, pool_input, action))

    let (st, extra_signatories) <-
      and_then(scenario_signatories_baseline(st, action, pool_datum))

    constant(
      Scenario.Step(
        st.labels,
        State { done: True, labels: [] },
        Transaction {
          ..transaction.placeholder,
          inputs: [pool_input],
          reference_inputs: [settings_input],
          outputs: [pool_output],
          withdrawals: withdrawals,
          redeemers: redeemers,
          extra_signatories: extra_signatories,
        },
      ),
    )
  }
}

fn multisig_signatories(
  multisig: Option<MultisigScript>,
) -> Fuzzer<List<VerificationKeyHash>> {
  when multisig is {
    Some(MultisigScript.Signature { key_hash }) -> constant([key_hash])
    Some(MultisigScript.AnyOf { scripts }) ->
      fuzz.map(
        fuzz.pick(
          list.map(
            scripts,
            fn(script) {
              when script is {
                MultisigScript.Signature { key_hash } -> key_hash
                _ -> fail @"Expected signature in multisig script"
              }
            },
          ),
        ),
        fn(pick) { [pick.2nd] },
      )
    Some(_) -> fail @"Expected multisig script with signatures"
    None -> constant([])
  }
}

fn scenario_signatories_baseline(
  st: State,
  action: ManageRedeemer,
  pool_datum: StablePoolDatum,
) -> Fuzzer<(State, List<VerificationKeyHash>)> {
  let signatories =
    when action is {
      ManageRedeemer.AdjustLinearAmplification { .. } ->
        multisig_signatories(pool_datum.linear_amplification_manager)
      _ -> fuzz.list_between(cardano.verification_key_hash(), 1, 5)
    }

  fuzz.both(constant(st), signatories)
}

fn get_D_recursive(d: Int, x: Int, y: Int, sum: Int, ann: Int, i: Int) -> Int {
  expect i < 255
  let d_p = d * d * d / ( 4 * x * y )
  let d_prev = d
  let d =
    ( ann * sum / 100 + d_p * 2 ) * d / ( ( ann - 100 ) * d / 100 + 3 * d_p )
  if d > d_prev {
    if d - d_prev <= 1 {
      if liquidity_invariant(x / precision, y / precision, ann / 400, d) {
        d
      } else {
        d - 1
      }
    } else {
      get_D_recursive(d, x, y, sum, ann, i + 1)
    }
  } else {
    if d_prev - d <= 1 {
      if liquidity_invariant(x / precision, y / precision, ann / 400, d) {
        d
      } else {
        d - 1
      }
    } else {
      get_D_recursive(d, x, y, sum, ann, i + 1)
    }
  }
}

fn get_D(a: Int, x: Int, y: Int) -> Int {
  let x = x * precision
  let y = y * precision
  let a = a * 200
  let sum: Int = x + y
  if sum == 0 {
    0
  } else {
    let d = sum
    let ann = a * 2
    let result_d = get_D_recursive(d, x, y, sum, ann, 0)
    trace result_d
    result_d
  }
}

fn scenario_pool_output_baseline(
  st: State,
  pool_input: Input,
  action: ManageRedeemer,
) -> Fuzzer<(State, Output)> {
  expect InlineDatum(pool_datum) = pool_input.output.datum
  expect input_datum: StablePoolDatum = pool_datum
  let
    reserve_a,
    reserve_b,
  <-
    pool_input_to_asset_reserves(
      input_datum.assets,
      input_datum.protocol_fees,
      pool_input.output,
    )
  let sum_invariant =
    get_D(input_datum.linear_amplification, reserve_a, reserve_b)
  let output_datum =
    StablePoolDatum { ..input_datum, sum_invariant: sum_invariant }

  let output =
    cardano.output_with(
      constant(pool_input.output.address),
      constant(pool_input.output.value),
      cardano.inline_datum_with(constant(as_data(output_datum))),
      constant(None),
    )

  fuzz.both(constant(st), output)
}

fn scenario_redeemers_baseline(
  st: State,
  action: ManageRedeemer,
) -> Fuzzer<(State, List<Pair<ScriptPurpose, Data>>)> {
  let redeemer: Pair<ScriptPurpose, Data> =
    Pair(
      ScriptPurpose.Withdraw(Credential.Script(validator_hash)),
      as_data(action),
    )

  let redeemers = [redeemer]

  fuzz.both(constant(st), constant(redeemers))
}

fn scenario_withdrawals_baseline() -> Fuzzer<Pairs<Credential, Lovelace>> {
  let amount <- fuzz.map(constant(0))

  let withdrawals =
    [
      // Always include a withdrawal for our validator, otherwise nothing
      // is being tested...
      Pair(Credential.Script(validator_hash), amount),
    ]

  withdrawals
}

fn signature_with(key_hash: Fuzzer<ByteArray>) -> Fuzzer<MultisigScript> {
  fuzz.map(key_hash, fn(key_hash) { MultisigScript.Signature { key_hash } })
}

fn anyof() -> Fuzzer<MultisigScript> {
  let scripts =
    fuzz.list_between(signature_with(cardano.verification_key_hash()), 1, 5)
  fuzz.map(scripts, fn(scripts) { MultisigScript.AnyOf { scripts } })
}

//TODO: Make more exhaustive
fn multisig() -> Fuzzer<MultisigScript> {
  let single_sig <- and_then(signature_with(cardano.verification_key_hash()))
  let multisig <- and_then(anyof())
  fuzz.one_of([single_sig, multisig])
}

fn ada_asset_class() -> Fuzzer<(PolicyId, ByteArray)> {
  constant((ada_policy_id, ada_asset_name))
}

fn asset_class() -> Fuzzer<(PolicyId, AssetName)> {
  fuzz.both(cardano.policy_id(), cardano.asset_name())
}

//TODO: fuzzify
fn scenario_settings_input_baseline(st: State) -> Fuzzer<(State, Input)> {
  fuzz.both(
    constant(st),
    constant(mk_valid_settings_input([constants.scooper], 1)),
  )
}

fn scenario_pool_datum_baseline(
  st: State,
  reserve_a: Int,
  reserve_b: Int,
  action: ManageRedeemer,
) -> Fuzzer<(State, StablePoolDatum)> {
  let assets =
    fuzz.both(fuzz.either(ada_asset_class(), asset_class()), asset_class())
  let circulating_lp = fuzz.int_at_least(1000)
  let lp_fee_basis_points =
    fuzz.both(fuzz.int_at_least(0), fuzz.int_at_least(0))
  let protocol_fee_basis_points =
    fuzz.both(fuzz.int_at_least(0), fuzz.int_at_least(0))
  let fee_manager = fuzz.option(multisig())
  let linear_amplification = fuzz.int_at_least(1)
  let linear_amplification_manager =
    when action is {
      ManageRedeemer.AdjustLinearAmplification { .. } ->
        fuzz.map(multisig(), fn(ms) { Some(ms) })
      _ -> fuzz.option(multisig())
    }
  let protocol_fees <-
    and_then(
      fuzz.tuple3(
        fuzz.int_at_least(0),
        fuzz.int_at_least(0),
        fuzz.int_at_least(0),
      ),
    )
  let pool_datum <-
    and_then(
      fuzz.map7(
        assets,
        circulating_lp,
        lp_fee_basis_points,
        protocol_fee_basis_points,
        fee_manager,
        linear_amplification,
        linear_amplification_manager,
        fn(
          assets,
          circulating_lp,
          lp_fee_basis_points,
          protocol_fee_basis_points,
          fee_manager,
          linear_amplification,
          linear_amplification_manager,
        ) -> StablePoolDatum {
          StablePoolDatum {
            identifier: pool_identifier,
            assets,
            circulating_lp,
            lp_fee_basis_points,
            protocol_fee_basis_points,
            fee_manager,
            market_open: 0,
            // Default to 0 for testing
            protocol_fees,
            linear_amplification,
            sum_invariant: get_D(linear_amplification, reserve_a, reserve_b),
            linear_amplification_manager,
          }
        },
      ),
    )

  constant((st, pool_datum))
}

const min_utxo: Int = 2000000

fn pool_datum_to_value(
  pool_datum: StablePoolDatum,
  reserve_a: Int,
  reserve_b: Int,
) -> Fuzzer<Value> {
  fuzz.map(
    constant(pool_datum),
    fn(pool_datum) -> Value {
      let (asset_1_policy, asset_1_name) = pool_datum.assets.1st
      let (asset_2_policy, asset_2_name) = pool_datum.assets.2nd

      let asset_amount_1 = pool_datum.protocol_fees.2nd
      let asset_amount_2 = pool_datum.protocol_fees.3rd

      let first_asset =
        assets.from_asset(
          asset_1_policy,
          asset_1_name,
          asset_amount_1 + reserve_a,
        )
      let second_asset =
        assets.from_asset(
          asset_2_policy,
          asset_2_name,
          asset_amount_2 + reserve_b,
        )

      assets.from_lovelace(min_utxo + pool_datum.protocol_fees.1st)
        |> assets.merge(first_asset)
        |> assets.merge(second_asset)
    },
  )
}

fn scenario_pool_input_baseline(
  st: State,
  action: ManageRedeemer,
) -> Fuzzer<(State, Input, StablePoolDatum)> {
  let reserve_a <- and_then(fuzz.int_at_least(1))
  let reserve_b <- and_then(fuzz.int_at_least(1))
  let (st, pool_datum) <-
    and_then(scenario_pool_datum_baseline(st, reserve_a, reserve_b, action))

  let pool_value = pool_datum_to_value(pool_datum, reserve_a, reserve_b)

  let pool_input =
    cardano.input_with(
      cardano.output_reference(),
      cardano.output_with(
        cardano.address_with(
          constant(Credential.Script(pool_script_hash)),
          fuzz.option(cardano.stake_credential()),
        ),
        pool_value,
        cardano.inline_datum_with(constant(as_data(pool_datum))),
        constant(None),
      ),
    )

  fuzz.tuple3(constant(st), pool_input, constant(pool_datum))
}

fn manage_redeemer() -> Fuzzer<ManageRedeemer> {
  let amount_1 <- and_then(fuzz.int_at_least(0))
  let amount_2 <- and_then(fuzz.int_at_least(0))
  let amount_3 <- and_then(fuzz.int_at_least(0))
  // fuzz.one_of(
  //   [
  //     ManageRedeemer.WithdrawFees {
  //       amount: (amount_1, amount_2, amount_3),
  //       treasury_output: 1,
  //       pool_input: 0,
  //     },
  //     ManageRedeemer.UpdatePoolFees { pool_input: 0 },
  //     ManageRedeemer.AdjustLinearAmplification { pool_input: 0 },
  //   ],
  // )
  constant(ManageRedeemer.AdjustLinearAmplification { pool_input: 0 })
}

test simple_d_test() {
  let d = get_D(16, 197, 6)
  trace d
  expect liquidity_invariant(197, 6, 16, d)
}

test simple_d_test_plus() fail {
  let d = get_D(16, 197, 6)
  trace d
  expect liquidity_invariant(197, 6, 16, d + 1)
}

test simple_d_test_minus() fail {
  let d = get_D(16, 197, 6)
  trace d
  expect liquidity_invariant(197, 6, 16, d - 1)
}
