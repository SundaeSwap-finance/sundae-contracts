use aiken/collection/list.{for_each}
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/fuzz.{and_then, constant}
use aiken/fuzz/scenario.{Label, Scenario}
use aiken/primitive/string
use calculation/process.{pool_input_to_asset_reserves}
use calculation/stableswap.{liquidity_invariant}
use cardano/address.{Credential}
use cardano/assets.{
  AssetName, Lovelace, PolicyId, Value, ada_asset_name, ada_policy_id,
}
use cardano/fuzz as cardano
use cardano/transaction.{InlineDatum, Input, Output, ScriptPurpose, Transaction}
use pool as handlers
use shared.{Ident, calc_precision, get_D}
use sundae/multisig.{MultisigScript}
use tests/constants
use tests/examples/ex_settings.{mk_valid_settings_input}
use types/pool.{ManageRedeemer, StablePoolDatum}
use types/settings.{SettingsDatum}

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
  action: ManageRedeemer,
  done: Bool,
}

fn default_state(action: ManageRedeemer) -> State {
  State { labels: [], action, done: False }
}

test prop_pool_manage_adjust_linear_ok(
  ok via scenario.ok(
    default_state(ManageRedeemer.AdjustLinearAmplification(0)),
    step,
  ),
) {
  run_as(ok)
  for_each(ok, post_conditions)
}

test prop_pool_manage_adjust_linear_ko(
  (labels, ko) via scenario.ko(
    default_state(ManageRedeemer.AdjustLinearAmplification(0)),
    step,
  ),
) fail {
  for_each(list.unique(labels), fuzz.label)
  run_as(ko)
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
    let action <- and_then(manage_redeemer(st.action))

    let pool_input_index =
      when action is {
        ManageRedeemer.AdjustLinearAmplification { pool_input } -> pool_input
        ManageRedeemer.UpdatePoolFees { pool_input } -> pool_input
        ManageRedeemer.WithdrawFees { pool_input, .. } -> pool_input
      }

    let (st, dummy_inputs) <-
      scenario.fork_and_then(
        200,
        fn() {
          fuzz.both(
            constant(st),
            fuzz.list_between(
              cardano.input(),
              pool_input_index,
              pool_input_index,
            ),
          )
        },
        fn() {
          fuzz.both(
            constant(
              State { ..st, labels: [@"Wrong pool input index", ..st.labels] },
            ),
            fuzz.list_between(
              cardano.input(),
              pool_input_index + 1,
              pool_input_index + 1,
            ),
          )
        },
      )

    let (st, pool_input, pool_datum) <-
      and_then(scenario_pool_input_baseline(st, action))

    let (st, settings_input, settings_datum) <-
      and_then(scenario_settings_input_baseline(st))

    let withdrawals <- and_then(scenario_withdrawals_baseline())

    let (st, redeemers) <- and_then(scenario_redeemers_baseline(st, action))

    let (st, pool_output) <-
      and_then(scenario_pool_output_baseline(st, pool_input, action))

    let (st, extra_signatories) <-
      and_then(
        scenario_signatories_baseline(st, action, pool_datum, settings_datum),
      )

    constant(
      Scenario.Step(
        st.labels,
        State { done: True, action, labels: [] },
        Transaction {
          ..transaction.placeholder,
          inputs: list.concat(dummy_inputs, [pool_input]),
          reference_inputs: [settings_input],
          outputs: [pool_output],
          withdrawals,
          redeemers,
          extra_signatories,
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
  settings_datum: SettingsDatum,
) -> Fuzzer<(State, List<VerificationKeyHash>)> {
  let signatories =
    when action is {
      ManageRedeemer.AdjustLinearAmplification { .. } ->
        multisig_signatories(pool_datum.linear_amplification_manager)
      ManageRedeemer.UpdatePoolFees { .. } ->
        multisig_signatories(pool_datum.fee_manager)
      ManageRedeemer.WithdrawFees { .. } ->
        multisig_signatories(Some(settings_datum.treasury_admin))
    }

  fuzz.both(constant(st), signatories)
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
  let (st, new_linear_amplification) <-
    and_then(
      when action is {
        ManageRedeemer.AdjustLinearAmplification { .. } ->
          scenario.fork(
            200,
            fn() { fuzz.both(constant(st), fuzz.int_at_least(1)) },
            fn() {
              fuzz.both(
                constant(
                  State {
                    ..st,
                    labels: [
                      @"Linear Amplification set to 0 or lower",
                      ..st.labels
                    ],
                  },
                ),
                fuzz.int_at_most(0),
              )
            },
          )
        _ ->
          scenario.fork(
            200,
            fn() {
              fuzz.both(
                constant(st),
                constant(input_datum.linear_amplification),
              )
            },
            fn() {
              fuzz.both(
                constant(
                  State {
                    ..st,
                    labels: [@"Linear Amplification changed", ..st.labels],
                  },
                ),
                fuzz.int(),
              )
            },
          )
      },
    )
  let sum_invariant = get_D(new_linear_amplification, reserve_a, reserve_b)
  let output_datum =
    StablePoolDatum {
      ..input_datum,
      linear_amplification: new_linear_amplification,
      sum_invariant,
    }

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
fn scenario_settings_input_baseline(
  st: State,
) -> Fuzzer<(State, Input, SettingsDatum)> {
  let settings_datum =
    fuzz.map5(
      multisig(),
      cardano.address_with(cardano.verification_key(), constant(None)),
      multisig(),
      cardano.address_with(cardano.verification_key(), constant(None)),
      fuzz.list(cardano.verification_key()),
      fn(
        settings_admin,
        metadata_admin,
        treasury_admin,
        treasury_address,
        authorized_staking_keys,
      ) {
        SettingsDatum {
          settings_admin,
          metadata_admin,
          treasury_admin,
          treasury_address,
          treasury_allowance: (1, 10),
          authorized_scoopers: Some([constants.scooper]),
          authorized_staking_keys,
          base_fee: 0,
          simple_fee: 2_500_000,
          strategy_fee: 5_000_000,
          pool_creation_fee: 0,
          extensions: [],
        }
      },
    )

  fuzz.tuple3(
    constant(st),
    constant(mk_valid_settings_input([constants.scooper], 1)),
    settings_datum,
  )
}

fn scenario_multisig_baseline(
  st: State,
) -> Fuzzer<(State, Option<MultisigScript>)> {
  fuzz.both(constant(st), fuzz.map(multisig(), fn(ms) { Some(ms) }))
}

fn scenario_none_multisig(
  st: State,
  multisig_type: String,
) -> Fuzzer<(State, Option<MultisigScript>)> {
  constant(
    (
      State {
        ..st,
        labels: [
          string.concat(multisig_type, @" Multisig set to None"),
          ..st.labels
        ],
      },
      None,
    ),
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
  let lp_fee = fuzz.both(fuzz.int_at_least(0), fuzz.int_at_least(0))
  let protocol_fee = fuzz.both(fuzz.int_at_least(0), fuzz.int_at_least(0))
  let fee_manager = fuzz.option(multisig())
  let linear_amplification = fuzz.int_at_least(1)
  let (st, linear_amplification_manager) <-
    and_then(
      when action is {
        ManageRedeemer.AdjustLinearAmplification { .. } ->
          scenario.fork(
            200,
            fn() { scenario_multisig_baseline(st) },
            fn() { scenario_none_multisig(st, @"Linear Amplification Manager") },
          )
        _ -> fuzz.both(constant(st), fuzz.option(multisig()))
      },
    )
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
      fuzz.map6(
        assets,
        circulating_lp,
        lp_fee,
        protocol_fee,
        fee_manager,
        linear_amplification,
        fn(
          assets,
          circulating_lp,
          lp_fee,
          protocol_fee,
          fee_manager,
          linear_amplification,
        ) -> StablePoolDatum {
          StablePoolDatum {
            identifier: pool_identifier,
            assets,
            circulating_lp,
            lp_fee,
            protocol_fee,
            fee_denominator: 10_000,
            fee_manager,
            market_open: 0,
            // Default to 0 for testing
            protocol_fees,
            linear_amplification,
            sum_invariant: get_D(linear_amplification, reserve_a, reserve_b),
            linear_amplification_manager,
            prescale: (1, 1),
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

fn manage_redeemer(action: ManageRedeemer) -> Fuzzer<ManageRedeemer> {
  when action is {
    ManageRedeemer.AdjustLinearAmplification { .. } ->
      fuzz.map(
        fuzz.int_between(0, 10),
        fn(pool_input) {
          ManageRedeemer.AdjustLinearAmplification { pool_input }
        },
      )
    ManageRedeemer.UpdatePoolFees { .. } ->
      fuzz.map(
        fuzz.int_between(0, 10),
        fn(pool_input) { ManageRedeemer.UpdatePoolFees { pool_input } },
      )
    ManageRedeemer.WithdrawFees { .. } ->
      fuzz.map3(
        fuzz.tuple3(
          fuzz.int_at_least(0),
          fuzz.int_at_least(0),
          fuzz.int_at_least(0),
        ),
        fuzz.int_between(0, 10),
        fuzz.int_between(0, 10),
        fn(amount, treasury_output, pool_input) {
          ManageRedeemer.WithdrawFees { amount, treasury_output, pool_input }
        },
      )
  }
}

test simple_d_test() {
  let d = get_D(16, 197, 6)
  trace d
  expect liquidity_invariant(197 * calc_precision, 6 * calc_precision, 16, d)
}

test simple_d_test_plus() fail {
  let d = get_D(16, 197, 6)
  trace d
  expect
    liquidity_invariant(197 * calc_precision, 6 * calc_precision, 16, d + 1)
}

test simple_d_test_minus() fail {
  let d = get_D(16, 197, 6)
  trace d
  expect
    liquidity_invariant(197 * calc_precision, 6 * calc_precision, 16, d - 1)
}
