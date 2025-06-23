use aiken/collection/list.{for_each}
use aiken/crypto.{ScriptHash}
use aiken/fuzz.{and_then, constant}
use aiken/fuzz/scenario.{Label, Scenario}
use cardano/address.{Credential}
use cardano/assets.{AssetName, PolicyId, Value, ada_asset_name, ada_policy_id}
use cardano/fuzz as cardano
use cardano/transaction.{Input, ScriptPurpose, Transaction}
use pool as handlers
use shared.{Ident}
use sundae/multisig.{MultisigScript}
use types/pool.{ManageRedeemer, StablePoolDatum}

const settings_policy_id: PolicyId =
  #"99999999999999999999999999999999999999999999999999999999"

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

    let (st, pool_input) <- and_then(scenario_pool_input_baseline(st))

    let (st, reference_inputs) <-
      scenario.fork_and_then(
        weights.scenario_reference_inputs,
        fn() { scenario_ref_inputs_baseline(st) },
        fn() { scenario_ref_inputs_missing_scopes(st) },
      )

    let withdrawals <- and_then(scenario_withdrawals_baseline())

    let (st, redeemers) <-
      scenario.fork_if_and_then(
        st.labels == [],
        weights.scenario_redeemers,
        fn() { scenario_redeemers_baseline(st, inputs, action) },
        fn() { scenario_redeemers_mixed(st, inputs, action) },
      )

    let (st, extra_signatories) <-
      scenario.fork2_and_then(
        weights.scenario_signatories,
        fn() { scenario_signatories_baseline(st, action) },
        fn() { scenario_signatories_missing_owner(st, action) },
        fn() { scenario_signatories_missing_other(st, action) },
      )

    return(
      Scenario.Step(
        st.labels,
        State { ..st, done: True, labels: [] },
        Transaction {
          ..transaction.placeholder,
          inputs: inputs,
          reference_inputs: reference_inputs,
          withdrawals: withdrawals,
          extra_signatories: extra_signatories,
          redeemers: redeemers,
        },
      ),
    )
  }
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

fn scenario_pool_datum_baseline(st: State) -> Fuzzer<(State, StablePoolDatum)> {
  let assets =
    fuzz.both(fuzz.either(ada_asset_class(), asset_class()), asset_class())
  let circulating_lp = fuzz.int_at_least(1000)
  let lp_fee_basis_points =
    fuzz.both(fuzz.int_at_least(0), fuzz.int_at_least(0))
  let protocol_fee_basis_points =
    fuzz.both(fuzz.int_at_least(0), fuzz.int_at_least(0))
  let fee_manager = fuzz.option(multisig())
  let linear_amplification = fuzz.int_at_least(0)
  let linear_amplification_manager = fuzz.option(multisig())
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
            sum_invariant: 1000,
            linear_amplification_manager,
          }
        },
      ),
    )

  constant((st, pool_datum))
}

const min_utxo: Int = 2000000

fn pool_datum_to_value(pool_datum: StablePoolDatum) -> Fuzzer<Value> {
  fuzz.map(
    constant(pool_datum),
    fn(pool_datum) -> Value {
      let (asset_1_policy, asset_1_name) = pool_datum.assets.1st
      let (asset_2_policy, asset_2_name) = pool_datum.assets.2nd

      let asset_amount_1 = pool_datum.protocol_fees.2nd
      let asset_amount_2 = pool_datum.protocol_fees.3rd

      let first_asset =
        assets.from_asset(asset_1_policy, asset_1_name, asset_amount_1)
      let second_asset =
        assets.from_asset(asset_2_policy, asset_2_name, asset_amount_2)

      assets.from_lovelace(min_utxo + pool_datum.protocol_fees.1st)
        |> assets.merge(first_asset)
        |> assets.merge(second_asset)
    },
  )
}

fn scenario_pool_input_baseline(st: State) -> Fuzzer<(State, Input)> {
  let (st, pool_datum) <- and_then(scenario_pool_datum_baseline(st))

  let pool_value = pool_datum_to_value(pool_datum)

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

  fuzz.both(constant(st), pool_input)
}

fn manage_redeemer() -> Fuzzer<ManageRedeemer> {
  let amount_1 <- and_then(fuzz.int_at_least(0))
  let amount_2 <- and_then(fuzz.int_at_least(0))
  let amount_3 <- and_then(fuzz.int_at_least(0))
  fuzz.one_of(
    [
      ManageRedeemer.WithdrawFees {
        amount: (amount_1, amount_2, amount_3),
        treasury_output: 1,
        pool_input: 0,
      },
      ManageRedeemer.UpdatePoolFees { pool_input: 0 },
      ManageRedeemer.AdjustLinearAmplification { pool_input: 0 },
    ],
  )
}
