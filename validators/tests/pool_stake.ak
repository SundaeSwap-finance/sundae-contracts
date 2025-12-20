use aiken/interval
use cardano/address.{Address, Script, VerificationKey, from_script}
use cardano/assets
use cardano/certificate.{RegisterCredential}
use cardano/script_context.{Publishing, ScriptContext, Withdrawing}
use cardano/transaction.{InlineDatum, NoDatum}
use pool_stake as pool_stake_validator
use sundae/multisig
use tests/fixtures/constants.{payment_key, settings_policy_id, stake_key}
use tx_util/builder.{
  add_signatory, add_tx_output, add_tx_ref_input, build_txn_context,
  new_tx_input, new_tx_output, set_withdrawals, with_asset_of_tx_input,
}
use types/settings.{SettingsDatum, settings_nft_name}

const treasury_admin_key = #"17bbd2d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad"

const pool_stake_hash = #"04040404040404040404040404040404040404040404040404040404"

fn treasury_address() -> Address {
  Address(VerificationKey(payment_key), None)
}

fn settings_datum() -> SettingsDatum {
  SettingsDatum {
    settings_admin: multisig.Signature(treasury_admin_key),
    metadata_admin: Address(VerificationKey(payment_key), None),
    treasury_admin: multisig.Signature(treasury_admin_key),
    treasury_address: treasury_address(),
    treasury_allowance: (1, 10),
    authorized_scoopers: Some([]),
    authorized_staking_keys: [VerificationKey(stake_key)],
    base_fee: 0,
    simple_fee: 2_500_000,
    strategy_fee: 5_000_000,
    pool_creation_fee: 0,
    extensions: [],
  }
}

fn settings_ref_input() {
  new_tx_input(
    #"abcd",
    from_script(settings_policy_id),
    2_000_000,
    InlineDatum(settings_datum()),
  )
    |> with_asset_of_tx_input(
        assets.from_asset(settings_policy_id, settings_nft_name, 1),
      )
}

// Treasury gets 90% (allowance 1/10 means admin keeps 10%)
fn treasury_payment(withdrawn: Int) -> Int {
  withdrawn - withdrawn / 10
}

fn withdraw_ctx(
  withdrawn: Int,
  treasury_payment: Int,
  treasury_datum: transaction.Datum,
  treasury_addr: Address,
  signed: Bool,
) -> ScriptContext {
  let treasury_output =
    new_tx_output(treasury_addr, treasury_payment, treasury_datum)
  let withdrawals = [Pair(Script(pool_stake_hash), withdrawn)]

  let base_ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_ref_input(settings_ref_input())
      |> set_withdrawals(withdrawals)
      |> add_tx_output(treasury_output)

  let ctx =
    if signed {
      base_ctx |> add_signatory(treasury_admin_key)
    } else {
      base_ctx
    }

  ScriptContext {
    transaction: ctx.transaction,
    redeemer: Void,
    info: Withdrawing(Script(pool_stake_hash)),
  }
}

fn run_pool_stake(ctx: ScriptContext) -> Bool {
  pool_stake_validator.pool_stake.else(settings_policy_id, 0, ctx)
}

// Withdraw success cases

test pool_stake_withdraw_success() {
  let amount = 10_000_000
  run_pool_stake(
    withdraw_ctx(
      amount,
      treasury_payment(amount),
      InlineDatum(Void),
      treasury_address(),
      True,
    ),
  )
}

test pool_stake_withdraw_overpay_success() {
  let amount = 10_000_000
  run_pool_stake(
    withdraw_ctx(
      amount,
      treasury_payment(amount) + 1_000_000,
      InlineDatum(Void),
      treasury_address(),
      True,
    ),
  )
}

// Withdraw failure cases

test pool_stake_withdraw_no_signature_fails() fail {
  let amount = 10_000_000
  run_pool_stake(
    withdraw_ctx(
      amount,
      treasury_payment(amount),
      InlineDatum(Void),
      treasury_address(),
      False,
    ),
  )
}

test pool_stake_withdraw_underpay_fails() fail {
  let amount = 10_000_000
  run_pool_stake(
    withdraw_ctx(
      amount,
      treasury_payment(amount) - 1,
      InlineDatum(Void),
      treasury_address(),
      True,
    ),
  )
}

test pool_stake_withdraw_wrong_datum_fails() fail {
  let amount = 10_000_000
  run_pool_stake(
    withdraw_ctx(
      amount,
      treasury_payment(amount),
      NoDatum,
      treasury_address(),
      True,
    ),
  )
}

test pool_stake_withdraw_wrong_address_fails() fail {
  let amount = 10_000_000
  let wrong_address = Address(VerificationKey(treasury_admin_key), None)
  run_pool_stake(
    withdraw_ctx(
      amount,
      treasury_payment(amount),
      InlineDatum(Void),
      wrong_address,
      True,
    ),
  )
}

test pool_stake_withdraw_no_treasury_output_fails() fail {
  let withdrawals = [Pair(Script(pool_stake_hash), 10_000_000)]
  let ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_ref_input(settings_ref_input())
      |> set_withdrawals(withdrawals)
      |> add_signatory(treasury_admin_key)

  run_pool_stake(
    ScriptContext {
      transaction: ctx.transaction,
      redeemer: Void,
      info: Withdrawing(Script(pool_stake_hash)),
    },
  )
}

// Publishing tests

fn publish_ctx(signed: Bool) -> ScriptContext {
  let base_ctx =
    interval.between(1, 2)
      |> build_txn_context()
      |> add_tx_ref_input(settings_ref_input())

  let ctx =
    if signed {
      base_ctx |> add_signatory(treasury_admin_key)
    } else {
      base_ctx
    }

  let cert =
    RegisterCredential { credential: Script(pool_stake_hash), deposit: Never }
  ScriptContext {
    transaction: ctx.transaction,
    redeemer: Void,
    info: Publishing { at: 0, certificate: cert },
  }
}

test pool_stake_publish_success() {
  run_pool_stake(publish_ctx(True))
}

test pool_stake_publish_no_signature_fails() fail {
  run_pool_stake(publish_ctx(False))
}
