use aiken/dict
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{Inline, ScriptCredential}
use sundae/multisig
use types/order.{Cancel, OrderDatum, OrderRedeemer, Scoop}

const stake_script_hash = #""

validator {
  fn spend(
    datum: OrderDatum,
    redeemer: OrderRedeemer,
    ctx: ScriptContext,
  ) -> Bool {
    when redeemer is {
      Cancel ->
        multisig.satisfied(
          datum.owner,
          ctx.transaction.extra_signatories,
          ctx.transaction.validity_range,
        )
      Scoop ->
        dict.has_key(
          ctx.transaction.withdrawals,
          Inline(ScriptCredential(stake_script_hash)),
        )
    }
  }
}
