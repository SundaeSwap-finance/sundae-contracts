use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{Inline, Script, ScriptCredential}
use sundae/multisig
use types/order.{Cancel, OrderDatum, OrderRedeemer, Scoop}

validator(stake_script_hash: Hash<Blake2b_224, Script>) {
  fn spend(datum: Data, redeemer: OrderRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      Cancel -> {
        expect datum: OrderDatum = datum
        multisig.satisfied(
          datum.owner,
          ctx.transaction.extra_signatories,
          ctx.transaction.validity_range,
        )
      }
      Scoop ->
        dict.has_key(
          ctx.transaction.withdrawals,
          Inline(ScriptCredential(stake_script_hash)),
        )
    }
  }
}
