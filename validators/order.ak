use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{Inline, Script, ScriptCredential}
use sundae/multisig
use types/order.{Cancel, OrderDatum, OrderRedeemer, Scoop}

/// The order validator is a basic order destined for execution against a pool
///
/// Other orders can be implemented as well, by using the same datum format, and
/// updating the scoopers to know how to index and include these orders
/// This would likely be done via a DAO vote.
///
/// The validator is parameterized by a specific stake script, more on that below.
///
/// Has two possible execution paths:
/// - Cancel: the "owner" signs the transaction, allowing the UTXO be spent; we impose
///   no additional constraints, so this could also be used to update an order by
///   spending it back into the same script.
///   Note: we use sundae.multisig, which allows multisig orders, or even orders that can
///   be "cancelled" by a script!
///
/// - Scoop: the order is processed as a batch against a pool.
///   Instead of re-checking the conditions in every single script, we instead use a clever trick
///   We look for the stake_script_hash in the list of withdrawals!
///   This means that the stake script must run, and it can always run, because withdrawals of 0 tokens are allowed!
///   That stake_script then checks that a pool NFT is present on the UTXO, where most of the subtle and important logic is implemented. 
validator(stake_script_hash: Hash<Blake2b_224, Script>) {
  // For the purposes of spending the order, we don't care what the datum is, so avoid deserializing it
  fn spend(datum: Data, redeemer: OrderRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      Cancel -> {
        // We only expect the datum here, to avoid any related costs in the other branch, which doesn't need it
        expect datum: OrderDatum = datum
        // Defer to sundae.multisig
        multisig.satisfied(
          datum.owner,
          ctx.transaction.extra_signatories,
          ctx.transaction.validity_range,
          ctx.transaction.withdrawals,
        )
      }
      Scoop -> {
        dict.foldl(ctx.transaction.withdrawals, False, fn(withdrawal, _amt, acc) {
          when withdrawal is {
            // TODO: we could make this more efficient by CBOR encoding the `Inline(ScriptCredential())` into the `stake_script_hash` parameter
            // or perhaps even the whole withdrawal list to compare it all at once!
            // and just doing a direct equaltiy comparison. Since this runs for each order, this could represent significant savings!
            Inline(ScriptCredential(script)) -> acc || script == stake_script_hash
            _ -> False
          }
        })
      }
    }
  }
}
