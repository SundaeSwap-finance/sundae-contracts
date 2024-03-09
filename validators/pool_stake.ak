use aiken/dict
use aiken/list
use aiken/transaction.{Transaction, ScriptContext, WithdrawFrom, Publish, InlineDatum}
use aiken/transaction/value.{PolicyId}
use types/settings.{find_settings_datum}
use sundae/multisig

/// The pool stake validator is one example script staking validator that could be attached to each pool, to delegate to a stake pool operator
///
/// It is parameterized by the settings policy ID, so we can check for the treasury administrator,
/// and an arbitrary _instance property. This can be treated as a nonce, so we can generate N different script hashes with the same behavior
/// so we can delegate to different pools.
///
/// Note that this scheme is highly future proof; when the Conway hardfork happens and adds new script purposes
/// for governance, normally that would mean the protocol could never take advantage of them, because the Plutus v2 scripts
/// would fail on the new script context.
/// However, in this case, the treasury admin can simply update to a new staking policy, rotate that onto the pools, and then use
/// the new features. This is because the staking script doesn't actually run when the pool runs! it only runs in its own transaction
/// so the fact that it becomes Plutus v66 makes no difference. 
validator(settings_policy_id: PolicyId, _instance: Int) {
  fn stake(_r: Data, ctx: ScriptContext) -> Bool {
    // Deconstruct the script context and transaction to pull off the fields we'll need
    // This is to avoid destructing many times, because each field access is O(n)
    let ScriptContext { transaction, purpose } = ctx
    let Transaction {
      reference_inputs,
      extra_signatories,
      validity_range,
      withdrawals,
      outputs,
      ..
    } = transaction

    // And find the settings datum; this ensures it exists, and lets us look at the settings datum
    let settings_datum = find_settings_datum(reference_inputs, settings_policy_id)
    
    // Regardless of what we're doing, the treasury administrator conditions must be satisfied (ex: multisig, DAO script, etc)
    expect multisig.satisfied(
      settings_datum.treasury_admin,
      extra_signatories,
      validity_range,
    )

    when purpose is {
      // To withdraw any amount of earned staking rewards,
      WithdrawFrom(_) -> {
        
        // We calculate the amount withdrawn by summing up *all* withdrawals
        // This is to avoid a double satisfaction problem, where withdrawing 100 ADA of rewards from
        // two different scripts each believe that 100 ADA has been paid to the treasury
        let amount_withdrawn = dict.foldl(withdrawals, 0, fn(_k, v, s) { v + s })
        // Allow the treasury administrator to keep some portion of the withdrawn rewards, for administrative costs like paying the scoopers
        // We multiply first, then divide, for maximal precision
        let allowance = amount_withdrawn * settings_datum.treasury_allowance.1st / settings_datum.treasury_allowance.2nd
        // The amount that must be sent to the treasury is the total amount withdrawn, minus the allowance that the adminsitrator can keep
        let to_treasury = amount_withdrawn - allowance
        // Find the first output to the treasury address; We assume there's only one output that receives the whole amount,
        // as there's not much benefit to splitting it up. TODO: is this true? could the treasury administrator want to earmark them separately or something?
        // If we find no treasury output, the transaction fails here.
        // We compare the *whole* address here, instead of the payment credential, because the treasury should also be earning staking rewards!
        expect Some(treasury_output) = list.find(outputs, fn(o) { o.address == settings_datum.treasury_address })
        // Make sure *at least* the amount we calculated is sent to the treasury; it can be larger,
        // if someone is feeling generous, or to deal with rounding for example, but it cannot be less
        expect value.lovelace_of(treasury_output.value) >= to_treasury
        // We enforce that the treasury is paid with a null datum
        // Sending to a script with *no* datum is very dangerous, and letting an arbitrary datum could
        // allow shenaigans.
        // If we wish to have a treasury script with a specific datum structure, then we can implement a proxy script and set that as the
        // treasury address
        treasury_output.datum == InlineDatum(Void)
      }
      // Publishing a cert succeeds, because we've already checked that the multisig script was satisfied above
      Publish(_) -> True
      // Prevent minting or spending, just in case
      _ -> False
    }
  }
}