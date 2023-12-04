use aiken/dict
use aiken/list
use aiken/transaction.{Transaction, ScriptContext, WithdrawFrom, Publish, InlineDatum}
use aiken/transaction/certificate.{CredentialRegistration, CredentialDelegation, CredentialDeregistration}
use aiken/transaction/value.{PolicyId}
use types/settings.{find_settings_datum}
use sundae/multisig

// We take in the settings_policy_id to read off thesettings
// and the instance ID so we can create N instances when the delegation gets large
validator(settings_policy_id: PolicyId, _instance: Int) {
  fn stake(_r: Data, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    let Transaction {
      reference_inputs,
      extra_signatories,
      validity_range,
      withdrawals,
      outputs,
      ..
    } = transaction

    let settings_datum = find_settings_datum(reference_inputs, settings_policy_id)

    when purpose is {
      WithdrawFrom(_) -> {
        // To withdraw funds, they must be paid in whole to the treasury
        expect multisig.satisfied(
          settings_datum.treasury_admin,
          extra_signatories,
          validity_range,
        )
        let amount_withdrawn = dict.foldl(withdrawals, 0, fn(_k, v, s) { v + s })
        let allowance = amount_withdrawn * settings_datum.treasury_allowance.1st / settings_datum.treasury_allowance.2nd
        let to_treasury = amount_withdrawn - allowance
        expect Some(treasury_output) = list.find(outputs, fn(o) { o.address == settings_datum.treasury_address })
        expect value.lovelace_of(treasury_output.value) >= to_treasury
        expect treasury_output.datum == InlineDatum(Void)
        True
      }
      Publish(cert) -> {
        when cert is {
          CredentialDelegation(..) | CredentialDeregistration(_) -> {
            // only the treasury administrator can change the delegation
            multisig.satisfied(
              settings_datum.treasury_admin,
              extra_signatories,
              validity_range,
            )
          }
          // Anyone can register the stake 
          CredentialRegistration(_) -> True
          _ -> False
        }
      }
      _ -> False
    }
  }
}