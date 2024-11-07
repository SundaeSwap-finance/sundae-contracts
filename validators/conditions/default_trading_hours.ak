use aiken/collection/list
use calculation/sub_conditions/default/process.{scoop_default} as default_process
use calculation/sub_conditions/default/verify_datum.{default_verify_datum} as default_verify_datum
use calculation/sub_conditions/trading_hours/process.{scoop_trading_hours} as trading_hours_process
use calculation/sub_conditions/trading_hours/verify_datum.{
  trading_hours_verify_datum,
} as trading_hours_verify_datum
use cardano/address.{Credential, Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use types/condition_pool.{
  ConditionManage, ConditionPoolDatum, ConditionPoolRedeemer, ConditionPoolScoop,
  find_pool_output,
}
use types/conditions/default_trading_hours.{DefaultTradingHoursDatum} as default_types
use types/settings.{find_settings_datum}

validator default_trading_hours(settings_policy_id: PolicyId) {
  withdraw(
    redeemer: ConditionPoolRedeemer,
    _account: Credential,
    transaction: Transaction,
  ) {
    // First, we destructure the transaction right upfront, because field access is O(n),
    // and we want access to these fields with just a single pass over the transaction
    // This will be a common pattern throughout the scripts
    // (in fact, I really want a compiler optimization that detects and lifts repeated field accesses into a destructure)
    let Transaction {
      inputs,
      outputs,
      reference_inputs,
      mint,
      datums,
      extra_signatories,
      validity_range,
      withdrawals,
      ..
    } = transaction

    // Find the pool output, the output datum, and destructure it to access the fields we need to process the scoop
    let (
      Output { address: pool_output_address, value: pool_output_value, .. },
      ConditionPoolDatum {
        identifier: actual_identifier,
        circulating_lp: actual_circulating_lp,
        protocol_fees: actual_protocol_fees,
        assets: actual_assets,
        market_open: actual_market_open,
        condition_datum: actual_condition_datum,
        ..
      },
    ) = find_pool_output(outputs)

    expect DefaultTradingHoursDatum {
      bid_fees_per_10_thousand: actual_bid_fees_per_10_thousand,
      ask_fees_per_10_thousand: actual_ask_fees_per_10_thousand,
      ..
    } = actual_condition_datum

    when redeemer is {
      ConditionPoolScoop { .. } | ConditionManage -> {
        // Then, (confusing terminology aside) find the specific pool UTXO being spent by this transaction
        expect Some(Input(_, pool_input)) =
          list.find(
            inputs,
            fn(input) { input.output.address == pool_output_address },
          )

        // And pattern match to get the pool script hash; in particular, this can be used to find the pool output,
        // *and* to know the policy ID of pool tokens, because this is a dual spending/minting validator.
        expect Script(pool_script_hash) = pool_input.address.payment_credential

        expect InlineDatum(pool_input_datum) = pool_input.datum

        expect ConditionPoolDatum {
          identifier,
          circulating_lp,
          protocol_fees,
          assets,
          market_open,
          condition_datum,
          ..
        } = pool_input_datum

        expect DefaultTradingHoursDatum {
          bid_fees_per_10_thousand,
          ask_fees_per_10_thousand,
          open_time,
          close_time,
        } = condition_datum

        // The protocol configures many global settings via a "settings" UTXO, updatable by certain administrators
        // This is included as a reference input, so we have a utility to check the reference inputs for the settings NFT
        // Note: it's important to check for the NFT, because checking just for the address would let someone pay random funds to the settings address.
        let settings_datum =
          find_settings_datum(reference_inputs, settings_policy_id)

        // Then, there are two different actions that can be taken against a pool:
        // - Scooping a batch of orders
        // - Withdrawing protocol fees to the treasury
        when redeemer is {
          // In the case of the scoop, the redeemer indicates which scooper is doing the scoop, and the order in which the inputs should be processed
          ConditionPoolScoop { signatory_index, scooper_index, input_order, .. } ->
            and {
              scoop_default(
                settings_datum,
                inputs,
                pool_script_hash,
                actual_ask_fees_per_10_thousand,
                actual_bid_fees_per_10_thousand,
                pool_input,
                actual_identifier,
                validity_range,
                withdrawals,
                datums,
                input_order,
                outputs,
                actual_protocol_fees,
                mint,
                extra_signatories,
                signatory_index,
                scooper_index,
                pool_output_value,
                actual_circulating_lp,
                actual_assets,
                actual_market_open,
                pool_output_address,
                assets,
                protocol_fees,
                identifier,
                circulating_lp,
                market_open,
                bid_fees_per_10_thousand,
                ask_fees_per_10_thousand,
              ),
              scoop_trading_hours(validity_range, open_time, close_time),
            }
          ConditionManage -> and {
              default_verify_datum(
                bid_fees_per_10_thousand,
                ask_fees_per_10_thousand,
              ),
              trading_hours_verify_datum(open_time, close_time),
            }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
