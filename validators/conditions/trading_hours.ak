use aiken/collection/list
use aiken/interval.{Finite, Interval, IntervalBound}
use calculation/process.{find_pool_output}
use calculation/shared.{millis_per_day}
use cardano/address.{Credential}
use cardano/transaction.{InlineDatum, Input, Output, Transaction, ValidityRange}
use types/conditions/trading_hours.{TradingHoursDatum} as trading_hours_types
use types/pool.{Manage, PoolDatum, PoolRedeemer, PoolScoop}

validator trading_hours {
  withdraw(
    redeemer: PoolRedeemer,
    _account: Credential,
    transaction: Transaction,
  ) {
    // First, we destructure the transaction right upfront, because field access is O(n),
    // and we want access to these fields with just a single pass over the transaction
    // This will be a common pattern throughout the scripts
    // (in fact, I really want a compiler optimization that detects and lifts repeated field accesses into a destructure)
    let Transaction { inputs, outputs, validity_range, .. } = transaction

    // Find the pool output, the output datum, and destructure it to access the fields we need to process the scoop
    let (
      Output { address: pool_output_address, .. },
      PoolDatum { condition_datum: actual_condition_datum_option, .. },
    ) = find_pool_output(outputs)

    // Then, (confusing terminology aside) find the specific pool UTXO being spent by this transaction
    expect Some(Input(_, pool_input)) =
      list.find(
        inputs,
        fn(input) { input.output.address == pool_output_address },
      )

    expect InlineDatum(pool_input_datum) = pool_input.datum

    expect PoolDatum { condition_datum: condition_datum_option, .. } =
      pool_input_datum

    expect Some(condition_datum) = condition_datum_option

    expect TradingHoursDatum { open_time, close_time } = condition_datum

    // Then, there are two different actions that can be taken against a pool:
    // - Scooping a batch of orders
    // - Withdrawing protocol fees to the treasury
    when redeemer is {
      // In the case of the scoop, the redeemer indicates which scooper is doing the scoop, and the order in which the inputs should be processed
      PoolScoop { .. } -> and {
          scoop_trading_hours(validity_range, open_time, close_time),
          actual_condition_datum_option == condition_datum_option,
        }
      Manage -> False
    }
  }

  else(_) {
    fail
  }
}

fn scoop_trading_hours(
  validity_range: ValidityRange,
  open_time: Int,
  close_time: Int,
) -> Bool {
  expect Interval {
    lower_bound: IntervalBound { bound_type: Finite(low_val), .. },
    upper_bound: IntervalBound { bound_type: Finite(high_val), .. },
  } = validity_range
  let low_val_d = low_val % millis_per_day
  let high_val_d = high_val % millis_per_day
  let validity_less_than_1_day = high_val - low_val < millis_per_day
  and {
    validity_less_than_1_day,
    time_between_start_finish(low_val_d, open_time, close_time),
    time_between_start_finish(high_val_d, open_time, close_time),
  }
}

fn time_between_start_finish(time: Int, start: Int, finish: Int) -> Bool {
  if start < finish {
    and {
      time > start,
      time < finish,
    }
  } else {
    or {
      and {
        time < start,
        time < finish,
      },
      and {
        time > start,
        time > finish,
      },
    }
  }
}
