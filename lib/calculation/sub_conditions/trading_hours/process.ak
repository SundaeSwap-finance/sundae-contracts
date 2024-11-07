use aiken/interval.{Finite, Interval, IntervalBound}
use calculation/shared.{millis_per_day}
use cardano/transaction.{ValidityRange}

pub fn scoop_trading_hours(
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
