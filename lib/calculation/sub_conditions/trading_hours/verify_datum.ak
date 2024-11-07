use calculation/shared.{millis_per_day}

pub fn trading_hours_verify_datum(open_time: Int, close_time: Int) -> Bool {
  and {
    open_time >= 0,
    open_time < millis_per_day,
    close_time >= 0,
    close_time < millis_per_day,
  }
}
