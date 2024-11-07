use shared

pub fn default_verify_datum(
  bid_fees_per_10_thousand: Int,
  ask_fees_per_10_thousand: Int,
) -> Bool {
  and {
    shared.fees_in_legal_range(bid_fees_per_10_thousand),
    shared.fees_in_legal_range(ask_fees_per_10_thousand),
  }
}
