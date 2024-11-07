pub type DefaultDatum {
  /// The basis points to charge on each trade for bid (A -> B) and ask (B -> A) orders
  /// For example, a 1% fee would be represented as 100 (out of 10,000), and a 0.3% fee would be represented as 30
  bid_fees_per_10_thousand: Int,
  ask_fees_per_10_thousand: Int,
}
