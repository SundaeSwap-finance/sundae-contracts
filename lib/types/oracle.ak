use cardano/transaction.{ValidityRange}
use shared.{Ident, SingletonValue}
use sundae/multisig

pub type OracleDatum {
  // The owner who is allowed to reclaim this datum at the end
  owner: multisig.MultisigScript,
  // The valid range for the scoop transaction that produced this Datum, which gives a confidence interval for when this price was valid
  valid_range: ValidityRange,
  // The pool identifier that this datum was produced for
  pool_ident: Ident,
  // The reserve *after* the scoop in question
  reserve_a: SingletonValue,
  reserve_b: SingletonValue,
  circulating_lp: SingletonValue,
}

pub type OracleRedeemer {
  // Mint an oracle, observing a specific pool ident, for the orders at
  // specific indices on the inputs
  Mint(Ident, List<Int>)
  // Burn the oracle token, so it can't be used to lie about the pool
  Burn
}
