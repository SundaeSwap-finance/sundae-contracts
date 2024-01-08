use aiken/transaction.{Datum, OutputReference, ValidityRange}
use aiken/transaction/credential.{Address, VerificationKey, Signature}
use shared.{Ident, SingletonValue}
use sundae/multisig.{MultisigScript}

// A SundaeSwap order consists of
pub type OrderDatum {
  pool_ident: Option<Ident>,
  // An owner that's allowed to modify it
  owner: MultisigScript,
  // The maximum amount of ADA that can be taken as a protocol fee
  // TODO: consider; with strategies, is this too dangerous?
  max_protocol_fee: Int,
  // A destination that the results of the order must be sent to
  destination: Destination,
  // And the actual order to execute
  details: Order,
  // Leave space for extensions
  extension: Data,
}

// A destination consists of
pub type Destination {
  // An address
  address: Address,
  // And a Datum
  datum: Datum,
}

// An order is either
pub type Order {
  // The details of the order aren't yet determined, but will be communicated out of band to the scoopers
  Strategy { signer: VerificationKey }
  // A swap, offering some amount of the UTXO, in return for a minimum payout
  Swap { offer: SingletonValue, min_received: SingletonValue }
  // A deposit, offering at most these quantities (at most, to aid in composibility)
  Deposit { assets: (SingletonValue, SingletonValue) }
  // A withdrawal of at most this many LP tokens
  Withdrawal { amount: SingletonValue }
  // Deposit these assets into the pool, and mint the appropriate number of LP tokens
  // ZapIn { assets: (SingletonValue, SingletonValue) }
  // ZapOut { assets: (Int, Int) }
  // Donate some value to the pool
  Donation { assets: (SingletonValue, SingletonValue) }
  // Create an oracle UTXO, for other protocols to read the price at a given time
  Oracle
}

// In order to redeem an order, you can either
pub type OrderRedeemer {
  // Execute the order
  Scoop
  // Or cancel/update it
  Cancel
}

// The details used to execute an order of type "Strategy"
pub type StrategyExecution {
  // The order that the execution is "valid" for
  tx_ref: OutputReference,
  // For what time window is this strategy valid
  validity_range: ValidityRange,
  // The details for how the order should be processed
  details: Order,
}

// A signed instance of StrategyExecution
pub type SignedStrategyExecution {
  // The strategy being signed
  strategy: StrategyExecution,
  // The signature of the bytes of Strategy
  signature: Signature,
}