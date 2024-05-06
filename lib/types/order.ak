use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction.{Datum, OutputReference, ValidityRange}
use aiken/transaction/credential.{Address, Script, Signature, VerificationKey}
use aiken/transaction/value.{AssetName, PolicyId}
use shared.{Ident, SingletonValue}
use sundae/multisig.{MultisigScript}

/// An order to execute within the SundaeSwap ecosystem
pub type OrderDatum {
  /// An optional pool identifier against which this order must be executed
  /// If set to none, the order is allowed to execute against any pool with the
  /// correct assets.
  pool_ident: Option<Ident>,
  /// An canonical "owner" of this order, with the possibility of multisig
  /// schemes and script restrictions. Allows updates or cancellations of the order
  owner: MultisigScript,
  // TODO: consider: does this undercut the usefulness of strategies? i.e.
  // might the strategy executor need to adjust the max_protocol_fee? or would that be too dangerous?
  /// The maximum amount of ADA that can be taken as a protocol fee
  /// For example, a user would typically set this to baseFee + incrementalFee
  /// which allows their order to execute as the only order in a batch; if it
  /// executes as a larger batch, the user may receive some of this ADA back as a rebate.
  max_protocol_fee: Int,
  /// Where the output of executing an order should be sent
  destination: Destination,
  /// A description of how to actually execute the order
  details: Order,
  /// The pool contracts should work with any order contract that adheres to this datum;
  /// to aid in extensibility, we leave room here for arbitrary extension data that other
  /// order scripts might want to take advantage of.
  extension: Data,
}

/// The destination where funds are to be sent after the order
pub type Destination {
  Fixed {
    /// The address to attach to the output (such as the users wallet, or another script)
    address: Address,
    /// The datum to attach to the output; Could be none in typical cases, but could also be used to compose with other protocols,
    /// such as paying the results of a swap back to a treasury contract
    datum: Datum,
  }
  /// The result gets paid back to the exact same address and datum it came from; useful for strategies
  /// with unbounded legs
  Self
}

/// There are two ways to delegate authorization of a strategy::
/// - by a signing key
/// - or by some script in the withdrawals of the transaction 
pub type StrategyAuthorization {
  Signature { signer: VerificationKey }
  Script { script: Hash<Blake2b_224, Script> }
}

/// The specific order details for executing an order
pub type Order {
  /// The details of the order aren't yet determined, but will be communicated out of band to the scoopers
  /// This can be thought of as "late-binding" the order details, so long as the strategy is signed by `signer`
  /// For example, you could use this to implement trailing stop loss, where the order to sell a token is only triggered
  /// after a specific threshold is met.
  Strategy { auth: StrategyAuthorization }
  /// A swap from one token to another, offering some subset of the UTXO, in return for at least some quantity of another token
  Swap { offer: SingletonValue, min_received: SingletonValue }
  /// A deposit, offering at most two quantities from the UTXO to provide liquidity to the pool, resulting in LP tokens paid to the output to track an ownership percentage of the pool
  /// We use an at-most semantics to aide in composibility; perhaps the deposit is part of a longer chain, and you only want to deposit a portion of the full asset.
  Deposit { assets: (SingletonValue, SingletonValue) }
  /// A withdrawal, which will burn some LP tokens in return for the corresponding portion of the pool
  /// We use an at-most amount semantics to aide in composibility; perhaps the withdrawal is part of a longer chain,
  /// and you only want to withdraw a portion of your full LP tokens
  Withdrawal { amount: SingletonValue }
  // Note: we decided not to implement native ZapIn / ZapOut for the purposes of time; they can be implemented via chained orders or strategies.
  // ZapIn { assets: (SingletonValue, SingletonValue) }
  // ZapOut { assets: (Int, Int) }
  /// Deposit these assets into the pool, and mint the appropriate number of LP tokens
  /// Donate some value to the pool; Likely not useful for an end user, but lets other
  /// protocols create interesting options: for example, an incentive program for their liquidity providers, etc.
  Donation { assets: (SingletonValue, SingletonValue) }
  // Record, like the verb; let some script witness a scoop, and enforce some property about the output
  // Used, for example, to record the state of the pool for an on-chain oracle, etc.
  Record { policy: (PolicyId, AssetName) }
}

/// An order can be spent either to Scoop (execute) it, or to cancel it
pub type OrderRedeemer {
  /// Execute the order
  Scoop
  /// Or cancel/update it
  Cancel
}

/// The details used to execute an order of type "Strategy"
pub type StrategyExecution {
  /// The order that the execution is "valid" for
  /// This prevents re-use of a signature against a different order
  tx_ref: OutputReference,
  /// For what time window is this strategy valid
  /// This allows specific time-sensitive strategies
  validity_range: ValidityRange,
  /// The details for how the order should be processed
  /// These are the late-bound details that the delegated authority chose to execute
  /// likely based on some arbitrary off-chain trigger condition
  details: Order,
  /// This extension data allows arbitrary other data to be attached that the pool
  /// doesn't care about, but that the order script might
  extensions: Data,
}

/// A specific strategy execution, plus a signature of that data
pub type SignedStrategyExecution {
  /// The details about how to execute the strategy for a given order
  strategy: StrategyExecution,
  /// An ed25519 signature of the serialized `strategy`
  signature: Option<Signature>,
}
