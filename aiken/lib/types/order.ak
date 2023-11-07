use aiken/bytearray
use aiken/cbor
use aiken/transaction.{Datum, NoDatum, OutputReference, ValidityRange}
use aiken/transaction/credential.{Address, VerificationKey, VerificationKeyCredential, Signature}
use shared.{Ident, SingletonValue}
use sundae/multisig.{MultisigScript}

// A SundaeSwap order consists of
pub type OrderDatum {
  pool_ident: Option<Ident>,
  // An owner that's allowed to modify it
  owner: MultisigScript,
  // The maximum amount of ADA that can be taken as a protocol fee
  max_protocol_fee: Int,
  // A destination that the results of the order must be sent to
  destination: Destination,
  // And the actual order to execute
  details: Order,
  // Leave space for extensions
  extension: Data,
}

test example_swap() {
  let addr =
    Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let dest = Destination { address: addr, datum: NoDatum }
  let swap =
    Swap(
      (#"", #"", 10000000),
      (
        #"d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf",
        #"44554d4d59",
        0,
      ),
    )
  trace bytearray.to_hex(
    cbor.serialise(
      OrderDatum {
        pool_ident: None,
        owner: multisig.Signature(
          #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
        ),
        max_protocol_fee: 2500000,
        destination: dest,
        details: swap,
        extension: Void,
      },
    ),
  )
  True
}

test example_deposit() {
  let addr =
    Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let dest = Destination { address: addr, datum: NoDatum }
  let deposit =
    Deposit((
      (#"", #"", 10000000),
      (
        #"d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf",
        #"44554d4d59",
        10000000,
      ),
    ))
  trace bytearray.to_hex(
    cbor.serialise(
      OrderDatum {
        pool_ident: None,
        owner: multisig.Signature(
          #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
        ),
        max_protocol_fee: 2500000,
        destination: dest,
        details: deposit,
        extension: Void,
      },
    ),
  )
  True
}

test example_withdrawal() {
  let addr =
    Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let dest = Destination { address: addr, datum: NoDatum }
  let withdrawal =
    Withdrawal(
      (
        #"d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf",
        #"6c",
        10000000
      ),
    )
  trace bytearray.to_hex(
    cbor.serialise(
      OrderDatum {
        pool_ident: None,
        owner: multisig.Signature(
          #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
        ),
        max_protocol_fee: 2500000,
        destination: dest,
        details: withdrawal,
        extension: Void,
      },
    ),
  )
  True
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
  // A zap of at most these assets on either side of the pair
  // Zap { assets: (SingletonValue, SingletonValue) }
  // Donate some value to the pool
  Donation { assets: (SingletonValue, SingletonValue) }
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

test example_swap_redeemer() {
  trace bytearray.to_hex(cbor.serialise(Scoop))
  True
}
