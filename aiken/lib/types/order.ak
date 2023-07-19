use aiken/bytearray
use aiken/cbor
use aiken/transaction.{Datum, NoDatum}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use shared.{SingletonValue}
use sundae/multisig.{MultisigScript}

// A SundaeSwap order consists of
pub type OrderDatum {
  // An owner that's allowed to modify it
  owner: MultisigScript,
  // A fee that the scooper is allowed to take for themselves
  // TODO: can we make this a value?
  scooper_fee: Int,
  // A destination that the results of the order must be sent to
  destination: Destination,
  // And the actual order to execute
  order: Order,
  // Leave space for extensions
  extension: Data,
}

test example_swap() {
  let addr =
    Address(
      VerificationKeyCredential(
        #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
      ),
      None,
    )
  let dest = Destination { address: addr, datum: NoDatum }
  let swap = Swap(((#"", #""), 1), ((#"", #""), 1))
  trace bytearray.to_hex(
    cbor.serialise(
      OrderDatum {
        owner: multisig.Signature(
          #"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad",
        ),
        scooper_fee: 2500000,
        destination: dest,
        order: swap,
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
  // Scoop it (in a specific order)
  Scoop
  // Or cancel/update it
  Cancel
}

test example_swap_redeemer() {
  trace bytearray.to_hex(cbor.serialise(Scoop))
  True
}
