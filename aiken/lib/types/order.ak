use aiken/bytearray
use aiken/cbor
use aiken/transaction.{Datum, NoDatum}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use shared.{Ident, SingletonValue}
use sundae/multisig.{MultisigScript}

// A SundaeSwap order consists of
pub type OrderDatum {
  pool_ident: Option<Ident>,
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
        scooper_fee: 2500000,
        destination: dest,
        order: swap,
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
        scooper_fee: 2500000,
        destination: dest,
        order: deposit,
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
        owner: multisig.Signature(
          #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
        ),
        scooper_fee: 2500000,
        destination: dest,
        order: withdrawal,
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
