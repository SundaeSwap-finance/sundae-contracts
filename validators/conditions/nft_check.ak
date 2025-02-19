use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/crypto.{Blake2b_256, Hash}
use calculation/process.{find_pool_output}
use cardano/address.{Address, Credential, Script, VerificationKey}
use cardano/assets.{Value, zero}
use cardano/transaction.{InlineDatum, Input, NoDatum, Output, Transaction}
use shared.{datum_of, is_script}
use sundae/multisig
use types/conditions/nft_check.{All,
  Any, Check, NftCheckDatum} as nft_check_types
use types/order.{Fixed, OrderDatum, Swap}
use types/pool.{Manage, PoolDatum, PoolRedeemer, PoolScoop}

validator nft_check {
  withdraw(
    redeemer: PoolRedeemer,
    _account: Credential,
    transaction: Transaction,
  ) {
    // First, we destructure the transaction right upfront, because field access is O(n),
    // and we want access to these fields with just a single pass over the transaction
    // This will be a common pattern throughout the scripts
    // (in fact, I really want a compiler optimization that detects and lifts repeated field accesses into a destructure)
    let Transaction { inputs, outputs, reference_inputs, datums, .. } =
      transaction

    // Find the pool output, the output datum, and destructure it to access the fields we need to process the scoop
    let (
      Output { address: pool_output_address, .. },
      PoolDatum { condition_datum: actual_condition_datum_option, .. },
    ) = find_pool_output(outputs)

    // Then, (confusing terminology aside) find the specific pool UTXO being spent by this transaction
    expect Some(Input { output: pool_input, .. }) =
      list.find(
        inputs,
        fn(input) { input.output.address == pool_output_address },
      )

    expect InlineDatum(pool_input_datum) = pool_input.datum

    expect PoolDatum { condition_datum: condition_datum_option, .. } =
      pool_input_datum

    expect Some(condition_datum) = condition_datum_option

    expect nft_check_datum: NftCheckDatum = condition_datum

    let nft_check_value = assets.from_asset_list(nft_check_datum.value)

    let reference_inputs_outputs =
      list.map(reference_inputs, fn(i) { i.output })

    // Then, there are two different actions that can be taken against a pool:
    // - Scooping a batch of orders
    // - Withdrawing protocol fees to the treasury
    when redeemer is {
      // In the case of the scoop, the redeemer indicates which scooper is doing the scoop, and the order in which the inputs should be processed
      PoolScoop { .. } -> {
        let orders =
          list.filter(
            inputs,
            fn(i) {
              and {
                i.output != pool_input,
                is_script(i.output.address.payment_credential),
              }
            },
          )
            |> list.map(fn(i) { i.output })
        and {
          scoop_nft_check(
            nft_check_value,
            nft_check_datum.check,
            reference_inputs_outputs,
            datums,
            orders,
          ),
          actual_condition_datum_option == condition_datum_option,
        }
      }
      Manage -> False
    }
  }

  else(_) {
    fail
  }
}

fn scoop_nft_check(
  nft_check_value: Value,
  nft_check: Check,
  reference_inputs: List<Output>,
  datums: Dict<Hash<Blake2b_256, Data>, Data>,
  orders: List<Output>,
) -> Bool {
  list.all(
    orders,
    fn(order) {
      // Find the datum that is associated with this order; we allow that datum to be either inline, or in the witness set,
      // to aid in composibility with other protocols
      // We also check that the datum is in the format we expect;
      // Note: we don't actually check the order address anywhere!! As long as it's a script, and the datum is in the correct format, we're good.
      // This lets us upgrade the order contract, or add other types of orders over time.
      expect Some(datum) = datum_of(datums, order)
      expect datum: OrderDatum = datum

      process_order(datum, nft_check_value, nft_check, reference_inputs)
    },
  )
}

/// Process a single order, ensuring that the provided order datum has a valid ComplianceToken
fn process_order(
  // The details of the order to execute, such as whether it's a swap, the limit, etc.
  order: OrderDatum,
  nft_check_value: Value,
  nft_check: Check,
  reference_inputs: List<Output>,
) -> Bool {
  // The owner of an order is the same as the public key in the token
  expect multisig.Signature { key_hash: owner } = order.owner

  let owner_value = owner_value(find_owner_outputs(reference_inputs, owner))

  when nft_check is {
    All ->
      list.all(
        assets.flatten(nft_check_value),
        fn(t) { assets.quantity_of(owner_value, t.1st, t.2nd) >= t.3rd },
      )
    Any ->
      list.any(
        assets.flatten(nft_check_value),
        fn(t) {
          if t.2nd != #"" {
            assets.quantity_of(owner_value, t.1st, t.2nd) >= t.3rd
          } else {
            list.foldl(
              dict.to_pairs(assets.tokens(owner_value, t.1st)),
              0,
              fn(t, acc) { acc + t.2nd },
            ) >= t.3rd
          }
        },
      )
  }
}

fn find_owner_outputs(outputs: List<Output>, owner: ByteArray) -> List<Output> {
  list.filter(
    outputs,
    fn(o) {
      when o.address.payment_credential is {
        VerificationKey(ri_owner) -> ri_owner == owner
        _ -> False
      }
    },
  )
}

fn owner_value(outputs: List<Output>) -> Value {
  list.foldl(outputs, zero, fn(o, acc) { assets.merge(acc, o.value) })
}

test test_scoop_nft_check() {
  let nft_check_value =
    assets.from_asset_list(
      [Pair("token1", [Pair("", 10)]), Pair("token2", [Pair("subtoken", 5)])],
    )

  let nft_check = Any

  let reference_inputs =
    [
      Output {
        address: Address {
          payment_credential: VerificationKey("owner1"),
          stake_credential: None,
        },
        value: assets.from_asset_list([Pair("token1", [Pair("", 10)])]),
        datum: NoDatum,
        reference_script: None,
      },
      Output {
        address: Address {
          payment_credential: VerificationKey("owner2"),
          stake_credential: None,
        },
        value: assets.from_asset_list([Pair("token2", [Pair("subtoken", 5)])]),
        datum: NoDatum,
        reference_script: None,
      },
    ]

  let datums = dict.empty

  let order_datum1 =
    OrderDatum {
      owner: multisig.Signature { key_hash: "owner1" },
      pool_ident: None,
      max_protocol_fee: 0,
      destination: Fixed {
        address: Address {
          payment_credential: VerificationKey("owner1"),
          stake_credential: None,
        },
        datum: NoDatum,
      },
      details: Swap(("ada", "lovelace", 10), ("rberry", "berry", 5)),
      extension: Void,
    }

  let order1 =
    Output {
      address: Address {
        payment_credential: Script("order"),
        stake_credential: None,
      },
      value: assets.zero,
      datum: InlineDatum(order_datum1),
      reference_script: None,
    }

  let order_datum2 =
    OrderDatum {
      owner: multisig.Signature { key_hash: "owner2" },
      pool_ident: None,
      max_protocol_fee: 0,
      destination: Fixed {
        address: Address {
          payment_credential: VerificationKey("owner2"),
          stake_credential: None,
        },
        datum: NoDatum,
      },
      details: Swap(("ada", "lovelace", 10), ("rberry", "berry", 5)),
      extension: Void,
    }

  let order2 =
    Output {
      address: Address {
        payment_credential: Script("order"),
        stake_credential: None,
      },
      value: assets.zero,
      datum: InlineDatum(order_datum2),
      reference_script: None,
    }

  let orders = [order1, order2]

  scoop_nft_check(nft_check_value, nft_check, reference_inputs, datums, orders)
}
