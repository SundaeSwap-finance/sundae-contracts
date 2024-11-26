use aiken/cbor.{serialise}
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/crypto.{
  Blake2b_256, Hash, VerificationKeyHash, blake2b_256, verify_ed25519_signature,
}
use aiken/interval.{Finite, Interval, IntervalBound}
use calculation/process.{find_pool_output}
use cardano/address.{Credential, from_verification_key}
use cardano/assets.{ada_asset_name, ada_policy_id}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, Transaction, ValidityRange,
}
use shared.{datum_of, is_script}
use sundae/multisig
use types/conditions/permissioned.{ComplianceToken,
  PermissionedDatum, TokenData} as permissioned_types
use types/order.{Fixed, OrderDatum, Swap}
use types/pool.{Manage, PoolDatum, PoolRedeemer, PoolScoop}

validator permissioned {
  withdraw(
    redeemer: PoolRedeemer,
    _account: Credential,
    transaction: Transaction,
  ) {
    // First, we destructure the transaction right upfront, because field access is O(n),
    // and we want access to these fields with just a single pass over the transaction
    // This will be a common pattern throughout the scripts
    // (in fact, I really want a compiler optimization that detects and lifts repeated field accesses into a destructure)
    let Transaction { inputs, outputs, validity_range, datums, .. } =
      transaction

    // Find the pool output, the output datum, and destructure it to access the fields we need to process the scoop
    let (
      Output { address: pool_output_address, .. },
      PoolDatum { condition_datum: actual_condition_datum_option, .. },
    ) = find_pool_output(outputs)

    // Then, (confusing terminology aside) find the specific pool UTXO being spent by this transaction
    expect Some(Input(_, pool_input)) =
      list.find(
        inputs,
        fn(input) { input.output.address == pool_output_address },
      )

    expect InlineDatum(pool_input_datum) = pool_input.datum

    expect PoolDatum { condition_datum: condition_datum_option, .. } =
      pool_input_datum

    expect Some(condition_datum) = condition_datum_option

    expect PermissionedDatum { whitelisted_oracles } = condition_datum

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
          scoop_permission(whitelisted_oracles, validity_range, datums, orders),
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

fn scoop_permission(
  oracles: List<VerificationKeyHash>,
  validity_range: ValidityRange,
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

      process_order(validity_range, datum, oracles)
    },
  )
}

/// Process a single order, comparing it to the output to ensure it was executed faithfully, and returning the new pool state
///
/// Most of the parameters here are for performance reasons, to avoid destructuring objects, since thats very expensive
fn process_order(
  // The validity range of the transaction, used to ensure the signed execution is within the correct time window
  validity_range: ValidityRange,
  // The details of the order to execute, such as whether it's a swap, the limit, etc.
  order: OrderDatum,
  // List of oracles that are whitelisted
  oracles: List<VerificationKeyHash>,
) -> Bool {
  // Has an attached compliance token
  expect compliance: ComplianceToken = order.extension

  // The owner of an order is the same as the public key in the token
  expect multisig.Signature(owner_sig) = order.owner

  expect Interval {
    lower_bound: IntervalBound { bound_type: Finite(tx_from), .. },
    upper_bound: IntervalBound { bound_type: Finite(tx_to), .. },
  } = validity_range

  expect Interval {
    lower_bound: IntervalBound { bound_type: Finite(token_from), .. },
    upper_bound: IntervalBound { bound_type: Finite(token_to), .. },
  } = compliance.token.validity_range

  let oracle_key_hash = blake2b_256(compliance.token.oracle_key)

  // The oracle public key is one of those listed in the approved oracles
  expect Some(..) = list.find(oracles, fn(o) { o == oracle_key_hash })
  and {
    owner_sig == compliance.token.user_key,
    // The destination of an order is the same as the one in the token
    order.destination == compliance.token.destination,
    // The order details hash to the same hash from the token
    blake2b_256(serialise(order.details)) == compliance.token.order_hash,
    // The valid range of the token entirely contains the valid range of the transaction
    token_from < tx_from,
    token_to > tx_to,
    // The signature is valid
    verify_ed25519_signature(
      compliance.token.oracle_key,
      serialise(compliance.token),
      compliance.oracle_signature,
    ),
  }
}

// oracle secret key: 2da637b0ab533b63f32d7adc3e1de5c26be33f878bf2364db3d4813c5873aac5
// oracle public key: 49e7cc40f180809b4d9e05030a09127883151517d4ed40948ad9ada2c81820ea

test process_order_with_signed_data() {
  let token_validity =
    Interval {
      lower_bound: IntervalBound { bound_type: Finite(0), is_inclusive: False },
      upper_bound: IntervalBound {
        bound_type: Finite(10000),
        is_inclusive: False,
      },
    }
  let tx_validity =
    Interval {
      lower_bound: IntervalBound {
        bound_type: Finite(1000),
        is_inclusive: False,
      },
      upper_bound: IntervalBound {
        bound_type: Finite(2000),
        is_inclusive: False,
      },
    }
  let user_pub_hash =
    #"0000000000000000000000000000000000000000000000000000000000000000"
  let destination =
    Fixed { address: from_verification_key(user_pub_hash), datum: NoDatum }
  let order =
    Swap {
      offer: (ada_policy_id, ada_asset_name, 10),
      min_received: (ada_policy_id, ada_asset_name, 10),
    }
  let oracle_key =
    #"49e7cc40f180809b4d9e05030a09127883151517d4ed40948ad9ada2c81820ea"
  let token_data =
    TokenData {
      did: #"0000",
      user_key: user_pub_hash,
      destination,
      order_hash: blake2b_256(serialise(order)),
      validity_range: token_validity,
      oracle_key,
    }

  // If the test case data changes use this trace to sign with the secret key commented above the test case
  trace serialise(token_data)

  let signature =
    #"93b0e5878a118198ff15cd82c6de676503b6d7c2158e4a148091033f583599d0ee63598d1f609a3953806103db193ddaf355ede27b34e7ca614251fdb183dd0d"

  let compliance_token =
    ComplianceToken { token: token_data, oracle_signature: signature }

  let order_datum =
    OrderDatum {
      pool_ident: None,
      owner: multisig.Signature(user_pub_hash),
      max_protocol_fee: 0,
      destination,
      details: order,
      extension: compliance_token,
    }

  let valid_oracles = [blake2b_256(oracle_key)]

  let valid_case =
    process_order(
      validity_range: tx_validity,
      order: order_datum,
      oracles: valid_oracles,
    )

  let wrong_details =
    process_order(
      validity_range: tx_validity,
      order: OrderDatum {
        ..order_datum,
        details: Swap {
          offer: (ada_policy_id, ada_asset_name, 20),
          min_received: (ada_policy_id, ada_asset_name, 10),
        },
      },
      oracles: valid_oracles,
    )

  let wrong_signature =
    process_order(
      validity_range: tx_validity,
      order: OrderDatum {
        ..order_datum,
        extension: ComplianceToken {
          ..compliance_token,
          token: TokenData { ..token_data, did: #"0010" },
        },
      },
      oracles: valid_oracles,
    )

  expect valid_case
  expect !wrong_details
  expect !wrong_signature
}
