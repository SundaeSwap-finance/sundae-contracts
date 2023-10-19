use aiken/bytearray
use aiken/cbor
use aiken/dict.{Dict}
use aiken/hash.{Blake2b_256, Hash}
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, TransactionId,
}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{PolicyId}
use calculation/deposit
use calculation/donation
use calculation/shared.{PoolState,
  do_2_exp, unsafe_fast_index_skip} as calc_shared
use calculation/swap
use calculation/withdrawal
use shared.{datum_of}
use sundae/multisig
use types/order.{Destination, OrderDatum}
use types/pool.{PoolDatum, lp_token_name}

pub fn count_script_inputs(tx_inputs: List<Input>) -> Int {
  list.foldl(
    tx_inputs,
    0,
    fn(input, total) {
      when input.output.address.payment_credential is {
        VerificationKeyCredential(_) -> total
        _ -> total + 1
      }
    },
  )
}

pub fn pool_input_to_state(
  pool_token_policy: PolicyId,
  datum: PoolDatum,
  input: Output,
) -> PoolState {
  let PoolDatum {
    assets,
    rewards,
    identifier,
    circulating_lp,
    fees_per_10_thousand,
    ..
  } = datum
  let (asset_a, asset_b) = assets
  let (asset_a_policy_id, asset_a_name) = asset_a
  let (asset_b_policy_id, asset_b_name) = asset_b
  let min_utxo =
    if asset_a_policy_id == value.ada_policy_id {
      rewards
    } else {
      0
    }
  PoolState {
    quantity_a: (
      asset_a_policy_id,
      asset_a_name,
      value.quantity_of(input.value, asset_a_policy_id, asset_a_name) - min_utxo,
    ),
    quantity_b: (
      asset_b_policy_id,
      asset_b_name,
      value.quantity_of(input.value, asset_b_policy_id, asset_b_name),
    ),
    quantity_lp: (pool_token_policy, lp_token_name(identifier), circulating_lp),
    fees_per_10_thousand,
    // Pay the transaction fee out of the rewards
    rewards,
  }
}

pub fn process_orders(
  datums: Dict<Hash<Blake2b_256, Data>, Data>,
  initial: PoolState,
  input_order: List<Int>,
  inputs: List<Input>,
  outputs: List<Output>,
  uniqueness_flag: Int,
) -> PoolState {
  when input_order is {
    [] -> initial
    [idx, ..rest] -> {
      // Check that this idx is unique
      let n = do_2_exp(idx)
      let n_2 = 2 * n
      let y = uniqueness_flag + n
      expect uniqueness_flag % n_2 < y % n_2
      let order = unsafe_fast_index_skip(inputs, idx).output
      expect [output, ..rest_outputs] = outputs

      expect Some(datum) = datum_of(datums, order)
      expect datum: OrderDatum = datum
      let OrderDatum { destination, scooper_fee, order: order_details, .. } =
        datum

      when order_details is {
        order.Swap(offer, min_received) -> {
          let next =
            swap.do_swap(
              initial,
              order.value,
              destination,
              scooper_fee,
              offer,
              min_received,
              output,
            )
          process_orders(datums, next, rest, inputs, rest_outputs, y)
        }
        order.Deposit(..) -> {
          let next = deposit.do_deposit(initial, order.value, datum, output)
          process_orders(datums, next, rest, inputs, rest_outputs, y)
        }
        order.Withdrawal(..) -> {
          let next =
            withdrawal.do_withdrawal(initial, order.value, datum, output)
          process_orders(datums, next, rest, inputs, rest_outputs, y)
        }
        // order.Zap(..) -> do_zap(initial, input, datum)
        order.Donation(..) -> {
          let (next, used_output) =
            donation.do_donation(initial, order.value, datum, output)
          if used_output {
            process_orders(datums, next, rest, inputs, rest_outputs, y)
          } else {
            process_orders(datums, next, rest, inputs, outputs, y)
          }
        }
      }
    }
  }
}

test process_orders_test() {
  let addr =
    Address(
      VerificationKeyCredential(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      None,
    )
  let ada = (#"", #"")
  let rberry =
    (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  let pool_state =
    PoolState {
      quantity_a: (#"", #"", 1_000_000_000),
      quantity_b: (rberry.1st, rberry.2nd, 1_000_000_000),
      quantity_lp: (lp.1st, lp.2nd, 1_000_000_000),
      fees_per_10_thousand: 5,
      rewards: 2_000_000,
    }
  let order_datum =
    OrderDatum {
      owner: multisig.Signature(
        #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
      ),
      scooper_fee: 2_500_000,
      destination: Destination { address: addr, datum: NoDatum },
      order: order.Donation {
        assets: (
          (ada.1st, ada.2nd, 1_000_000),
          (rberry.1st, rberry.2nd, 1_000_000),
        ),
      },
      extension: Void,
    }
  // There's no remainder so do_donation totally ignores this Output record
  let output =
    Output {
      address: addr,
      value: value.from_lovelace(999_999_999_999_999_999),
      datum: NoDatum,
      reference_script: None,
    }

  let order_output =
    Output {
      address: addr,
      value: value.from_lovelace(1_000_000)
        |> value.add(rberry.1st, rberry.2nd, 1_000_000),
      datum: InlineDatum(order_datum),
      reference_script: None,
    }

  let order_datum_data: Data = order_datum
  let datums =
    dict.new()
      |> dict.insert(
           key: hash.blake2b_256(cbor.serialise(order_datum)),
           value: order_datum_data,
           compare: bytearray.compare,
         )

  let input =
    Input {
      output_reference: OutputReference {
        transaction_id: TransactionId {
          hash: #"0000000000000000000000000000000000000000000000000000000000000000",
        },
        output_index: 0,
      },
      output: order_output,
    }

  let input_order =
    [0]
  let inputs =
    [input]
  let outputs =
    [output]

  let final_pool_state =
    process_orders(datums, pool_state, input_order, inputs, outputs, 0)

  expect final_pool_state.quantity_a.3rd == 1_001_000_000
  expect final_pool_state.quantity_b.3rd == 1_001_000_000
  expect final_pool_state.rewards == 4_500_000
  True
}
