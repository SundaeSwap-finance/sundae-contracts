use aiken/bytearray
use aiken/cbor
use aiken/dict.{Dict}
use aiken/hash.{Blake2b_256, Hash}
use aiken/interval.{IntervalBound, NegativeInfinity, Finite}
use aiken/transaction.{InlineDatum, NoDatum, Input, Output, OutputReference, TransactionId}
use aiken/transaction/credential.{Address, VerificationKeyCredential}
use aiken/transaction/value.{PolicyId}
use aiken/time.{PosixTime}
use calculation/deposit
use calculation/donation
use calculation/shared.{
  PoolState, check_and_set_unique, unsafe_fast_index_skip_with_tail,
} as calc_shared
use calculation/swap
use calculation/withdrawal
use shared.{Ident, datum_of, pool_lp_name}
use sundae/multisig
use types/order.{Destination, OrderDatum, SignedStrategyExecution}
use types/pool.{PoolDatum}

pub fn pool_input_to_state(
  pool_token_policy: PolicyId,
  datum: PoolDatum,
  input: Output,
  valid_from: IntervalBound<PosixTime>,
) -> PoolState {
  let PoolDatum {
    assets,
    protocol_fees,
    identifier,
    circulating_lp,
    fees_per_10_thousand,
    market_open,
    fee_finalized,
  } = datum
  let (asset_a, asset_b) = assets
  let (asset_a_policy_id, asset_a_name) = asset_a
  let (asset_b_policy_id, asset_b_name) = asset_b
  let min_utxo =
    if asset_a_policy_id == value.ada_policy_id {
      protocol_fees
    } else {
      0
    }
  let valid_from = when valid_from.bound_type is {
    NegativeInfinity -> market_open
    Finite(t) -> t
    _ -> error
  }
  // Let the fees decay linearly up until fee_finalized
  let fees =
    if valid_from > fee_finalized {
      fees_per_10_thousand.2nd
    } else {
      let duration = fee_finalized - market_open
      if duration == 0 {
        fees_per_10_thousand.2nd
      } else {
        let elapsed = valid_from - market_open
        let range = fees_per_10_thousand.2nd - fees_per_10_thousand.1st
        fees_per_10_thousand.1st + (elapsed * range / duration)
      }
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
    quantity_lp: (pool_token_policy, pool_lp_name(identifier), circulating_lp),
    fees_per_10_thousand: fees,
    // Pay the transaction fee out of the protocol_fees
    protocol_fees,
  }
}

fn validate_pool_id(order_pool_ident: Option<Ident>, pool_ident: Ident) -> Bool {
  when order_pool_ident is {
    Some(i) -> i == pool_ident
    None -> True
  }
}

pub fn process_orders(
  this_pool_ident: Ident,
  datums: Dict<Hash<Blake2b_256, Data>, Data>,
  initial: PoolState,
  input_order: List<(Int, Option<SignedStrategyExecution>)>,
  actual_protocol_fee: Int,
  prev_index: Int,
  all_inputs: List<Input>,
  remaining_inputs: List<Input>,
  outputs: List<Output>,
  uniqueness_flag: Int,
  count: Int,
) -> (PoolState, Int) {
  when input_order is {
    [] -> (initial, count)
    [(idx, _), ..rest] -> {
      // Check that this idx is unique
      let next_uniqueness_flag = check_and_set_unique(uniqueness_flag, idx)
      let next_input_list =
        if idx >= prev_index {
          unsafe_fast_index_skip_with_tail(remaining_inputs, idx - prev_index)
        } else {
          unsafe_fast_index_skip_with_tail(all_inputs, idx)
        }
      expect [input_to_process, ..rest_of_input_list] = next_input_list

      let order = input_to_process.output
      expect [output, ..rest_outputs] = outputs

      expect Some(datum) = datum_of(datums, order)
      expect datum: OrderDatum = datum
      let OrderDatum { pool_ident, destination, max_protocol_fee, details, .. } =
        datum

      expect validate_pool_id(pool_ident, this_pool_ident)

      // Make sure the scooper can only take up to the max fee the user has agreed to
      expect max_protocol_fee >= actual_protocol_fee

      when details is {
        order.Strategy(..) -> todo
        order.Swap(offer, min_received) -> {
          let next =
            swap.do_swap(
              initial,
              order.value,
              destination,
              actual_protocol_fee,
              offer,
              min_received,
              output,
            )
          process_orders(
            this_pool_ident,
            datums,
            next,
            rest,
            actual_protocol_fee,
            idx + 1,
            all_inputs,
            rest_of_input_list,
            rest_outputs,
            next_uniqueness_flag,
            count + 1,
          )
        }
        order.Deposit(..) -> {
          let next = deposit.do_deposit(initial, order.value, datum, actual_protocol_fee, output)
          process_orders(
            this_pool_ident,
            datums,
            next,
            rest,
            actual_protocol_fee,
            idx,
            all_inputs,
            rest_of_input_list,
            rest_outputs,
            next_uniqueness_flag,
            count + 1,
          )
        }
        order.Withdrawal(..) -> {
          let next =
            withdrawal.do_withdrawal(initial, order.value, datum, actual_protocol_fee, output)
          process_orders(
            this_pool_ident,
            datums,
            next,
            rest,
            actual_protocol_fee,
            idx,
            all_inputs,
            rest_of_input_list,
            rest_outputs,
            next_uniqueness_flag,
            count + 1,
          )
        }
        // order.Zap(..) -> do_zap(initial, input, datum)
        order.Donation(..) -> {
          let (next, used_output) =
            donation.do_donation(initial, order.value, datum, actual_protocol_fee, output)
          if used_output {
            process_orders(
              this_pool_ident,
              datums,
              next,
              rest,
              actual_protocol_fee,
              idx,
              all_inputs,
              rest_of_input_list,
              rest_outputs,
              next_uniqueness_flag,
              count + 1,
            )
          } else {
            process_orders(
              this_pool_ident,
              datums,
              next,
              rest,
              actual_protocol_fee,
              idx,
              all_inputs,
              rest_of_input_list,
              outputs,
              next_uniqueness_flag,
              count + 1,
            )
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
  let rberry = (#"01010101010101010101010101010101010101010101010101010101", "RBERRY")
  let lp = (#"99999999999999999999999999999999999999999999999999999999", "LP")
  let pool_state = PoolState {
    quantity_a: (#"", #"", 1_000_000_000),
    quantity_b: (rberry.1st, rberry.2nd, 1_000_000_000),
    quantity_lp: (lp.1st, lp.2nd, 1_000_000_000),
    fees_per_10_thousand: 5,
    protocol_fees: 2_000_000,
  }
  let order_datum = OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2_500_000,
    destination: Destination {
      address: addr,
      datum: NoDatum,
    },
    details: order.Donation {
      assets: ((ada.1st, ada.2nd, 1_000_000), (rberry.1st, rberry.2nd, 1_000_000)),
    },
    extension: Void,
  }
  // There's no remainder so do_donation totally ignores this Output record
  let output = Output {
    address: addr,
    value: value.from_lovelace(999_999_999_999_999_999),
    datum: NoDatum,
    reference_script: None,
  }

  let order_output = Output {
    address: addr,
    value: value.from_lovelace(1_000_000)
      |> value.add(rberry.1st, rberry.2nd, 1_000_000),
    datum: InlineDatum(order_datum),
    reference_script: None,
  }

  let order_datum_data: Data = order_datum
  let datums = dict.new()
    |> dict.insert(
         key: hash.blake2b_256(cbor.serialise(order_datum)),
         value: order_datum_data,
         compare: bytearray.compare)

  let input = Input {
    output_reference: OutputReference {
      transaction_id: TransactionId {
        hash: #"0000000000000000000000000000000000000000000000000000000000000000"
      },
      output_index: 0,
    },
    output: order_output,
  }

  let input_order = [(0, None)]
  let inputs = [input]
  let outputs = [output]

  let (final_pool_state, count) = process_orders(#"", datums, pool_state, input_order, 2_500_000, 0, inputs, inputs, outputs, 0, 0)

  expect final_pool_state.quantity_a.3rd == 1_001_000_000
  expect final_pool_state.quantity_b.3rd == 1_001_000_000
  expect final_pool_state.protocol_fees == 4_500_000
  expect count == 1
  True
}
