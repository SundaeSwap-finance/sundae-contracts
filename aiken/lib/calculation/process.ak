use aiken/dict.{Dict}
use aiken/hash.{Blake2b_256, Hash}
use aiken/transaction.{Input, Output}
use aiken/transaction/value.{PolicyId}
use calculation/deposit
use calculation/donation
use calculation/shared.{PoolState,
  do_2_exp, unsafe_fast_index_skip} as calc_shared
use calculation/swap
use calculation/withdrawal
use shared.{datum_of}
use types/order.{OrderDatum}
use types/pool.{PoolDatum, lp_token_name}

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
