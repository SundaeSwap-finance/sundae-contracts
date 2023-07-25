use aiken/dict.{Dict}
use aiken/hash.{Blake2b_256, Hash}
use aiken/transaction.{Output, ScriptContext}
use aiken/transaction/value.{PolicyId}
use calculation/deposit
use calculation/donation
use calculation/shared.{PoolState} as calc_shared
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
  let assets = datum.assets
  let min_utxo =
    if assets.1st.1st == value.ada_policy_id {
      datum.rewards
    } else {
      0
    }
  PoolState {
    quantity_a: (
      assets.1st.1st,
      assets.1st.2nd,
      value.quantity_of(input.value, assets.1st.1st, assets.1st.2nd) - min_utxo,
    ),
    quantity_b: (
      assets.2nd.1st,
      assets.2nd.2nd,
      value.quantity_of(input.value, assets.2nd.1st, assets.2nd.2nd),
    ),
    quantity_lp: (
      pool_token_policy,
      lp_token_name(datum.identifier),
      datum.circulating_lp,
    ),
    fee: datum.fees,
    // Pay the transaction fee out of the rewards
    rewards: datum.rewards,
  }
}

pub fn process_orders(
  datums: Dict<Hash<Blake2b_256, Data>, Data>,
  initial: PoolState,
  orders: List<Output>,
  outputs: List<Output>,
) -> PoolState {
  when orders is {
    [] -> initial
    [order, ..rest_orders] -> {
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
          process_orders(datums, next, rest_orders, rest_outputs)
        }
        order.Deposit(..) -> {
          let next = deposit.do_deposit(initial, order.value, datum, output)
          process_orders(datums, next, rest_orders, rest_outputs)
        }
        order.Withdrawal(..) -> {
          let next =
            withdrawal.do_withdrawal(initial, order.value, datum, output)
          process_orders(datums, next, rest_orders, rest_outputs)
        }
        // order.Zap(..) -> do_zap(initial, input, datum)
        order.Donation(..) -> {
          let (next, used_output) =
            donation.do_donation(initial, order.value, datum, output)
          if used_output {
            process_orders(datums, next, rest_orders, rest_outputs)
          } else {
            process_orders(datums, next, rest_orders, outputs)
          }
        }
      }
    }
  }
}
