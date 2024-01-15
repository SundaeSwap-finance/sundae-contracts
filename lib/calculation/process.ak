use aiken/bytearray
use aiken/cbor
use aiken/list
use aiken/dict.{Dict}
use aiken/hash.{Blake2b_256, Hash}
use aiken/interval.{IntervalBound, NegativeInfinity, Finite}
use aiken/transaction.{InlineDatum, NoDatum, Input, Output, OutputReference, TransactionId, ValidityRange}
use aiken/transaction/credential.{Address, ScriptCredential, VerificationKeyCredential}
use aiken/transaction/value.{Value, PolicyId, AssetName}
use calculation/deposit
use calculation/donation
use calculation/shared.{
  check_and_set_unique, unsafe_fast_index_skip_with_tail,
} as calc_shared
use calculation/swap
use calculation/withdrawal
use calculation/strategy
use shared.{Ident, datum_of, pool_lp_name, is_script}
use sundae/multisig
use types/order.{Order, Destination, OrderDatum, SignedStrategyExecution}
use types/pool.{PoolDatum}

pub fn process_all_orders(
  pool_token_policy: PolicyId,
  pool_datum: PoolDatum,
  pool_input: Output,
  tx_valid_range: ValidityRange,
  datums: Dict<Hash<Blake2b_256, Data>, Data>,
  input_order: List<(Int, Option<SignedStrategyExecution>)>,
  amortized_base_fee: Int,
  simple_fee: Int,
  strategy_fee: Int,
  all_inputs: List<Input>,
  outputs: List<Output>,
) -> (
  Int, // final reserve_a
  Int, // final reserve_b
  Int, // final circulating_lp
  Int, // number of simple orders we processed
  Int, // number of strategy orders we processed
) {
  let PoolDatum {
    assets,
    protocol_fees,
    identifier,
    circulating_lp,
    fees_per_10_thousand,
    market_open,
    fee_finalized,
  } = pool_datum
  let (asset_a, asset_b) = assets
  let (asset_a_policy, asset_a_name) = asset_a
  let (asset_b_policy, asset_b_name) = asset_b
  let min_utxo =
    if asset_a_policy == value.ada_policy_id {
      protocol_fees
    } else {
      0
    }
  let valid_from = when tx_valid_range.lower_bound.bound_type is {
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
  process_orders(
    identifier,
    tx_valid_range,
    datums,

    // PoolState
    asset_a_policy,
    asset_a_name,
    asset_b_policy,
    asset_b_name,
    pool_token_policy,
    pool_lp_name(identifier),
    value.quantity_of(pool_input.value, asset_a_policy, asset_a_name) - min_utxo,
    value.quantity_of(pool_input.value, asset_b_policy, asset_b_name),
    circulating_lp,

    input_order,
    fees,
    amortized_base_fee,
    simple_fee,
    strategy_fee,
    0,
    all_inputs,
    all_inputs,
    list.drop(outputs, 1),
    0,
    0,
    0,
  )
}

fn validate_pool_id(order_pool_ident: Option<Ident>, pool_ident: Ident) -> Bool {
  when order_pool_ident is {
    Some(i) -> i == pool_ident
    None -> True
  }
}

pub fn process_order(
  // We unpack the pool state here, rather than pass around a struct, for performance reasons
  // pool_state: PoolState
  asset_a_policy: PolicyId,
  asset_a_name: AssetName,
  asset_b_policy: PolicyId,
  asset_b_name: AssetName,
  lp_token_policy: PolicyId,
  lp_token_name: AssetName,
  reserve_a: Int,
  reserve_b: Int,
  circulating_lp: Int,

  signed_execution: Option<SignedStrategyExecution>,
  output_reference: OutputReference,
  tx_valid_range: ValidityRange,
  value: Value,
  details: Order,
  max_protocol_fee: Int,
  datum: OrderDatum,
  destination: Destination,
  fees_per_10_thousand: Int,
  amortized_base_fee: Int,
  simple_fee: Int,
  strategy_fee: Int,
  output: Output,
  outputs: List<Output>,
  rest_outputs: List<Output>
) -> (
  // PoolState, unpacked for performance
  Int, // New reserve_a
  Int, // New reserve_b
  Int, // New circulating_lp
  Bool, // whether we consumed an output or not
) {
  when details is {
    order.Strategy(signer) -> {
      expect Some(execution) = signed_execution
      let details = strategy.get_strategy(
        output_reference,
        tx_valid_range,
        signer,
        execution,
      )
      process_order(
        // pool_state, unpacked for performance
        asset_a_policy,
        asset_a_name,
        asset_b_policy,
        asset_b_name,
        lp_token_policy,
        lp_token_name,
        reserve_a,
        reserve_b,
        circulating_lp,

        None,
        output_reference,
        tx_valid_range,
        value,
        details,
        max_protocol_fee,
        datum,
        destination,
        fees_per_10_thousand,
        amortized_base_fee,
        // We pass strategy_fee here, instead of simple_fee,
        // reuse the code to calculate the result, but charge the higher fee
        strategy_fee,
        strategy_fee,
        output,
        outputs,
        rest_outputs,
      )
    }
    order.Swap(offer, min_received) -> {
      // Make sure the scooper can only take up to the max fee the user has agreed to
      let fee = amortized_base_fee + simple_fee
      expect max_protocol_fee >= fee
      let (new_reserve_a, new_reserve_b) =
        swap.do_swap(
          // The pool state, unpacked for performance
          // pool_state,
          asset_a_policy,
          asset_a_name,
          asset_b_policy,
          asset_b_name,
          reserve_a,
          reserve_b,

          value,
          destination,
          fees_per_10_thousand,
          fee,
          offer,
          min_received,
          output,
        )
      (
        new_reserve_a,
        new_reserve_b,
        circulating_lp,
        True,
      )
    }
    order.Deposit(..) -> {
      // Make sure the scooper can only take up to the max fee the user has agreed to
      let fee = amortized_base_fee + simple_fee
      expect max_protocol_fee >= fee
      let (new_a_reserve, new_b_reserve, new_circulating_lp) = deposit.do_deposit(
        asset_a_policy,
        asset_a_name,
        asset_b_policy,
        asset_b_name,
        lp_token_policy,
        lp_token_name,
        reserve_a,
        reserve_b,
        circulating_lp,
        value,
        datum,
        fee,
        output,
      )
      (
        new_a_reserve,
        new_b_reserve,
        new_circulating_lp,
        True,
      )
    }
    order.Withdrawal(..) -> {
      // Make sure the scooper can only take up to the max fee the user has agreed to
      let fee = amortized_base_fee + simple_fee
      expect max_protocol_fee >= fee
      let (new_a_reserve, new_b_reserve, new_circulating_lp) =
        withdrawal.do_withdrawal(
          asset_a_policy,
          asset_a_name,
          asset_b_policy,
          asset_b_name,
          lp_token_policy,
          lp_token_name,
          reserve_a,
          reserve_b,
          circulating_lp,
          value,
          datum,
          fee,
          output,
      )
      (new_a_reserve, new_b_reserve, new_circulating_lp, True)
    }
    // order.Zap(..) -> do_zap(initial, input, datum)
    order.Donation(..) -> {
      // Make sure the scooper can only take up to the max fee the user has agreed to
      let fee = amortized_base_fee + simple_fee
      expect max_protocol_fee >= fee
      let (next_reserve_a, next_reserve_b, consume_output) =
        donation.do_donation(
          asset_a_policy,
          asset_a_name,
          asset_b_policy,
          asset_b_name,
          reserve_a,
          reserve_b,
          value,
          datum,
          fee,
          output
        )
      (next_reserve_a, next_reserve_b, circulating_lp, consume_output)
    }
  }
}

pub fn process_orders(
  this_pool_ident: Ident,
  tx_valid_range: ValidityRange,
  datums: Dict<Hash<Blake2b_256, Data>, Data>,

  // Pool State; we unpack this for performance reasons
  // The asset IDs will be the same for the whole recursion, so will get lifted by the optimization we wrote
  // The reserves will change, but now we don't have to unpack them every time
  asset_a_policy: PolicyId,
  asset_a_name: AssetName,
  asset_b_policy: PolicyId,
  asset_b_name: AssetName,
  lp_token_policy: PolicyId,
  lp_token_name: AssetName,
  reserve_a: Int,
  reserve_b: Int,
  circulating_lp: Int,

  input_order: List<(Int, Option<SignedStrategyExecution>)>,
  fees_per_10_thousand: Int,
  amortized_base_fee: Int,
  simple_fee: Int,
  strategy_fee: Int,
  prev_index: Int,
  all_inputs: List<Input>,
  remaining_inputs: List<Input>,
  outputs: List<Output>,
  uniqueness_flag: Int,
  simple_count: Int,
  strategy_count: Int,
) -> (
  Int, // final reserve_a
  Int, // final reserve_b
  Int, // final circulating_lp
  Int, // number of simple orders we processed
  Int, // number of strategy orders we processed
) {
  when input_order is {
    [] -> (reserve_a, reserve_b, circulating_lp, simple_count, strategy_count)
    [(idx, sse), ..rest] -> {
      // Check that this idx is unique
      let next_uniqueness_flag = check_and_set_unique(uniqueness_flag, idx)
      let next_input_list =
        if idx >= prev_index {
          unsafe_fast_index_skip_with_tail(remaining_inputs, idx - prev_index)
        } else {
          unsafe_fast_index_skip_with_tail(all_inputs, idx)
        }
      expect [input_to_process, ..rest_of_input_list] = next_input_list

      let Input{ output_reference, output: order } = input_to_process
      expect [output, ..rest_outputs] = outputs

      // make sure we only process scripts; this is so that our "order count" and "indexing set" logic is correct
      expect is_script(order.address.payment_credential)

      expect Some(datum) = datum_of(datums, order)
      expect datum: OrderDatum = datum
      let OrderDatum { pool_ident, destination, max_protocol_fee, details, .. } =
        datum

      expect validate_pool_id(pool_ident, this_pool_ident)
      
      let (next_simple_count, next_strategy_count) = when details is {
        order.Strategy(..) -> (simple_count, strategy_count + 1)
        _ -> (simple_count + 1, strategy_count)
      }

      let (new_reserve_a, new_reserve_b, new_circulating_lp, consume_output) = process_order(
        asset_a_policy,
        asset_a_name,
        asset_b_policy,
        asset_b_name,
        lp_token_policy,
        lp_token_name,
        reserve_a,
        reserve_b,
        circulating_lp,
        sse,
        output_reference,
        tx_valid_range,
        order.value,
        details,
        max_protocol_fee,
        datum, // TODO: can we get rid of this?
        destination,
        fees_per_10_thousand,
        amortized_base_fee,
        simple_fee,
        strategy_fee,
        output,
        outputs,
        rest_outputs,
      )

      let next_orders = if consume_output { rest_outputs } else { outputs }

      process_orders(
        this_pool_ident,
        tx_valid_range,
        datums,
        asset_a_policy,
        asset_a_name,
        asset_b_policy,
        asset_b_name,
        lp_token_policy,
        lp_token_name,
        new_reserve_a,
        new_reserve_b,
        new_circulating_lp,
        rest,
        fees_per_10_thousand,
        amortized_base_fee,
        simple_fee,
        strategy_fee,
        idx + 1,
        all_inputs,
        rest_of_input_list,
        next_orders,
        next_uniqueness_flag,
        next_simple_count,
        next_strategy_count,
      )
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
    address: Address(
      ScriptCredential(#"4af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"),
      None,
    ),
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
  let valid_range = interval.between(1,2)

  let input_order = [(0, None)]
  let inputs = [input]
  let outputs = [output]

  let (final_reserve_a, final_reserve_b, final_circulating_lp, simple, strategies) = process_orders(
    #"",
    valid_range,
    datums,
    #"",
    #"",
    rberry.1st,
    rberry.2nd,
    lp.1st,
    lp.2nd,
    1_000_000_000,
    1_000_000_000,
    1_000_000_000,
    input_order,
    5,
    2_500_000,
    0,
    0,
    0,
    inputs,
    inputs,
    outputs,
    0,
    0,
    0
  )

  expect final_reserve_a == 1_001_000_000
  expect final_reserve_b == 1_001_000_000
  expect final_circulating_lp == 1_000_000_000
  expect simple == 1
  expect strategies == 0
  True
}
