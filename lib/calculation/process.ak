use aiken/bytearray
use aiken/cbor
use aiken/math
use aiken/dict.{Dict}
use aiken/hash.{Blake2b_256, Hash}
use aiken/interval.{IntervalBound, NegativeInfinity, Finite}
use aiken/transaction.{InlineDatum, NoDatum, Input, Output, OutputReference, TransactionId, ValidityRange}
use aiken/transaction/credential.{Address, ScriptCredential, StakeCredential, VerificationKeyCredential}
use aiken/transaction/value.{Value, PolicyId, AssetName}
use aiken/time.{PosixTime}
use calculation/deposit
use calculation/donation
use calculation/record
use calculation/shared.{
  PoolState, check_and_set_unique, unsafe_fast_index_skip_with_tail,
} as calc_shared
use calculation/swap
use calculation/withdrawal
use calculation/strategy
use shared.{Ident, datum_of, pool_lp_name, is_script}
use sundae/multisig
use types/order.{Order, Destination, Fixed, OrderDatum, SignedStrategyExecution}
use types/pool.{PoolDatum}

pub fn calculate_fees(
  fees: (Int, Int),
  market_open: PosixTime,
  fee_finalized: PosixTime,
  valid_from: PosixTime,
) {
  // So, if the transaction is guaranteed to run after fee_finalized, charge the final fee rate
  if valid_from > fee_finalized {
    fees.2nd
  } else {
    // Otherwise, calculate the duration of this decay period
    let duration = fee_finalized - market_open
    if duration == 0 {
      // If it's zero (market_open == fee_finalized) then just use the final fee rate
      fees.2nd
    } else {
      // Otherwise, calculate how much has elapsed since the market open
      // then the full range of the fee rate
      // then, starting from the opening fee rate, linearly increase (or decrease) the fees by that range
      let elapsed = valid_from - market_open
      let range = fees.2nd - fees.1st
      fees.1st + (elapsed * range / duration)
    }
  }
}

/// Construct the initial pool state for processing a set of orders
pub fn pool_input_to_state(
  pool_token_policy: PolicyId,
  datum: PoolDatum,
  input: Output,
  valid_from: IntervalBound<PosixTime>,
  continuation: fn(
    PolicyId, AssetName, Int,
    PolicyId, AssetName, Int,
    PolicyId, AssetName, Int,
    Int, Int, Int
  ) -> Bool,
) -> Bool {
  let PoolDatum {
    assets,
    protocol_fees,
    identifier,
    circulating_lp,
    bid_fees_per_10_thousand,
    ask_fees_per_10_thousand,
    market_open,
    fee_finalized,
    ..
  } = datum
  let (asset_a, asset_b) = assets
  let (asset_a_policy_id, asset_a_name) = asset_a
  let (asset_b_policy_id, asset_b_name) = asset_b
  // If asset_a is ADA, then we need to not consider the protocol fees as part of this
  // We don't have to check asset_b, because assets are guaranteed to be in lexicographical order.
  let min_utxo =
    if asset_a_policy_id == value.ada_policy_id {
      protocol_fees
    } else {
      0
    }
  
  // Get the maximum of market_open and the transaction valid from so we can calculate the fees correctly
  // Note: we use valid_from, as this favors the protocol: you pay the fees for the *earliest* moment your order *could* have executed.
  // Scoopers could in theory set a wide validity range to cause users to overpay, but this should be considered malicious activity and
  // get the scooper removed from the list of valid scoopers / ignore scooper rewards
  // TODO: we could solve this by enforcing a validity range, and checking the length is within 4 hours.
  let valid_from = when valid_from.bound_type is {
    NegativeInfinity -> market_open
    Finite(t) -> math.max(t, market_open)
    _ -> error
  }
  // Calculate the fees per 10k rate to use for this whole scoop
  // We let the creator of the pool specify a fee rate that decays (or increases) between market_open and fee_finalized
  let bid_fees = calculate_fees(bid_fees_per_10_thousand, market_open, fee_finalized, valid_from)
  let ask_fees = calculate_fees(ask_fees_per_10_thousand, market_open, fee_finalized, valid_from)
    
  // Then construct the pool state. We include the assets here, instead of just the reserves, so we can check the values of each order
  // TODO: we could potentially save quite a bit by not passing around this object, and passing around a lot of parameters instead...
  continuation(
    asset_a_policy_id, asset_a_name, value.quantity_of(input.value, asset_a_policy_id, asset_a_name) - min_utxo,
    asset_b_policy_id, asset_b_name, value.quantity_of(input.value, asset_b_policy_id, asset_b_name),
    pool_token_policy, pool_lp_name(identifier), circulating_lp,
    bid_fees, ask_fees, protocol_fees
  )
}

/// If the order is restricted to a specific pool, then make sure pool_ident matches that; otherwise just return true
fn validate_pool_id(order_pool_ident: Option<Ident>, pool_ident: Ident) -> Bool {
  when order_pool_ident is {
    Some(i) -> i == pool_ident
    None -> True
  }
}

/// Process a single order, comparing it to the output to ensure it was executed faithfully, and returning the new pool state 
///
/// Most of the parameters here are for performance reasons, to avoid destructuring objects, since thats very expensive
pub fn process_order(
  // The pool state as of the time the order is executed; If we process multiple orders, this gets passed through each time
  pool_policy_a: PolicyId,
  pool_asset_name_a: AssetName,
  pool_quantity_a: Int,
  pool_policy_b: PolicyId,
  pool_asset_name_b: AssetName,
  pool_quantity_b: Int,
  pool_policy_lp: PolicyId,
  pool_asset_name_lp: AssetName,
  pool_quantity_lp: Int,
  // The signed strategy execution (or none, if this is a simple order); this lets us just recurse once to reuse all the same handling logic
  signed_execution: Option<SignedStrategyExecution>,
  // The output reference of the **order we're processing**; Used to check that the signed execution we get for a strategy is actually for this order
  output_reference: OutputReference,
  // The validity range of the transaction, used to ensure the signed execution is within the correct time window
  tx_valid_range: ValidityRange,
  // The transaction withdrawals, so we can check strategy executions
  withdrawals: Dict<StakeCredential, Int>,
  // The input being processed
  input: Output,
  // The value attached to the input, so we can ensure that any surplus tokens are paid out to the destination along with the results of the order; this lets transactions chain
  value: Value,
  // The details of the order to execute, such as whether it's a swap, the limit, etc.
  details: Order,
  // The max protocol fee that *can* be charged from the order; depending on how big the batch size is, some may be returned as a rebate, but this lets the user limit the maximum that gets charged in case the protocol fees in the settings change
  max_protocol_fee: Int,
  // The destination where the result of the order must be sent; useful for chaining transactions, as it lets you specify a datum for the output
  destination: Destination,
  // The liquidity provider fee to charge for bid (A -> B) and ask (B -> A) orders, expressed as parts per 10,000. also called basis points
  bid_fees_per_10_thousand: Int,
  ask_fees_per_10_thousand: Int,
  // The base fee, divided among all the participants in the scoop
  amortized_base_fee: Int,
  // The amount to charge for simple vs strategy orders, taken from the settings
  simple_fee: Int,
  strategy_fee: Int,
  // A list of outputs, so we can destructure the next output
  // TODO: we can probably avoid returning the outputs, and just return a boolean
  outputs: List<Output>,
  // A continuation to call with the next pool state and the list of outputs; this is more efficient than constructing an object and tuples
  continuation: fn(Int, Int, Int, List<Output>) -> Bool,
) -> Bool { // Returns the updated pool state, the correct list of outputs to resume from, and total fee charged by the order
  when details is {
    order.Strategy(signer) -> {
      // For a strategy, we can basically "unpack" (and validate) the signed execution, and then recurse to process the deferred order
      expect Some(execution) = signed_execution
      let details = strategy.get_strategy(
        output_reference,
        tx_valid_range,
        withdrawals,
        signer,
        execution,
      )
      process_order(
        pool_policy_a,
        pool_asset_name_a,
        pool_quantity_a,
        pool_policy_b,
        pool_asset_name_b,
        pool_quantity_b,
        pool_policy_lp,
        pool_asset_name_lp,
        pool_quantity_lp,
        None, // No need to pass a signed execution through again; it doesn't make sense to have multiple nested strategies
        output_reference,
        tx_valid_range,
        withdrawals,
        input,
        value,
        details,
        max_protocol_fee,
        destination,
        bid_fees_per_10_thousand,
        ask_fees_per_10_thousand,
        amortized_base_fee,
        // We pass strategy_fee here, instead of simple_fee,
        // because even though we're using the same logic, we're charging a different fee for strategies
        strategy_fee,
        strategy_fee,
        outputs,
        continuation,
      )
    }
    order.Swap(offer, min_received) -> {
      expect [output, ..rest_outputs] = outputs
      // Make sure the scooper can only take up to the max fee the user has agreed to
      let fee = amortized_base_fee + simple_fee
      expect max_protocol_fee >= fee
      // Calculate the result of the swap itself, according to the AMM formula, and validate it against the output
      let new_pool_a, new_pool_b <-
        swap.do_swap(
          pool_policy_a,
          pool_asset_name_a,
          pool_quantity_a,
          pool_policy_b,
          pool_asset_name_b,
          pool_quantity_b,
          input,
          destination,
          bid_fees_per_10_thousand,
          ask_fees_per_10_thousand,
          fee,
          offer,
          min_received,
          output,
        )
      continuation(new_pool_a, new_pool_b, pool_quantity_lp, rest_outputs)
    }
    order.Deposit(assets) -> {
      expect [output, ..rest_outputs] = outputs
      // Make sure the scooper can only take up to the max fee the user has agreed to
      // i.e. the amortized base fee (which is the base fee shared by all orders), and the incremental fee (i.e. the fee paid by each order individually) should be no more than the max fee in the datum
      let fee = amortized_base_fee + simple_fee
      expect max_protocol_fee >= fee
      // Calculate and validate the result of a deposit
      let new_a, new_b, new_lp <- deposit.do_deposit(
        pool_policy_a, pool_asset_name_a, pool_quantity_a,
        pool_policy_b, pool_asset_name_b, pool_quantity_b,
        pool_policy_lp, pool_asset_name_lp, pool_quantity_lp,
        input, assets,
        destination, fee,
        output
      )
      continuation(new_a, new_b, new_lp, rest_outputs)
    }
    order.Withdrawal(amount) -> {
      expect [output, ..rest_outputs] = outputs
      // Make sure the scooper can only take up to the max fee the user has agreed to
      // (See above)
      let fee = amortized_base_fee + simple_fee
      expect max_protocol_fee >= fee
      // Calculate and validate the result of a withdrawal
      let new_a, new_b, new_lp <-
        withdrawal.do_withdrawal(
          pool_policy_a, pool_asset_name_a, pool_quantity_a,
          pool_policy_b, pool_asset_name_b, pool_quantity_b,
          pool_policy_lp, pool_asset_name_lp, pool_quantity_lp,
          input, amount,
          destination, fee,
          output
        )
      continuation(new_a, new_b, new_lp, rest_outputs)
    }
    // NOTE: we decided not to implement zap, for time constraints, and because a zap can be easily implemented as a chained order, as it is in V1
    // The cheaper fees the DAO voted on should make this acceptable
    // order.Zap(..) -> do_zap(initial, input, datum)
    order.Donation(assets) -> {
      expect [output, ..rest_outputs] = outputs
      // Make sure the scooper can only take up to the max fee the user has agreed to
      // (See above)
      let fee = amortized_base_fee + simple_fee
      expect max_protocol_fee >= fee
      // Calculate and validate the result of a donation
      let new_a, new_b, used_output <-
        donation.do_donation(
          pool_policy_a, pool_asset_name_a, pool_quantity_a,
          pool_policy_b, pool_asset_name_b, pool_quantity_b,
          input, assets,
          destination, fee,
          output
        )
      // If a donation has no change (ex it has exactly the donation + the fee) then we don't have an output dedicated to the order
      // so we can skip over it
      // TODO: can we just return used_output here instead of passing around the lists of outputs?
      if used_output {
        continuation(new_a, new_b, pool_quantity_lp, rest_outputs)
      } else {
        continuation(new_a, new_b, pool_quantity_lp, outputs)
      }
    }
    order.Record(policy) -> {
      // Make sure the scooper can only take up to the max fee the user has agreed to
      expect [output, ..rest_outputs] = outputs
      let fee = amortized_base_fee + simple_fee
      expect max_protocol_fee >= fee
      expect record.check_record(input, destination, fee, output, policy)
      continuation(pool_quantity_a, pool_quantity_b, pool_quantity_lp, rest_outputs)
    }
  }
}

/// Recursively process all orders in the correct order
/// There's a lot of parameters here, mostly for efficiency (though with some redundancies being removed in another branch)
pub fn process_orders(
  // The pool identifier we're processing, so we can check the order if it has a specific pool
  this_pool_ident: Ident,
  // The transaction valid range, if we end up processing a strategy
  tx_valid_range: ValidityRange,
  // The withdrawals attached to the transaction, for validating strategies
  withdrawals: Dict<StakeCredential, Int>,
  // The datums in the witness set, in case we need to lookup a non-inline datum
  datums: Dict<Hash<Blake2b_256, Data>, Data>,
  // The initial / current pool state, passed recursively as we process each order
  pool_policy_a: PolicyId,
  pool_asset_name_a: AssetName,
  pool_quantity_a: Int,
  pool_policy_b: PolicyId,
  pool_asset_name_b: AssetName,
  pool_quantity_b: Int,
  pool_policy_lp: PolicyId,
  pool_asset_name_lp: AssetName,
  pool_quantity_lp: Int,
  // The list of remaining indices into the inputs, specifying which orders to process
  input_order: List<(Int, Option<SignedStrategyExecution>, Int)>,
  // The liquidity provider fee, for bid (swapping asset A to asset B) and ask (asset B to asset A) orders, expressed as parts per 10,000
  bid_fees_per_10_thousand: Int,
  ask_fees_per_10_thousand: Int,
  // The protocol base fee, split across each order
  amortized_base_fee: Int,
  // The simple and strategy fees from the settings datum
  simple_fee: Int,
  strategy_fee: Int,
  // The previous order we processed, to check if we need to restart the loop; TODO: we actually pass +1 from this, and i'm not sure how to explain why we do this...
  prev_index: Int,
  // *all* inputs on the transaction, in case we need to start over from the beginning (i.e. wrap around)
  all_inputs: List<Input>,
  // Just the remaining inputs in the list, in case it's more efficient to keep walking from here
  remaining_inputs: List<Input>,
  // The list of remaining outputs to compare the orders against; usually we pass the `tail` of this list recursively, but in the case of donations with no change, we pass outputs through unchanged
  outputs: List<Output>,
  // A number that, when interpreted as a bit flag, indicates which orders we've already processed; used to check if an order is processed more than once (see InputSorting.md)
  uniqueness_flag: Int,
  // A recursive aggregator for the number of "simple" and "strategy" orders we see; used for computing the fee without traversing the list independently, since we're already walking this list
  simple_count: Int,
  strategy_count: Int,
  // A continuation to call with the final pool state; more efficient than constructing tuples / objects
  continuation: fn(Int, Int, Int, Int, Int) -> Bool,
) -> Bool { // Returns the final pool state, and the count of each order type
  // The main "pump" of the recursive loop is the input_order, which is a set of indices into the inputs list
  // specified by the scooper for the order to process each order in.
  // Once we've reached the end of the list, we can return, but otherwise
  when input_order is {
    [] -> continuation(pool_quantity_a, pool_quantity_b, pool_quantity_lp, simple_count, strategy_count)
    [(idx, sse, _), ..rest] -> {
      // First, it's important to check that each order is processed only once;
      // This is quite subtle, so check InputSorting.md for a full explanation
      let next_uniqueness_flag = check_and_set_unique(uniqueness_flag, idx)

      // Then, we identify where to find the inputs; in particular, to avoid "starting from the beginning" every single time
      // when indices are monotonic through the list, we can just continue to advance through the list
      // so, all_inputs will always contain the full list of inputs
      // while remaining_inputs will just contain the ones "after" the last one we processed.
      // So, here, we check if we can continue down this path, or if we need to start from the beginning again
      let next_input_list =
        if idx >= prev_index {
          unsafe_fast_index_skip_with_tail(remaining_inputs, idx - prev_index)
        } else {
          unsafe_fast_index_skip_with_tail(all_inputs, idx)
        }
      
      expect [input_to_process, ..rest_of_input_list] = next_input_list
      let Input{ output_reference, output: order } = input_to_process

      // It's important that we fail if we ever try to process a UTXO from a wallet address
      // This is a bit unfortunate, because it means we can't support processing orders directly out of a users wallet
      // but is important, because we rely on this to check that every order is processed.
      // If we didn't do this check, a scooper could include a UTXO from their wallet, and leave a *real* order un-processed, and steal those users funds.
      expect is_script(order.address.payment_credential)

      // Find the datum that is associated with this order; we allow that datum to be either inline, or in the witness set,
      // to aid in composibility with other protocols
      // We also check that the datum is in the format we expect;
      // Note: we don't actually check the order address anywhere!! As long as it's a script, and the datum is in the correct format, we're good.
      // This lets us upgrade the order contract, or add other types of orders over time.
      expect Some(datum) = datum_of(datums, order)
      expect datum: OrderDatum = datum
      let OrderDatum { pool_ident, destination, max_protocol_fee, details, .. } =
        datum

      // Make sure we're allowed to process this order (i.e. if the user specified a specific pool, we have to honor that)
      expect validate_pool_id(pool_ident, this_pool_ident)
      
      // Check whether this is a strategy, then increment the count appropriately
      let (next_simple_count, next_strategy_count) = when details is {
        order.Strategy(..) -> (simple_count, strategy_count + 1)
        _ -> (simple_count + 1, strategy_count)
      }

      // And finally, process this one individual order and compute the next state
      // Note that we get back next_orders here, which is needed if we process a donation that has no change UTXO
      let new_a, new_b, new_lp, next_orders <- process_order(
        pool_policy_a, pool_asset_name_a, pool_quantity_a,
        pool_policy_b, pool_asset_name_b, pool_quantity_b,
        pool_policy_lp, pool_asset_name_lp, pool_quantity_lp,
        sse,
        output_reference,
        tx_valid_range,
        withdrawals,
        order,
        order.value,
        details,
        max_protocol_fee,
        destination,
        bid_fees_per_10_thousand,
        ask_fees_per_10_thousand,
        amortized_base_fee,
        simple_fee,
        strategy_fee,
        outputs,
      )

      // And recursively process the rest of the orders
      process_orders(
        this_pool_ident,
        tx_valid_range,
        withdrawals,
        datums,
        pool_policy_a, pool_asset_name_a, new_a,
        pool_policy_b, pool_asset_name_b, new_b,
        pool_policy_lp, pool_asset_name_lp, new_lp,
        rest,                  // This advances to the next element from input_order
        bid_fees_per_10_thousand,
        ask_fees_per_10_thousand,
        amortized_base_fee,
        simple_fee,
        strategy_fee,
        idx + 1,               // This is the "previous index" within the input list; TODO: I'm not actually sure why we add 1?
        all_inputs,            // See the notes above about all_inputs vs remaining_inputs
        rest_of_input_list,
        next_orders,
        next_uniqueness_flag,
        next_simple_count,
        next_strategy_count,
        continuation,
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
  let pool_state = PoolState {
    quantity_a: (#"", #"", 1_000_000_000),
    quantity_b: (rberry.1st, rberry.2nd, 1_000_000_000),
    quantity_lp: (lp.1st, lp.2nd, 1_000_000_000),
  }
  let order_datum = OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2_500_000,
    destination: Fixed {
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
    value: value.from_lovelace(3_500_000)
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

  let input_order = [(0, None, 0)]
  let inputs = [input]
  let outputs = [output]

  let new_a, new_b, new_lp, simple, strategies <- process_orders(
    #"", valid_range, dict.new(), datums,
    pool_state.quantity_a.1st,
    pool_state.quantity_a.2nd,
    pool_state.quantity_a.3rd,
    pool_state.quantity_b.1st,
    pool_state.quantity_b.2nd,
    pool_state.quantity_b.3rd,
    pool_state.quantity_lp.1st,
    pool_state.quantity_lp.2nd,
    pool_state.quantity_lp.3rd,
    input_order,
    5, 5, 2_500_000,
    0, 0, 0,
    inputs, inputs,
    outputs,
    0, 0, 0
  )

  expect new_a == 1_001_000_000
  expect new_b == 1_001_000_000
  expect new_lp== 1_000_000_000
  expect simple == 1
  expect strategies == 0
  True
}

test process_30_shuffled_orders_test() {
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
  }
  let order_datum = OrderDatum {
    pool_ident: None,
    owner: multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    ),
    max_protocol_fee: 2_500_000,
    destination: Fixed { 
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
    value: value.from_lovelace(3_500_000)
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

  let inputs = [input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input, input]
  // shuffled order processing:
  let input_order = [
    (11, None, 0),
    (26, None, 0),
    (18, None, 0),
    (2, None, 0),
    (28, None, 0),
    (13, None, 0),
    (16, None, 0),
    (1, None, 0),
    (8, None, 0),
    (27, None, 0),
    (12, None, 0),
    (23, None, 0),
    (22, None, 0),
    (14, None, 0),
    (10, None, 0),
    (0, None, 0),
    (24, None, 0),
    (5, None, 0),
    (7, None, 0),
    (17, None, 0),
    (20, None, 0),
    (29, None, 0),
    (19, None, 0),
    (21, None, 0),
    (9, None, 0),
    (25, None, 0),
    (6, None, 0),
    (4, None, 0),
    (3, None, 0),
    (15, None, 0),
  ]
  let outputs = [output]

  let new_a, new_b, new_lp, simple, strategies <- process_orders(
    #"", valid_range, dict.new(), datums,
    pool_state.quantity_a.1st, pool_state.quantity_a.2nd, pool_state.quantity_a.3rd,
    pool_state.quantity_b.1st, pool_state.quantity_b.2nd, pool_state.quantity_b.3rd,
    pool_state.quantity_lp.1st, pool_state.quantity_lp.2nd, pool_state.quantity_lp.3rd,
    input_order,
    5, 5, 2_500_000,
    0, 0, 0,
    inputs, inputs,
    outputs,
    0, 0, 0
  )

  new_a == 1_030_000_000 &&
  new_b == 1_030_000_000 &&
  new_lp == 1_000_000_000 &&
  simple == 30 &&
  strategies == 0
}
