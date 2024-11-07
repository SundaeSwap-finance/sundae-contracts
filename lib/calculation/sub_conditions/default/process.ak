use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/crypto.{DataHash, ScriptHash, VerificationKeyHash}
use aiken/interval
use calculation/process.{process_orders}
use calculation/sub_conditions/shared.{minted_correct_pool_tokens} as sub_conditions_shared
use cardano/address.{Address, Credential}
use cardano/assets.{AssetName, Lovelace, PolicyId, Value}
use cardano/transaction.{Input, Output, ValidityRange}
use shared.{
  AssetClass, Ident, count_orders, has_expected_pool_value, pool_lp_name,
}
use types/order.{SignedStrategyExecution}
use types/settings.{SettingsDatum}

pub fn scoop_default(
  settings_datum: SettingsDatum,
  inputs: List<Input>,
  pool_script_hash: ScriptHash,
  actual_ask_fees_per_10_thousand: Int,
  actual_bid_fees_per_10_thousand: Int,
  pool_input: Output,
  actual_identifier: Ident,
  validity_range: ValidityRange,
  withdrawals: Pairs<Credential, Lovelace>,
  datums: Dict<DataHash, Data>,
  input_order: List<(Int, Option<SignedStrategyExecution>, Int)>,
  outputs: List<Output>,
  actual_protocol_fees: Int,
  mint: Value,
  extra_signatories: List<VerificationKeyHash>,
  signatory_index: Int,
  scooper_index: Int,
  pool_output_value: Value,
  actual_circulating_lp: Int,
  actual_assets: (AssetClass, AssetClass),
  actual_market_open: Int,
  pool_output_address: Address,
  assets: (AssetClass, AssetClass),
  protocol_fees: Int,
  identifier: Ident,
  circulating_lp: Int,
  market_open: Int,
  bid_fees_per_10_thousand: Int,
  ask_fees_per_10_thousand: Int,
) -> Bool {
  // Deconstruct the settings datum with the fields we need for a scoop
  let SettingsDatum {
    authorized_scoopers,
    base_fee,
    simple_fee,
    strategy_fee,
    ..
  } = settings_datum

  // Do a simple scan over the orders to count up the number of orders we'll be processing
  // This is unavoidable, because it's part of making sure that the provided redeemer set isn't
  // excluding orders
  let real_order_count = count_orders(inputs)
  // Calculate the portion of the fee that each order will be
  // entitled to pay;
  // Because the division is rounded down, we add real_order_count and subtact 1
  // to ensure that we take the ceiling instead, and round in the protocols favor.
  let amortized_base_fee =
    ( base_fee + real_order_count - 1 ) / real_order_count
  // Make sure it's not negative, for example if base_fee was negative
  expect amortized_base_fee >= 0

  // Construct the initial pool state from the datum and the locked values
  // This intermediate state will be updated as we process each order, allowing us to do a scan over each input
  // In particular, it calculates what fees we should be charging (because of the linear fee decay) and the actual tradable reserves
  // (excluding protocol fees, which shouldn't factor into the price)
  // Note: this abomination is brought to you by the fact that constructing and destructuring structs
  // is expensive, so it's cheaper to have **massive** lambdas / continuations
  let
    pool_policy_a,
    pool_asset_name_a,
    pool_quantity_a,
    pool_policy_b,
    pool_asset_name_b,
    pool_quantity_b,
    pool_policy_lp,
    pool_asset_name_lp,
    pool_quantity_lp,
    bid_fees,
    ask_fees,
    initial_protocol_fees,
  <-
    default_pool_input_to_state(
      pool_script_hash,
      assets,
      protocol_fees,
      identifier,
      circulating_lp,
      bid_fees_per_10_thousand,
      ask_fees_per_10_thousand,
      pool_input,
    )

  // Process the orders in order, and decide the final pool state we should see
  // This also counts up the number of simple / strategy orders, which let us compute the effective protocol fee.
  // for optimization purposes, there are quite a lot of parameters, and their interaction is quite subtle
  let
    final_a,
    final_b,
    final_lp,
    simple_count,
    strategy_count,
  <-
    process_orders(
      actual_identifier,
      // The pool identifier, so we can check that each order is for this pool
      validity_range,
      // The validity range of the transaction, so we can check strategies haven't expired
      withdrawals,
      // Include the withdrawals, in case a strategy has some kind of attached script condition
      datums,
      // The datums, so we can look up the datum of each order (which may be inline, but may also be in the datums dict)
      // The initial pool state, such as the reserves and circulating LP
      pool_policy_a,
      pool_asset_name_a,
      pool_quantity_a,
      pool_policy_b,
      pool_asset_name_b,
      pool_quantity_b,
      pool_policy_lp,
      pool_asset_name_lp,
      pool_quantity_lp,
      input_order,
      // The input ordering specified by the scooper
      bid_fees,
      // The liquidity provider fee to charge for bids (A -> B), in parts per 10,000 (basis points)
      ask_fees,
      // ... for Ask (swap B -> A)
      amortized_base_fee,
      // The base fee split by the number of orders, paid for each user
      simple_fee,
      // The fee to charge for each "simple" order (swap, deposit, withdrawal, etc.)
      strategy_fee,
      // The fee to charge for each "strategy" order
      0,
      // The previous index we processed, intitially 0; this lets us detect if we need to "restart" the input list
      inputs,
      // *All* inputs, so we can start over at the beginning of the list if we want
      inputs,
      // *Remaining* inputs, so we can advance through the list one by one so long as the orders are in order
      list.drop(outputs, 1),
      // The list of outputs we should be comparing orders against
      0,
      // A uniqueness bit-flag, to detect which orders have already been processed; see lib/calculation/InputSorting.md
      0,
      // The accumulated count of "simple" orders, for calculating the fee; set to 0 to start, but incremented in each recursion
      0,
    )
  // The accumulated count of "strategy" orders, see line above.
  // We need to make sure that the number of orders matches the amount that we processed
  // so the scooper doesn't "under-report" the orders and steal the funds on the order
  expect simple_count + strategy_count == real_order_count

  // We calculate the expected total collected protocol fee
  // We multiply amortized_base_fee, which everyone paid, by the number of orders
  // and then the respective fees for each simple order and strategy order
  let expected_fees_collected =
    amortized_base_fee * real_order_count + simple_count * simple_fee + strategy_count * strategy_fee

  // Make sure we actually increased the protocol fee by exactly this amount
  expect actual_protocol_fees == initial_protocol_fees + expected_fees_collected

  // The pool should have all of the scooper fees, and the quantity of each token of the outcome
  // Note that initializing the state with `-transaction.fee` means this gets subracted out of the protocol fees
  // TODO: do we need to account for this? it seems to have gotten lost in some changes.
  expect
    minted_correct_pool_tokens(
      pool_script_hash,
      mint,
      identifier,
      circulating_lp,
      final_lp,
    )

  // Check that the scooper is authorized; the protocol can allow *any* scoopers, or limit it to a set of actors
  // It's safe to use values provided in the redeemer to efficiently skip to the expected scooper / expected signature
  // because at the end of the day, we just care that the scooper has signed the transaction. If the scooper provides
  // anything but the correct indexes, it'll just fail the transaction.
  expect
    when authorized_scoopers is {
      Some(authorized_scoopers) -> {
        // OPTIMIZATION: skip 10 entries at a time
        // OPTIMIZATION: assume scooper is first extra_signatory? have to assume there will only ever be one extra_signatory
        expect Some(scooper_sig) = list.at(extra_signatories, signatory_index)
        expect Some(scooper) = list.at(authorized_scoopers, scooper_index)
        // must be an authorized scooper
        scooper_sig == scooper
      }
      _ -> True
    }

  // the market must have opened; this allows projects to pre-create their pool, potentially across multiple protocols, and allows
  // people to open orders ahead of time, and avoids things like sniping bots, etc.
  // TODO: should we *only* prevent swaps / withdrawals? would it be ok to allow deposits?
  // TODO: should we have a "blackout period", where withdrawals are prevented, similar to IPOs?
  expect interval.is_entirely_after(validity_range, market_open)

  // We also check that the pool output has the right value (as mentioned above)
  // In particular, the pool must have:
  // - the pool NFT
  // - the correctly adjusted assets from swapping, deposits, withdrawals, etc.
  // - an additional amount of ADA corresponding to the protocol fees
  // - NOTHING ELSE; This is important because someone could add tons of junk tokens and increase the execution units, potentially even freezing the UTXO
  expect
    has_expected_pool_value(
      pool_script_hash,
      actual_identifier,
      pool_output_value,
      pool_policy_a,
      pool_asset_name_a,
      final_a,
      pool_policy_b,
      pool_asset_name_b,
      final_b,
      final_lp,
      actual_protocol_fees,
    )
  // Now, we check various things about the output datum to ensure they're each correct.
  // Check that the datum correctly records the final circulating LP, accounting for any deposits and withdrawals
  // In particular, this is important because that circulating supply is exaclty what determines the users ownership of assets in the pool
  // If this gets out of sync with what we've actually minted, then users will be able to either redeem assets they aren't entitled to,
  // or be unable to access funds they are entitled to.
  expect actual_circulating_lp == final_lp

  // Make sure the protocol fees have been correctly updated
  expect actual_protocol_fees == initial_protocol_fees + expected_fees_collected

  // And make sure each of these fields is unchanged
  and {
    identifier == actual_identifier,
    assets == actual_assets,
    bid_fees_per_10_thousand == actual_bid_fees_per_10_thousand,
    ask_fees_per_10_thousand == actual_ask_fees_per_10_thousand,
    market_open == actual_market_open,
    // Finally, make sure we don't change the stake credential; this can only be done when withdrawing fees, by the treasury administrator
    pool_input.address.stake_credential == pool_output_address.stake_credential,
  }
}

/// Construct the initial pool state for processing a set of orders
pub fn default_pool_input_to_state(
  pool_token_policy: PolicyId,
  assets: (AssetClass, AssetClass),
  protocol_fees: Int,
  identifier: Ident,
  circulating_lp: Int,
  bid_fees_per_10_thousand: Int,
  ask_fees_per_10_thousand: Int,
  input: Output,
  continuation: fn(
    PolicyId,
    AssetName,
    Int,
    PolicyId,
    AssetName,
    Int,
    PolicyId,
    AssetName,
    Int,
    Int,
    Int,
    Int,
  ) ->
    Bool,
) -> Bool {
  let (asset_a, asset_b) = assets
  let (asset_a_policy_id, asset_a_name) = asset_a
  let (asset_b_policy_id, asset_b_name) = asset_b
  // If asset_a is ADA, then we need to not consider the protocol fees as part of this
  // We don't have to check asset_b, because assets are guaranteed to be in lexicographical order.
  let min_utxo =
    if asset_a_policy_id == assets.ada_policy_id {
      protocol_fees
    } else {
      0
    }
  // Get the maximum of market_open and the transaction valid from so we can calculate the fees correctly
  // Note: we use valid_from, as this favors the protocol: you pay the fees for the *earliest* moment your order *could* have executed.
  // Scoopers could in theory set a wide validity range to cause users to overpay, but this should be considered malicious activity and
  // get the scooper removed from the list of valid scoopers / ignore scooper rewards
  // TODO: we could solve this by enforcing a validity range, and checking the length is within 4 hours.
  let bid_fees = bid_fees_per_10_thousand
  let ask_fees = ask_fees_per_10_thousand
  // Then construct the pool state. We include the assets here, instead of just the reserves, so we can check the values of each order
  // TODO: we could potentially save quite a bit by not passing around this object, and passing around a lot of parameters instead...
  continuation(
    asset_a_policy_id,
    asset_a_name,
    assets.quantity_of(input.value, asset_a_policy_id, asset_a_name) - min_utxo,
    asset_b_policy_id,
    asset_b_name,
    assets.quantity_of(input.value, asset_b_policy_id, asset_b_name),
    pool_token_policy,
    pool_lp_name(identifier),
    circulating_lp,
    bid_fees,
    ask_fees,
    protocol_fees,
  )
}
