use aiken/builtin
use aiken/bytearray
use aiken/dict
use aiken/hash
use aiken/interval
use aiken/list
use aiken/math
use aiken/option
use aiken/transaction.{
  Datum, InlineDatum, Input, Mint, NoDatum, Output, OutputReference,
  ScriptContext, Spend, Transaction, TransactionId,
}
use aiken/transaction/credential.{
  Address, ScriptCredential, VerificationKeyCredential,
}
use aiken/transaction/value.{MintedValue, PolicyId, Value, ada_policy_id}
use calculation/process.{pool_input_to_state, process_orders}
use calculation/shared.{PoolState} as calc_shared
use shared.{AssetClass, Ident, spent_output, pool_nft_name, pool_lp_name, has_exact_token_count, count_orders}
use sundae/multisig
use tests/examples/ex_settings.{mk_valid_settings_input}
use tests/examples/ex_shared.{wallet_address, script_address, mk_output_reference, mk_tx_hash}
use types/order.{Destination, OrderDatum, Swap, Deposit}
use types/pool.{
  CreatePool, MintLP, PoolDatum, PoolMintRedeemer, PoolRedeemer, PoolScoop, WithdrawFees
}
use types/settings.{SettingsDatum, find_settings_datum}

/// The core / base "pooled AMM" script for the SundaeSwap v3 protocol
///
/// Parameterized by the Settings policy ID, which makes the script unique, as well as lets us validate / read global settings.
///
/// This script is responsible for:
///  - Guarding access to the pools assets on behalf of the depositors
///  - Enabling and executing a batch of orders against those assets, implementing a standard 'AMM' swap protocol
///  - Serving as a minting policy, minting the Pool NFT and LP tokens
///  - Accumulating protocol fees on behalf of the protocol
///  - Allowing protocol rewards to be withdrawn to a DAO treasury
///
/// It does so by allowing a permissioned set of entities to "scoop" a batch of orders, and ensuring that each one
/// pays out to the appropriate destination.
///
/// This set of people is permissioned to prevent classes of "sandwich" attacks, wherein a malicious actor could
/// execute and match orders in their *own* favor, rather than the users favor.
///
/// Also of particular sensitivity is the optimizations applied; To achieve high-throughput, low-cost DeFi, executing
/// each order independently suffers from some amount of overhead.  If, for example, the execution costs are split into:
/// A - The costs that need to be run in each transaction, regardless of what is executed
/// B - The costs that need to be executed for each order
/// C - The costs exclusive to batching, such as sorting the orders
///
/// It is likely, then, that protocols converge on the per-order cost of batching being lower than un-batched variants, i.e.
///
/// A + B*n + C < (A + B) * n
validator(settings_policy_id: PolicyId) {
  fn spend(datum: PoolDatum, redeemer: PoolRedeemer, ctx: ScriptContext) {
    // First, we destructure the transaction right upfront, because field access is O(n),
    // and we want access to these fields with just a single pass over the transaction
    // This will be a common pattern throughout the scripts
    // (in fact, I really want a compiler optimization that detects and lifts repeated field accesses into a destructure)
    let transaction = ctx.transaction
    let Transaction {
      inputs,
      outputs,
      reference_inputs,
      mint,
      datums,
      extra_signatories,
      validity_range,
      ..
    } = transaction

    // Then, (confusing terminology aside) find the specific pool UTXO being spent by this transaction
    let pool_input = spent_output(ctx)

    // And pattern match to get the pool script hash; in particular, this can be used to find the pool output,
    // *and* to know the policy ID of pool tokens, because this is a dual spending/minting validator.
    expect ScriptCredential(pool_script_hash) = pool_input.address.payment_credential

    // Find the pool output; we can assume the pool output is the first output, because:
    // - The ledger doesn't reorder outputs, just inputs
    // - We check that the address is correct, so if the first output was to a different contract, we would fail
    // - We check that the datum is the correct type, meaning we can't construct an invalid pool output
    // - Later, we check that the pool output has the correct value, meaning it *must* contain the pool token, so we can't pay to the pool script multiple times
    expect Some(pool_output) = list.head(outputs)
    expect pool_output.address == pool_input.address
    expect InlineDatum(output_datum) = pool_output.datum
    expect output_datum: PoolDatum = output_datum

    // Similarly, destructure the pool datum we found on the output, to access the fields we need to process the scoop
    let PoolDatum {
      identifier: actual_identifier,
      circulating_lp: actual_circulating_lp,
      protocol_fees: actual_protocol_fees,
      ..
    } = output_datum

    // The protocol configures many global settings via a "settings" UTXO, updatable by certain administrators
    // This is included as a reference input, so we have a utility to check the reference inputs for the settings NFT
    // Note: it's important to check for the NFT, because checking just for the address would let someone pay random funds to the settings address.
    let settings_datum = find_settings_datum(reference_inputs, settings_policy_id) 

    // Then, there are two different actions that can be taken against a pool:
    // - Scooping a batch of orders
    // - Withdrawing protocol fees to the treasury
    when redeemer is {
      // In the case of the scoop, the redeemer indicates which scooper is doing the scoop, and the order in which the inputs should be processed
      PoolScoop{ signatory_index, scooper_index, input_order, .. } -> {
        // Deconstruct the settings datum with the fields we need for a scoop
        let SettingsDatum { authorized_scoopers, base_fee, simple_fee, strategy_fee, .. } =
          settings_datum

        // Do a simple scan over the orders to count up the number of orders we'll be processing
        // This is unavoidable, because it's part of making sure that the provided redeemer set isn't
        // excluding orders
        let real_order_count = count_orders(inputs)
        // Calculate the portion of the fee that each order will be
        // entitled to pay;
        // Because the division is rounded down, we add real_order_count and subtact 1
        // to ensure that we take the ceiling instead, and round in the protocols favor.
        let amortized_base_fee = (base_fee + real_order_count - 1) / real_order_count
        // Make sure it's not negative, for example if base_fee was negative
        expect amortized_base_fee >= 0

        // Construct the initial pool state from the datum and the locked values
        // This intermediate state will be updated as we process each order, allowing us to do a scan over each input
        // In particular, it calculates what fees we should be charging (because of the linear fee decay) and the actual tradable reserves
        // (excluding protocol fees, which shouldn't factor into the price)
        let (initial_state, fees_per_10_thousand, initial_protocol_fees) = pool_input_to_state(pool_script_hash, datum, pool_input, validity_range.lower_bound)

        // Process the orders in order, and decide the final pool state we should see
        // This also counts up the number of simple / strategy orders, which let us compute the effective protocol fee.
        // for optimization purposes, there are quite a lot of parameters, and their interaction is quite subtle
        let (outcome, simple_count, strategy_count) =
          process_orders(
            actual_identifier,        // The pool identifier, so we can check that each order is for this pool
            validity_range,           // The validity range of the transaction, so we can check strategies haven't expired
            datums,                   // The datums, so we can look up the datum of each order (which may be inline, but may also be in the datums dict)
            initial_state,            // The initial pool state, such as the reserves and circulating LP
            input_order,              // The input ordering specified by the scooper
            fees_per_10_thousand,     // The liquidity provider fee to charge, in parts per 10,000 (basis points)
            amortized_base_fee,       // The base fee split by the number of orders, paid for each user
            simple_fee,               // The fee to charge for each "simple" order (swap, deposit, withdrawal, etc.)
            strategy_fee,             // The fee to charge for each "strategy" order
            0,                        // The previous index we processed, intitially 0; this lets us detect if we need to "restart" the input list
            inputs,                   // *All* inputs, so we can start over at the beginning of the list if we want
            inputs,                   // *Remaining* inputs, so we can advance through the list one by one so long as the orders are in order
            list.drop(outputs, 1),    // The list of outputs we should be comparing orders against
            0,                        // A uniqueness bit-flag, to detect which orders have already been processed; see lib/calculation/InputSorting.md
            0,                        // The accumulated count of "simple" orders, for calculating the fee; set to 0 to start, but incremented in each recursion
            0,                        // The accumulated count of "strategy" orders, see line above.
          )

        let PoolState {
          quantity_lp: expected_circulating_lp,
          ..
        } = outcome

        // We need to make sure that the number of orders matches the amount that we processed
        // so the scooper doesn't "under-report" the orders and steal the funds on the order
        expect simple_count + strategy_count == real_order_count

        // We calculate the expected total collected protocol fee
        // We multiply amortized_base_fee, which everyone paid, by the number of orders
        // and then the respective fees for each simple order and strategy order
        let expected_fees_collected =
          amortized_base_fee * real_order_count +
          simple_count * simple_fee +
          strategy_count * strategy_fee

        // Make sure we actually increased the protocol fee by exactly this amount
        expect actual_protocol_fees == initial_protocol_fees + expected_fees_collected

        // The pool should have all of the scooper fees, and the quantity of each token of the outcome
        // Note that initializing the state with `-transaction.fee` means this gets subracted out of the protocol fees
        // TODO: do we need to account for this? it seems to have gotten lost in some changes.
        expect minted_correct_pool_tokens(pool_script_hash, mint, datum, outcome)

        // Check that the scooper is authorized; the protocol can allow *any* scoopers, or limit it to a set of actors
        // It's safe to use values provided in the redeemer to efficiently skip to the expected scooper / expected signature
        // because at the end of the day, we just care that the scooper has signed the transaction. If the scooper provides
        // anything but the correct indexes, it'll just fail the transaction.
        expect when authorized_scoopers is {
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
        expect interval.is_entirely_after(validity_range, datum.market_open)

        expect output_datum.circulating_lp == outcome.quantity_lp.3rd

        // We also check that the pool output has the right value (as mentioned above)
        // In particular, the pool must have:
        // - the pool NFT
        // - the correctly adjusted assets from swapping, deposits, withdrawals, etc.
        // - an additional amount of ADA corresponding to the protocol fees
        // - NOTHING ELSE; This is important because someone could add tons of junk tokens and increase the execution units, potentially even freezing the UTXO
        expect
          has_expected_pool_value(
            pool_script_hash,
            output_datum.identifier,
            pool_output.value,
            outcome,
            actual_protocol_fees,
          )
        
        // Now, we check various things about the output datum to ensure they're each correct.
        // Check that the datum correctly records the final circulating LP, accounting for any deposits and withdrawals
        // In particular, this is important because that circulating supply is exaclty what determines the users ownership of assets in the pool
        expect actual_circulating_lp == expected_circulating_lp.3rd

        // Make sure the protocol fees have been correctly updated
        expect actual_protocol_fees == initial_protocol_fees + expected_fees_collected

        // And make sure each of these fields is unchanged
        and {
          datum.identifier == output_datum.identifier,
          datum.assets == output_datum.assets,
          datum.fees_per_10_thousand == output_datum.fees_per_10_thousand,
          datum.market_open == output_datum.market_open,

          // Finally, make sure we don't change the stake credential; this can only be done when withdrawing fees, by the treasury administrator
          pool_input.address.stake_credential == pool_output.address.stake_credential,
        }
      }
      WithdrawFees { amount, treasury_output } -> {
        let PoolDatum {
          protocol_fees: initial_protocol_fees,
          ..
        } = datum
        // Make sure we withdraw *only* up to what we've earned
        // We allow less than, so that you can leave some behind for the minUTXO cost, or continuing to earn staking rewards, etc.
        expect amount <= initial_protocol_fees

        // As part of withdrawing, we should decrease the protocol fees by the amount we're withdrawing
        // but, importantly, *nothing else*; so we construct a datum with everything from the initial datum, plus the protofol fees updated
        let expected_datum = PoolDatum {
          identifier: datum.identifier,
          assets: datum.assets,
          circulating_lp: datum.circulating_lp,
          fees_per_10_thousand: datum.fees_per_10_thousand,
          fee_finalized: datum.fee_finalized,
          market_open: datum.market_open,
          protocol_fees: initial_protocol_fees - amount,
        }
        expect output_datum == expected_datum

        // Now, check that the pool output decreases *only* by the amount we're withdrawing, and not by fewer or greater ADA
        let expected_output_value = value.merge(pool_input.value, value.from_lovelace(-amount))
        expect pool_output.value == expected_output_value

        // Only the treasury administrator is allowed to withdraw the fees, to prevent DDOS, and because of the allowance below
        expect multisig.satisfied(
          settings_datum.treasury_admin,
          extra_signatories,
          validity_range,
        )

        // Asking the DAO to approve every single cost individually would be a small cognitive DDOS on the community
        // Instead, the DAO can set an "allowance", which is a percentage of each withdrawal that is entrusted to the
        // treasury administrator to pay those basic fees, such as paying the scoopers, or running incentive programs, etc.
        //
        // In particular, it's a percentage, to ensure that splitting up the withdrawals into multiple transactions doesn't
        // allow them to game that withdrawal.
        let allowance = amount * settings_datum.treasury_allowance.1st / settings_datum.treasury_allowance.2nd
        let to_treasury = amount - allowance

        // And, we must pay everything except the allowance amount to the treasury address
        // We use the `treasury_output` index to skip to it quickly, rather than scanning for the output
        // TODO: should we instead sum all the values at the treasury output, to allow
        // paying out in multiple UTXOs for some reason?
        expect Some(treasury_output) = list.at(outputs, treasury_output)
        expect treasury_output.address == settings_datum.treasury_address
        // We ensure that it's sent with the Void datum. This is because the treasury is likely to be a script address
        // and scripts are unspendable without a datum; We also don't have any notion of what the "correct" datum would be
        // so we just enforce it to void. If the output datum ever needed a specific datum, we would have to use a proxy address
        // that worked with the void datum, and paid to the real treasury with the correct datum.
        // TODO: should we just let the treasury admin specify the datum on the redeemer? Or include it in the settings?
        expect treasury_output.datum == InlineDatum(Void)
        // And make sure we pay at least that much in that output. It could be more, for example to donate other ADA from other sources to that address
        expect value.lovelace_of(treasury_output.value) >= to_treasury
        True
      }
    }
  }

  fn mint(r: PoolMintRedeemer, ctx: ScriptContext) {
    // When minting, we can be doing one of two things: minting the pool itself, or minting the LP token
    when r is {
      // For creating a new pool, one of our design objectives was to avoid requiring interaction with any global 
      // "factory", as in v1; this created a lot of contention, and didn't serve it's original goal of ensuring that
      // the (pair, fee) was unique.
      CreatePool(assets, pool_output_ix, metadata_output_ix) -> {
        expect Mint(own_policy_id) = ctx.purpose

        // The assets on the pool must be sorted
        // This is partially to make off-chain indexing easier, and as an on-chain optimization,
        // so we can always assume that, if ADA is one of the pairs, it's the first asset.
        // This also prevents creating an X/X pool, since we require the assets to be stricly less than
        // Note, it is a NON GOAL to enforce that any specific (pair, fee) combination is unique; while aggregating liquidity
        // can be beneficial for capital efficiency, in practice that will happen anyway, and smaller pools can be useful as
        // a pressure release valve. Additionally, we have bigger plans around interesting order splitting modes of operation that
        // make that less important.
        let (asset_a, asset_b) = assets
        let coin_pair_ordering_is_canonical =
          compare_asset_class(asset_a, asset_b) == Less

        // We use the first input ref spent in this transaction to uniquely identify the pool
        // This is a trick widely used to produce NFTs, and ensures that we can generate a hash that is unique to this pool
        expect Some(first_input) = list.at(ctx.transaction.inputs, 0)
        let first_input_index =
          int_to_ident(first_input.output_reference.output_index)

        // Calculate the identifier by hashing the txRef of the first input
        // this makes the pool unique
        // With CIP-68 identifiers (which take up 4 bytes), we have 28 bytes of space;
        // ideally we'd use blake2b-224, which is exactly a 224 byte hash, but that won't be on-chain until plutus v3;
        // So, we use blake2b-256 and drop a few bytes. Now, does this compromise the uniqueness guarantees?
        // It is strongly believed by the cryptographer community that blake2b-256 provides indifferentiability from random oracles
        // [https://eprint.iacr.org/2013/322.pdf]
        // which allows us to treat the 256 bits as uniformly sampled. This means that there are 2^256 equally likely values, and the
        // chance of collision is 1 in 2^256, unfathomably small.
        // Even when used in bulk, the birthday paradox means that you would need to generate 2^128 different hashes before you had even a 50% chance of collision.
        // This is known as "128 bit security", and is considered a gold standard of cryptography.
        // So, how does dropping 4 bytes (32 bits) from this hash impact that analysis?
        // There are still 2^224 possible values, meaning it offers 2^112 bit security, still considered "unbreakable".
        // As a comparison, at the time of this writing the entire bitcoin network is performing 500 exa-hashes per second, or
        // 500 * 10^18 hashes per second. If the entire bitcoin network was directed towards trying to generate a collision on pool ident, it would take
        // Roughly 10^13 seconds, or 317,098 years to have a 50% chance of collision. Not to mention the cost of storing and comparing to detect if you'd performed
        // a collision, or the cost of grinding UTXOs on cardano to give fresh inputs to the pool script.
        let new_pool_id =
          first_input.output_reference.transaction_id.hash
            |> bytearray.concat(#"23") // '#' character
            |> bytearray.concat(first_input_index)
            |> hash.blake2b_256
            |> bytearray.drop(4) 

        // With that pool identifier, we can attach 3 different CIP-68 pool identifiers:
        // - (100) indicates a tracking token, for the purposes of on-chain metadata read by off-chain infra, so we can provide a nice experience to the users wallet for example
        // - (222) indicates the pool NFT, which uniquely identifies the UTXO that holds pool assets
        // - (333) indicates the fungible LP token, which represents a percentage ownership of the pool
        let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) = shared.pool_token_names(new_pool_id)

        // Then, find the settings datum, so we can ensure the reference token is paid to the metadata admin
        let reference_inputs = ctx.transaction.reference_inputs
        let settings_datum = find_settings_datum(reference_inputs, settings_policy_id)

        // And grab the pool output
        expect Some(pool_output) =
          list.at(ctx.transaction.outputs, pool_output_ix)

        // Grab the initial reserves of each token by looking at what's paid to the UTXO
        let coin_a_amt =
          value.quantity_of(pool_output.value, asset_a.1st, asset_a.2nd)
        let coin_b_amt =
          value.quantity_of(pool_output.value, asset_b.1st, asset_b.2nd)

        // Only ada has a null policy id. If coin A is ada, subtract the 2 ada
        // rider from the output to get the true amount in the pool.
        // TODO: we should instead subtract the protocol fees, rather than relying
        // on this constant, so that we can adjust the "rider" off-chain if needed.
        // TODO: should we check for a minimum protocol fees amount, as a "create pool" fee paid to the protocol?
        let coin_a_amt_sans_rider =
          if bytearray.is_empty(asset_a.1st) {
            coin_a_amt - 2_000_000
          } else {
            coin_a_amt
          }

        // Calculate the initial number of LP tokens; In particular, we adopt Uniswaps convention of
        // computing this value as the square root of the product of the two values.
        // This helps minimize precision loss: it gives decent initial liquidity values for a range of
        // sizes of pools, such that an individual LP token is granular enough for depositing and withdrawing for most users.
        expect Some(initial_lq) = math.sqrt(coin_a_amt_sans_rider * coin_b_amt)

        // Use the asset names we computed above to compute the whole Asset IDs of the tokens we expect
        let new_pool_ref = (own_policy_id, new_pool_ref_token)
        let new_pool_nft = (own_policy_id, new_pool_nft_token)
        let new_pool_lq = (own_policy_id, new_pool_lp_token)

        // And check that we mint the correct tokens, and nothing else.
        let expected_mint =
              shared.to_value((new_pool_ref.1st, new_pool_ref.2nd, 1))
            |> value.merge(shared.to_value((new_pool_nft.1st, new_pool_nft.2nd, 1)))
            |> value.merge(shared.to_value((new_pool_lq.1st, new_pool_lq.2nd, initial_lq)))
        let mint_is_correct =
          value.from_minted_value(ctx.transaction.mint) == expected_mint

        // Confirm that the correct funds (at least one asset A, at least one asset B, exactly 3 tokens, including the pool NFT) is correct here
        // Note: should we instead just compare the expected pool output?
        let funds_spent_to_pool =
          coin_a_amt_sans_rider >= 1 && coin_b_amt >= 1 && list.length(
            value.flatten(pool_output.value),
          ) <= 3 && value.quantity_of(
            pool_output.value,
            own_policy_id,
            new_pool_nft_token,
          ) == 1
        
        // Make sure we send the pool metadata token to the metadata admin
        // We use an index from the redeemer to skip to the right output, in case there are multiple outputs to the metadata admin
        // This is safe to do for the usual reasons: if they point at a UTXO without the ref token, the transaction will fail.
        expect Some(metadata_output) =
          list.at(ctx.transaction.outputs, metadata_output_ix)
        expect metadata_output.address == settings_datum.metadata_admin
        expect value.quantity_of(metadata_output.value, own_policy_id, new_pool_ref_token) == 1

        // Check that the pool datum is inline, because a datum hash could brick this pool
        expect InlineDatum(d) = pool_output.datum
        expect pool_output_datum: PoolDatum = d

        // And check that the datum is initialized correctly; This is part of why we have a minting policy handling this,
        // as it allows us to authenticate the providence of the datum.
        // A datum is valid so long as
        // - the pool identifier is set correctly
        // - the assets is set correctly
        // - the initial circulating supply is set correctly
        // - and the market open time is before the fee finalized time; TODO: should we relax this?
        //   I'm not sure it's harmful if someone initializes this with a feeFinalized in the past
        let pool_output_datum_correct = and {
          pool_output_datum.identifier == new_pool_id,
          pool_output_datum.assets == (asset_a, asset_b),
          pool_output_datum.circulating_lp == initial_lq,
          pool_output_datum.market_open <= pool_output_datum.fee_finalized,
        }

        // And then check each of the conditions above as the condition for minting
        and {
          coin_pair_ordering_is_canonical,
          mint_is_correct,
          funds_spent_to_pool,
          pool_output_datum_correct,
        }
      }
      // when minting an LP token, we just need to make sure the pool NFT is present in one of the inputs,
      // meaning the pool script will enforce the correct name and quantity.
      // 
      // Of particular note, you might expect this to fail when minting the initial LP tokens
      // but the minting policy only runs once, so it would be running with a different redeemer in that case
      // And it's not possible to include a *separate* minting redeemer to run the script twice; (TODO: check this)
      // even if it were, the pool token wouldn't be on the **inuputs** when we're minting the pool.
      //
      // It's also important that the pool script and the minting script checks that *no other* tokens of this policy are minted,
      // for example for a different pool. It should be ok if a token from a *different* policy is minted, though.
      MintLP(pool_ident) -> {
        expect Mint(own_policy_id) = ctx.purpose
        let pool_nft_name = shared.pool_nft_name(pool_ident)
        let allows_to_spend =
          fn(v) {
            value.quantity_of(v, own_policy_id, pool_nft_name) == 1
          }
        list.any(
          ctx.transaction.inputs,
          fn(input) { allows_to_spend(input.output.value) },
        )
      }
    }
  }
}

/// This is responsible for checking that the minting value on the transaction is valid
/// based on the pool state, the policy ID, and the initial incoming datum.
fn minted_correct_pool_tokens(
  pool_policy_id: PolicyId,
  mint: MintedValue,
  datum: PoolDatum,
  outcome: PoolState,
) -> Bool {
  // Unwrap the silly MintedValue special type
  let minted_tokens =
    value.tokens(value.from_minted_value(mint), pool_policy_id)

  // If the initial datum has the same circulating LP as the outcome, then we expect no minted tokens
  // Otherwise, the minted tokens should be exactly the pool LP tokens and nothing else
  // TODO: confirm that the "minting 0 ada" problem doesn't apply here; we have real-world transactions, so I doubt it does, but I want to confirm.
  // TODO: This should allow minting of other tokens, from other policy IDs, perhaps, for composibility?
  if datum.circulating_lp == outcome.quantity_lp.3rd {
    dict.is_empty(minted_tokens)
  } else {
    dict.to_list(minted_tokens) == [
      (
        pool_lp_name(datum.identifier),
        outcome.quantity_lp.3rd - datum.circulating_lp,
      ),
    ]
  }
}

/// Check that the UTXO contents are correct given a specific pool outcome
/// In particular, it must have the final A reserves, the final B reserves, the pool NFT, and the protocol fees
fn has_expected_pool_value(
  pool_script_hash: PolicyId,
  identifier: Ident,
  output_value: Value,
  outcome: PoolState,
  final_protocol_fees: Int,
) -> Bool {
  let PoolState { quantity_a, quantity_b, .. } = outcome
  let (quantity_a_policy_id, quantity_a_name, quantity_a_amt) = quantity_a
  let (quantity_b_policy_id, quantity_b_name, quantity_b_amt) = quantity_b
  // Asset A *could* be ADA; in which case there should be 3 tokens on the output
  // (ADA, Asset B, and the NFT)
  // We do this as an optimization, since constructing values is expensive
  // OPTIMIZATION: is it possible to check this with a single traversal? Each quantity_of represents a small linear scan
  if quantity_a_policy_id == ada_policy_id {
    and {
      has_exact_token_count(output_value, 3),
      value.lovelace_of(output_value) == final_protocol_fees + quantity_a_amt,
      value.quantity_of(output_value, quantity_b_policy_id, quantity_b_name) == quantity_b_amt,

      value.quantity_of(
        output_value,
        pool_script_hash,
        pool_nft_name(identifier),
      ) == 1,
    }
  } else {
    // Otherwise, we expect 4 tokens (ADA, Asset A, Asset B, and the NFT)
    // and the corresponding values
    and {
      has_exact_token_count(output_value, 4),
      value.lovelace_of(output_value) == final_protocol_fees,
      value.quantity_of(output_value, quantity_a_policy_id, quantity_a_name) == quantity_a_amt,
      value.quantity_of(output_value, quantity_b_policy_id, quantity_b_name) == quantity_b_amt,
      value.quantity_of(
        output_value,
        pool_script_hash,
        pool_nft_name(identifier),
      ) == 1,
    }
  }
}

// Compare two policy IDs to determine a sort order; used in particular to enforce an ordering on the assets in the pool
fn compare_asset_class(a: AssetClass, b: AssetClass) {
  let (aPolicy, aToken) = a
  let (bPolicy, bToken) = b
  when bytearray.compare(aPolicy, bPolicy) is {
    Less -> Less
    Equal -> bytearray.compare(aToken, bToken)
    Greater -> Greater
  }
}

// Convert a specific integer (like a UTXO index) into a byte array, so we can construct a hashable string when minting the pool
fn int_to_ident(n: Int) -> Ident {
  expect n < 256
  bytearray.push(#"", n)
}

// Tests

type ScoopTestOptions {
  edit_escrow_1_value: Option<Value>,
  edit_escrow_2_value: Option<Value>,
  edit_escrow_destination: Option<Address>,
  edit_fee: Option<Value>,
  edit_swap_fees: Option<(Int,Int)>,
  edit_pool_output_value: Option<Value>,
  edit_settings_datum: Option<Datum>,
}

fn default_scoop_test_options() -> ScoopTestOptions {
  ScoopTestOptions {
    edit_escrow_1_value: None,
    edit_escrow_2_value: None,
    edit_escrow_destination: None,
    edit_fee: None,
    edit_swap_fees: None,
    edit_pool_output_value: None,
    edit_settings_datum: None,
  }
}

test ok_scoop_swap_swap() {
  let options = default_scoop_test_options()
  scoop(options)
}

test ok_scoop_swap_deposit() {
  let options = default_scoop_test_options()
  scoop_swap_deposit(options)
}

test scoop_bad_destination() fail {
  let burn_addr =
    wallet_address(#"12000000000000000000000000000000000000000000000000000000")
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_escrow_destination: Some(burn_addr),
    }
  scoop(options)
}

test scoop_payouts_swapped() fail {
  let dummy_policy_id =
    #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_escrow_1_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_702_095),
      ),
      edit_escrow_2_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_896_088),
      ),
    }
  scoop(options)
}

test pool_validator_ignores_fee() {
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_fee: Some(value.from_lovelace(1_000_000_000)),
    }
  scoop(options)
}

test scoop_high_swap_fees() {
  let hash_of_pool_script =
    #"00000000000000000000000000000000000000000000000000000000"
  let pool_id = #"00000000000000000000000000000000000000000000000000000000"
  let pool_nft_name = shared.pool_nft_name(pool_id)
  let dummy_policy_id =
    #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let swap_fee = 100
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_swap_fees: Some((swap_fee, swap_fee)),
      edit_escrow_1_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_802_950),
      ),
      edit_escrow_2_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_611_678),
      ),
      edit_pool_output_value: Some(
        value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
          |> value.add(
               dummy_policy_id,
               dummy_asset_name,
               1_000_000_000 - ( 9_802_950 + 9_611_678 ),
             )
          |> value.add(hash_of_pool_script, pool_nft_name, 1),
      ),
    }
  scoop(options)
}

test output_missing_nft() fail {
  let dummy_policy_id =
    #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_pool_output_value: Some(
        value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
          |> value.add(
               dummy_policy_id,
               dummy_asset_name,
               1_000_000_000 - ( 9_896_088 + 9_702_095 ),
             ),
      ),
    }
  scoop(options)
}

test scooper_not_in_settings() fail {
  let somebody = #"11111111111111111111111111111111111111111111111111111111"
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_settings_datum: Some(
        InlineDatum(
          SettingsDatum {
            settings_admin: multisig.AnyOf([]),
            metadata_admin: Address(
              VerificationKeyCredential(
                #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
              ),
              None,
            ),
            treasury_admin: multisig.AnyOf([]),
            treasury_address: Address(
              VerificationKeyCredential(
                #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
              ),
              None,
            ),
            treasury_allowance: (1, 10),
            authorized_scoopers: Some([somebody]),
            authorized_staking_keys: [],
            base_fee: 0,
            simple_fee: 2_500_000,
            strategy_fee: 5_000_000,
            extensions: Void,
          },
        ),
      ),
    }
  scoop(options)
}

fn scoop(options: ScoopTestOptions) {
  let settings_policy_id =
    #"00000000000000000000000000000000000000000000000000000000"
  let dummy_policy_id =
    #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let scooper = #"00000000000000000000000000000000000000000000000000000000"
  let hash_of_pool_script =
    #"00000000000000000000000000000000000000000000000000000000"
  let hash_of_escrow_script =
    #"00000000000000000000000000000000000000000000000000000000"
  let user_addr =
    wallet_address(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513")
  let owner =
    multisig.Signature(
      #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
    )
  let pool_id = #"00000000000000000000000000000000000000000000000000000000"
  let pool_datum =
    PoolDatum {
      identifier: pool_id,
      assets: ((#"", #""), (dummy_policy_id, dummy_asset_name)),
      circulating_lp: 1_000_000_000,
      fees_per_10_thousand: option.or_else(options.edit_swap_fees, (5,5)),
      market_open: 0,
      fee_finalized: 100,
      protocol_fees: 2_000_000,
    }
  let pool_out_datum =
    PoolDatum {
      identifier: pool_id,
      assets: ((#"", #""), (dummy_policy_id, dummy_asset_name)),
      circulating_lp: 1_000_000_000,
      fees_per_10_thousand: option.or_else(options.edit_swap_fees, (5,5)),
      market_open: 0,
      fee_finalized: 0,
      protocol_fees: 7_000_000,
    }
  let pool_nft_name = shared.pool_nft_name(pool_id)
  let pool_address = script_address(hash_of_pool_script)
  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: value.from_lovelace(1_000_000_000 + 2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 1_000_000_000)
          |> value.add(hash_of_pool_script, pool_nft_name, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }
  let dest = Destination { address: user_addr, datum: NoDatum }
  let swap =
    Swap((#"", #"", 10_000_000), (dummy_policy_id, dummy_asset_name, 0))
  let escrow_datum =
    OrderDatum {
      pool_ident: None,
      owner,
      max_protocol_fee: 2_500_000,
      destination: dest,
      details: swap,
      extension: builtin.i_data(0),
    }
  let escrow_address = script_address(hash_of_escrow_script)
  let escrow1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: escrow_address,
        value: value.from_lovelace(4_500_000 + 10_000_000),
        datum: InlineDatum(escrow_datum),
        reference_script: None,
      },
    }
  let escrow2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: escrow_address,
        value: value.from_lovelace(4_500_000 + 10_000_000),
        datum: InlineDatum(escrow_datum),
        reference_script: None,
      },
    }
  let settings_input = {
    let Input { output_reference, output } =
      mk_valid_settings_input([scooper], 1)
    let updated_output =
      Output {
        ..output,
        datum: option.or_else(options.edit_settings_datum, output.datum),
      }
    Input { output_reference, output: updated_output }
  }
  let escrow1_out =
    Output {
      address: option.or_else(options.edit_escrow_destination, user_addr),
      value: option.or_else(
        options.edit_escrow_1_value,
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_896_088),
      ),
      datum: NoDatum,
      reference_script: None,
    }
  let escrow2_out =
    Output {
      address: option.or_else(options.edit_escrow_destination, user_addr),
      value: option.or_else(
        options.edit_escrow_2_value,
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_702_095),
      ),
      datum: NoDatum,
      reference_script: None,
    }
  let pool_output =
    Output {
      address: pool_address,
      value: option.or_else(
        options.edit_pool_output_value,
        value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
          |> value.add(
               dummy_policy_id,
               dummy_asset_name,
               1_000_000_000 - ( 9_896_088 + 9_702_095 ),
             )
          |> value.add(hash_of_pool_script, pool_nft_name, 1),
      ),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }
  let ctx =
    ScriptContext {
      transaction: Transaction {
        inputs: [pool_input, escrow1_in, escrow2_in],
        reference_inputs: [settings_input],
        outputs: [pool_output, escrow1_out, escrow2_out],
        fee: option.or_else(options.edit_fee, value.from_lovelace(1_000_000)),
        mint: value.to_minted_value(value.from_lovelace(0)),
        certificates: [],
        withdrawals: dict.new(),
        validity_range: interval.between(1, 2),
        extra_signatories: [scooper],
        redeemers: dict.new(),
        datums: dict.new(),
        id: mk_tx_hash(1),
      },
      purpose: Spend(pool_input.output_reference),
    }
  let pool_redeemer = PoolScoop(0, 0, 0, [(1, None), (2, None)])
  let result = spend(settings_policy_id, pool_datum, pool_redeemer, ctx)
  result
}

fn scoop_swap_deposit(options: ScoopTestOptions) {
  let settings_policy_id = #"00000000000000000000000000000000000000000000000000000000"
  let dummy_policy_id = #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let scooper = #"00000000000000000000000000000000000000000000000000000000"
  let hash_of_pool_script = #"00000000000000000000000000000000000000000000000000000000"
  let hash_of_escrow_script = #"00000000000000000000000000000000000000000000000000000000"
  let user_addr = wallet_address(
    #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"
  )
  let owner = multisig.Signature(
    #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513",
  )
  let pool_id = #"000000000000000000000000000000000000000000000000000000000000"
  let pool_datum = PoolDatum {
    identifier: pool_id,
    assets: (
      (#"", #""),
      (dummy_policy_id, dummy_asset_name),
    ),
    circulating_lp: 1_000_000_000,
    fees_per_10_thousand: option.or_else(options.edit_swap_fees, (5,5)),
    market_open: 0,
    fee_finalized: 0,
    protocol_fees: 2_000_000,
  }
  let pool_out_datum = PoolDatum {
    identifier: pool_id,
    assets: (
      (#"", #""),
      (dummy_policy_id, dummy_asset_name),
    ),
    circulating_lp: 1_009_900_990,
    fees_per_10_thousand: option.or_else(options.edit_swap_fees, (5,5)),
    market_open: 0,
    fee_finalized: 0,
    protocol_fees: 7_000_000,
  }
  let pool_nft_name = shared.pool_nft_name(pool_id)
  let pool_address = script_address(hash_of_pool_script)
  let pool_input = Input {
    output_reference: mk_output_reference(0),
    output: Output {
      address: pool_address,
      value: value.from_lovelace(1_000_000_000 + 2_000_000)
        |> value.add(dummy_policy_id, dummy_asset_name, 1_000_000_000)
        |> value.add(hash_of_pool_script, pool_nft_name, 1),
      datum: InlineDatum(pool_datum),
      reference_script: None,
    },
  }
  let dest = Destination { address: user_addr, datum: NoDatum }
  let swap =
    Swap(
      (#"", #"", 10_000_000),
      (dummy_policy_id, dummy_asset_name, 0),
    )
  let deposit =
    Deposit((
      (#"", #"", 10_000_000),
      (dummy_policy_id, dummy_asset_name, 10_000_000),
    ))
  let escrow_datum_1 = OrderDatum {
    pool_ident: None,
    owner: owner,
    max_protocol_fee: 2_500_000,
    destination: dest,
    details: swap,
    extension: builtin.i_data(0),
  }
  let escrow_datum_2 = OrderDatum {
    pool_ident: None,
    owner: owner,
    max_protocol_fee: 2_500_000,
    destination: dest,
    details: deposit,
    extension: builtin.i_data(0),
  }
  let escrow_address = script_address(hash_of_escrow_script)
  let escrow1_in = Input {
    output_reference: mk_output_reference(2),
    output: Output {
      address: escrow_address,
      value: value.from_lovelace(4_500_000 + 10_000_000),
      datum: InlineDatum(escrow_datum_1),
      reference_script: None,
    },
  }
  let escrow2_in = Input {
    output_reference: mk_output_reference(3),
    output: Output {
      address: escrow_address,
      value: value.from_lovelace(4_500_000 + 10_000_000)
        |> value.add(dummy_policy_id, dummy_asset_name, 10_000_000),
      datum: InlineDatum(escrow_datum_2),
      reference_script: None,
    },
  }
  let settings_input = {
    let Input {output_reference, output} = mk_valid_settings_input([scooper], 1)
    let updated_output = Output {
      ..output,
      datum: option.or_else(options.edit_settings_datum, output.datum)
    }
    Input {
      output_reference: output_reference,
      output: updated_output,
    }
  }
  let escrow1_out = Output {
    address: option.or_else(options.edit_escrow_destination, user_addr),
    value: option.or_else(options.edit_escrow_1_value,
      value.from_lovelace(2_000_000)
        |> value.add(dummy_policy_id, dummy_asset_name, 9_896_088)),
    datum: NoDatum,
    reference_script: None,
  }
  let escrow2_out = Output {
    address: option.or_else(options.edit_escrow_destination, user_addr),
    value: value.from_lovelace(2_000_000)
      |> value.add(hash_of_pool_script, pool_lp_name(pool_id), 9_900_990)
      |> value.add(dummy_policy_id, dummy_asset_name, 196_990),
    datum: NoDatum,
    reference_script: None,
  }
  let pool_output = Output {
    address: pool_address,
    value: option.or_else(options.edit_pool_output_value,
      value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
        |> value.add(dummy_policy_id, dummy_asset_name, 1_000_000_000 - 9_896_088 + 10_000_000 - 196_990)
        |> value.add(hash_of_pool_script, pool_nft_name, 1)),
    datum: InlineDatum(pool_out_datum),
    reference_script: None,
  }

  let ctx = ScriptContext {
    transaction: Transaction {
      inputs: [pool_input, escrow1_in, escrow2_in],
      reference_inputs: [settings_input],
      outputs: [pool_output, escrow1_out, escrow2_out],
      fee: option.or_else(options.edit_fee, value.from_lovelace(1_000_000)),
      mint: value.to_minted_value(
        value.from_lovelace(0)
          |> value.add(hash_of_pool_script, pool_lp_name(pool_id), 9_900_990)
      ),
      certificates: [],
      withdrawals: dict.new(),
      validity_range: interval.between(1, 2),
      extra_signatories: [scooper],
      redeemers: dict.new(),
      datums: dict.new(),
      id: mk_tx_hash(1),
    },
    purpose: Spend(pool_input.output_reference),
  }
  let pool_redeemer = PoolScoop(0, 0, 0, [(1, None),(2, None)])
  let result = spend(settings_policy_id, pool_datum, pool_redeemer, ctx)
  result
}

test mint_test() {
  let settings_policy_id = #"00000000000000000000000000000000000000000000000000000000"
  let hash_of_pool_script = #"00000000000000000000000000000000000000000000000000000000"
  let pool_address = script_address(hash_of_pool_script)
  let rberry_policy_id = #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let rberry_token_name = #"524245525259"
  let user_address =
    wallet_address(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513")
  let settings_input = mk_valid_settings_input([], 1)
  let funds_input = Input {
    output_reference: OutputReference {
      transaction_id: mk_tx_hash(0),
      output_index: 0
    },
    output: Output {
      address: user_address,
      value: value.from_lovelace(10_000_000_000)
        |> value.add(rberry_policy_id, rberry_token_name, 1_000_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
  let pool_id =
    funds_input.output_reference.transaction_id.hash
      |> bytearray.concat(#"23") // '#' character
      |>
      bytearray.concat(int_to_ident(funds_input.output_reference.output_index))
      |> hash.blake2b_256
      |> bytearray.drop(4)
  let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) = shared.pool_token_names(pool_id)
  let pool_output = Output {
    address: pool_address,
    value: value.from_lovelace(1_002_000_000)
      |> value.add(rberry_policy_id, rberry_token_name, 1_000_000_000)
      |> value.add(hash_of_pool_script, new_pool_nft_token, 1),
    datum: InlineDatum(PoolDatum {
      identifier: pool_id,
      assets: ((#"", #""), (rberry_policy_id, rberry_token_name)),
      circulating_lp: 1_000_000_000,
      fees_per_10_thousand: (5, 5),
      market_open: 0,
      fee_finalized: 0,
      protocol_fees: 2_000_000,
    }),
    reference_script: None,
  }
  let lp_output = Output {
    address: user_address,
    value: value.from_lovelace(2_000_000)
      |> value.add(hash_of_pool_script, pool_lp_name(pool_id), 1_000_000_000),
    datum: NoDatum,
    reference_script: None,
  }
  let ref_output = Output {
    address: user_address,
    value: value.from_lovelace(2_000_000)
      |> value.add(hash_of_pool_script, new_pool_ref_token, 1),
    datum: NoDatum,
    reference_script: None,
  }
  let poolMintRedeemer = CreatePool {
    assets: ((#"", #""), (rberry_policy_id, rberry_token_name)),
    pool_output: 0,
    metadata_output: 2,
  }
  let ctx = ScriptContext {
    transaction: Transaction {
      inputs: [funds_input],
      reference_inputs: [settings_input],
      outputs: [pool_output, lp_output, ref_output],
      fee: value.from_lovelace(1_000_000),
      mint: value.to_minted_value(
        value.from_lovelace(0)
          |> value.add(hash_of_pool_script, new_pool_lp_token, 1_000_000_000)
          |> value.add(hash_of_pool_script, new_pool_nft_token, 1)
          |> value.add(hash_of_pool_script, new_pool_ref_token, 1)
      ),
      certificates: [],
      withdrawals: dict.new(),
      validity_range: interval.between(1, 2),
      extra_signatories: [],
      redeemers: dict.new(),
      datums: dict.new(),
      id: mk_tx_hash(1),
    },
    purpose: Mint(hash_of_pool_script),
  }
  let result = mint(settings_policy_id, poolMintRedeemer, ctx)
  result
}
