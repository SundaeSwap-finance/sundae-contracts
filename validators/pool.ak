use aiken/bytearray
use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/interval
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, ScriptContext, Spend,
  Transaction, TransactionId, WithdrawFrom,
}
use aiken/transaction/credential.{Address, Inline, Script, ScriptCredential}
use aiken/transaction/value.{
  AssetName, MintedValue, PolicyId, Value, ada_policy_id,
}
use calculation/process.{pool_input_to_state, process_orders}
use shared.{
  AssetClass, Ident, count_orders, pool_lp_name, pool_nft_name, spent_output,
}
use sundae/multisig
use types/pool.{
  BurnPool, CreatePool, Manage, ManageRedeemer, MintLP, PoolDatum,
  PoolMintRedeemer, PoolRedeemer, PoolScoop, UpdatePoolFees, WithdrawFees,
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
validator(
  manage_stake_script_hash: Hash<Blake2b_224, Script>,
  settings_policy_id: PolicyId,
) {
  pub fn spend(datum: PoolDatum, redeemer: PoolRedeemer, ctx: ScriptContext) {
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
      withdrawals,
      ..
    } = transaction

    // Then, (confusing terminology aside) find the specific pool UTXO being spent by this transaction
    let pool_input = spent_output(ctx)

    // And pattern match to get the pool script hash; in particular, this can be used to find the pool output,
    // *and* to know the policy ID of pool tokens, because this is a dual spending/minting validator.
    expect ScriptCredential(pool_script_hash) =
      pool_input.address.payment_credential

    // The protocol configures many global settings via a "settings" UTXO, updatable by certain administrators
    // This is included as a reference input, so we have a utility to check the reference inputs for the settings NFT
    // Note: it's important to check for the NFT, because checking just for the address would let someone pay random funds to the settings address.
    let settings_datum =
      find_settings_datum(reference_inputs, settings_policy_id)

    // Then, there are two different actions that can be taken against a pool:
    // - Scooping a batch of orders
    // - Withdrawing protocol fees to the treasury
    when redeemer is {
      // In the case of the scoop, the redeemer indicates which scooper is doing the scoop, and the order in which the inputs should be processed
      PoolScoop { signatory_index, scooper_index, input_order } -> {
        // Find the pool output, the output datum, and destructure it to access the fields we need to process the scoop
        let (
          Output { address: pool_output_address, value: pool_output_value, .. },
          PoolDatum {
            identifier: actual_identifier,
            circulating_lp: actual_circulating_lp,
            protocol_fees: actual_protocol_fees,
            assets: actual_assets,
            bid_fees_per_10_thousand: actual_bid_fees_per_10_thousand,
            ask_fees_per_10_thousand: actual_ask_fees_per_10_thousand,
            market_open: actual_market_open,
            ..
          },
        ) = find_pool_output(outputs)

        // Ensure that the pool output is to the same payment credential; This is critical, because it ensures that the pool NFT
        // or liquidity aren't paid to some other script in control of an attacker.
        // Note that we check the stake credential is correctly updated (or not) in the various redeemer cases below.
        // We also check that the pool output has the correct output, which ensures it contains the pool NFT,
        // meaning this can't just be a "token output" with the correct payment credential, but everything paid elsewhere.
        expect
          pool_output_address.payment_credential == ScriptCredential(
            pool_script_hash,
          )

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
          pool_input_to_state(
            pool_script_hash,
            datum,
            pool_input,
            validity_range.lower_bound,
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
        expect
          actual_protocol_fees == initial_protocol_fees + expected_fees_collected

        // The pool should have all of the scooper fees, and the quantity of each token of the outcome
        // Note that initializing the state with `-transaction.fee` means this gets subracted out of the protocol fees
        // TODO: do we need to account for this? it seems to have gotten lost in some changes.
        expect
          minted_correct_pool_tokens(pool_script_hash, mint, datum, final_lp)

        // Check that the scooper is authorized; the protocol can allow *any* scoopers, or limit it to a set of actors
        // It's safe to use values provided in the redeemer to efficiently skip to the expected scooper / expected signature
        // because at the end of the day, we just care that the scooper has signed the transaction. If the scooper provides
        // anything but the correct indexes, it'll just fail the transaction.
        expect
          when authorized_scoopers is {
            Some(authorized_scoopers) -> {
              // OPTIMIZATION: skip 10 entries at a time
              // OPTIMIZATION: assume scooper is first extra_signatory? have to assume there will only ever be one extra_signatory
              expect Some(scooper_sig) =
                list.at(extra_signatories, signatory_index)
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
            actual_protocol_fees,
          )
        // Now, we check various things about the output datum to ensure they're each correct.
        // Check that the datum correctly records the final circulating LP, accounting for any deposits and withdrawals
        // In particular, this is important because that circulating supply is exaclty what determines the users ownership of assets in the pool
        // If this gets out of sync with what we've actually minted, then users will be able to either redeem assets they aren't entitled to,
        // or be unable to access funds they are entitled to.
        expect actual_circulating_lp == final_lp

        // Make sure the protocol fees have been correctly updated
        expect
          actual_protocol_fees == initial_protocol_fees + expected_fees_collected

        // And make sure each of these fields is unchanged
        and {
          datum.identifier == actual_identifier,
          datum.assets == actual_assets,
          datum.bid_fees_per_10_thousand == actual_bid_fees_per_10_thousand,
          datum.ask_fees_per_10_thousand == actual_ask_fees_per_10_thousand,
          datum.market_open == actual_market_open,
          // Finally, make sure we don't change the stake credential; this can only be done when withdrawing fees, by the treasury administrator
          pool_input.address.stake_credential == pool_output_address.stake_credential,
        }
      }
      Manage ->
        // There must be a redeemer for a withdrawal against the manage stake script,
        // and the redeemer must be valid
        dict.foldl(
          ctx.transaction.redeemers,
          False,
          fn(script_purpose, redeemer, acc) {
            when script_purpose is {
              WithdrawFrom(Inline(ScriptCredential(script))) -> {
                let is_valid_manage_script_invoke =
                  if script == manage_stake_script_hash {
                    expect redeemer: ManageRedeemer = redeemer
                    let redeemer_pool_input =
                      when redeemer is {
                        UpdatePoolFees { pool_input } -> pool_input
                        WithdrawFees { pool_input, .. } -> pool_input
                      }
                    expect Spend(OutputReference { output_index, .. }) =
                      ctx.purpose
                    // Manage redeemer must have the correct index of this pool input
                    output_index == redeemer_pool_input
                  } else {
                    False
                  }
                acc || is_valid_manage_script_invoke
              }
              _ -> acc
            }
          },
        )
    }
  }

  pub fn mint(r: PoolMintRedeemer, ctx: ScriptContext) {
    // When minting, we can be doing one of two things: minting the pool itself, or minting the LP token
    when r is {
      // For creating a new pool, one of our design objectives was to avoid requiring interaction with any global 
      // "factory", as in v1; this created a lot of contention, and didn't serve it's original goal of ensuring that
      // the (pair, fee) was unique.
      CreatePool(assets, pool_output_ix, metadata_output_ix) -> {
        expect Mint(own_policy_id) = ctx.purpose

        // And grab the pool output
        expect Some(pool_output) =
          list.at(ctx.transaction.outputs, pool_output_ix)

        // Check that the pool datum is inline, because a datum hash could brick this pool
        expect InlineDatum(d) = pool_output.datum
        expect pool_output_datum: PoolDatum = d

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
        let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) =
          shared.pool_token_names(new_pool_id)

        // Then, find the settings datum, so we can ensure the reference token is paid to the metadata admin
        let reference_inputs = ctx.transaction.reference_inputs
        let settings_datum =
          find_settings_datum(reference_inputs, settings_policy_id)

        // Grab the initial reserves of each token by looking at what's paid to the UTXO
        let coin_a_amt =
          value.quantity_of(pool_output.value, asset_a.1st, asset_a.2nd)
        let coin_b_amt =
          value.quantity_of(pool_output.value, asset_b.1st, asset_b.2nd)

        // Ensure that the pool pays the pool creation fee, if any, by ensuring that the initial protocol_fees value is greater than or equal to the fee
        expect
          pool_output_datum.protocol_fees >= settings_datum.pool_creation_fee

        // Only ada has a null policy id. If coin A is ada, subtract the initial protocol_fees setting from the coin A amount
        // rider from the output to get the true amount in the pool.
        let coin_a_amt_sans_protocol_fees =
          if bytearray.is_empty(asset_a.1st) {
            coin_a_amt - pool_output_datum.protocol_fees
          } else {
            coin_a_amt
          }

        // Check that the quantity of LP tokens is correct; In particular, we adopt Uniswaps convention of
        // using the sqrt of the product of the two values for the initial number of LP tokens to mint..
        // This helps minimize precision loss: it gives decent initial liquidity values for a range of
        // sizes of pools, such that an individual LP token is granular enough for depositing and withdrawing for most users.
        // In particular, though, we don't calculate the sqrt here, which is an expensive function; we instead verify that the
        // amount minted is valid by checking that it squares to the correct product
        let initial_lq = pool_output_datum.circulating_lp
        expect
          shared.is_sqrt(coin_a_amt_sans_protocol_fees * coin_b_amt, initial_lq)

        // And check that we mint the correct tokens, and nothing else.
        let expected_mint =
          shared.to_value((own_policy_id, new_pool_ref_token, 1))
            |> value.merge(shared.to_value((own_policy_id, new_pool_nft_token, 1)))
            |> value.merge(
                shared.to_value((own_policy_id, new_pool_lp_token, initial_lq)),
              )
        let mint_is_correct =
          value.from_minted_value(ctx.transaction.mint) == expected_mint

        // Confirm that the correct funds (asset A, asset B, the correct amount of ADA, and the pool NFT) get paid to the pool output
        let funds_spent_to_pool =
          has_expected_pool_value(
            own_policy_id,
            new_pool_id,
            pool_output.value,
            asset_a.1st,
            asset_a.2nd,
            coin_a_amt_sans_protocol_fees,
            asset_b.1st,
            asset_b.2nd,
            coin_b_amt,
            pool_output_datum.protocol_fees,
          )

        // Make sure we send the pool metadata token to the metadata admin
        // We use an index from the redeemer to skip to the right output, in case there are multiple outputs to the metadata admin
        // This is safe to do for the usual reasons: if they point at a UTXO without the ref token, the transaction will fail.
        expect Some(metadata_output) =
          list.at(ctx.transaction.outputs, metadata_output_ix)
        expect metadata_output.address == settings_datum.metadata_admin
        expect
          value.quantity_of(
            metadata_output.value,
            own_policy_id,
            new_pool_ref_token,
          ) == 1

        // We also check that the datum on the metadata output is void; It would be complex and in-flexible to enforce any particular structure on this, so we
        // instead leave it to the metadata admin to spend the output and provide it the correct datum; We also don't want to leave it unspecified, because
        // 1) the metadata admin might actually be a script address, in which case having no datum will permanently lock the metadata
        // 2) the pool minter might include malicious metadata, such as an icon pointing at hardcore porn; until the metadata admin spent it, this would appear in users wallets,
        //    and potentially even on access UIs for the Sundae protocol
        expect metadata_output.datum == InlineDatum(Void)

        // And check that the datum is initialized correctly; This is part of why we have a minting policy handling this,
        // as it allows us to authenticate the providence of the datum.
        // A datum is valid so long as
        // - the pool identifier is set correctly
        // - the assets is set correctly
        // - the initial circulating supply is set correctly
        // - the market open time is before the fee finalized time; TODO: should we relax this?
        //   I'm not sure it's harmful if someone initializes this with a feeFinalized in the past
        // - the initial and final fees per 10,000 are both non-negative (>= 0%)
        // - the intitial and final fees per 10,000 are both less than or equal to 10000 (<= 100%)
        let pool_output_datum_correct = and {
            pool_output_datum.identifier == new_pool_id,
            pool_output_datum.assets == (asset_a, asset_b),
            pool_output_datum.circulating_lp == initial_lq,
            pool_output_datum.market_open <= pool_output_datum.fee_finalized,
            shared.fees_in_legal_range(
              pool_output_datum.bid_fees_per_10_thousand,
            ),
            shared.fees_in_legal_range(
              pool_output_datum.ask_fees_per_10_thousand,
            ),
          }

        // Make sure that the pool output is paid into own_policy_id (the pool script, remember this is a multivalidator)
        // and that one of the valid staking addresses is attached
        expect
          pool_output.address.payment_credential == ScriptCredential(
            own_policy_id,
          )
        expect
          list.any(
            settings_datum.authorized_staking_keys,
            fn(a) { pool_output.address.stake_credential == Some(Inline(a)) },
          )

        // And then check each of the conditions above as the condition for minting
        and {
          coin_pair_ordering_is_canonical,
          mint_is_correct,
          funds_spent_to_pool,
          pool_output_datum_correct,
        }
      }
      // When minting an LP token, we just need to make sure the pool script is being spent, as it will enforce the correct
      // name and quantity of the LP tokens.
      //
      // To do that, we could check for the pool NFT on the inputs, but this is expensive, especially if the pool input ends up being one of the last.
      // So instead we check that the pool NFT is in the first output (this is safe to assume because it's unique, and if it's in any other output it will fail)
      // and that we're not minting the pool token (i.e. someone could "pretend" to mint LP tokens, but also mint the pool token to make it look like a scoop)
      // 
      // So, lets enumerate the possible cases:
      // - We use the CreatePool redeemer; this checks that *only* the correct pool token and correct number of LP tokens are minted
      // - We use the MintLP redeemer; this checks that the pool token (which is unique and locked in the pool script) is in the outputs, and not minted
      //   - the pool script checks that only the correct number of LP tokens, and nothing else under this policy ID, are minted
      // And the impossible cases:
      // - During CreatePool, it would be impossible to mint multiple of the same pool tokens; a different pool token; a different number of LP tokens; or a different pool's LP tokens
      // - During MintLP, it would be impossible to mint the relevant pool token; thus, the pool script must run, and thus it will be impossible to mint another pool token, a different pool
      //   ident pool token, a different quantity of LP tokens, or a different pools LP tokens
      MintLP(pool_ident) -> {
        expect Mint(own_policy_id) = ctx.purpose
        let pool_nft_name = shared.pool_nft_name(pool_ident)
        expect Some(pool_output) = list.head(ctx.transaction.outputs)
        and {
          (
            pool_output.value |> value.quantity_of(own_policy_id, pool_nft_name)
          ) == 1,
          (
            ctx.transaction.mint
              |> value.from_minted_value
              |> value.quantity_of(own_policy_id, pool_nft_name)
          ) == 0,
        }
      }
      BurnPool(pool_ident) -> {
        // Burning an asset is only possible when spending it, so if we enforce
        // that the mints consist of exactly 1 burn for the specified pool NFT
        // then we can defer to the pool spending validator
        expect Mint(own_policy_id) = ctx.purpose
        let pool_nft_name = shared.pool_nft_name(pool_ident)
        let expected_mint = shared.to_value((own_policy_id, pool_nft_name, -1))
        value.from_minted_value(ctx.transaction.mint) == expected_mint
      }
    }
  }
}

fn find_pool_output(outputs: List<Output>) -> (Output, PoolDatum) {
  // Find the pool output; we can assume the pool output is the first output, because:
  // - The ledger doesn't reorder outputs, just inputs
  // - We check that the address is correct, so if the first output was to a different contract, we would fail
  // - We check that the datum is the correct type, meaning we can't construct an invalid pool output
  // - Later, we check that the pool output has the correct value, meaning it *must* contain the pool token, so we can't pay to the pool script multiple times
  expect Some(pool_output) = list.head(outputs)
  expect InlineDatum(output_datum) = pool_output.datum
  expect output_datum: PoolDatum = output_datum
  (pool_output, output_datum)
}

/// This is responsible for checking that the minting value on the transaction is valid
/// based on the pool state, the policy ID, and the initial incoming datum.
fn minted_correct_pool_tokens(
  pool_policy_id: PolicyId,
  mint: MintedValue,
  datum: PoolDatum,
  quantity_lp: Int,
) -> Bool {
  // Unwrap the silly MintedValue special type
  // Note also we only look at the tokens with this policyID
  // so that we can still mint other tokens
  let minted_tokens =
    value.tokens(value.from_minted_value(mint), pool_policy_id)

  // If the initial datum has the same circulating LP as the outcome, then we expect no minted tokens
  // Otherwise, the minted tokens should be exactly the pool LP tokens and nothing else
  // TODO: confirm that the "minting 0 ada" problem doesn't apply here; we have real-world transactions, so I doubt it does, but I want to confirm.
  // TODO: This should allow minting of other tokens, from other policy IDs, perhaps, for composibility?
  if datum.circulating_lp == quantity_lp {
    dict.is_empty(minted_tokens)
  } else {
    dict.to_list(minted_tokens) == [
      (pool_lp_name(datum.identifier), quantity_lp - datum.circulating_lp),
    ]
  }
}

/// Check that the UTXO contents are correct given a specific pool outcome
/// In particular, it must have the final A reserves, the final B reserves, the pool NFT, and the protocol fees
pub fn has_expected_pool_value(
  pool_script_hash: PolicyId,
  identifier: Ident,
  output_value: Value,
  pool_policy_a: PolicyId,
  pool_asset_name_a: AssetName,
  pool_quantity_a: Int,
  pool_policy_b: PolicyId,
  pool_asset_name_b: AssetName,
  pool_quantity_b: Int,
  final_protocol_fees: Int,
) -> Bool {
  // Asset A *could* be ADA; in which case there should be 3 tokens on the output
  // (ADA, Asset B, and the NFT)
  if pool_policy_a == ada_policy_id {
    let actual =
      list.foldl(
        value.flatten(output_value),
        // (token count, lovelace amount, token b amount, pool nft amount)
        (0, 0, 0, 0),
        fn(asset, acc) {
          let token_count = acc.1st + 1
          if asset.1st == pool_policy_a {
            (token_count, acc.2nd + asset.3rd, acc.3rd, acc.4th)
          } else if asset.1st == pool_policy_b && asset.2nd == pool_asset_name_b {
            (token_count, acc.2nd, acc.3rd + asset.3rd, acc.4th)
          } else {
            expect asset == (pool_script_hash, pool_nft_name(identifier), 1)
            (token_count, acc.2nd, acc.3rd, acc.4th + 1)
          }
        },
      )
    let expected =
      (3, final_protocol_fees + pool_quantity_a, pool_quantity_b, 1)
    // Rather than constructing a value directly (which can be expensive)
    // we can just compare the expected token count and amounts with a single pass over the value
    expected == actual
  } else {
    // Asset A isn't ADA, Asset B will *never* be ADA; in this case, there should be 4 tokens on the output:
    // ADA, the Pool NFT, Asset A, and Asset B
    (4, final_protocol_fees, pool_quantity_a, pool_quantity_b, 1) == list.foldl(
      value.flatten(output_value),
      // (token count, lovelace amount, token a amount, token b amount, pool nft amount)
      (0, 0, 0, 0, 0),
      fn(asset, acc) {
        let token_count = acc.1st + 1
        if asset.1st == ada_policy_id {
          (token_count, acc.2nd + asset.3rd, acc.3rd, acc.4th, acc.5th)
        } else if asset.1st == pool_policy_a && asset.2nd == pool_asset_name_a {
          (token_count, acc.2nd, acc.3rd + asset.3rd, acc.4th, acc.5th)
        } else if asset.1st == pool_policy_b && asset.2nd == pool_asset_name_b {
          (token_count, acc.2nd, acc.3rd, acc.4th + asset.3rd, acc.5th)
        } else {
          expect asset == (pool_script_hash, pool_nft_name(identifier), 1)
          (token_count, acc.2nd, acc.3rd, acc.4th, acc.5th + 1)
        }
      },
    )
  }
}

// Compare two policy IDs to determine a sort order; used in particular to enforce an ordering on the assets in the pool
fn compare_asset_class(a: AssetClass, b: AssetClass) {
  let (a_policy, a_token) = a
  let (b_policy, b_token) = b
  when bytearray.compare(a_policy, b_policy) is {
    Less -> Less
    Equal -> bytearray.compare(a_token, b_token)
    Greater -> Greater
  }
}

// Convert a specific integer (like a UTXO index) into a byte array, so we can construct a hashable string when minting the pool
pub fn int_to_ident(n: Int) -> Ident {
  expect n < 256
  bytearray.push(#"", n)
}

validator(settings_policy_id: PolicyId) {
  pub fn manage(redeemer: ManageRedeemer, ctx: ScriptContext) {
    let transaction = ctx.transaction
    let Transaction {
      inputs,
      outputs,
      reference_inputs,
      mint,
      extra_signatories,
      validity_range,
      withdrawals,
      ..
    } = transaction
    let settings_datum =
      find_settings_datum(reference_inputs, settings_policy_id)
    when redeemer is {
      WithdrawFees { amount, treasury_output, pool_input } -> {
        expect Some(pool_input) = list.at(inputs, pool_input)
        let pool_input = pool_input.output
        expect InlineDatum(datum) = pool_input.datum
        expect datum: PoolDatum = datum
        expect ScriptCredential(pool_script_hash) =
          pool_input.address.payment_credential
        let PoolDatum {
          circulating_lp: initial_circulating_lp,
          protocol_fees: initial_protocol_fees,
          ..
        } = datum
        // Make sure we withdraw *only* up to what we've earned
        // We allow less than, so that you can leave some behind for the minUTXO cost, or continuing to earn staking rewards, etc.
        expect amount <= initial_protocol_fees

        // Only the treasury administrator is allowed to withdraw the fees, to prevent DDOS, and because of the allowance below
        expect
          multisig.satisfied(
            settings_datum.treasury_admin,
            extra_signatories,
            validity_range,
            withdrawals,
          )

        // Asking the DAO to approve every single cost individually would be a small cognitive DDOS on the community
        // Instead, the DAO can set an "allowance", which is a percentage of each withdrawal that is entrusted to the
        // treasury administrator to pay those basic fees, such as paying the scoopers, or running incentive programs, etc.
        //
        // In particular, it's a percentage, to ensure that splitting up the withdrawals into multiple transactions doesn't
        // allow them to game that withdrawal.
        let allowance =
          amount * settings_datum.treasury_allowance.1st / settings_datum.treasury_allowance.2nd
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

        if initial_circulating_lp == 0 {
          // If there is no liquidity, just require the treasury admin to withdraw everything
          expect amount == initial_protocol_fees
          // If circulating_lp is 0, all of the assets have been withdrawn, and so the UTXO will be
          // ADA (for the treasury fees) and the pool NFT; so we can very cleverly check that the pool
          // NFT is burned by negating the input, and stripping off the lovelace
          expect
            value.from_minted_value(mint) == value.negate(
              value.without_lovelace(pool_input.value),
            )
          True
        } else {
          let (
            Output {
              address: pool_output_address,
              value: pool_output_value,
              ..
            },
            output_datum,
          ) = find_pool_output(outputs)
          expect
            pool_output_address.payment_credential == ScriptCredential(
              pool_script_hash,
            )

          // As part of withdrawing, we should decrease the protocol fees by the amount we're withdrawing
          // but, importantly, *nothing else*; so we construct a datum with everything from the initial datum, plus the protofol fees updated
          let expected_datum =
            PoolDatum { ..datum, protocol_fees: initial_protocol_fees - amount }
          expect output_datum == expected_datum

          // Now, check that the pool output decreases *only* by the amount we're withdrawing, and not by fewer or greater ADA
          let expected_output_value =
            value.merge(pool_input.value, value.from_lovelace(-amount))
          expect pool_output_value == expected_output_value
          expect
            list.any(
              settings_datum.authorized_staking_keys,
              fn(a) { pool_output_address.stake_credential == Some(Inline(a)) },
            )
          True
        }
      }
      UpdatePoolFees { pool_input } -> {
        expect Some(pool_input) = list.at(inputs, pool_input)
        let pool_input = pool_input.output
        expect InlineDatum(datum) = pool_input.datum
        expect datum: PoolDatum = datum

        let (
          Output { address: pool_output_address, value: pool_output_value, .. },
          pool_output_datum,
        ) = find_pool_output(outputs)
        let PoolDatum {
          bid_fees_per_10_thousand,
          ask_fees_per_10_thousand,
          fee_manager: output_fee_manager,
          ..
        } = pool_output_datum

        expect shared.fees_in_legal_range(bid_fees_per_10_thousand)
        expect shared.fees_in_legal_range(ask_fees_per_10_thousand)

        let expected_datum =
          PoolDatum {
            ..datum,
            bid_fees_per_10_thousand: bid_fees_per_10_thousand,
            ask_fees_per_10_thousand: ask_fees_per_10_thousand,
            fee_manager: output_fee_manager,
          }
        expect pool_output_datum == expected_datum

        expect Some(fee_manager) = datum.fee_manager
        expect
          multisig.satisfied(
            fee_manager,
            extra_signatories,
            validity_range,
            withdrawals,
          )

        expect pool_output_address == pool_input.address
        expect pool_output_value == pool_input.value
        True
      }
    }
  }
}
