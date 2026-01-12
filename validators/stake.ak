use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{ScriptHash}
use cardano/assets
use cardano/script_context.{ScriptContext, Withdrawing}
use cardano/transaction.{Input, Spend, Transaction}
use types/pool.{DirectScoop, PoolRedeemer}

/// The stake validator is the indirection used to allow orders to safely execute in a scoop
/// Each order can very efficiently check for this stake script, and then this stake script can look for the pool token
/// just once, even if we have 35 orders.
///
/// It's parameterized by the pool_script_hash, so it knows what pool token to look for.
validator stake(pool_script_hash: ScriptHash) {
  else(ctx: ScriptContext) {
    when ctx.info is {
      Withdrawing(_) -> {
        // We can assume that the pool output is the first output, because outputs aren't reordered by the ledger,
        // and nothing is harmed by making this assumption;
        // Additionally, if we find an output that *isn't* the pool output, the worst that will happen is the
        // transaction will fail, because it doesn't have the unique pool identifier
        // We should also ensure when creating the pool that there are no withdrawals, to prevent
        // spending orders as if they were a scoop while minting a pool (which would satisfy the pool NFT being on the output)
        // That way we can also avoid having to check the mint here, whcih saves a good bit of execution units
        expect Some(pool_output) = list.head(ctx.transaction.outputs)
        // Lookup anything with the policy ID of the pool script hash we're parameterized by
        let pool_tokens = assets.tokens(pool_output.value, pool_script_hash)
        let pool_tokens_list = dict.to_pairs(pool_tokens)
        // And then check that it's specifically the pool NFT by taking advantage of the CIP-68 label
        let is_pool_nft =
          fn(
            /// Note: we don't allow this address to be delegated to a pool, only withdrawals (which will always be zero)
            /// because a very subtle attack vector would be to register the address, delegate to a pool,
            /// and break any offchain code that was assuming the withdraw would be zero
            /// because most off-chain code won't have access to the rewards calculation!
            kvp: Pair<ByteArray, Int>,
          ) {
            // we use the 2nd byte here, because the first byte is zero as a "parenthesis"
            builtin.index_bytearray(kvp.1st, 1) == 0xd
          }
        // Check that pool NFT is present on output
        expect list.any(pool_tokens_list, is_pool_nft)

        // SECURITY: Check that the pool is NOT being spent with DirectScoop.
        // DirectScoop doesn't process orders, so if we allowed it here, orders could be
        // "authorized" but not actually processed, allowing their funds to be stolen.
        // A future version of this stake script could add a DirectScoop redeemer that
        // properly validates each order's output, but for now we simply disallow it.
        // This is safe because DirectScoop is meant for direct pool interaction without orders.
        let pool_input =
          list.find(
            ctx.transaction.inputs,
            fn(input) {
              let input_pool_tokens =
                assets.tokens(input.output.value, pool_script_hash)
              let input_pool_tokens_list = dict.to_pairs(input_pool_tokens)
              list.any(input_pool_tokens_list, is_pool_nft)
            },
          )
        when pool_input is {
          Some(Input { output_reference, .. }) -> {
            // Look up the redeemer for this pool input
            let pool_redeemer =
              pairs.get_first(ctx.transaction.redeemers, Spend(output_reference))
            when pool_redeemer is {
              Some(redeemer) -> {
                // If it's DirectScoop, fail - orders cannot be processed with DirectScoop
                expect pool_redeemer: PoolRedeemer = redeemer
                when pool_redeemer is {
                  DirectScoop { .. } -> False
                  _ -> True
                }
              }
              // No redeemer found - this shouldn't happen but fail safely
              None -> False
            }
          }
          // No pool input found - fail
          None -> False
        }
      }
      _ -> False
    }
  }
}
