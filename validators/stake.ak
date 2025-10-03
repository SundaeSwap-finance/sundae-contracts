use aiken/builtin
use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{ScriptContext, WithdrawFrom}
use aiken/transaction/credential.{Script}
use aiken/transaction/value

/// The stake validator is the indirection used to allow orders to safely execute in a scoop
/// Each order can very efficiently check for this stake script, and then this stake script can look for the pool token
/// just once, even if we have 35 orders.
///
/// It's parameterized by the pool_script_hash, so it knows what pool token to look for.
validator(pool_script_hash: Hash<Blake2b_224, Script>) {
  fn stake(_r: Data, ctx: ScriptContext) -> Bool {
    when ctx.purpose is {
      WithdrawFrom(_) -> {
        // We can assume that the pool output is the first output, because outputs aren't reordered by the ledger,
        // and nothing is harmed by making this assumption;
        // Additionally, if we find an output that *isn't* the pool output, the worst that will happen is the
        // transaction will fail, because it doesn't have the unique pool identifier
        // We should also ensure when creating the pool that there are no withdrawals, to prevent
        // spending orders as if they were a scoop while minting a pool (which would satisfy the pool NFT being on the output)
        // That way we can also avoid having to check the mint here, whcih saves a good bit of execution units
        expect Some(pool_output) = list.head(ctx.transaction.outputs)
        // Lookup anything with the policy ID of the pool script hash we're parameterized by
        let pool_tokens = value.tokens(pool_output.value, pool_script_hash)
        let pool_tokens_list = dict.to_list(pool_tokens)
        // And then check that it's specifically the pool NFT by taking advantage of the CIP-68 label
        let is_pool_nft =
          fn(
            /// Note: we don't allow this address to be delegated to a pool, only withdrawals (which will always be zero)
            /// because a very subtle attack vector would be to register the address, delegate to a pool,
            /// and break any offchain code that was assuming the withdraw would be zero
            /// because most off-chain code won't have access to the rewards calculation!
            kvp: (ByteArray, Int),
          ) {
            // we use the 2nd byte here, because the first byte is zero as a "parenthesis"
            builtin.index_bytearray(kvp.1st, 1) == 0x0d
          }
        // If any of the tokens are the pool NFT (needed because there are likely other tradable assets on the pool)
        // then we can be sure the pool validator itself will run, and that will enforce all of the important security constraints
        list.any(pool_tokens_list, is_pool_nft)
      }
      _ -> False
    }
  }
}
