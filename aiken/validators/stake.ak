use aiken/builtin
use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{ScriptContext, WithdrawFrom}
use aiken/transaction/credential.{Script}
use aiken/transaction/value

validator(pool_script_hash: Hash<Blake2b_224, Script>) {
  fn stake(_r: Data, ctx: ScriptContext) -> Bool {
    when ctx.purpose is {
      WithdrawFrom(_) -> {
        // We can assume that the pool output is the first output
        // and it's safe to check that the pool NFT is on this output, so long
        // as the pool minting policy checks that there are *no* withdrawals present in the transaction
        expect Some(pool_output) = list.head(ctx.transaction.outputs)
        let pool_tokens = value.tokens(pool_output.value, pool_script_hash)
        let pool_tokens_list = dict.to_list(pool_tokens)
        let is_pool_nft =
          fn(kvp: (ByteArray, Int)) {
            // distinguish the pool nft token name by the second byte of its
            // cip68 label
            builtin.index_bytearray(kvp.1st, 1) == 0x0d
          }
        list.any(pool_tokens_list, is_pool_nft)
      }
      _ -> False
    }
  }
}
