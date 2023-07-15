use aiken/bytearray
use aiken/dict
use aiken/list
use aiken/transaction.{ScriptContext, WithdrawFrom}
use aiken/transaction/value

const pool_policy_id = #""

validator {
  fn stake(_r: Data, ctx: ScriptContext) -> Bool {
    when ctx.purpose is {
      WithdrawFrom(_) ->
        // At east one output must contain the pool NFT
        list.any(
          ctx.transaction.inputs,
          fn(out) {
            let pool_tokens = value.tokens(out.output.value, pool_policy_id)
            let pool_tokens_list = dict.to_list(pool_tokens)
            let is_pool_nft =
              fn(kvp: (ByteArray, Int)) { bytearray.take(kvp.1st, 1) == "p" }
            list.any(pool_tokens_list, is_pool_nft)
          },
        )
      _ -> False
    }
    // TODO
  }
}
