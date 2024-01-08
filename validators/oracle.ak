use aiken/list
use aiken/transaction/value
use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction.{ScriptContext, InlineDatum}
use aiken/transaction/credential.{Script, ScriptCredential}
use sundae/multisig
use shared
use types/pool.{PoolDatum}
use types/oracle.{OracleRedeemer, OracleDatum, Mint, Burn}

// The oracle script holds an oracle token, and a snapshot of the pool price at the *end* of some scoop.
// This allows other protocols to build integrations that read the pool price (for some confidence interval) without worrying about contention
//
// It's important to use the price at the *end* of the scoop, or at the beginning, rather than just using the price
// "at the time" the order was processed. If we expose the pool price mid-stream, then it is easy to sandwich the order between two others.
// By using the snapshot at the end of the order, such an attacker exposes themselves to arbitrage opportunities which makes such an attack riskier.   
validator(pool_script_hash: Hash<Blake2b_224, Script>) {
  // In order to spend the oracle script, two things must be true:
  // - it must be signed by the "owner"
  // - there must be no oracle tokens on the outputs
  // This allows reclaiming funds that were accidentally locked at the script address,
  // while also enforcing that the oracle token is burned
  fn spend(datum: Data, _r: Data, ctx: ScriptContext) -> Bool {
    let own_input = shared.spent_output(ctx)
    expect ScriptCredential(own_script_hash) = own_input.address.payment_credential
    expect datum: OracleDatum = datum
    and {
      multisig.satisfied(
        datum.owner,
        ctx.transaction.extra_signatories,
        ctx.transaction.validity_range,
      ),
      list.all(
        ctx.transaction.outputs,
        fn(output) {
          value.quantity_of(output.value, own_script_hash, shared.oracle_sft_name()) == 0
        },
      )
    }
  }
  // In order to mint an orcale token, two things must be true:
  // - each oracle token on the outputs must be paid with a quantity of 1 to the oracle script
  // - the datum for each must have the correct timing and pricing information
  // Burning an oracle token is always allowed
  fn mint(redeemer: OracleRedeemer, ctx: ScriptContext) {
    when redeemer is {
      Mint(pool_ident) -> {
        expect transaction.Mint(own_policy_id) = ctx.purpose
        let pool_lp_name = shared.pool_lp_name(pool_ident)
        let pool_nft_name = shared.pool_nft_name(pool_ident)

        expect Some(pool_output) = list.head(ctx.transaction.outputs)
        expect pool_output.address.payment_credential == ScriptCredential(pool_script_hash)
        expect value.quantity_of(pool_output.value, own_policy_id, pool_nft_name) == 1
        expect InlineDatum(pool_datum) = pool_output.datum
        expect pool_datum: PoolDatum = pool_datum
        let PoolDatum {
          assets: (asset_a, asset_b),
          circulating_lp,
          ..
        } = pool_datum

        let reserve_a = (asset_a.1st, asset_a.2nd, value.quantity_of(pool_output.value, asset_a.1st, asset_a.2nd))
        let reserve_b = (asset_b.1st, asset_b.2nd, value.quantity_of(pool_output.value, asset_b.1st, asset_b.2nd))
        let circulating_lp = (pool_script_hash, pool_lp_name, circulating_lp)

        let oracle_name = shared.oracle_sft_name()

        list.all(
          ctx.transaction.outputs,
          fn(output) {
            let qty = value.quantity_of(output.value, own_policy_id, oracle_name)
            when qty is {
              0 -> True
              1 -> {
                expect output.address.payment_credential == ScriptCredential(own_policy_id)
                expect Some(oracle_datum) = shared.datum_of(ctx.transaction.datums, output)
                expect oracle_datum: OracleDatum = oracle_datum
                expect oracle_datum.valid_range == ctx.transaction.validity_range
                expect oracle_datum.pool_ident == pool_ident
                expect reserve_a == oracle_datum.reserve_a
                expect reserve_b == oracle_datum.reserve_b
                expect circulating_lp == oracle_datum.circulating_lp
                True
              }
              _ -> False
            }
          }
        )
      }
      Burn -> True
    }
  }
}
