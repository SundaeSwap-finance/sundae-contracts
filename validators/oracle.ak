use aiken/dict
use aiken/list
use aiken/hash.{Blake2b_224, Hash}
use aiken/interval
use aiken/transaction.{Transaction, TransactionId, Input, OutputReference, Output, ScriptContext, InlineDatum}
use aiken/transaction/credential.{Address, Script, ScriptCredential}
use aiken/transaction/value
use sundae/multisig
use shared
use types/order.{OrderDatum, Fixed}
use types/pool.{PoolDatum}
use types/oracle.{OracleRedeemer, OracleDatum, Mint, Burn}
use tests/examples/ex_shared.{script_address, mk_tx_hash}

// The oracle script holds an oracle token, and a snapshot of the pool price at the *end* of some scoop.
// This allows other protocols to build integrations that read the pool price (for some confidence interval) without worrying about contention
//
// In particular, this is an agnostic format; because the `record` order type just specifies the policy ID that needs to be minted,
// you could write other versions of the "Oracle" script that supported other oracle formats, like Charli3 or OrcFax.
//
// It's important to use the price at the *end* of the scoop, or at the beginning, rather than just using the price
// "at the time" the order was processed. If we expose the pool price mid-stream, then it is easy to sandwich the order between two others.
// By using the snapshot at the end of the order, such an attacker exposes themselves to arbitrage opportunities which makes such an attack riskier.
validator(pool_script_hash: Hash<Blake2b_224, Script>) {
  // In order to spend the oracle script, two things must be true:
  // - it must be signed by the "owner"
  // - there must be no oracle tokens on the outputs
  // This allows reclaiming funds that were accidentally locked at the script address,
  // while also enforcing that the oracle token is burned; this is important, as people
  // will be relying on the oracle token to authenticate the actual values
  fn spend(datum: Data, _r: Data, ctx: ScriptContext) -> Bool {
    let own_input = shared.spent_output(ctx)
    expect ScriptCredential(own_script_hash) = own_input.address.payment_credential
    expect datum: OracleDatum = datum
    and {
      multisig.satisfied(
        datum.owner,
        ctx.transaction.extra_signatories,
        ctx.transaction.validity_range,
        ctx.transaction.withdrawals,
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
      Mint(pool_ident, order_indices) -> {
        expect transaction.Mint(own_policy_id) = ctx.purpose

        // Calculate the expected pool token names, so we can look for the pool name, and record the LP tokens
        let pool_lp_name = shared.pool_lp_name(pool_ident)
        let pool_nft_name = shared.pool_nft_name(pool_ident)

        // Find the pool output, i.e. the one with the pool NFT, so we can record the prices
        expect Some(pool_output) = list.head(ctx.transaction.outputs)
        expect pool_output.address.payment_credential == ScriptCredential(pool_script_hash)
        expect value.quantity_of(pool_output.value, own_policy_id, pool_nft_name) == 1

        // Then unpack the pool datum
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

        // For each output that produces an oracle, there should be an oracle
        // order in the inputs given by the nth item of the order_indices list
        // in the redeemer
        let (_, no_duplicate_minted_oracles) = list.foldl(
          ctx.transaction.outputs,
          (0, True),
          fn(output, state) {
            let (oracle_minted_index, no_duplicates) = state
            let qty = value.quantity_of(output.value, own_policy_id, oracle_name)
            when qty is {
              0 -> (oracle_minted_index, no_duplicates)
              1 -> {
                expect Some(this_order_index) = list.at(order_indices, oracle_minted_index)
                expect Some(oracle_order) = list.at(ctx.transaction.inputs, this_order_index)
                expect Some(oracle_order_datum) = shared.datum_of(ctx.transaction.datums, oracle_order.output)
                expect oracle_order_datum: OrderDatum = oracle_order_datum
                expect owner = oracle_order_datum.extension
                expect owner: multisig.MultisigScript = owner

                expect output.address.payment_credential == ScriptCredential(own_policy_id)
                expect Some(oracle_datum) = shared.datum_of(ctx.transaction.datums, output)
                expect oracle_datum: OracleDatum = oracle_datum
                expect oracle_datum.valid_range == ctx.transaction.validity_range
                expect oracle_datum.pool_ident == pool_ident
                expect oracle_datum.owner == owner
                expect reserve_a == oracle_datum.reserve_a
                expect reserve_b == oracle_datum.reserve_b
                expect circulating_lp == oracle_datum.circulating_lp
                (oracle_minted_index + 1, no_duplicates)
              }
              _ -> (0, False)
            }
          }
        )
        no_duplicate_minted_oracles
      }
      Burn -> True
    }
  }
}

test oracle_basic() {
  mint_oracle(
    identity,
    identity,
    identity,
  )
}

test oracle_burn_mint() {
  mint_oracle(
    identity,
    identity,
    fn(_) {
      Burn
    }
  )
}

test oracle_redeemer_indices_can_have_extras() {
  let pool_id = #"00"
  mint_oracle(
    identity,
    identity,
    fn(_) {
      Mint(pool_id, [0, 1, 999, -1])
    }
  )
}

test oracle_redeemer_indices_must_match_up() fail {
  let pool_id = #"00"
  mint_oracle(
    identity,
    identity,
    fn(_) {
      Mint(pool_id, [1, 0])
    }
  )
}

// Minting policy enforces that the reserves are correct
test oracle_wrong_datum() fail {
  mint_oracle(
    fn(old_datum) {
      OracleDatum {
        ..old_datum,
        reserve_a: ("", "", 1_000_000_000_000_000_000),
      }
    },
    identity,
    identity,
  )
}

// If we mint a token with the wrong name, we can choose whatever datum we want
test oracle_fake_token() {
  mint_oracle(
    fn(old_datum) {
      OracleDatum {
        ..old_datum,
        reserve_a: ("", "", 1_000_000_000_000_000_000),
      }
    },
    fn(_) {
      "fake"
    },
    identity,
  )
}

fn mint_oracle(
  modify_oracle_datum: fn(OracleDatum) -> OracleDatum,
  modify_oracle_name: fn(ByteArray) -> ByteArray,
  modify_redeemer: fn(OracleRedeemer) -> OracleRedeemer,
) {
  let oracle_policy_id = #"00000000000000000000000000000000000000000000000000000000"
  let oracle_address = Address {
    payment_credential: ScriptCredential(oracle_policy_id),
    stake_credential: None,
  }
  let order_address = Address {
    payment_credential: ScriptCredential(#"1234"),
    stake_credential: None,
  }
  let pool_script_hash = #"00000000000000000000000000000000000000000000000000000000"
  let pool_address = script_address(pool_script_hash)
  let rberry_policy_id = #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let rberry_token_name = #"524245525259"
  let pool_id = #"00"
  let pool_lp_name = shared.pool_lp_name(pool_id)
  let pool_nft_name = shared.pool_nft_name(pool_id)
  // This looks like a fresh pool but pretend that we're scooping
  let pool_output = Output {
    address: pool_address,
    value: value.from_lovelace(1_000_000_000)
      |> value.add(rberry_policy_id, rberry_token_name, 1_000_000_000)
      |> value.add(pool_script_hash, pool_nft_name, 1),
    datum: InlineDatum(PoolDatum {
      identifier: pool_id,
      assets: ((#"", #""), (rberry_policy_id, rberry_token_name)),
      circulating_lp: 1_000_000_000,
      bid_fees_per_10_thousand: (5, 5),
      ask_fees_per_10_thousand: (5, 5),
      fee_manager: None,
      market_open: 0,
      fee_finalized: 0,
      protocol_fees: 2_000_000,
    }),
    reference_script: None,
  }
  let user_1_multisig = multisig.Signature(#"01")
  let user_2_multisig = multisig.Signature(#"02")
  let oracle_name = modify_oracle_name(shared.oracle_sft_name())
  let oracle_order_input_1 = Input {
    output_reference: OutputReference {
      transaction_id: TransactionId { hash: #"00" },
      output_index: 0,
    },
    output: Output {
      address: order_address,
      value: value.from_lovelace(1_000_000),
      datum: InlineDatum(OrderDatum {
        pool_ident: None,
        owner: multisig.AnyOf([]),
        max_protocol_fee: 1_000_000,
        destination: Fixed(oracle_address, InlineDatum(multisig.AnyOf([]))),
        details: order.Record((oracle_policy_id, oracle_name)),
        extension: user_1_multisig,
      }),
      reference_script: None,
    },
  }
  let oracle_order_input_2 = Input {
    output_reference: OutputReference {
      transaction_id: TransactionId { hash: #"00" },
      output_index: 1,
    },
    output: Output {
      address: order_address,
      value: value.from_lovelace(1_000_000),
      datum: InlineDatum(OrderDatum {
        pool_ident: None,
        owner: multisig.AnyOf([]),
        max_protocol_fee: 1_000_000,
        destination: Fixed(oracle_address, InlineDatum(multisig.AnyOf([]))),
        details: order.Record((oracle_policy_id, oracle_name)),
        extension: user_2_multisig,
      }),
      reference_script: None,
    },
  }

  let oracleMintRedeemer = modify_redeemer(Mint(pool_id, [0, 1]))
  let oracle_output_1 = Output {
    address: script_address(oracle_policy_id),
    value: value.from_lovelace(1_000_000)
      |> value.add(oracle_policy_id, oracle_name, 1),
    datum: InlineDatum(modify_oracle_datum(
      OracleDatum {
        owner: user_1_multisig,
        valid_range: interval.between(1, 2),
        pool_ident: pool_id,
        reserve_a: ("", "", 1_000_000_000),
        reserve_b: (rberry_policy_id, rberry_token_name, 1_000_000_000),
        circulating_lp: (pool_script_hash, pool_lp_name, 1_000_000_000),
      },
    )),
    reference_script: None,
  }
  let oracle_output_2 = Output {
    address: script_address(oracle_policy_id),
    value: value.from_lovelace(1_000_000)
      |> value.add(oracle_policy_id, oracle_name, 1),
    datum: InlineDatum(modify_oracle_datum(
      OracleDatum {
        owner: user_2_multisig,
        valid_range: interval.between(1, 2),
        pool_ident: pool_id,
        reserve_a: ("", "", 1_000_000_000),
        reserve_b: (rberry_policy_id, rberry_token_name, 1_000_000_000),
        circulating_lp: (pool_script_hash, pool_lp_name, 1_000_000_000),
      },
    )),
    reference_script: None,
  }
  let ctx = ScriptContext {
    transaction: Transaction {
      inputs: [oracle_order_input_1, oracle_order_input_2],
      outputs: [pool_output, oracle_output_1, oracle_output_2],
      reference_inputs: [],
      fee: value.from_lovelace(1_000_000),
      mint: value.to_minted_value(
        value.from_lovelace(0)
          |> value.add(oracle_policy_id, oracle_name, 1)
      ),
      certificates: [],
      withdrawals: dict.new(),
      validity_range: interval.between(1, 2),
      extra_signatories: [],
      redeemers: dict.new(),
      datums: dict.new(),
      id: mk_tx_hash(1),
    },
    purpose: transaction.Mint(oracle_policy_id),
  }
  let result = mint(oracle_policy_id, oracleMintRedeemer, ctx)
  result
}
