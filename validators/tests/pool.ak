use aiken/builtin
use aiken/bytearray
use aiken/dict
use aiken/hash
use aiken/interval
use aiken/option
use aiken/transaction.{
  Datum, InlineDatum, Input, NoDatum, Output, OutputReference,
  ScriptContext, Spend, Transaction, TransactionId,
}
use aiken/transaction/credential.{
  Address, VerificationKeyCredential, from_verification_key, from_script, with_delegation_key
}
use aiken/transaction/value.{Value}
use shared.{
  pool_lp_name,
}
use sundae/multisig
use tests/examples/ex_settings.{mk_valid_settings_input}
use tests/examples/ex_shared.{
  mk_output_reference, mk_tx_hash, script_address, wallet_address,
}
use types/order.{Deposit, Destination, Fixed, Self, OrderDatum, Swap}
use types/pool.{
  CreatePool, PoolDatum, PoolScoop,
}
use types/settings.{SettingsDatum}
use tx_util/builder.{add_asset_to_tx_output, add_tx_input, add_tx_output, add_tx_ref_input, build_txn_context, mint_assets, new_tx_input, new_tx_output}
use pool as pool_validator


type ScoopTestOptions {
  edit_escrow_1_value: Option<Value>,
  edit_escrow_2_value: Option<Value>,
  edit_escrow_intended_destination: Option<Destination>,
  edit_escrow_actual_destination: Option<Destination>,
  edit_fee: Option<Value>,
  edit_swap_fees: Option<(Int,Int)>,
  edit_pool_input_address: Option<Address>,
  edit_pool_output_address: Option<Address>,
  edit_pool_output_value: Option<Value>,
  edit_settings_datum: Option<Datum>,
}

fn default_scoop_test_options() -> ScoopTestOptions {
  ScoopTestOptions {
    edit_escrow_1_value: None,
    edit_escrow_2_value: None,
    edit_escrow_intended_destination: None,
    edit_escrow_actual_destination: None,
    edit_fee: None,
    edit_swap_fees: None,
    edit_pool_input_address: None,
    edit_pool_output_address: None,
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
      edit_escrow_actual_destination: Some(Fixed(burn_addr, NoDatum)),
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
          |> value.add(pool_script_hash, pool_nft_name, 1),
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

const pool_script_hash = #"00000000000000000000000000000000000000000000000000000000"
const random_hash = #"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"
const other_hash =  #"01010101010101010101010101010101010101010101010101010101"
test scoop_pool_output_wallet_address() fail {
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_pool_output_address: Some(from_verification_key(random_hash)),
    }
  scoop(options)
}
test scoop_pool_output_wrong_script() fail {
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_pool_output_address: Some(from_script(random_hash)),
    }
  scoop(options)
}
test scoop_pool_output_change_staking_credential() fail {
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_pool_input_address: Some(from_script(pool_script_hash) |> with_delegation_key(random_hash)), 
      edit_pool_output_address: Some(from_script(pool_script_hash) |> with_delegation_key(other_hash)),
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
            pool_creation_fee: 0,
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
  let pool_address = script_address(pool_script_hash)
  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: option.or_else(options.edit_pool_input_address, pool_address),
        value: value.from_lovelace(1_000_000_000 + 2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 1_000_000_000)
          |> value.add(pool_script_hash, pool_nft_name, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }
  let dest = option.or_else(options.edit_escrow_intended_destination, Fixed { address: user_addr, datum: NoDatum })
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
  let (escrow1_out_addr, escrow1_out_datum) = when options.edit_escrow_actual_destination is {
    Some(Fixed(dest, datum)) -> (dest, datum)
    Some(Self) -> (escrow_address, InlineDatum(escrow_datum))
    None -> (user_addr, NoDatum)
  }
  let escrow1_out =
    Output {
      address: escrow1_out_addr,
      value: option.or_else(
        options.edit_escrow_1_value,
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_896_088),
      ),
      datum: escrow1_out_datum,
      reference_script: None,
    }
  // TODO: manage separately?
  let (escrow2_out_addr, escrow2_out_datum) = when options.edit_escrow_actual_destination is {
    Some(Fixed(dest, datum)) -> (dest, datum)
    Some(Self) -> (escrow_address, InlineDatum(escrow_datum))
    None -> (user_addr, NoDatum)
  }
  let escrow2_out =
    Output {
      address: escrow2_out_addr,
      value: option.or_else(
        options.edit_escrow_2_value,
        value.from_lovelace(2_000_000)
          |> value.add(dummy_policy_id, dummy_asset_name, 9_702_095),
      ),
      datum: escrow2_out_datum,
      reference_script: None,
    }
  let pool_output =
    Output {
      address: option.or_else(options.edit_pool_output_address, pool_address),
      value: option.or_else(
        options.edit_pool_output_value,
        value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
          |> value.add(
               dummy_policy_id,
               dummy_asset_name,
               1_000_000_000 - ( 9_896_088 + 9_702_095 ),
             )
          |> value.add(pool_script_hash, pool_nft_name, 1),
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
  let pool_redeemer = PoolScoop(
    0,
    0,
    [
      (1, None, 0),
      (2, None, 0),
    ]
  )
  let result = pool_validator.spend(settings_policy_id, pool_datum, pool_redeemer, ctx)
  result
}

fn scoop_swap_deposit(options: ScoopTestOptions) {
  let settings_policy_id = #"00000000000000000000000000000000000000000000000000000000"
  let dummy_policy_id = #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let dummy_asset_name = #"53554e444145"
  let scooper = #"00000000000000000000000000000000000000000000000000000000"
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
  let pool_address = script_address(pool_script_hash)
  let pool_input = Input {
    output_reference: mk_output_reference(0),
    output: Output {
      address: pool_address,
      value: value.from_lovelace(1_000_000_000 + 2_000_000)
        |> value.add(dummy_policy_id, dummy_asset_name, 1_000_000_000)
        |> value.add(pool_script_hash, pool_nft_name, 1),
      datum: InlineDatum(pool_datum),
      reference_script: None,
    },
  }
  let dest = options.edit_escrow_intended_destination |> option.or_else(Fixed { address: user_addr, datum: NoDatum })
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
  let (escrow1_out_addr, escrow1_out_datum) = when options.edit_escrow_actual_destination is {
    Some(Fixed(dest, datum)) -> (dest, datum)
    Some(Self) -> (escrow_address, InlineDatum(escrow_datum_1))
    None -> (user_addr, NoDatum)
  }
  let escrow1_out = Output {
    address: escrow1_out_addr,
    value: option.or_else(options.edit_escrow_1_value,
      value.from_lovelace(2_000_000)
        |> value.add(dummy_policy_id, dummy_asset_name, 9_896_088)),
    datum: escrow1_out_datum,
    reference_script: None,
  }
  let (escrow2_out_addr, escrow2_out_datum) = when options.edit_escrow_actual_destination is {
    Some(Fixed(dest, datum)) -> (dest, datum)
    Some(Self) -> (escrow_address, InlineDatum(escrow_datum_2))
    None -> (user_addr, NoDatum)
  }
  let escrow2_out = Output {
    address: escrow2_out_addr,
    value: value.from_lovelace(2_000_000)
      |> value.add(pool_script_hash, pool_lp_name(pool_id), 9_900_990)
      |> value.add(dummy_policy_id, dummy_asset_name, 196_990),
    datum: escrow2_out_datum,
    reference_script: None,
  }
  let pool_output = Output {
    address: pool_address,
    value: option.or_else(options.edit_pool_output_value,
      value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
        |> value.add(dummy_policy_id, dummy_asset_name, 1_000_000_000 - 9_896_088 + 10_000_000 - 196_990)
        |> value.add(pool_script_hash, pool_nft_name, 1)),
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
          |> value.add(pool_script_hash, pool_lp_name(pool_id), 9_900_990)
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
  let pool_redeemer = PoolScoop(
    0,
    0,
    [
      (1, None, 0),
      (2, None, 0),
    ]
  )
  let result = pool_validator.spend(settings_policy_id, pool_datum, pool_redeemer, ctx)
  result
}

test scoop_strategy_self() {
  let options = ScoopTestOptions {
    ..default_scoop_test_options(),
    edit_escrow_intended_destination: Some(Self),
    edit_escrow_actual_destination: Some(Self),
  }
  scoop(options)
}

fn pool_test_tx_input() -> Input {
  let funds_input =
    new_tx_input(
      mk_tx_hash(0).hash,
      wallet_address(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513"),
      0,
      NoDatum,
    )
    funds_input
}

test mint_test_two_nfts() fail {
  let pool_id = pool_ident_from_input(pool_test_tx_input())
  let (_, new_pool_nft_token, _) = shared.pool_token_names(pool_id)
  // if we add on another pool NFT token to the pool output, it should fail
  mint_test_modify(
      fn(output) { Output { ..output, value: value.add(output.value, pool_script_hash, new_pool_nft_token, 1) } },
      identity,
      identity,
      identity
    )
}

fn pool_ident_from_input (tx_input: Input) -> ByteArray {
  tx_input.output_reference.transaction_id.hash
      |> bytearray.concat(#"23") // '#' character
      |>
      bytearray.concat(pool_validator.int_to_ident(tx_input.output_reference.output_index))
      |> hash.blake2b_256
      |> bytearray.drop(4)
}

fn mint_test_modify(
  modify_pool_output: fn(Output) -> Output,
  modify_lp_output: fn(Output) -> Output,
  modify_ref_output: fn(Output) -> Output,
  modify_datum: fn(Datum) -> Datum) -> Bool {
  let settings_policy_id = #"00000000000000000000000000000000000000000000000000000000"
  let pool_address = script_address(pool_script_hash) |> with_delegation_key(#"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad")
  let rberry_policy_id = #"9a9693a9a37912a5097918f97918d15240c92ab729a0b7c4aa144d77"
  let rberry_token_name = #"524245525259"
  let user_address =
    wallet_address(#"6af53ff4f054348ad825c692dd9db8f1760a8e0eacf9af9f99306513")
  let settings_input = mk_valid_settings_input([], 1)

  let funds_input = pool_test_tx_input()
  let pool_id = pool_ident_from_input(funds_input)
  let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) = shared.pool_token_names(pool_id)
  let inline_pool_datum = modify_datum(InlineDatum(
    PoolDatum {
      identifier: pool_id,
      assets: ((#"", #""), (rberry_policy_id, rberry_token_name)),
      circulating_lp: 1_000_000_000,
      fees_per_10_thousand: (5, 5),
      market_open: 0,
      fee_finalized: 0,
      protocol_fees: 2_000_000,
    }
  ))
  let pool_output_val =
    value.from_asset(rberry_policy_id, rberry_token_name, 1_000_000_000)
      |> value.add(pool_script_hash, new_pool_nft_token, 1)
      |> value.merge(value.from_lovelace(1_002_000_000))
  let pool_output = new_tx_output(pool_address, 0, inline_pool_datum) // 1_002_000_000 = 1_000_000_000 ADA for pool + 2_000_000 ADA for protocol_fees
      |> add_asset_to_tx_output(pool_output_val)
      |> modify_pool_output

  let lp_output_val =
    value.from_asset(pool_script_hash, pool_lp_name(pool_id), 1_000_000_000)
      |> value.merge(value.from_lovelace(2_000_000))
  let lp_output =
    new_tx_output(user_address, 0, NoDatum) // we can probably get rid of the rider, it gets auto added
      |> add_asset_to_tx_output(lp_output_val)
      |> modify_lp_output

  let ref_output_val =
    value.from_asset(pool_script_hash, new_pool_ref_token, 1)
      |> value.merge(value.from_lovelace(2_000_000))
  let ref_output =
    new_tx_output(user_address, 0, InlineDatum(Void)) // we can probably get rid of the rider, it gets auto added
      |> add_asset_to_tx_output(ref_output_val)
      |> modify_ref_output

  let pool_mint_redeemer = CreatePool {
    assets: ((#"", #""), (rberry_policy_id, rberry_token_name)),
    pool_output: 0,
    metadata_output: 2,
  }

  let ctx = interval.between(1,2)
    |> build_txn_context()
    |> mint_assets(pool_script_hash, value.to_minted_value(
        value.from_lovelace(0)
          |> value.add(pool_script_hash, new_pool_lp_token, 1_000_000_000)
          |> value.add(pool_script_hash, new_pool_nft_token, 1)
          |> value.add(pool_script_hash, new_pool_ref_token, 1)
      ))
    |> add_tx_input(funds_input)
    |> add_tx_ref_input(settings_input)

    // these must be in reverse order like so, in order to get [pool_output, lp_output, ref_output]
    |> add_tx_output(ref_output)
    |> add_tx_output(lp_output)
    |> add_tx_output(pool_output)

  let result = pool_validator.mint(settings_policy_id, pool_mint_redeemer, ctx)
  result
}

test mint_test() {
  mint_test_modify(identity, identity, identity, identity)
}

// make sure pool_output.address is checked to be the pool address
test mint_test_wrong_address () fail {
  let minted = mint_test_modify(
    // change pool nft output address to destination that shouldn't be possible
    fn (output) {
      Output{
        ..output,
        // TODO: move some of this stuff to constants?
        address: from_verification_key(random_hash) |> with_delegation_key(#"725011d2c296eb3341e159b6c5c6991de11e81062b95108c9aa024ad")
      }
    },
    identity,
    identity,
    identity
  )
  minted
}

// make sure we can't include any spam on the datum
test mint_test_nonvoid_datum() fail {
  let minted = mint_test_modify(
    identity,
    identity,
    fn (ref_metadata_output) {
      Output {
        ..ref_metadata_output,
        datum: InlineDatum("Evil data")
      }
    },
  identity
  )
  minted
}