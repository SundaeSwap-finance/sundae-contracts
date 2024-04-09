use aiken/builtin
use aiken/bytearray
use aiken/dict.{Dict}
use aiken/hash
use aiken/interval
use aiken/option
use aiken/transaction.{
  Datum, InlineDatum, Input, NoDatum, Output, OutputReference,
  ScriptContext, Spend, Transaction, TransactionId,
}
use aiken/transaction/credential.{
  Inline, Address, StakeCredential, ScriptCredential, VerificationKeyCredential, from_verification_key, from_script, with_delegation_key
}
use aiken/transaction/value.{Value, ada_policy_id, ada_asset_name}
use shared.{
  pool_lp_name,
}
use sundae/multisig
use tests/examples/ex_settings.{mk_valid_settings_input, mk_valid_settings_datum, example_treasury_admin, example_metadata_admin, example_treasury_address, example_settings_admin}
use tests/examples/ex_shared.{
  mk_output_reference, mk_tx_hash, script_address, wallet_address, compare_stake,
}
use types/order.{Deposit, Destination, Fixed, Self, OrderDatum, Swap}
use types/pool.{
  PoolMintRedeemer, CreatePool, PoolDatum, PoolScoop, WithdrawFees,
  UpdatePoolFees, BurnPool,
}
use calculation/shared.{PoolState} as calc_shared
use types/settings.{SettingsDatum, settings_nft_name}
use tx_util/builder.{add_asset_to_tx_output, add_tx_input, add_tx_output, add_tx_ref_input, build_txn_context, mint_assets, new_tx_input, new_tx_output, with_asset_of_tx_input}
use pool as pool_validator
use tests/constants


type ScoopTestOptions {
  edit_order_1_value: Option<Value>,
  edit_order_2_value: Option<Value>,
  edit_order_intended_destination: Option<Destination>,
  edit_order_actual_destination: Option<Destination>,
  edit_fee: Option<Value>,
  edit_swap_fees: Option<((Int,Int), (Int, Int))>,
  edit_fee_admin: Option<Option<multisig.MultisigScript>>,
  edit_withdrawals: Option<Dict<StakeCredential, Int>>,
  edit_pool_input_address: Option<Address>,
  edit_pool_output_address: Option<Address>,
  edit_pool_output_value: Option<Value>,
  edit_pool_output_datum: Option<PoolDatum>,
  edit_settings_datum: Option<Datum>,
}

fn default_scoop_test_options() -> ScoopTestOptions {
  ScoopTestOptions {
    edit_order_1_value: None,
    edit_order_2_value: None,
    edit_order_intended_destination: None,
    edit_order_actual_destination: None,
    edit_fee: None,
    edit_swap_fees: None,
    edit_fee_admin: None,
    edit_withdrawals: None,
    edit_pool_input_address: None,
    edit_pool_output_address: None,
    edit_pool_output_value: None,
    edit_pool_output_datum: None,
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
      edit_order_actual_destination: Some(Fixed(burn_addr, NoDatum)),
    }
  scoop(options)
}

test scoop_payouts_swapped() fail {
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_order_1_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(constants.rberry_policy, constants.rberry_asset_name, 9_702_095),
      ),
      edit_order_2_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(constants.rberry_policy, constants.rberry_asset_name, 9_896_088),
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
  let pool_nft_name = shared.pool_nft_name(constants.pool_ident)
  let swap_fee = 100
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_swap_fees: Some(((swap_fee, swap_fee), (swap_fee, swap_fee))),
      edit_order_1_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(constants.rberry_policy, constants.rberry_asset_name, 9_802_950),
      ),
      edit_order_2_value: Some(
        value.from_lovelace(2_000_000)
          |> value.add(constants.rberry_policy, constants.rberry_asset_name, 9_611_678),
      ),
      edit_pool_output_value: Some(
        value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
          |> value.add(
               constants.rberry_policy,
               constants.rberry_asset_name,
               1_000_000_000 - ( 9_802_950 + 9_611_678 ),
             )
          |> value.add(constants.pool_script_hash, pool_nft_name, 1),
      ),
    }
  scoop(options)
}

test output_missing_nft() fail {
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_pool_output_value: Some(
        value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
          |> value.add(
               constants.rberry_policy,
               constants.rberry_asset_name,
               1_000_000_000 - ( 9_896_088 + 9_702_095 ),
             ),
      ),
    }
  scoop(options)
}

test scoop_pool_output_wallet_address() fail {
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_pool_output_address: Some(from_verification_key(constants.random_hash)),
    }
  scoop(options)
}
test scoop_pool_output_wrong_script() fail {
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_pool_output_address: Some(from_script(constants.random_hash)),
    }
  scoop(options)
}
test scoop_pool_output_change_staking_credential() fail {
  let options =
    ScoopTestOptions {
      ..default_scoop_test_options(),
      edit_pool_input_address: Some(from_script(constants.pool_script_hash) |> with_delegation_key(constants.random_hash)), 
      edit_pool_output_address: Some(from_script(constants.pool_script_hash) |> with_delegation_key(constants.other_hash)),
    }
  scoop(options)
}

test scooper_not_in_settings() fail {
  let somebody = constants.not_a_scooper
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
  let user_addr = wallet_address(constants.payment_key)
  let owner = multisig.Signature(constants.payment_key)
  let fees = option.or_else(options.edit_swap_fees, ((5,5),(5,5)))
  let pool_datum =
    PoolDatum {
      identifier: constants.pool_ident,
      assets: ((ada_policy_id, ada_asset_name), (constants.rberry_policy, constants.rberry_asset_name)),
      circulating_lp: 1_000_000_000,
      bid_fees_per_10_thousand: fees.1st,
      ask_fees_per_10_thousand: fees.2nd,
      fee_manager: None,
      market_open: 0,
      fee_finalized: 100,
      protocol_fees: 2_000_000,
    }
  let pool_out_datum =
    PoolDatum {
      identifier: constants.pool_ident,
      assets: ((ada_policy_id, ada_asset_name), (constants.rberry_policy, constants.rberry_asset_name)),
      circulating_lp: 1_000_000_000,
      bid_fees_per_10_thousand: fees.1st,
      ask_fees_per_10_thousand: fees.2nd,
      fee_manager: None,
      market_open: 0,
      fee_finalized: 0,
      protocol_fees: 7_000_000,
    }
  let pool_nft_name = shared.pool_nft_name(constants.pool_ident)
  let pool_address = script_address(constants.pool_script_hash)
  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: option.or_else(options.edit_pool_input_address, pool_address),
        value: value.from_lovelace(1_000_000_000 + 2_000_000)
          |> value.add(constants.rberry_policy, constants.rberry_asset_name, 1_000_000_000)
          |> value.add(constants.pool_script_hash, pool_nft_name, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }
  let dest = option.or_else(options.edit_order_intended_destination, Fixed { address: user_addr, datum: NoDatum })
  let swap =
    Swap((ada_policy_id, ada_asset_name, 10_000_000), (constants.rberry_policy, constants.rberry_asset_name, 0))
  let order_datum =
    OrderDatum {
      pool_ident: None,
      owner,
      max_protocol_fee: 2_500_000,
      destination: dest,
      details: swap,
      extension: builtin.i_data(0),
    }
  let order_address = script_address(constants.order_script_hash)
  let order1_in =
    Input {
      output_reference: mk_output_reference(2),
      output: Output {
        address: order_address,
        value: value.from_lovelace(4_500_000 + 10_000_000),
        datum: InlineDatum(order_datum),
        reference_script: None,
      },
    }
  let order2_in =
    Input {
      output_reference: mk_output_reference(3),
      output: Output {
        address: order_address,
        value: value.from_lovelace(4_500_000 + 10_000_000),
        datum: InlineDatum(order_datum),
        reference_script: None,
      },
    }
  let settings_input = {
    let Input { output_reference, output } =
      mk_valid_settings_input([constants.scooper], 1)
    let updated_output =
      Output {
        ..output,
        datum: option.or_else(options.edit_settings_datum, output.datum),
      }
    Input { output_reference, output: updated_output }
  }
  let (order1_out_addr, order1_out_datum) = when options.edit_order_actual_destination is {
    Some(Fixed(dest, datum)) -> (dest, datum)
    Some(Self) -> (order_address, InlineDatum(order_datum))
    None -> (user_addr, NoDatum)
  }
  let order1_out =
    Output {
      address: order1_out_addr,
      value: option.or_else(
        options.edit_order_1_value,
        value.from_lovelace(2_000_000)
          |> value.add(constants.rberry_policy, constants.rberry_asset_name, 9_896_088),
      ),
      datum: order1_out_datum,
      reference_script: None,
    }
  // TODO: manage separately?
  let (order2_out_addr, order2_out_datum) = when options.edit_order_actual_destination is {
    Some(Fixed(dest, datum)) -> (dest, datum)
    Some(Self) -> (order_address, InlineDatum(order_datum))
    None -> (user_addr, NoDatum)
  }
  let order2_out =
    Output {
      address: order2_out_addr,
      value: option.or_else(
        options.edit_order_2_value,
        value.from_lovelace(2_000_000)
          |> value.add(constants.rberry_policy, constants.rberry_asset_name, 9_702_095),
      ),
      datum: order2_out_datum,
      reference_script: None,
    }
  let pool_output =
    Output {
      address: option.or_else(options.edit_pool_output_address, pool_address),
      value: option.or_else(
        options.edit_pool_output_value,
        value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
          |> value.add(
               constants.rberry_policy,
               constants.rberry_asset_name,
               1_000_000_000 - ( 9_896_088 + 9_702_095 ),
             )
          |> value.add(constants.pool_script_hash, pool_nft_name, 1),
      ),
      datum: InlineDatum(pool_out_datum),
      reference_script: None,
    }
  let ctx =
    ScriptContext {
      transaction: Transaction {
        inputs: [pool_input, order1_in, order2_in],
        reference_inputs: [settings_input],
        outputs: [pool_output, order1_out, order2_out],
        fee: option.or_else(options.edit_fee, value.from_lovelace(1_000_000)),
        mint: value.to_minted_value(value.from_lovelace(0)),
        certificates: [],
        withdrawals: dict.new(),
        validity_range: interval.between(1, 2),
        extra_signatories: [constants.scooper],
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
  let result = pool_validator.spend(constants.settings_policy_id, pool_datum, pool_redeemer, ctx)
  result
}

fn scoop_swap_deposit(options: ScoopTestOptions) {
  let user_addr = wallet_address(constants.payment_key)
  let owner = multisig.Signature(constants.payment_key)
  let pool_fees = option.or_else(options.edit_swap_fees, ((5,5),(5,5)))
  let pool_datum = PoolDatum {
    identifier: constants.pool_ident,
    assets: (
      (ada_policy_id, ada_asset_name),
      (constants.rberry_policy, constants.rberry_asset_name),
    ),
    circulating_lp: 1_000_000_000,
    bid_fees_per_10_thousand: pool_fees.1st,
    ask_fees_per_10_thousand: pool_fees.2nd,
    fee_manager: None,
    market_open: 0,
    fee_finalized: 0,
    protocol_fees: 2_000_000,
  }
  let pool_out_datum = PoolDatum {
    identifier: constants.pool_ident,
    assets: (
      (ada_policy_id, ada_asset_name),
      (constants.rberry_policy, constants.rberry_asset_name),
    ),
    circulating_lp: 1_009_900_990,
    bid_fees_per_10_thousand: pool_fees.1st,
    ask_fees_per_10_thousand: pool_fees.2nd,
    fee_manager: None,
    market_open: 0,
    fee_finalized: 0,
    protocol_fees: 7_000_000,
  }
  let pool_nft_name = shared.pool_nft_name(constants.pool_ident)
  let pool_address = script_address(constants.pool_script_hash)
  let pool_input = Input {
    output_reference: mk_output_reference(0),
    output: Output {
      address: pool_address,
      value: value.from_lovelace(1_000_000_000 + 2_000_000)
        |> value.add(constants.rberry_policy, constants.rberry_asset_name, 1_000_000_000)
        |> value.add(constants.pool_script_hash, pool_nft_name, 1),
      datum: InlineDatum(pool_datum),
      reference_script: None,
    },
  }
  let dest = options.edit_order_intended_destination |> option.or_else(Fixed { address: user_addr, datum: NoDatum })
  let swap =
    Swap(
      (ada_policy_id, ada_asset_name, 10_000_000),
      (constants.rberry_policy, constants.rberry_asset_name, 0),
    )
  let deposit =
    Deposit((
      (ada_policy_id, ada_asset_name, 10_000_000),
      (constants.rberry_policy, constants.rberry_asset_name, 10_000_000),
    ))
  let order_datum_1 = OrderDatum {
    pool_ident: None,
    owner: owner,
    max_protocol_fee: 2_500_000,
    destination: dest,
    details: swap,
    extension: builtin.i_data(0),
  }
  let order_datum_2 = OrderDatum {
    pool_ident: None,
    owner: owner,
    max_protocol_fee: 2_500_000,
    destination: dest,
    details: deposit,
    extension: builtin.i_data(0),
  }
  let order_address = script_address(constants.order_script_hash)
  let order1_in = Input {
    output_reference: mk_output_reference(2),
    output: Output {
      address: order_address,
      value: value.from_lovelace(4_500_000 + 10_000_000),
      datum: InlineDatum(order_datum_1),
      reference_script: None,
    },
  }
  let order2_in = Input {
    output_reference: mk_output_reference(3),
    output: Output {
      address: order_address,
      value: value.from_lovelace(4_500_000 + 10_000_000)
        |> value.add(constants.rberry_policy, constants.rberry_asset_name, 10_000_000),
      datum: InlineDatum(order_datum_2),
      reference_script: None,
    },
  }
  let settings_input = {
    let Input {output_reference, output} = mk_valid_settings_input([constants.scooper], 1)
    let updated_output = Output {
      ..output,
      datum: option.or_else(options.edit_settings_datum, output.datum)
    }
    Input {
      output_reference: output_reference,
      output: updated_output,
    }
  }
  let (order1_out_addr, order1_out_datum) = when options.edit_order_actual_destination is {
    Some(Fixed(dest, datum)) -> (dest, datum)
    Some(Self) -> (order_address, InlineDatum(order_datum_1))
    None -> (user_addr, NoDatum)
  }
  let order1_out = Output {
    address: order1_out_addr,
    value: option.or_else(options.edit_order_1_value,
      value.from_lovelace(2_000_000)
        |> value.add(constants.rberry_policy, constants.rberry_asset_name, 9_896_088)),
    datum: order1_out_datum,
    reference_script: None,
  }
  let (order2_out_addr, order2_out_datum) = when options.edit_order_actual_destination is {
    Some(Fixed(dest, datum)) -> (dest, datum)
    Some(Self) -> (order_address, InlineDatum(order_datum_2))
    None -> (user_addr, NoDatum)
  }
  let order2_out = Output {
    address: order2_out_addr,
    value: value.from_lovelace(2_000_000)
      |> value.add(constants.pool_script_hash, pool_lp_name(constants.pool_ident), 9_900_990)
      |> value.add(constants.rberry_policy, constants.rberry_asset_name, 196_990),
    datum: order2_out_datum,
    reference_script: None,
  }
  let pool_output = Output {
    address: pool_address,
    value: option.or_else(options.edit_pool_output_value,
      value.from_lovelace(1_000_000_000 + 20_000_000 + 5_000_000 + 2_000_000)
        |> value.add(constants.rberry_policy, constants.rberry_asset_name, 1_000_000_000 - 9_896_088 + 10_000_000 - 196_990)
        |> value.add(constants.pool_script_hash, pool_nft_name, 1)),
    datum: InlineDatum(pool_out_datum),
    reference_script: None,
  }

  let ctx = ScriptContext {
    transaction: Transaction {
      inputs: [pool_input, order1_in, order2_in],
      reference_inputs: [settings_input],
      outputs: [pool_output, order1_out, order2_out],
      fee: option.or_else(options.edit_fee, value.from_lovelace(1_000_000)),
      mint: value.to_minted_value(
        value.from_lovelace(0)
          |> value.add(constants.pool_script_hash, pool_lp_name(constants.pool_ident), 9_900_990)
      ),
      certificates: [],
      withdrawals: dict.new(),
      validity_range: interval.between(1, 2),
      extra_signatories: [constants.scooper],
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
  let result = pool_validator.spend(constants.settings_policy_id, pool_datum, pool_redeemer, ctx)
  result
}

fn withdraw_fees_transaction (options: ScoopTestOptions, withdraw_amount: Int) {
  let withdraw_fees_redeemer = WithdrawFees {
    amount: withdraw_amount,
    treasury_output: 1,
  }
  
  let pool_nft_name = shared.pool_nft_name(constants.pool_ident)
  let default_pool_output_address = Address { payment_credential: ScriptCredential(constants.pool_script_hash), stake_credential: Some(Inline(VerificationKeyCredential(example_settings_admin))) }
  let pool_output_address = option.or_else(options.edit_pool_output_address, default_pool_output_address)
  let pool_fees = option.or_else(options.edit_swap_fees, ((5,5),(5,5)))
  let protocol_fees = 2_000_000
  let pool_datum = PoolDatum {
    identifier: constants.pool_ident,
    assets: (
      (ada_policy_id, ada_asset_name),
      (constants.rberry_policy, constants.rberry_asset_name),
    ),
    circulating_lp: 1_000_000_000,
    bid_fees_per_10_thousand: pool_fees.1st,
    ask_fees_per_10_thousand: pool_fees.2nd,
    fee_manager: None,
    market_open: 0,
    fee_finalized: 0,
    protocol_fees,
  }
  // pool_test_tx_input deduplicate?
  let pool_input = new_tx_input(
    mk_tx_hash(0).hash,
    script_address(constants.pool_script_hash),
    1_000_000_000 + protocol_fees,
    InlineDatum(pool_datum))
    |> with_asset_of_tx_input(value.from_asset(constants.rberry_policy, constants.rberry_asset_name, 1_000_000_000))
    |> with_asset_of_tx_input(value.from_asset(constants.pool_script_hash, pool_nft_name, 1))

  let pool_out_datum = PoolDatum {
    ..pool_datum,
    protocol_fees: pool_datum.protocol_fees - withdraw_amount,
  }

  let pool_output = new_tx_output(pool_output_address, 0, InlineDatum(pool_out_datum))
    |> add_asset_to_tx_output(value.from_lovelace(1_000_000_000 + protocol_fees - withdraw_amount))
    |> add_asset_to_tx_output(value.from_asset(constants.rberry_policy, constants.rberry_asset_name, 1_000_000_000))
    |> add_asset_to_tx_output(value.from_asset(constants.pool_script_hash, pool_nft_name, 1))

  let treasury_output = new_tx_output(
    Address(VerificationKeyCredential(example_treasury_address), None),
    0,
    InlineDatum(Void))
    |> add_asset_to_tx_output(value.from_lovelace(withdraw_amount))

  let settings_datum = option.or_else(options.edit_settings_datum, InlineDatum(mk_valid_settings_datum([])))
  let settings_input = new_tx_input(
    mk_tx_hash(1).hash,
    script_address(constants.settings_policy_id),
    1,
    settings_datum) |> with_asset_of_tx_input(value.from_asset(constants.settings_policy_id, settings_nft_name, 1))

  let ctx = interval.between(1,2)
    |> build_txn_context()
    |> add_tx_ref_input(settings_input)
    |> add_tx_input(pool_input)
    |> add_tx_output(treasury_output)
    |> add_tx_output(pool_output)
    |> builder.add_signatory(example_treasury_admin)
    |> builder.spend(pool_input.output_reference)
  let result = pool_validator.spend(constants.settings_policy_id, pool_datum, withdraw_fees_redeemer, ctx)
  result
}

test withdraw_fees_transaction_test() {
  withdraw_fees_transaction(default_scoop_test_options(), 100)
}

test withdraw_fees_transaction_change_pool_staking_address_valid_test() {
  let example_address = VerificationKeyCredential( #"123456")
  let options = ScoopTestOptions {
    ..default_scoop_test_options(),
    edit_settings_datum: Some(InlineDatum(SettingsDatum {
      ..mk_valid_settings_datum([]),
      authorized_staking_keys: [
        VerificationKeyCredential(
          example_settings_admin,
        ),
        example_address
      ],
    })),
    edit_pool_output_address: Some(Address{payment_credential: ScriptCredential(constants.pool_script_hash), stake_credential: Some(Inline(example_address))}),
  }
  withdraw_fees_transaction(options, 100)
}

test withdraw_fees_transaction_change_pool_staking_address_invalid_test() fail {
  let example_address = VerificationKeyCredential( #"123456")
  let thief_address = VerificationKeyCredential( #"654321")

  let options = ScoopTestOptions {
    ..default_scoop_test_options(),
    edit_settings_datum: Some(InlineDatum(SettingsDatum {
      ..mk_valid_settings_datum([]),
      authorized_staking_keys: [
        VerificationKeyCredential(
          example_settings_admin,
        ),
        example_address
      ],
    })),
    edit_pool_output_address: Some(Address{payment_credential: ScriptCredential(constants.pool_script_hash), stake_credential: Some(Inline(thief_address))}),
  }
  withdraw_fees_transaction(options, 100)
}

test overdraw_test() fail {
  let example_address = VerificationKeyCredential( #"123456")
  let thief_address = VerificationKeyCredential( #"654321")

  let options = ScoopTestOptions {
    ..default_scoop_test_options(),
    edit_settings_datum: Some(InlineDatum(SettingsDatum {
      ..mk_valid_settings_datum([]),
      authorized_staking_keys: [
        VerificationKeyCredential(
          example_settings_admin,
        ),
        example_address
      ],
    })),
    edit_pool_output_address: Some(Address{payment_credential: ScriptCredential(constants.pool_script_hash), stake_credential: Some(Inline(thief_address))}),
  }
  withdraw_fees_transaction(options, 5_000_000)
}

fn update_pool_fees_transaction (options: ScoopTestOptions) {
  
  let update_fees_redeemer = UpdatePoolFees

  let pool_nft_name = shared.pool_nft_name(constants.pool_ident)
  let default_pool_output_address = Address { payment_credential: ScriptCredential(constants.pool_script_hash), stake_credential: Some(Inline(VerificationKeyCredential(example_settings_admin))) }
  let pool_output_address = option.or_else(options.edit_pool_output_address, default_pool_output_address)
  let pool_fees = option.or_else(options.edit_swap_fees, ((5,5),(5,5)))
  let fee_manager = option.or_else(options.edit_fee_admin, Some(multisig.Signature(constants.pool_script_hash)))
  let pool_datum = PoolDatum {
    identifier: constants.pool_ident,
    assets: (
      (ada_policy_id, ada_asset_name),
      (constants.rberry_policy, constants.rberry_asset_name),
    ),
    circulating_lp: 1_000_000_000,
    bid_fees_per_10_thousand: pool_fees.1st,
    ask_fees_per_10_thousand: pool_fees.2nd,
    fee_manager: fee_manager,
    market_open: 0,
    fee_finalized: 0,
    protocol_fees: 2_000_000,
  }
  let pool_rider = 2_000_000
  // pool_test_tx_input deduplicate?
  let pool_input_address = option.or_else(options.edit_pool_input_address, default_pool_output_address)
  let pool_input = new_tx_input(
    mk_tx_hash(0).hash,
    pool_input_address,
    1_000_000_000 + pool_rider,
    InlineDatum(pool_datum))
    |> with_asset_of_tx_input(value.from_asset(constants.rberry_policy, constants.rberry_asset_name, 1_000_000_000))
    |> with_asset_of_tx_input(value.from_asset(constants.pool_script_hash, pool_nft_name, 1))

  let pool_out_datum = PoolDatum {
    ..pool_datum,
    bid_fees_per_10_thousand: (10,10),
    ask_fees_per_10_thousand: (310,150),
  }

  let pool_output = new_tx_output(pool_output_address, 0, InlineDatum(pool_out_datum))
    |> add_asset_to_tx_output(value.from_lovelace(1_000_000_000 + pool_rider))
    |> add_asset_to_tx_output(value.from_asset(constants.rberry_policy, constants.rberry_asset_name, 1_000_000_000))
    |> add_asset_to_tx_output(value.from_asset(constants.pool_script_hash, pool_nft_name, 1))

  let settings_datum = option.or_else(options.edit_settings_datum, InlineDatum(mk_valid_settings_datum([])))
  let settings_input = new_tx_input(
    mk_tx_hash(1).hash,
    script_address(constants.settings_policy_id),
    1,
    settings_datum) |> with_asset_of_tx_input(value.from_asset(constants.settings_policy_id, settings_nft_name, 1))

  let ctx = interval.between(1,2)
    |> build_txn_context()
    |> add_tx_ref_input(settings_input)
    |> add_tx_input(pool_input)
    |> add_tx_output(pool_output)
    |> builder.add_signatory(constants.pool_script_hash)
    |> builder.spend(pool_input.output_reference)
  let withdrawals = option.or_else(options.edit_withdrawals, dict.new())
  let ctx = ScriptContext(
    Transaction {
      ..ctx.transaction,
      withdrawals,
    },
    ctx.purpose,
  )
  let result = pool_validator.spend(constants.settings_policy_id, pool_datum, update_fees_redeemer, ctx)
  result
}

test update_pool_fees_transaction_test() {
  update_pool_fees_transaction(default_scoop_test_options())
}

test cannot_update_pool_fees_transaction_test() fail {
  let settings = ScoopTestOptions {
    ..default_scoop_test_options(),
    edit_fee_admin: Some(None),
  }
  update_pool_fees_transaction(settings)
}

test update_pool_fees_transaction_with_script_test() {
  let settings = ScoopTestOptions {
    ..default_scoop_test_options(),
    edit_fee_admin: Some(Some(multisig.Script(#"1234"))),
    edit_withdrawals: Some(dict.from_ascending_list([(Inline(ScriptCredential(#"1234")), 100)], compare_stake)),
  }
  update_pool_fees_transaction(settings)
}

test scoop_strategy_self() {
  let options = ScoopTestOptions {
    ..default_scoop_test_options(),
    edit_order_intended_destination: Some(Self),
    edit_order_actual_destination: Some(Self),
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
      fn(output) { Output { ..output, value: value.add(output.value, constants.pool_script_hash, new_pool_nft_token, 1) } },
      identity,
      identity,
      identity,
      identity,
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
  modify_datum: fn(Datum) -> Datum,
  modify_redeemer: fn(PoolMintRedeemer) -> PoolMintRedeemer,
) -> Bool {
  let pool_address = script_address(constants.pool_script_hash) |> with_delegation_key(example_settings_admin)
  let user_address = wallet_address(constants.payment_key)
  let settings_input = mk_valid_settings_input([], 1)

  let funds_input = pool_test_tx_input()
  let pool_id = pool_ident_from_input(funds_input)
  let (new_pool_ref_token, new_pool_nft_token, new_pool_lp_token) = shared.pool_token_names(pool_id)
  let inline_pool_datum = modify_datum(InlineDatum(
    PoolDatum {
      identifier: pool_id,
      assets: ((ada_policy_id, ada_asset_name), (constants.rberry_policy, constants.rberry_asset_name)),
      circulating_lp: 1_000_000_000,
      bid_fees_per_10_thousand: (5, 5),
      ask_fees_per_10_thousand: (5, 5),
      fee_manager: None,
      market_open: 0,
      fee_finalized: 0,
      protocol_fees: 2_000_000,
    }
  ))
  let pool_output_val =
    value.from_asset(constants.rberry_policy, constants.rberry_asset_name, 1_000_000_000)
      |> value.add(constants.pool_script_hash, new_pool_nft_token, 1)
      |> value.merge(value.from_lovelace(1_002_000_000))
  let pool_output = new_tx_output(pool_address, 0, inline_pool_datum) // 1_002_000_000 = 1_000_000_000 ADA for pool + 2_000_000 ADA for protocol_fees
      |> add_asset_to_tx_output(pool_output_val)
      |> modify_pool_output

  let lp_output_val =
    value.from_asset(constants.pool_script_hash, pool_lp_name(pool_id), 1_000_000_000)
      |> value.merge(value.from_lovelace(2_000_000))
  let lp_output =
    new_tx_output(user_address, 0, NoDatum) // we can probably get rid of the rider, it gets auto added
      |> add_asset_to_tx_output(lp_output_val)
      |> modify_lp_output

  let ref_output_val =
    value.from_asset(constants.pool_script_hash, new_pool_ref_token, 1)
      |> value.merge(value.from_lovelace(2_000_000))
  let metadata_admin = wallet_address(example_metadata_admin)
  let ref_output =
    new_tx_output(metadata_admin, 0, InlineDatum(Void)) // we can probably get rid of the rider, it gets auto added
      |> add_asset_to_tx_output(ref_output_val)
      |> modify_ref_output

  let pool_mint_redeemer = modify_redeemer(CreatePool {
    assets: ((ada_policy_id, ada_asset_name), (constants.rberry_policy, constants.rberry_asset_name)),
    pool_output: 0,
    metadata_output: 2,
  })

  let ctx = interval.between(1,2)
    |> build_txn_context()
    |> mint_assets(constants.pool_script_hash, value.to_minted_value(
        value.from_lovelace(0)
          |> value.add(constants.pool_script_hash, new_pool_lp_token, 1_000_000_000)
          |> value.add(constants.pool_script_hash, new_pool_nft_token, 1)
          |> value.add(constants.pool_script_hash, new_pool_ref_token, 1)
      ))
    |> add_tx_input(funds_input)
    |> add_tx_ref_input(settings_input)

    // these must be in reverse order like so, in order to get [pool_output, lp_output, ref_output]
    |> add_tx_output(ref_output)
    |> add_tx_output(lp_output)
    |> add_tx_output(pool_output)

  let result = pool_validator.mint(constants.settings_policy_id, pool_mint_redeemer, ctx)
  result
}

test mint_test() {
  mint_test_modify(identity, identity, identity, identity, identity)
}

test mint_exotic_pair() {
  let funds_input = pool_test_tx_input()
  let pool_id = pool_ident_from_input(funds_input)
  let (_, new_pool_nft_token, _) = shared.pool_token_names(pool_id)
  let exotic_pair_value =
    value.from_asset(#"01", #"54", 1_000_000_000)
      |> value.add(#"02", #"55", 1_000_000_000)
      |> value.add(constants.pool_script_hash, new_pool_nft_token, 1)
      |> value.add(#"", #"", 2_000_000)

  mint_test_modify(
    fn (output) {
      Output {
        ..output,
        value: exotic_pair_value
      }
    },
    identity,
    identity,
    fn(pool_datum) {
      expect InlineDatum(pool_datum) = pool_datum
      expect pool_datum: PoolDatum = pool_datum
      InlineDatum(PoolDatum {
        ..pool_datum,
        assets: ((#"01", #"54"), (#"02", #"55")),
      })
    },
    fn(redeemer) {
      when redeemer is {
        CreatePool(_, ix, mx) -> CreatePool(((#"01", #"54"), (#"02", #"55")), ix, mx)
        _ -> fail "expected create pool"
      }
    }
  )
}

// make sure pool_output.address is checked to be the pool address
test mint_test_wrong_address () fail {
  let minted = mint_test_modify(
    // change pool nft output address to destination that shouldn't be possible
    fn (output) {
      Output{
        ..output,
        // TODO: move some of this stuff to constants?
        address: from_verification_key(constants.random_hash) |> with_delegation_key(constants.stake_key)
      }
    },
    identity,
    identity,
    identity,
    identity,
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
  identity,
  identity,
  )
  minted
}

test has_expected_pool_value_test() {
  let (_, pool_nft_token, pool_lp_token) = shared.pool_token_names(constants.pool_ident)
  let pool_value = value.from_lovelace(102_000_000)
    |> value.add(constants.rberry_policy, constants.rberry_asset_name, 100_000_000)
    |> value.add(constants.pool_script_hash, pool_nft_token, 1)
  let outcome = PoolState {
    quantity_lp: (constants.pool_script_hash, pool_lp_token, 99),
    quantity_a: (ada_policy_id, ada_asset_name, 100_000_000),
    quantity_b: (constants.rberry_policy, constants.rberry_asset_name, 100_000_000)
  }
  let protocol_fees = 2_000_000

  pool_validator.has_expected_pool_value(
    constants.pool_script_hash,
    constants.pool_ident,
    pool_value,
    outcome,
    protocol_fees,
  )
}

test has_expected_pool_value_test2() {
  let (_, pool_nft_token, pool_lp_token) = shared.pool_token_names(constants.pool_ident)
  let pool_value = value.from_lovelace(2_000_000)
    |> value.add(constants.other_policy, constants.other_asset_name, 100_000_000)
    |> value.add(constants.rberry_policy, constants.rberry_asset_name, 100_000_000)
    |> value.add(constants.pool_script_hash, pool_nft_token, 1)
  let outcome = PoolState {
    quantity_lp: (constants.pool_script_hash, pool_lp_token, 99),
    quantity_a: (constants.other_policy, constants.other_asset_name, 100_000_000),
    quantity_b: (constants.rberry_policy, constants.rberry_asset_name, 100_000_000)
  }
  let protocol_fees = 2_000_000

  pool_validator.has_expected_pool_value(
    constants.pool_script_hash,
    constants.pool_ident,
    pool_value,
    outcome,
    protocol_fees,
  )
}

fn evaporate_pool_tx(options: ScoopTestOptions, withdraw_amount: Int) {
  let withdraw_fees_redeemer = WithdrawFees {
    amount: withdraw_amount,
    treasury_output: 0,
  }
  
  let pool_nft_name = shared.pool_nft_name(constants.pool_ident)
  let pool_fees = option.or_else(options.edit_swap_fees, ((5,5),(5,5)))
  let pool_datum = PoolDatum {
    identifier: constants.pool_ident,
    assets: (
      (ada_policy_id, ada_asset_name),
      (constants.rberry_policy, constants.rberry_asset_name),
    ),
    circulating_lp: 0,
    bid_fees_per_10_thousand: pool_fees.1st,
    ask_fees_per_10_thousand: pool_fees.2nd,
    fee_manager: None,
    market_open: 0,
    fee_finalized: 0,
    protocol_fees: 18_000_000,
  }
  // pool_test_tx_input deduplicate?
  let pool_input = new_tx_input(
    mk_tx_hash(0).hash,
    script_address(constants.pool_script_hash),
    18_000_000,
    InlineDatum(pool_datum))
    |> with_asset_of_tx_input(value.from_asset(constants.pool_script_hash, pool_nft_name, 1))

  let treasury_output = new_tx_output(
    Address(VerificationKeyCredential(example_treasury_address), None),
    0,
    InlineDatum(Void))
    |> add_asset_to_tx_output(value.from_lovelace(withdraw_amount))

  let settings_datum = option.or_else(options.edit_settings_datum, InlineDatum(mk_valid_settings_datum([])))
  let settings_input = new_tx_input(
    mk_tx_hash(1).hash,
    script_address(constants.settings_policy_id),
    1,
    settings_datum) |> with_asset_of_tx_input(value.from_asset(constants.settings_policy_id, settings_nft_name, 1))

  let ctx = interval.between(1,2)
    |> build_txn_context()
    |> add_tx_ref_input(settings_input)
    |> add_tx_input(pool_input)
    |> add_tx_output(treasury_output)
    |> mint_assets(constants.pool_script_hash, value.to_minted_value(value.from_asset(constants.pool_script_hash, pool_nft_name, -1)))
    |> builder.add_signatory(example_treasury_admin)
    |> builder.spend(pool_input.output_reference)
  let result = pool_validator.spend(constants.settings_policy_id, pool_datum, withdraw_fees_redeemer, ctx)
  result
}

test evaporate_pool_test() {
  evaporate_pool_tx(default_scoop_test_options(), 18_000_000)
}

test dont_evaporate_pool_test() fail {
  evaporate_pool_tx(default_scoop_test_options(), 17_000_000)
}


test attempt_evaporate_pool_test() {
  let withdraw_fees_redeemer = WithdrawFees {
    amount: 18_000_000,
    treasury_output: 0,
  }
  
  let pool_nft_name = shared.pool_nft_name(constants.pool_ident)
  let pool_fees = ((5,5),(5,5))
  let pool_datum = PoolDatum {
    identifier: constants.pool_ident,
    assets: (
      (ada_policy_id, ada_asset_name),
      (constants.rberry_policy, constants.rberry_asset_name),
    ),
    circulating_lp: 0,
    bid_fees_per_10_thousand: pool_fees.1st,
    ask_fees_per_10_thousand: pool_fees.2nd,
    fee_manager: None,
    market_open: 0,
    fee_finalized: 0,
    protocol_fees: 18_000_000,
  }
  // pool_test_tx_input deduplicate?
  let pool_input = new_tx_input(
    mk_tx_hash(0).hash,
    script_address(constants.pool_script_hash),
    18_000_000,
    InlineDatum(pool_datum))
    |> with_asset_of_tx_input(
      value.from_asset(constants.pool_script_hash, pool_nft_name, 1)
    )

  let treasury_output = new_tx_output(
    Address(VerificationKeyCredential(example_treasury_address), None),
    0,
    InlineDatum(Void))
    |> add_asset_to_tx_output(value.from_lovelace(18_000_000))

  let settings_datum = InlineDatum(mk_valid_settings_datum([]))
  let settings_input = new_tx_input(
    mk_tx_hash(1).hash,
    script_address(constants.settings_policy_id),
    1,
    settings_datum) |> with_asset_of_tx_input(value.from_asset(constants.settings_policy_id, settings_nft_name, 1))

  let ctx = interval.between(1,2)
    |> build_txn_context()
    |> add_tx_ref_input(settings_input)
    |> add_tx_input(pool_input)
    |> add_tx_output(treasury_output)
    |> mint_assets(constants.pool_script_hash, value.to_minted_value(value.from_asset(constants.pool_script_hash, pool_nft_name, -1)))
    |> builder.add_signatory(example_treasury_admin)
    |> builder.spend(pool_input.output_reference)
  pool_validator.spend(constants.settings_policy_id, pool_datum, withdraw_fees_redeemer, ctx)
}

test burn_pool() {
  let user_addr = wallet_address(constants.payment_key)
  let fees = ((5,5),(5,5))
  let pool_datum =
    PoolDatum {
      identifier: constants.pool_ident,
      assets: ((ada_policy_id, ada_asset_name), (constants.rberry_policy, constants.rberry_asset_name)),
      circulating_lp: 0,
      bid_fees_per_10_thousand: fees.1st,
      ask_fees_per_10_thousand: fees.2nd,
      fee_manager: None,
      market_open: 0,
      fee_finalized: 0,
      protocol_fees: 2_000_000,
    }
  let pool_nft_name = shared.pool_nft_name(constants.pool_ident)
  let pool_address = script_address(constants.pool_script_hash)
  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: value.from_lovelace(2_000_000)
          |> value.add(constants.pool_script_hash, pool_nft_name, 1),
        datum: InlineDatum(pool_datum),
        reference_script: None,
      },
    }
  let settings_input = {
    let Input { output_reference, output } =
      mk_valid_settings_input([constants.scooper], 1)
    Input { output_reference, output }
  }
  let change_output =
    Output {
      address: user_addr,
      value: value.from_lovelace(2_000_000),
      datum: NoDatum,
      reference_script: None,
    }

  let (_, pool_nft_token, _) = shared.pool_token_names(constants.pool_ident)
  let ctx = interval.between(1,2)
    |> build_txn_context()
    |> mint_assets(constants.pool_script_hash, value.to_minted_value(
        value.from_lovelace(0)
          |> value.add(constants.pool_script_hash, pool_nft_token, -1)
      ))
    |> add_tx_input(pool_input)
    |> add_tx_ref_input(settings_input)
    |> add_tx_output(change_output)

  let pool_mint_redeemer = BurnPool(constants.pool_ident)
  let result = pool_validator.mint(constants.settings_policy_id, pool_mint_redeemer, ctx)
  result
}