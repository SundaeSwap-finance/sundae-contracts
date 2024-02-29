use aiken/bytearray
use aiken/builtin
use aiken/dict.{Dict}
use aiken/hash.{Blake2b_256, Hash}
use aiken/list
use aiken/transaction.{
  DatumHash, InlineDatum, Input, NoDatum, Output, ScriptContext, Spend,
  find_input,
}
use aiken/transaction/credential.{Credential, ScriptCredential}
use aiken/transaction/value.{AssetName, PolicyId}

use tests/examples/ex_shared.{
  script_address, wallet_address, mk_output_reference
}

/// An alias type for the pool identifier, to make it slightly easier to reason about
pub type Ident = ByteArray

/// An asset class is a combination of a policy ID and asset name; uniquely identifies one type of token, useful to make the code more readable
pub type AssetClass = (PolicyId, AssetName)

/// A rational number, represented as the numerator and denominator
pub type Rational = (Int, Int)

/// A "value" for which we know there is only one value; convenient for performance reasons, and to make the code more readable
pub type SingletonValue = (PolicyId, AssetName, Int)

/// Convert a singleton value to an actual value; convenient for comparing or adding together values
pub fn to_value(sv: SingletonValue) -> value.Value {
  value.from_asset(sv.1st, sv.2nd, sv.3rd)
}

/// Find the datum for a specific output
/// That is, the datum may either be in the output itself, as a datum hash
/// or in the transaction witnesses, which are passed in here
/// We don't use this everywhere, but we use it for orders in particular because
/// that lets us compose with other protocols (another protocol can produce an non-inline datum, and it can still be scooped)
/// Provided the scooper knows about the datum itself
pub fn datum_of(
  datums: Dict<Hash<Blake2b_256, Data>, Data>,
  output: Output,
) -> Option<Data> {
  when output.datum is {
    transaction.NoDatum -> None
    transaction.DatumHash(h) -> dict.get(datums, h)
    transaction.InlineDatum(d) -> Some(d)
  }
}

/// Find the **input** (which was the output of some other transaction) for which we're actually evaluating the script to determine if it is spendable
/// Also called "own_input" in places
pub fn spent_output(ctx: ScriptContext) -> Output {
  expect Spend(out_ref) = ctx.purpose
  expect Some(input) = find_input(ctx.transaction.inputs, out_ref)
  input.output
}

/// Find a specific input index, and validate that it has at least one of the specified token
/// Useful for looking up the pool token, etc.
/// NOTE: this is actually unused, because we chose to go with the optimization in the body
pub fn get_input_with_token(
  /// The list of transaction inputs
  inputs: List<Input>,
  /// The index at which we should expect the input
  idx: Int,
  /// The token we expect to find on the UTXO
  token: AssetClass,
) -> Output {
  // Optimization: assume first reference input by farming for high reference script hashes
  expect Some(input) = list.at(inputs, idx)
  expect value.quantity_of(input.output.value, token.1st, token.2nd) >= 1
  input.output
}

/// A more permissive version of get_input_with_token, where we don't know the index to expect the input at
/// NOTE: this is actually unused
pub fn find_input_with_token(inputs: List<Input>, token: AssetClass) -> Output {
  expect Some(input) =
    list.find(
      inputs,
      fn(input) {
        value.quantity_of(input.output.value, token.1st, token.2nd) >= 1
      },
    )
  input.output
}

// We need to count the number of input orders to enforce that every order is "considered"
// however, we don't know the order script address (indeed, there could be many types!);
// and for efficiency, we don't want to do an equality comparison each time anyway
// So, we use a clever trick, and count the number of *script* addresses, and subtract the constant below.
// This relies on a delicate confluence of factors that should be reconsidered very carefully in the final version:
// - There is an exact number of "non-order" scripts that will be in each transaction
// - The number of entries in the input order map is checked against this count of orders
// - We force each entry in the input order map to be unique
// - We force the script to evaluate each entry in the order map as an order
// - If the input can't be processed as an order (ex: it has a incompatible datum, or enforces different logic)
//   the script will fail
// For example, if someone included two pools, then either:
// - the input list will leave it off, failing the check for number of orders
// - the input list will include an order twice to make the counts match, but failing the uniqueness check
// - the input list will include the pool script, which will fail to parse
const exact_non_order_script_inputs = 1

/// Check whether a specific credential is a script credential; this can be useful for identifying pool and order scripts,
/// even if we don't know what the specific hash is going to be
pub fn is_script(credential: Credential) -> Bool {
  when credential is {
    ScriptCredential(_) -> True
    _ -> False
  }
}

/// Count the number of orders on the inputs,
/// without knowing the exact script address they will use
/// This is quite subtle, so see the note above
pub fn count_orders(tx_inputs: List<Input>) -> Int {
  when tx_inputs is {
    [] -> -exact_non_order_script_inputs
    [input, ..rest] ->
      // The reason this is important is twofold:
      // - We count them in the first place to ensure that we process every order by the end of the transaction
      // - and we compare the script this way so that if we implement a new order script, with additional logic,
      //   and the correct datum format, it can be processed the pool just fine
      when input.output.address.payment_credential is {
        ScriptCredential(_) -> count_orders(rest) + 1
        _ -> count_orders(rest)
      }
  }
}

/// Check whether a specific value has *exactly* the right amount of tokens
/// This is important to, for example, efficiently check the pool output has the correct value
/// Check where this is used, because it's a very subtle bit of logic for where this is safe to do
pub fn has_exact_token_count(val: value.Value, count: Int) {
  count == list.foldl(
    // Convert the value to a list, so we can fold over it
    dict.to_list(value.to_dict(val)),
    0,
    fn(tk_pair, total) {
      // The first fold is over the list of policy IDs
      // and the second fold is over the list of tokens
      // so we can count the distinct Asset IDs
      // TODO: can we just use list.length here?
      list.foldl(
        dict.to_list(builtin.snd_pair(tk_pair)),
        0,
        fn(_tn, tn_length) { tn_length + 1 },
      ) + total
    },
  )
}

test test_has_exact_token_count() {
  let val = value.from_lovelace(100)
    |> value.add("abc", "def", 100)
    |> value.add("xyz", "www", 200)
  expect has_exact_token_count(val, 3)

  let val = value.from_lovelace(100)
    |> value.add("abc", "def", 100)
  expect has_exact_token_count(val, 2)

  let val = value.from_lovelace(100)
    |> value.add("abc", "def", 100)
  expect !has_exact_token_count(val, 3)
  True
}

/// The CIP-68 (100) "reference" token for a given pool identifier; this token lets you set the metadata like the name, decimals, image, etc.
/// See https://github.com/cardano-foundation/CIPs/tree/master/CIP-0068 for more details on the specific prefix
pub fn pool_ref_name(pool_ident: Ident) {
  bytearray.concat(#"000643b0", pool_ident)
}

/// The CIP-68 (222) "nft" token for a given pool identifier; this token is an NFT used to track the actual swappable funds in the pool 
pub fn pool_nft_name(pool_ident: Ident) {
  bytearray.concat(#"000de140", pool_ident)
}

/// The CIP-68 (333) "fungible" token for a given pool identifier; this token is used to track a percentage ownership of funds in a given pool
pub fn pool_lp_name(pool_ident: Ident) {
  bytearray.concat(#"0014df10", pool_ident)
}

/// Construct all three pool token names
/// This is a minor convenience; theoretically it would be more efficient to construct each independently,
/// but this is likely so small as to not be worth sacrificing the performance
/// Note: it's important to pay attention to the order of the token names returned, because they're indistinguishable to the caller!
pub fn pool_token_names(pool_ident: Ident) {
  (
    pool_ref_name(pool_ident),
    pool_nft_name(pool_ident),
    pool_lp_name(pool_ident),
  )
}

// Test for count_orders, with 16 inputs of which 10 are orders.
test count_orders_test() {
  let hash_of_pool_script =
    #"00000000000000000000000000000000000000000000000000000000"
  let pool_address = script_address(hash_of_pool_script)
  let pool_input =
    Input {
      output_reference: mk_output_reference(0),
      output: Output {
        address: pool_address,
        value: value.from_lovelace(0),
        datum: NoDatum,
        reference_script: None,
      },
    }

  let hash_of_escrow_script =
    #"00000000000000000000000000000000000000000000000000000000"
  let escrow_address = script_address(hash_of_escrow_script)
  let escrow1_in = Input {
    output_reference: mk_output_reference(2),
    output: Output {
      address: escrow_address,
      value: value.from_lovelace(0),
      datum: NoDatum,
      reference_script: None,
    },
  }
  let escrow2_in = Input {
    output_reference: mk_output_reference(3),
    output: Output {
      address: escrow_address,
      value: value.from_lovelace(0),
      datum: NoDatum,
      reference_script: None,
    },
  }
  let other_address = wallet_address(#"fede0000000000000000000000000000000000000000000000000000")
  let other_in = Input {
    output_reference: mk_output_reference(4),
    output: Output {
      address: other_address,
      value: value.from_lovelace(0),
      datum: NoDatum,
      reference_script: None,
    },
  }

  let inputs = [
    pool_input, other_in, escrow1_in, escrow2_in, other_in, escrow1_in,
    escrow2_in, other_in, escrow1_in, escrow2_in, other_in, escrow1_in,
    escrow2_in, other_in, escrow1_in, escrow2_in
  ]

  count_orders(inputs) == 10
}
