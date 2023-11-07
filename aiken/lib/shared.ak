use aiken/bytearray
use aiken/builtin
use aiken/dict.{Dict}
use aiken/hash.{Blake2b_256, Hash}
use aiken/list
use aiken/transaction.{
  DatumHash, InlineDatum, Input, NoDatum, Output, ScriptContext, Spend,
  find_input,
}
use aiken/transaction/credential.{VerificationKeyCredential}
use aiken/transaction/value.{AssetName, PolicyId}

pub type Ident =
  ByteArray

pub type AssetClass =
  (PolicyId, AssetName)

pub type Rational =
  (Int, Int)

pub type SingletonValue =
  (PolicyId, AssetName, Int)

pub fn to_value(sv: SingletonValue) -> value.Value {
  value.from_asset(sv.1st, sv.2nd, sv.3rd)
}

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

pub fn spent_output(ctx: ScriptContext) -> Output {
  expect Spend(out_ref) = ctx.purpose
  expect Some(input) = find_input(ctx.transaction.inputs, out_ref)
  input.output
}

pub fn get_input_with_token(
  inputs: List<Input>,
  idx: Int,
  token: AssetClass,
) -> Output {
  // Optimization: assume first reference input by farming for high reference script hashes
  expect Some(input) = list.at(inputs, idx)
  expect value.quantity_of(input.output.value, token.1st, token.2nd) >= 1
  input.output
}

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

pub fn count_orders(tx_inputs: List<Input>) -> Int {
  list.foldl(
    tx_inputs,
    0,
    fn(input, total) {
      when input.output.address.payment_credential is {
        VerificationKeyCredential(_) -> total
        _ -> total + 1
      }
    },
  ) - exact_non_order_script_inputs
}

pub fn has_exact_token_count(val: value.Value, count: Int) {
  count == list.foldl(
    dict.to_list(value.to_dict(val)),
    0,
    fn(tk_pair, total) {
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


pub fn pool_token_names(pool_ident: Ident) {
  (
    pool_ref_name(pool_ident),
    pool_nft_name(pool_ident),
    pool_lp_name(pool_ident),
  )
}

pub fn pool_ref_name(pool_ident: Ident) {
  bytearray.concat(#"000643b0", pool_ident)
}

pub fn pool_nft_name(pool_ident: Ident) {
  bytearray.concat(#"000de140", pool_ident)
}

pub fn pool_lp_name(pool_ident: Ident) {
  bytearray.concat(#"0014df10", pool_ident)
}
