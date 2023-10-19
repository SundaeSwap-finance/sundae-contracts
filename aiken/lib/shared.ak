use aiken/builtin
use aiken/bytearray
use aiken/dict.{Dict}
use aiken/hash.{Blake2b_256, Hash}
use aiken/interval.{Finite, Interval}
use aiken/list
use aiken/time.{PosixTime}
use aiken/transaction.{
  DatumHash, InlineDatum, Input, NoDatum, Output, ScriptContext, Spend,
  find_input,
}
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

pub fn is_after(range: Interval<PosixTime>, point: PosixTime) {
  when range.lower_bound.bound_type is {
    Finite(low) ->
      if range.lower_bound.is_inclusive {
        point <= low
      } else {
        point < low
      }
    _ -> False
  }
}

pub fn get_unique_token_count(val: value.Value) -> Int {
  list.foldr(
    dict.to_list(value.to_dict(val)),
    0,
    fn(tk_pair, total) {
      list.foldr(
        dict.to_list(builtin.snd_pair(tk_pair)),
        0,
        fn(_tn, tn_length) { tn_length + 1 },
      ) + total
    },
  )
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

pub fn legal_swap_fees() -> List<Int> {
  [5, 30, 100]
}

pub const settings_nft_name: AssetName = "settings"

pub fn pool_nft_name(pool_ident: Ident) {
  bytearray.concat(#"70", pool_ident)
}

pub fn pool_lq_name(pool_ident: Ident) {
  bytearray.concat(#"6c", pool_ident)
}
