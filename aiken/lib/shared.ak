use aiken/interval.{Finite, Interval}
use aiken/list
use aiken/time.{PosixTime}
use aiken/transaction.{
  DatumHash, InlineDatum, Input, NoDatum, Output, ScriptContext, Spend,
  find_datum, find_input,
}
use aiken/transaction/value.{AssetName, PolicyId}

pub type Ident =
  ByteArray

pub type AssetClass =
  (PolicyId, AssetName)

pub type Rational =
  (Int, Int)

pub type SingletonValue =
  (AssetClass, Int)

pub fn to_value(sv: SingletonValue) -> value.Value {
  value.from_asset(sv.1st.1st, sv.1st.2nd, sv.2nd)
}

const settings_script_hash: PolicyId = #""

const settings_token_name: AssetName = #""

pub fn settings_asset_class() -> AssetClass {
  (settings_script_hash, settings_token_name)
}

pub const order_script_hash: PolicyId = #""

pub fn datum_of(ctx: ScriptContext, output: Output) -> Option<Data> {
  when output.datum is {
    transaction.NoDatum -> None
    transaction.DatumHash(h) -> find_datum([], ctx.transaction.datums, h)
    transaction.InlineDatum(d) -> Some(d)
  }
}

pub fn spent_output(ctx: ScriptContext) -> Output {
  expect Spend(out_ref) = ctx.purpose
  expect Some(input) = find_input(ctx.transaction.inputs, out_ref)
  input.output
}

pub fn is_after(range: Interval<PosixTime>, point: PosixTime) {
  when (range.lower_bound.bound_type, range.lower_bound.is_inclusive) is {
    (Finite(low), True) -> point <= low
    (Finite(low), False) -> point < low
    _ -> False
  }
}

pub fn get_input_with_token(
  inputs: List<Input>,
  idx: Int,
  token: AssetClass,
) -> Output {
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
