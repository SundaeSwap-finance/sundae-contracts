use aiken/interval.{Finite, Interval}
use aiken/time.{PosixTime}
use aiken/transaction.{
  DatumHash, InlineDatum, NoDatum, Output, ScriptContext, Spend, find_datum,
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
  (AssetClass, Int)

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
