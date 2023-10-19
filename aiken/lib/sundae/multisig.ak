use aiken/interval.{Finite, Interval, IntervalBound}
use aiken/list
use aiken/time.{PosixTime}
use aiken/transaction.{ValidityRange}

pub type MultisigScript {
  Signature { key_hash: ByteArray }
  AllOf { scripts: List<MultisigScript> }
  AnyOf { scripts: List<MultisigScript> }
  AtLeast { required: Int, scripts: List<MultisigScript> }
  Before { time: PosixTime }
  After { time: PosixTime }
}

pub fn satisfied(
  script: MultisigScript,
  signatories: List<ByteArray>,
  valid_range: ValidityRange,
) -> Bool {
  when script is {
    Signature { key_hash } -> list.has(signatories, key_hash)
    AllOf { scripts } ->
      list.all(scripts, fn(s) { satisfied(s, signatories, valid_range) })
    AnyOf { scripts } ->
      list.any(scripts, fn(s) { satisfied(s, signatories, valid_range) })
    AtLeast { required, scripts } ->
      required <= list.count(
        scripts,
        fn(s) { satisfied(s, signatories, valid_range) },
      )
    // TODO: could be simplified with https://github.com/aiken-lang/aiken/issues/566
    Before { time } ->
      when valid_range.upper_bound.bound_type is {
        Finite(hi) ->
          if valid_range.upper_bound.is_inclusive {
            hi <= time
          } else {
            hi < time
          }
        _ -> False
      }
    After { time } ->
      when valid_range.lower_bound.bound_type is {
        Finite(lo) ->
          if valid_range.lower_bound.is_inclusive {
            time <= lo
          } else {
            time < lo
          }
        _ -> False
      }
  }
}

test satisfying() {
  let key_hash1 = "key1"
  let key_hash2 = "key2"
  let key_hash3 = "key3"
  let sig1 = Signature { key_hash: key_hash1 }
  let sig2 = Signature { key_hash: key_hash2 }
  let sig3 = Signature { key_hash: key_hash3 }
  let all_of = AllOf { scripts: [sig1, sig2] }
  let any_of = AnyOf { scripts: [sig1, sig2] }
  let at_least = AtLeast { required: 2, scripts: [sig1, sig2, sig3] }
  let before = Before { time: 10 }
  let after = After { time: 10 }
  let between = AllOf { scripts: [After { time: 10 }, Before { time: 15 }] }
  let vesting =
    AnyOf {
      scripts: [
        AllOf { scripts: [before, sig1] },
        // clawback
        AllOf { scripts: [after, sig2] },
      ],
    }
  // vested
  let valid_range =
    fn(lo: Int, hi: Int) -> ValidityRange {
      Interval {
        lower_bound: IntervalBound {
          bound_type: Finite(lo),
          is_inclusive: True,
        },
        upper_bound: IntervalBound {
          bound_type: Finite(hi),
          is_inclusive: False,
        },
      }
    }
  // Helper method because ? binds more tightly than !
  let unsatisfied =
    fn(n: MultisigScript, s: List<ByteArray>, v: ValidityRange) {
      !satisfied(n, s, v)
    }
  and {
    satisfied(sig1, [key_hash1], valid_range(0, 1))?,
    satisfied(sig2, [key_hash1, key_hash2], valid_range(0, 1))?,
    satisfied(all_of, [key_hash1, key_hash2], valid_range(0, 1))?,
    satisfied(any_of, [key_hash2], valid_range(0, 1))?,
    satisfied(at_least, [key_hash2, key_hash3], valid_range(0, 1))?,
    satisfied(before, [], valid_range(0, 5))?,
    satisfied(after, [], valid_range(15, 20))?,
    satisfied(after, [], valid_range(10, 15))?,
    satisfied(between, [], valid_range(12, 13))?,
    satisfied(vesting, [key_hash1], valid_range(0, 5))?,
    satisfied(vesting, [key_hash2], valid_range(15, 20))?,
    unsatisfied(sig1, [key_hash2], valid_range(0, 1))?,
    unsatisfied(sig3, [key_hash1, key_hash2], valid_range(0, 1))?,
    unsatisfied(all_of, [key_hash1, key_hash3], valid_range(0, 1))?,
    unsatisfied(any_of, [key_hash3], valid_range(0, 1))?,
    unsatisfied(at_least, [key_hash2], valid_range(0, 1))?,
    unsatisfied(before, [], valid_range(5, 15))?,
    unsatisfied(before, [], valid_range(5, 10))?,
    unsatisfied(before, [], valid_range(10, 10))?,
    unsatisfied(after, [], valid_range(5, 15))?,
    unsatisfied(between, [], valid_range(0, 5))?,
    unsatisfied(between, [], valid_range(0, 13))?,
    unsatisfied(between, [], valid_range(0, 20))?,
    unsatisfied(between, [], valid_range(13, 20))?,
    unsatisfied(between, [], valid_range(13, 15))?,
    unsatisfied(between, [], valid_range(15, 20))?,
    unsatisfied(vesting, [key_hash2], valid_range(0, 5))?,
    unsatisfied(vesting, [key_hash1], valid_range(15, 20))?,
    unsatisfied(vesting, [key_hash3], valid_range(10, 10))?,
    unsatisfied(vesting, [key_hash3], valid_range(0, 5))?,
    unsatisfied(vesting, [key_hash3], valid_range(15, 20))?,
  }
}
