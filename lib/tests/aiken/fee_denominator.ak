/// Tests for the configurable fee_denominator functionality in stableswap v2
///
/// The fee_denominator allows for finer-grained fee precision than the traditional
/// basis points (10,000). For example, using 1,000,000 allows fees as precise as
/// 0.0001% instead of 0.01%.

use shared.{fees_in_legal_range}

/// Test that fees_in_legal_range works with traditional basis points (10,000)
test fees_in_legal_range_basis_points() {
  let fee_denominator = 10_000
  and {
    // 0% fee is valid
    fees_in_legal_range(0, fee_denominator),
    // 1% fee (100 bp) is valid
    fees_in_legal_range(100, fee_denominator),
    // 0.3% fee (30 bp) is valid
    fees_in_legal_range(30, fee_denominator),
    // 100% fee is valid (though extreme)
    fees_in_legal_range(10_000, fee_denominator),
  }
}

/// Test that fees_in_legal_range rejects invalid fees for basis points
test fees_in_legal_range_basis_points_invalid() {
  let fee_denominator = 10_000
  and {
    // Negative fee is invalid
    !fees_in_legal_range(-1, fee_denominator),
    // Over 100% fee is invalid
    !fees_in_legal_range(10_001, fee_denominator),
  }
}

/// Test that fees_in_legal_range works with higher precision (1,000,000)
test fees_in_legal_range_high_precision() {
  let fee_denominator = 1_000_000
  and {
    // 0% fee is valid
    fees_in_legal_range(0, fee_denominator),
    // 1% fee (10,000 ppm) is valid
    fees_in_legal_range(10_000, fee_denominator),
    // 0.01% fee (100 ppm) is valid - not possible with basis points
    fees_in_legal_range(100, fee_denominator),
    // 0.001% fee (10 ppm) is valid - very fine granularity
    fees_in_legal_range(10, fee_denominator),
    // 100% fee is valid (though extreme)
    fees_in_legal_range(1_000_000, fee_denominator),
  }
}

/// Test that fees_in_legal_range rejects invalid fees for high precision
test fees_in_legal_range_high_precision_invalid() {
  let fee_denominator = 1_000_000
  and {
    // Negative fee is invalid
    !fees_in_legal_range(-1, fee_denominator),
    // Over 100% fee is invalid
    !fees_in_legal_range(1_000_001, fee_denominator),
  }
}

/// Test equivalent fee percentages across different denominators
/// 1% should work identically regardless of denominator
test equivalent_fee_percentages() {
  and {
    // 1% with basis points (100/10000)
    fees_in_legal_range(100, 10_000),
    // 1% with parts per million (10000/1000000)
    fees_in_legal_range(10_000, 1_000_000),
    // 1% with parts per billion (10000000/1000000000)
    fees_in_legal_range(10_000_000, 1_000_000_000),
  }
}
