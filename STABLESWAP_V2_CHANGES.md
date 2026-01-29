# Stableswap V2 Changes Summary

This document summarizes the changes made to implement token prescaling and configurable fee denominators in the stableswap contract.

## Problem Statement

The original stableswap contract had two limitations:

1. **No token prescaling**: The stableswap invariant assumes tokens are in equivalent units. Tokens with different decimal places (e.g., USDC with 6 decimals vs DAI with 18 decimals) or different real-world units (e.g., gold in ounces vs grams) would have incorrect pricing.

2. **Fixed basis points precision**: Fees were hardcoded to use basis points (10,000), limiting fee granularity to 0.01%. Some use cases require finer precision.

## Analysis: Why Mathematical Tricks Won't Work

The stableswap invariant is:
```
4A(x + y) + D = 4AD + D³/(4xy)
```

This curve **fundamentally targets a 1:1 ratio** between x and y in base units. We evaluated several approaches:

- **Initializing with "pre-scaled" reserves**: Doesn't work because token quantities come directly from UTXO values via `assets.quantity_of()`.
- **Abusing the amplification factor (A)**: A only controls curve flatness, not the target ratio.
- **Manipulating the sum invariant (D)**: D represents equilibrium sum, doesn't change the target ratio.

**Conclusion**: Code changes are required.

## Implementation

### 1. Token Prescaling

Added `prescale: (Int, Int)` field to `StablePoolDatum`:

```aiken
/// Pre-scaling coefficients for each asset, to normalize tokens with different decimals
/// or different real-world units (e.g., gold in ounces vs grams).
/// Before entering the stableswap invariant calculation, each asset's quantity is multiplied
/// by its prescale factor. This allows the curve to target a 1:1 ratio in "normalized" units.
prescale: (Int, Int),
```

**Usage Examples**:
- USDC (6 decimals) + DAI (18 decimals): `prescale: (1_000_000_000_000, 1)`
- Equal decimals: `prescale: (1, 1)`
- Gold ounces vs grams: `prescale: (31, 1)` (approximately)

**Where prescale is applied**:
- `lib/calculation/swap.ak`: Before `swap_invariant()` and `liquidity_invariant()` calls
- `lib/calculation/deposit.ak`: Before `liquidity_invariant()` call
- `lib/calculation/withdrawal.ak`: Before `liquidity_invariant()` call
- `validators/pool.ak`: In `CreatePool` and `AdjustLinearAmplification`

**Validation**:
- Prescale values must be > 0 (enforced in `CreatePool`)
- Prescale is immutable after pool creation (enforced in `PoolScoop` and `AdjustLinearAmplification`)

### 2. Configurable Fee Denominator

Added `fee_denominator: Int` field to `StablePoolDatum` and renamed fee fields:

```aiken
/// The fee rate to charge on each trade for bid (A -> B) and ask (B -> A) orders
/// The actual fee percentage is lp_fee / fee_denominator
lp_fee: (Int, Int),          // renamed from lp_fee_basis_points
protocol_fee: (Int, Int),    // renamed from protocol_fee_basis_points
fee_denominator: Int,        // NEW: configurable precision
```

**Usage Examples**:
- Traditional basis points: `fee_denominator: 10_000` (1% = 100)
- Parts per million: `fee_denominator: 1_000_000` (0.01% = 100)
- Parts per billion: `fee_denominator: 1_000_000_000` (0.0001% = 1000)

**Updated function**:
```aiken
// lib/shared.ak
pub fn fees_in_legal_range(fees: Int, fee_denominator: Int) {
  and {
    fees >= 0,
    fees <= fee_denominator,
  }
}
```

**Validation**:
- Fee denominator must be > 0 (enforced in `CreatePool`)
- Fee denominator is immutable after pool creation (enforced in `UpdatePoolFees`)

## Files Changed

### Core Types
- `lib/types/pool.ak`: Added `prescale`, `fee_denominator`; renamed `lp_fee_basis_points` -> `lp_fee`, `protocol_fee_basis_points` -> `protocol_fee`

### Shared Utilities
- `lib/shared.ak`: Added `default_fee_denominator`, updated `fees_in_legal_range(fee, denominator)`

### Calculation Logic
- `lib/calculation/swap.ak`: Added prescale parameters, use `fee_denominator` instead of hardcoded `bp_precision`
- `lib/calculation/deposit.ak`: Added prescale parameters
- `lib/calculation/withdrawal.ak`: Added prescale parameters
- `lib/calculation/process.ak`: Thread prescale and fee_denominator through all order processing

### Validators
- `validators/pool.ak`:
  - `PoolScoop`: Extract and validate new fields, pass to `process_orders`
  - `CreatePool`: Validate prescale > 0, fee_denominator > 0, apply prescale to initial invariant
  - `WithdrawFees`: Use fee_denominator for fee validation
  - `UpdatePoolFees`: Ensure fee_denominator doesn't change
  - `AdjustLinearAmplification`: Apply prescale to invariant check, ensure prescale doesn't change
- `validators/oracle.ak`: Updated test fixtures

### Tests
- `lib/tests/aiken/swap.ak`: Updated `do_swap` calls with new parameters
- `lib/tests/aiken/withdrawal.ak`: Updated `do_withdrawal` calls with new parameters
- `lib/tests/aiken/deposit.ak`: Updated `do_deposit` calls with new parameters
- `lib/tests/examples/ex_pool.ak`: Updated example datum
- `validators/tests/pool.ak`: Updated all `StablePoolDatum` literals
- `validators/tests/pool.manage.ak`: Updated fuzz test generators

### New Test Files
- `lib/tests/aiken/prescale.ak`: Tests for prescale functionality with various scaling factors
- `lib/tests/aiken/fee_denominator.ak`: Tests for configurable fee precision

## Migration Notes

For existing pools, migration would require creating new pools with the v2 datum structure. The new fields have these defaults for equivalent behavior:
- `prescale: (1, 1)` - no scaling
- `fee_denominator: 10_000` - traditional basis points

## Off-Chain Changes Required

The scooper (off-chain component) needs updates to:

1. **Compute prescaled values** when calculating swap results using Newton's method
2. **Use fee_denominator** instead of hardcoded 10,000 for fee calculations
3. **Include prescale in D calculations** when computing `next_sum_invariant`

## Testing Status

- All existing tests updated to use new field names and parameters
- New unit tests added for prescale and fee_denominator functionality
- **Note**: Aiken compiler not available in current environment; tests need to be run manually

## Branch Information

- Development branch: `version/stableswap_v2` (local)
- Push branch: `claude/add-token-prescaling-mBFE6`
