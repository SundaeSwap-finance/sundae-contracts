# SundaeSwap Tx3 Draft Gap Analysis

Status: current after initial draft + `trix check`

## Validation status

The current draft in `tx3/main.tx3` passes:

```sh
cd tx3
trix check
```

So the file is syntactically and semantically valid Tx3.

## What is already aligned well

These parts of the draft intentionally mirror the current Sundae blueprint / Aiken types closely:

- `MultisigScript`
  - constructor names match blueprint names
  - field names match blueprint names
- `MaybeBytes`
  - models `Option<ByteArray>` as `Some` / `None`
- `Destination`
  - constructor names match (`Fixed`, `Self`)
- `Order`
  - MVP subset uses the same constructor names as Sundae (`Swap`, `Deposit`, `Withdrawal`)
- `OrderDatum`
  - field names match the current Aiken type:
    - `pool_ident`
    - `owner`
    - `max_protocol_fee`
    - `destination`
    - `details`
    - `extension`
- `OrderRedeemer`
  - matches `Scoop | Cancel`

## Known approximation / mismatch areas

These are the main places where the current draft is not yet an exact encoding of Sundae's on-chain interface.

### 1. `AnyAsset` may not be an exact replacement for Sundae `SingletonValue`

On-chain, Sundae uses tuple-like values of the shape:

- `(policy_id, asset_name, amount)`

In the draft, these are modeled using Tx3's built-in `AnyAsset` type.

This is convenient and probably the best first draft, but it still needs confirmation that the generated Plutus-data encoding matches the Sundae blueprint shape expected by the contracts.

**Risk:** if Tx3 serializes `AnyAsset` differently from the blueprint tuple shape, the draft will be conceptually right but binary-incompatible.

### 2. `AssetPair` is only a stand-in

Sundae deposit and donation orders use a tuple of two singleton values on-chain.

The current draft uses:

- `type AssetPair { asset_a: AnyAsset, asset_b: AnyAsset }`

This is easy to read, but it is not guaranteed to encode the same as Sundae's tuple-of-two-values representation.

**This is the biggest known shape mismatch in the MVP draft.**

### 3. `extension` is currently narrowed to `Bytes`

On-chain, `extension` is generic Plutus `Data`.

The current draft sets:

- `extension: Bytes`
- and fills it with `""`

This is a pragmatic MVP choice because Tx3 does not appear to expose a generic `Data` type in the language docs the way the Aiken blueprint does.

**Implication:** the draft is currently only suitable for simple order-posting flows that don't rely on extension data.

### 4. `TxDatum::InlineDatum` is not exact

On-chain, `Datum` can contain:

- `NoDatum`
- `DatumHash(ByteArray)`
- `InlineDatum(Data)`

The draft currently approximates this as:

- `InlineDatum { datum: Bytes }`

That is not exact because the blueprint expects generic `Data`, not raw `Bytes`.

For the MVP this is acceptable because the draft only emits:

- `TxDatum::NoDatum { }`

### 5. `pool_ident` is always `None`

The current draft intentionally posts only untargeted orders:

- `pool_ident = None`

This is valid Sundae behavior, but not full coverage of the datum space.

### 6. destination handling is intentionally narrow

The draft only emits:

- `Destination::Fixed`
- with `TxDatum::NoDatum`

It does not yet model:

- `Destination::Self`
- fixed destinations with datum hash
- fixed destinations with inline structured datum payloads

### 7. only MVP order variants are implemented

Currently implemented:

- `Swap`
- `Deposit`
- `Withdrawal`

Not yet implemented:

- `Donation`
- `Strategy`
- `Record`

## Tooling note

`trix check` succeeds, but `trix build` / `trix inspect tir` still need a bit more project/profile cleanup before they are useful in this repo-local draft setup.

So right now:

- language validation works
- deeper build/introspection workflow is not fully wired yet

## Best next implementation steps

### Step 1 — explicit pool-targeted variants

This step is now done in `tx3/main.tx3`.

Current targeted txs:

- `submit_swap_for_pool`
- `submit_deposit_for_pool`
- `submit_withdrawal_for_pool`

These set:

- `pool_ident = Some { value: pool_ident }`

### Step 2 — confirm `AnyAsset` encoding

We should verify whether Tx3's `AnyAsset` lowers to the same Plutus-data shape as Sundae's tuple `(Bytes, Bytes, Int)`.

If yes, the draft gets much closer to production-ready.

If not, we need an alternate representation strategy.

### Step 3 — replace `AssetPair` stand-in with an exact representation

If Tx3 can express the exact tuple-of-two-singletons shape, we should switch to it.

If not, this becomes a documented tooling limitation for the MVP.

### Step 4 — widen datum coverage carefully

After exactness on basic assets is confirmed:

- add `pool_ident = Some`
- add richer destination datums if possible
- then add `Donation`
- then consider `Record` and `Strategy`

## Recommendation

The best next code change is now:

1. keep the current MVP txs and newly added targeted variants
2. confirm whether `AnyAsset` matches Sundae's singleton tuple encoding
3. continue treating `AssetPair` / generic `Data` support as the main exactness blockers
