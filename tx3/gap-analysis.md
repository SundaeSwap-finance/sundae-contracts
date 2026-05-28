# SundaeSwap Tx3 Draft Gap Analysis

Status: current after build / TIR investigation

## Validation status

The current draft in `tx3/main.tx3` now supports the basic local workflow:

```sh
cd tx3
trix check
trix build -p local
trix inspect tir --tx submit_swap --pretty -p local
```

So the file is:

- syntactically and semantically valid Tx3
- buildable as a local Tx3 project
- inspectable at the TIR level

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

### 1. `AnyAsset` is likely a good match for Sundae `SingletonValue`

On-chain, Sundae uses tuple-like values of the shape:

- `(policy_id, asset_name, amount)`

Tx3's local type reference says:

- `AnyAsset` wire type = `(Bytes, Bytes, Int)`

So the current draft's use of `AnyAsset` for:

- `Swap.offer`
- `Swap.min_received`
- `Withdrawal.amount`

is now **likely compatible** with Sundae `SingletonValue`.

TIR still shows these fields as `Assets` nodes:

```sh
cd tx3
trix inspect tir --tx submit_swap --pretty -p local
```

but that appears to be an intermediate representation detail, not necessarily the final on-wire encoding.

**Working conclusion:** `AnyAsset` is probably the right Tx3 representation for Sundae `SingletonValue`.

### 2. Deposit asset pairs are now modeled as `List<AnyAsset>`

Sundae deposit and donation orders use a tuple of two singleton values on-chain.

The Sundae blueprint expects:

- `Tuple$Tuple$ByteArray_ByteArray_Int_Tuple$ByteArray_ByteArray_Int`
- i.e. `[[Bytes, Bytes, Int], [Bytes, Bytes, Int]]`

The current draft now models this as:

- `assets: List<AnyAsset>`
- emitted as `[deposit_a, deposit_b]`

Observed in:

```sh
cd tx3
trix inspect tir --tx submit_deposit --pretty -p local
```

The lowered deposit datum now contains a plain `List` with two `Assets` items.

Given the Tx3 type reference for `AnyAsset`, this is a much better match than the earlier record-based stand-in.

**Working conclusion:** deposit orders are now likely much closer to Sundae's exact on-chain encoding.

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

The local project workflow now works after a small project-shape adjustment:

- `OrderScript` is modeled as a `party`, not a `policy`

This is a tooling workaround for local `trix build` / `trix inspect tir` success in this draft project.

So right now:

- language validation works
- local build works
- local TIR inspection works
- exact datum compatibility is still the open problem

## Best next implementation steps

### Step 1 — explicit pool-targeted variants

This step is now done in `tx3/main.tx3`.

Current targeted txs:

- `submit_swap_for_pool`
- `submit_deposit_for_pool`
- `submit_withdrawal_for_pool`

These set:

- `pool_ident = Some { value: pool_ident }`

### Step 2 — `AnyAsset` encoding investigation

This step is now in a much better place.

Result so far:

- Tx3 reference docs say `AnyAsset` wire type is `(Bytes, Bytes, Int)`
- Sundae `SingletonValue` expects the same logical shape
- TIR uses an `Assets` node, but that does not by itself prove a wire mismatch

Current working assumption:

- `AnyAsset` is the correct representation for Sundae singleton asset values

What remains is end-to-end confirmation against real produced transaction data.

### Step 3 — pair/list representation

The record-based pair stand-in has been replaced.

Current approach:

- use `List<AnyAsset>`
- emit exactly two elements for deposit orders

This is the best current approximation and may in fact be exact enough for Sundae's pair-of-singletons representation.

### Step 4 — widen datum coverage carefully

After exactness on basic assets is confirmed:

- add `pool_ident = Some`
- add richer destination datums if possible
- then add `Donation`
- then consider `Record` and `Strategy`

## Recommendation

The best next step is now an end-to-end encoding check rather than more protocol surface area:

1. keep the current MVP txs and newly added targeted variants
2. treat `AnyAsset` as likely correct for Sundae singleton values
3. treat `List<AnyAsset>` as the best current representation for deposit pairs
4. focus remaining research on:
   - generic `Data` coverage
   - end-to-end confirmation of produced datum encoding
   - eventually `Donation`, `Record`, and `Strategy`
