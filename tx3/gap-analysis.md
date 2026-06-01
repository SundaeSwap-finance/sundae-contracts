# SundaeSwap Tx3 Draft Gap Analysis

Status: current after preview integration testing

## Validation status

The current draft in `tx3/main.tx3` now supports both local inspection and real preview resolution.

Local workflow:

```sh
cd tx3
trix check
trix build -p local
trix inspect tir --tx submit_swap --pretty -p local
```

Preview workflow proven so far:

- `submit_swap` resolves on preview and has been submitted successfully
- `submit_deposit` resolves on preview and has been submitted successfully
- `submit_withdrawal` resolves on preview and has been submitted successfully

So the file is:

- syntactically and semantically valid Tx3
- buildable as a local Tx3 project
- inspectable at the TIR level
- usable for real preview user-facing pool-targeted order submission flows

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

### 1. `AnyAsset(...)` is wrong for datum construction, but raw list literals work

On-chain, Sundae uses tuple-like values of the shape:

- `(policy_id, asset_name, amount)`

Tx3's local type reference says:

- `AnyAsset` wire type = `(Bytes, Bytes, Int)`

That shape match is real, but the constructor behavior matters.

Observed end-to-end:

- `AnyAsset(policy, name, amount)` inside a datum lowers to `Assets(...)`
- TRP then fails with:

```text
TRP RPC error -32006: error coercing Assets([AssetExpr { ... }]) into DataExpr
```

But this alternate form works:

- `[policy, name, amount]`

Raw heterogeneous list literals lower as plain datum `List` nodes and resolve successfully through TRP, even when assigned to fields typed as `AnyAsset`.

**Current conclusion:** for Sundae datum fields, use raw list literals like `[policy, name, amount]`, and reserve `AnyAsset(...)` for tx amount/value expressions.

### 2. Deposit asset pairs can be encoded as nested raw lists

Sundae deposit and donation orders use a tuple of two singleton values on-chain.

The Sundae blueprint expects:

- `Tuple$Tuple$ByteArray_ByteArray_Int_Tuple$ByteArray_ByteArray_Int`
- i.e. `[[Bytes, Bytes, Int], [Bytes, Bytes, Int]]`

The current draft now models this as:

- `assets: List<AnyAsset>`
- but populates it with nested raw lists:
  - `[[asset_a_policy, asset_a_name, asset_a_amount], [asset_b_policy, asset_b_name, asset_b_amount]]`

Observed in TIR and local TRP resolution:

- nested tuple data lowers as `List` of `List`
- local resolve succeeds

**Current conclusion:** nested raw lists are a viable way to express Sundae's pair-of-singletons datum shape in current Tx3.

### 3. `extension` is still modeled as `Bytes`, but now uses the Sundae-compatible empty marker

On-chain, `extension` is generic Plutus `Data`.

The current draft still types this field as:

- `extension: Bytes`

But user-facing order txs now fill it with the encoded Sundae empty marker:

- `0xd87980`

rather than plain empty bytes.

**Implication:** this is sufficient for the preview-tested user order flows, but the type is still not a full generic `Data` model.

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

### 5. app-friendly default flows currently use `pool_ident = Some(...)`

The draft supports both:

- untargeted orders with `pool_ident = None`
- pool-targeted orders with `pool_ident = Some(...)`

On-chain, untargeted orders are valid Sundae behavior. But in real preview testing, the practical wallet-facing flows that have been proven end-to-end are now exposed as the default submit txs, while untargeted behavior remains available through the explicit `*_any_pool` variants.

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
- local and preview TRP resolve work for the MVP order-posting flows when datum singleton values are written as raw lists
- user-facing submit txs now use `input*` so fragmented wallet UTxOs can satisfy the logical source input via multiple UTxOs
- preview-tested default swap, deposit, and withdrawal flows are working

## Best next implementation steps

### Step 1 — explicit pool-targeted submit flows

This step is now done in `tx3/main.tx3`.

Current default submit txs:

- `submit_swap`
- `submit_deposit`
- `submit_withdrawal`

These set:

- `pool_ident = Some { value: pool_ident }`

These are also the variants that have now been proven on preview for real user-facing order submission.

### Step 2 — `AnyAsset` encoding investigation

This step is now resolved enough to guide implementation.

Result so far:

- Tx3 reference docs say `AnyAsset` wire type is `(Bytes, Bytes, Int)`
- Sundae `SingletonValue` expects the same logical shape
- `AnyAsset(...)` is still the wrong constructor for datum positions
- raw list literals like `[policy, name, amount]` work in datum positions

Observed with:

- generated Python SDK via `trix codegen`
- local devnet / TRP endpoint
- dedicated probe txs
- real `submit_swap` resolve

So the current practical rule is:

- `AnyAsset(...)` works for tx amounts
- raw list literals should be used for Sundae datum singleton values

### Step 3 — pair/list representation

The record-based pair stand-in has been replaced.

Current approach:

- use `List<AnyAsset>`
- emit exactly two nested raw tuple-lists for deposit orders

This now appears to be exact enough for Sundae's pair-of-singletons representation in the current toolchain.

### Step 4 — move to order lifecycle completion

After proving the user-facing submit flows on preview, the next step is:

- validate `cancel_order`
- document any remaining TRP / SDK quirks around `UtxoRef` environment encoding
- then consider broader surface area such as richer destination data, `Donation`, `Record`, and `Strategy`

## Recommendation

The best next step is now to complete the basic order lifecycle:

1. keep using `AnyAsset(...)` for tx amount/value expressions
2. keep using raw list literals for Sundae datum singleton values
3. keep using pool-targeted submit flows as the proven wallet-facing path
4. move to `cancel_order` and confirm a posted preview order can be cancelled cleanly
