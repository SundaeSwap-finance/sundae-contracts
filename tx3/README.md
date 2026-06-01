# Sundae Tx3 v3

This directory contains the Tx3 protocol package for Sundae v3 order submission on Cardano.

It is aimed at integrators who want to:

- post swap orders
- post deposit orders
- post withdrawal orders
- cancel posted orders

against the deployed Sundae order script.

## What is supported

Recommended public flows:

- `submit_swap`
- `submit_deposit`
- `submit_withdrawal`
- `cancel_order`

These are the pool-targeted, wallet-facing flows that were validated on preview.

Additional non-default flows also exist:

- `submit_swap_any_pool`
- `submit_deposit_any_pool`
- `submit_withdrawal_any_pool`

Use the `*_any_pool` variants only if you intentionally want `pool_ident = None`.

## Network configuration

Network-specific values live in profile/env config, not in protocol logic.

Important pieces:

- `order_script_ref` comes from `.env.<profile>`
- `OrderScript` can also be supplied by the profile via the lowercase `orderscript` key

See:

- `deployments.md` for preview/mainnet script addresses and reference UTxOs
- `trix.toml` for available profiles
- `.env.preview` / `.env.mainnet` for committed env defaults

## Conventions

A few conventions matter when building orders:

- `owner_key_hash` should usually be the wallet's **stake key hash**
- the default submit flows write a wallet-style destination:
  - payment credential from `destination_payment_key_hash`
  - inline stake credential from `destination_stake_key_hash`
  - `TxDatum::NoDatum`
- asset amounts are raw on-chain units:
  - ADA in lovelace
  - tokens in base units
- for CLI helper asset strings:
  - `ada`
  - or `<policy_id>.<asset_name_hex>`

## Python quickstart

Generate the SDK and install its dependencies:

```sh
cd tx3
trix codegen -p preview
pip install -r gen/python/sundae/requirements.txt
```

The generated SDK expects two runtime party bindings:

- `User` = the wallet building the transaction
- `OrderScript` = the deployed Sundae order script address for that network

Minimal setup looks like:

```python
from sundae import Client, Profile
from tx3_sdk import Party
from tx3_sdk.trp.client import ClientOptions

client = Client(
    ClientOptions(
        endpoint="https://cardano-preview.trp-m1.demeter.run",
        headers={"dmtr-api-key": "<demeter-trp-api-key>"},
    ),
    Profile.PREVIEW,
)

client.with_user(Party.address("<wallet address>"))
```

The preview/mainnet profiles already carry the deployed `orderscript` party value.
Call `with_orderscript(...)` only if you want to override it.

For actual submission, `User` must be a signer-backed party rather than address-only.
The runnable example scripts below support both modes:

- default = resolve only
- `--submit` = resolve, sign, and submit
- all scripts print the resolved tx CBOR hex
- `--out <path>` writes the resolved tx CBOR hex to a file

## Runnable Python examples

Scripts live under `tx3/examples/python`:

- `submit_swap.py`
- `submit_deposit.py`
- `submit_withdrawal.py`
- `cancel_order.py`

Set common environment variables first:

```sh
export DEMETER_TRP_API_KEY="<demeter-trp-api-key>"
export SUNDAE_USER_ADDRESS="<wallet address>"
```

Optional, only if you want to override the profile-supplied order script address:

```sh
export SUNDAE_ORDER_SCRIPT_ADDRESS="<override order script address>"
```

Optional, only for `--submit`:

```sh
export SUNDAE_USER_MNEMONIC="word1 word2 ..."
```

These examples were smoke-tested on preview at the `resolve()` layer for:

- swap
- deposit

The withdrawal and cancel examples require live wallet-specific inputs:

- withdrawal: an LP asset and amount actually present in the wallet
- cancel: a live order UTxO to cancel

### Resolve a swap

```sh
cd tx3
python examples/python/submit_swap.py
```

### Resolve a deposit

```sh
cd tx3
python examples/python/submit_deposit.py
```

### Resolve a withdrawal

```sh
cd tx3
python examples/python/submit_withdrawal.py \
  --lp-asset <lp_policy>.<lp_asset_name_hex> \
  --lp-amount <lp_amount>
```

### Resolve a cancel

```sh
cd tx3
python examples/python/cancel_order.py \
  --order-utxo <tx_hash>#<index>
```

### Submit instead of resolve

If you have configured `SUNDAE_USER_MNEMONIC`, add `--submit`:

```sh
cd tx3
python examples/python/submit_swap.py --submit
```

For deployment-specific addresses and reference UTxOs, see `deployments.md`.

## TypeScript showcase

This repo also generates a TypeScript client to demonstrate Tx3 codegen beyond Python.

Generate the bindings:

```sh
cd tx3
trix codegen -p preview
```

Install the minimal runtime dependencies in your own JS/TS project:

```sh
npm install tx3-sdk
npm install -D tsx typescript
```

Then try the showcase example:

```sh
cd tx3
DEMETER_TRP_API_KEY="<demeter-trp-api-key>" \
SUNDAE_USER_ADDRESS="<wallet address>" \
npx tsx examples/typescript/submit_swap.ts
```

That example imports the generated client from:

- `gen/typescript/sundae-v3/protocol.ts`

and showcases the same pool-targeted `submit_swap` flow used in the Python examples.

## Current destination model

The default submit flows are meant for standard wallet returns.

Today they write a wallet-style Sundae destination using:

- `destination_payment_key_hash`
- `destination_stake_key_hash`
- `TxDatum::NoDatum`

So this package is currently best suited for standard wallet destinations.

Important caveat:

- richer destination forms like `Destination::Self` or script destinations are not yet exposed as public submit flows here
- `TxDatum::InlineDatum` is still modeled only as raw `Bytes`

## Preview helper script

For quick probing without writing your own client code:

```sh
cd tx3
python scripts/preview_resolve.py --help
```

Examples:

### Resolve a swap

```sh
python scripts/preview_resolve.py \
  --api-key "$DEMETER_TRP_API_KEY" \
  --tx-kind swap \
  --pool-ident 35a34996f515c5a28c8df9eada81f03f4f2756d92e7f73cde1f4e593 \
  --offer d8906ca5c7ba124a0407a32dab37b2c82b13b3dcd9111e42940dcea4.0014df105553444d \
  --offer-amount 1000000 \
  --receive ada \
  --min-received 1 \
  --order-ada 3000000 \
  --max-protocol-fee 600000
```

### Resolve a deposit

```sh
python scripts/preview_resolve.py \
  --api-key "$DEMETER_TRP_API_KEY" \
  --tx-kind deposit \
  --pool-ident 35a34996f515c5a28c8df9eada81f03f4f2756d92e7f73cde1f4e593 \
  --asset-a ada \
  --asset-a-amount 5000000 \
  --asset-b d8906ca5c7ba124a0407a32dab37b2c82b13b3dcd9111e42940dcea4.0014df105553444d \
  --asset-b-amount 1000000 \
  --order-ada 3000000 \
  --max-protocol-fee 600000
```

### Resolve a withdrawal

```sh
python scripts/preview_resolve.py \
  --api-key "$DEMETER_TRP_API_KEY" \
  --tx-kind withdrawal \
  --pool-ident 35a34996f515c5a28c8df9eada81f03f4f2756d92e7f73cde1f4e593 \
  --lp-asset 44a1eb2d9f58add4eb1932bd0048e6a1947e85e3fe4f32956a110414.0014df1035a34996f515c5a28c8df9eada81f03f4f2756d92e7f73cde1f4e593 \
  --lp-amount 1000 \
  --order-ada 3000000 \
  --max-protocol-fee 600000
```

### Resolve a cancel

```sh
python scripts/preview_resolve.py \
  --api-key "$DEMETER_TRP_API_KEY" \
  --tx-kind cancel \
  --order-utxo <tx_hash>#<index> \
  --order-script-ref 92ec2274938de291d3837b7facf9eddfaed57cd6ff97e26af57cb7a9978e3887#0
```

## Regression checks

Run:

```sh
cd tx3
./scripts/run_regressions.sh
```

This verifies:

- `trix check`
- local/preview builds
- preview codegen
- `trix inspect tir` for the current tx surface
- generated Python SDK imports
- helper script loading
- example script CLI/help sanity

## Files you will likely use

- `main.tx3` — protocol definition
- `deployments.md` — preview/mainnet script addresses and refs
- `examples/python/` — runnable Python SDK examples
- `examples/typescript/submit_swap.ts` — minimal TypeScript codegen showcase
- `scripts/preview_resolve.py` — quick preview resolver helper
- `args-submit-swap.json`
- `args-submit-deposit.json`
- `args-submit-withdrawal.json`
- `args-cancel-order.json`
