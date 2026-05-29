# Sundae Tx3 MVP

This directory contains the in-repo Tx3 draft for Sundae's user-facing order flows.

## Status

Official MVP user-facing flows:

- `submit_swap`
- `submit_deposit`
- `submit_withdrawal`
- `cancel_order`

These are the flows that have been validated as the practical preview-network path for wallet users.

## Recommended integration path

For wallet-facing integrations:

- use the default submit flows, which are pool-targeted
- use the owner's stake key hash for `owner_key_hash`
- use destination payment + stake key hashes in the order datum
- source `order_script_ref` from the profile env
- use the generated Python SDK or `scripts/preview_resolve.py` for preview probing

Secondary / advanced flows still exist in `main.tx3`:

- `submit_swap_any_pool`
- `submit_deposit_any_pool`
- `submit_withdrawal_any_pool`

These untargeted variants are useful for experimentation, but the default submit flows are the recommended public surface.

## Local regression workflow

Run the local regression checks:

```sh
cd tx3
./scripts/run_regressions.sh
```

This runs:

- `trix check`
- `trix build -p local`
- `trix build -p preview`
- `trix codegen -p preview`
- `trix inspect tir` checks for every MVP tx
- generated Python SDK import checks
- `preview_resolve.py --help`

These are deterministic compile/codegen/TIR regressions. The real end-to-end behavior is documented in the preview examples below.

## Preview resolve helper

Use the preview helper with a Demeter TRP key:

```sh
cd tx3
python scripts/preview_resolve.py --help
```

Supported tx kinds:

- `swap`
- `deposit`
- `withdrawal`
- `cancel`

For submit flows, the default variant is the recommended pool-targeted path. Use `--variant any-pool` only for advanced untargeted probing.

## Known-good preview examples

### Default swap

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

### Default deposit

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

### Default withdrawal

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

### Cancel order

```sh
python scripts/preview_resolve.py \
  --api-key "$DEMETER_TRP_API_KEY" \
  --tx-kind cancel \
  --order-utxo <tx_hash>#<index> \
  --order-script-ref 92ec2274938de291d3837b7facf9eddfaed57cd6ff97e26af57cb7a9978e3887#0
```

## Example arg files

See:

- `args-submit-swap.json`
- `args-submit-deposit.json`
- `args-submit-withdrawal.json`
- `args-cancel-order.json`
- `args-submit-swap-any-pool.json`
- `args-submit-deposit-any-pool.json`
- `args-submit-withdrawal-any-pool.json`
