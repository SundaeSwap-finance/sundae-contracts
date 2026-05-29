# Sundae Tx3 deployment metadata

This file records the known Sundae deployment constants that integrators need when binding parties and selecting profile env values.

## Preview

- network: `CardanoPreview`
- order script hash: `cfad1914b599d18bffd14d2bbd696019c2899cbdd6a03325cdf680bc`
- order script address: `addr_test1wr866xg5kkvarzll69xjh0tfvqvu9zvuhht2qve9ehmgp0qfgf3wc`
- order script reference UTxO: `92ec2274938de291d3837b7facf9eddfaed57cd6ff97e26af57cb7a9978e3887#0`
- profile env file: `.env.preview`

## Mainnet

- network: `CardanoMainnet`
- order script hash: `fa6a58bbe2d0ff05534431c8e2f0ef2cbdc1602a8456e4b13c8f3077`
- order script address: `addr1w8ax5k9mutg07p2ngscu3chsauktmstq92z9de938j8nqacprc9mw`
- order script reference UTxO: `f5f1bdfad3eb4d67d2fc36f36f47fc2938cf6f001689184ab320735a28642cf2#0`
- profile env file: `.env.mainnet`

## Usage note

The published Tx3 profiles currently provide environment values such as `order_script_ref`, but the `OrderScript` party address is still expected to be bound by the client.

For example, SDK users should bind the order script party explicitly:

```python
from sundae import Client, Profile
from tx3_sdk import Party

client = Client(..., Profile.MAINNET)
client.with_orderscript(
    Party.address("addr1w8ax5k9mutg07p2ngscu3chsauktmstq92z9de938j8nqacprc9mw")
)
```

Likewise for preview:

```python
client = Client(..., Profile.PREVIEW)
client.with_orderscript(
    Party.address("addr_test1wr866xg5kkvarzll69xjh0tfvqvu9zvuhht2qve9ehmgp0qfgf3wc")
)
```
