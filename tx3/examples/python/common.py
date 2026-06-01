from __future__ import annotations

import argparse
import importlib.util
import os
import sys
from pathlib import Path

from pycardano import Address

ROOT = Path(__file__).resolve().parents[2]

from tx3_sdk import Party  # noqa: E402
from tx3_sdk.signer import CardanoSigner  # noqa: E402
from tx3_sdk.trp.client import ClientOptions  # noqa: E402

DEFAULT_PREVIEW_TRP_URL = "https://cardano-preview.trp-m1.demeter.run"
DEFAULT_PREVIEW_ORDER_SCRIPT_REF = (
    "92ec2274938de291d3837b7facf9eddfaed57cd6ff97e26af57cb7a9978e3887#0"
)
DEFAULT_POOL_IDENT = "35a34996f515c5a28c8df9eada81f03f4f2756d92e7f73cde1f4e593"
DEFAULT_USDM = (
    "d8906ca5c7ba124a0407a32dab37b2c82b13b3dcd9111e42940dcea4.0014df105553444d"
)
DEFAULT_LP = (
    "44a1eb2d9f58add4eb1932bd0048e6a1947e85e3fe4f32956a110414."
    "0014df1035a34996f515c5a28c8df9eada81f03f4f2756d92e7f73cde1f4e593"
)


def load_generated_sdk():
    gen_root = ROOT / "gen" / "python"
    candidates = [gen_root / "sundae-v3" / "__init__.py", gen_root / "sundae" / "__init__.py"]

    target = next((path for path in candidates if path.exists()), None)
    if target is None:
        raise RuntimeError("could not locate generated sundae SDK under tx3/gen/python")

    module_name = "sundae_generated"
    spec = importlib.util.spec_from_file_location(module_name, target)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load generated SDK module from {target}")

    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module


sdk = load_generated_sdk()
Client = sdk.Client
Profile = sdk.Profile
CancelOrderParams = sdk.CancelOrderParams
SubmitSwapParams = sdk.SubmitSwapParams
SubmitDepositParams = sdk.SubmitDepositParams
SubmitWithdrawalParams = sdk.SubmitWithdrawalParams


def normalize_hex(value: str) -> str:
    value = value.strip()
    if value.startswith("0x"):
        value = value[2:]
    return value.lower()


def parse_asset(value: str) -> tuple[str, str]:
    value = value.strip()
    if value.lower() == "ada":
        return "", ""

    if "." not in value:
        raise ValueError("asset must be 'ada' or '<policy_id>.<asset_name_hex>'")

    policy, name = value.split(".", 1)
    policy = normalize_hex(policy)
    name = normalize_hex(name)

    if len(policy) != 56:
        raise ValueError("policy id must be 56 hex chars")

    bytes.fromhex(policy)
    bytes.fromhex(name)
    return policy, name


def payment_and_stake_key_hashes(address: str) -> tuple[str, str]:
    parsed = Address.from_primitive(address)

    payment = parsed.payment_part
    if payment is None or not hasattr(payment, "payload"):
        raise ValueError("address does not have a payment credential")

    stake = parsed.staking_part
    if stake is None or not hasattr(stake, "payload"):
        raise ValueError("address does not have a staking credential")

    return payment.payload.hex(), stake.payload.hex()


def add_common_preview_args(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--trp-url",
        default=os.getenv("TRP_URL", DEFAULT_PREVIEW_TRP_URL),
        help="Preview TRP endpoint.",
    )
    parser.add_argument(
        "--api-key",
        default=os.getenv("DEMETER_TRP_API_KEY", ""),
        help="Demeter TRP API key. Defaults to DEMETER_TRP_API_KEY.",
    )
    parser.add_argument(
        "--user",
        default=os.getenv("SUNDAE_USER_ADDRESS", ""),
        help="Wallet address used as the User party.",
    )
    parser.add_argument(
        "--order-script",
        default=os.getenv("SUNDAE_ORDER_SCRIPT_ADDRESS", ""),
        help="Optional OrderScript override. Defaults to the selected profile party value.",
    )
    parser.add_argument(
        "--mnemonic",
        default=os.getenv("SUNDAE_USER_MNEMONIC", ""),
        help="Optional wallet mnemonic. Required only for --submit.",
    )
    parser.add_argument(
        "--submit",
        action="store_true",
        help="Resolve, sign, and submit instead of resolve-only.",
    )
    parser.add_argument(
        "--out",
        default="",
        help="Optional path to write the resolved tx CBOR hex.",
    )


def require_api_key_and_user(args: argparse.Namespace) -> None:
    if not args.api_key:
        raise SystemExit(
            "error: missing API key (use --api-key or DEMETER_TRP_API_KEY)"
        )
    if not args.user:
        raise SystemExit(
            "error: missing user address (use --user or SUNDAE_USER_ADDRESS)"
        )
    if args.submit and not args.mnemonic:
        raise SystemExit(
            "error: --submit requires --mnemonic or SUNDAE_USER_MNEMONIC"
        )


def make_preview_client(args: argparse.Namespace) -> tuple[Client, bool]:
    client = Client(
        ClientOptions(
            endpoint=args.trp_url,
            headers={"dmtr-api-key": args.api_key},
        ),
        Profile.PREVIEW,
    )

    user_party = (
        Party.signer(CardanoSigner.from_mnemonic(args.user, args.mnemonic))
        if args.mnemonic
        else Party.address(args.user)
    )

    client.with_user(user_party)
    if args.order_script:
        client.with_orderscript(Party.address(args.order_script))
    return client, user_party.is_signer


async def resolve_or_submit(builder, do_submit: bool, can_sign: bool, out: str = "") -> None:
    resolved = await builder.resolve()
    print("resolve ok")
    print(f"  tx hash:    {resolved.hash}")
    print(f"  tx hex len: {len(resolved.tx_hex)}")
    print(f"  tx hex:     {resolved.tx_hex}")

    if out:
        Path(out).write_text(resolved.tx_hex)
        print(f"  wrote tx hex: {out}")

    if not do_submit:
        return

    if not can_sign:
        raise SystemExit("error: no signer configured for submit")

    signed = await resolved.sign()
    submitted = await signed.submit()
    print("submit ok")
    print(f"  tx hash:    {submitted.hash}")
