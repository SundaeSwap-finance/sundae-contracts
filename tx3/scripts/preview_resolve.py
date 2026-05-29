#!/usr/bin/env python3
import argparse
import asyncio
import os
import sys
from pathlib import Path

from pycardano import Address

ROOT = Path(__file__).resolve().parents[1]
sys.path.append(str(ROOT / "gen" / "python"))

from sundae import (  # noqa: E402
    CancelOrderParams,
    Client,
    Profile,
    SubmitDepositForPoolParams,
    SubmitDepositParams,
    SubmitSwapForPoolParams,
    SubmitSwapParams,
    SubmitWithdrawalForPoolParams,
    SubmitWithdrawalParams,
)
from tx3_sdk import Party  # noqa: E402
from tx3_sdk.trp.client import ClientOptions  # noqa: E402

DEFAULT_TRP_URL = "https://cardano-preview.trp-m1.demeter.run"
DEFAULT_ORDER_SCRIPT = "addr_test1wr866xg5kkvarzll69xjh0tfvqvu9zvuhht2qve9ehmgp0qfgf3wc"
DEFAULT_USER = "addr_test1qrhgxhuwx4khwn65hyze3nzn73xtlz2t2l98zjhsdgwewpaslfudgz0znlrp3nh24zna0vnrnlznec5zcgyahsyqes7qejgm0h"
DEFAULT_OFFER = (
    "d8906ca5c7ba124a0407a32dab37b2c82b13b3dcd9111e42940dcea4.0014df105553444d"
)
DEFAULT_RECEIVE = "ada"
DEFAULT_POOL_IDENT = "35a34996f515c5a28c8df9eada81f03f4f2756d92e7f73cde1f4e593"
DEFAULT_ORDER_SCRIPT_REF = "92ec2274938de291d3837b7facf9eddfaed57cd6ff97e26af57cb7a9978e3887#0"


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


async def main() -> int:
    parser = argparse.ArgumentParser(
        description="Resolve a Sundae preview tx against TRP."
    )
    parser.add_argument("--trp-url", default=os.getenv("TRP_URL", DEFAULT_TRP_URL))
    parser.add_argument(
        "--api-key",
        default=os.getenv("DEMETER_TRP_API_KEY", ""),
        help="Demeter TRP API key. Defaults to DEMETER_TRP_API_KEY env var.",
    )
    parser.add_argument("--user", default=DEFAULT_USER)
    parser.add_argument("--order-script", default=DEFAULT_ORDER_SCRIPT)
    parser.add_argument(
        "--tx-kind",
        choices=["swap", "deposit", "withdrawal", "cancel"],
        default="swap",
        help="Which Sundae tx family to resolve.",
    )
    parser.add_argument(
        "--owner-key-hash",
        default="",
        help="Override the owner key hash written into the order datum. Defaults to the user's stake key hash.",
    )
    parser.add_argument(
        "--destination-payment-key-hash",
        default="",
        help="Override the payment key hash written into the order datum destination.",
    )
    parser.add_argument(
        "--destination-stake-key-hash",
        default="",
        help="Override the stake key hash written into the order datum destination.",
    )
    parser.add_argument(
        "--variant",
        choices=["default", "for-pool"],
        default="default",
        help="Resolve the untargeted or pool-targeted tx variant.",
    )
    parser.add_argument(
        "--pool-ident",
        default=DEFAULT_POOL_IDENT,
        help="Pool ident hex. Required for --variant for-pool.",
    )
    parser.add_argument("--offer", default=DEFAULT_OFFER)
    parser.add_argument("--offer-amount", type=int, default=1_000_000)
    parser.add_argument("--receive", default=DEFAULT_RECEIVE)
    parser.add_argument("--min-received", type=int, default=1_000_000)
    parser.add_argument("--asset-a", default="ada")
    parser.add_argument("--asset-a-amount", type=int, default=5_000_000)
    parser.add_argument("--asset-b", default=DEFAULT_OFFER)
    parser.add_argument("--asset-b-amount", type=int, default=1_000_000)
    parser.add_argument("--lp-asset", default="ada")
    parser.add_argument("--lp-amount", type=int, default=1_000_000)
    parser.add_argument(
        "--order-utxo",
        default="",
        help="Order UTxO ref to cancel, in <tx_hash>#<index> form.",
    )
    parser.add_argument(
        "--order-script-ref",
        default=DEFAULT_ORDER_SCRIPT_REF,
        help="Reference script UTxO for cancel flow, in <tx_hash>#<index> form.",
    )
    parser.add_argument("--order-ada", type=int, default=3_000_000)
    parser.add_argument("--max-protocol-fee", type=int, default=1_000_000)
    parser.add_argument("--min-collateral-ada", type=int, default=5_000_000)
    parser.add_argument(
        "--out",
        default="",
        help="Optional path to save resolved tx hex.",
    )
    args = parser.parse_args()

    if not args.api_key:
        print("error: missing API key (use --api-key or DEMETER_TRP_API_KEY)")
        return 2

    offer_policy, offer_name = parse_asset(args.offer)
    receive_policy, receive_name = parse_asset(args.receive)
    asset_a_policy, asset_a_name = parse_asset(args.asset_a)
    asset_b_policy, asset_b_name = parse_asset(args.asset_b)
    lp_policy, lp_name = parse_asset(args.lp_asset)
    user_payment_key_hash, user_stake_key_hash = payment_and_stake_key_hashes(args.user)
    owner_key_hash = normalize_hex(args.owner_key_hash) or user_stake_key_hash
    destination_payment_key_hash = normalize_hex(args.destination_payment_key_hash) or user_payment_key_hash
    destination_stake_key_hash = normalize_hex(args.destination_stake_key_hash) or user_stake_key_hash
    pool_ident = normalize_hex(args.pool_ident)

    if args.variant == "for-pool" and not pool_ident:
        print("error: --pool-ident is required for --variant for-pool")
        return 2
    if args.tx_kind == "cancel" and not args.order_utxo:
        print("error: --order-utxo is required for cancel")
        return 2

    print("Resolving Sundae tx with:")
    print(f"  TRP URL:           {args.trp_url}")
    print(f"  User:              {args.user}")
    print(f"  Owner key hash:    {owner_key_hash}")
    print(f"  Order script:      {args.order_script}")
    print(f"  Tx kind:           {args.tx_kind}")
    print(f"  Variant:           {args.variant}")
    print(f"  Dest payment kh:   {destination_payment_key_hash}")
    print(f"  Dest stake kh:     {destination_stake_key_hash}")
    if pool_ident:
        print(f"  Pool ident:        {pool_ident}")
    if args.tx_kind == "swap":
        print(f"  Offer asset:       {args.offer}")
        print(f"  Offer amount:      {args.offer_amount}")
        print(f"  Receive asset:     {args.receive}")
        print(f"  Min received:      {args.min_received}")
    elif args.tx_kind == "deposit":
        print(f"  Asset A:           {args.asset_a}")
        print(f"  Asset A amount:    {args.asset_a_amount}")
        print(f"  Asset B:           {args.asset_b}")
        print(f"  Asset B amount:    {args.asset_b_amount}")
    elif args.tx_kind == "withdrawal":
        print(f"  LP asset:          {args.lp_asset}")
        print(f"  LP amount:         {args.lp_amount}")
    else:
        print(f"  Order UTxO:        {args.order_utxo}")
        print(f"  Script ref UTxO:   {args.order_script_ref}")
    print(f"  Order ADA:         {args.order_ada}")
    print(f"  Max protocol fee:  {args.max_protocol_fee}")
    print()

    client = Client(
        ClientOptions(
            endpoint=args.trp_url,
            headers={"dmtr-api-key": args.api_key},
        ),
        Profile.PREVIEW,
    )

    client.with_user(Party.address(args.user)).with_orderscript(
        Party.address(args.order_script)
    )

    swap_common = dict(
        owner_key_hash=owner_key_hash,
        destination_payment_key_hash=destination_payment_key_hash,
        destination_stake_key_hash=destination_stake_key_hash,
        order_ada=args.order_ada,
        max_protocol_fee=args.max_protocol_fee,
        offer_policy=offer_policy,
        offer_name=offer_name,
        offer_amount=args.offer_amount,
        min_received_policy=receive_policy,
        min_received_name=receive_name,
        min_received_amount=args.min_received,
    )

    deposit_common = dict(
        owner_key_hash=owner_key_hash,
        destination_payment_key_hash=destination_payment_key_hash,
        destination_stake_key_hash=destination_stake_key_hash,
        order_ada=args.order_ada,
        max_protocol_fee=args.max_protocol_fee,
        asset_a_policy=asset_a_policy,
        asset_a_name=asset_a_name,
        asset_a_amount=args.asset_a_amount,
        asset_b_policy=asset_b_policy,
        asset_b_name=asset_b_name,
        asset_b_amount=args.asset_b_amount,
    )

    withdrawal_common = dict(
        owner_key_hash=owner_key_hash,
        destination_payment_key_hash=destination_payment_key_hash,
        destination_stake_key_hash=destination_stake_key_hash,
        order_ada=args.order_ada,
        max_protocol_fee=args.max_protocol_fee,
        lp_policy=lp_policy,
        lp_name=lp_name,
        lp_amount=args.lp_amount,
    )

    if args.tx_kind == "swap":
        if args.variant == "default":
            builder = client.submit_swap(SubmitSwapParams(**swap_common))
        else:
            builder = client.submit_swap_for_pool(
                SubmitSwapForPoolParams(pool_ident=pool_ident, **swap_common)
            )
    elif args.tx_kind == "deposit":
        if args.variant == "default":
            builder = client.submit_deposit(SubmitDepositParams(**deposit_common))
        else:
            builder = client.submit_deposit_for_pool(
                SubmitDepositForPoolParams(pool_ident=pool_ident, **deposit_common)
            )
    elif args.tx_kind == "withdrawal":
        if args.variant == "default":
            builder = client.submit_withdrawal(
                SubmitWithdrawalParams(**withdrawal_common)
            )
        else:
            builder = client.submit_withdrawal_for_pool(
                SubmitWithdrawalForPoolParams(
                    pool_ident=pool_ident, **withdrawal_common
                )
            )
    else:
        builder = client.cancel_order(
            CancelOrderParams(
                order_utxo=args.order_utxo,
                owner_key_hash=owner_key_hash,
            )
        ).env(
            {
                "order_script_ref": args.order_script_ref,
                "min_collateral_ada": args.min_collateral_ada,
            }
        )

    try:
        resolved = await builder.resolve()
    except Exception as exc:
        print(f"resolve failed: {type(exc).__name__}: {exc}")
        return 1

    print("resolve ok")
    print(f"  tx hash:     {resolved.hash}")
    print(f"  tx hex len:  {len(resolved.tx_hex)}")
    print(f"  tx hex: {resolved.tx_hex}")

    if args.out:
        out_path = Path(args.out)
        out_path.write_text(resolved.tx_hex)
        print(f"  wrote tx hex: {out_path}")

    return 0


if __name__ == "__main__":
    raise SystemExit(asyncio.run(main()))
