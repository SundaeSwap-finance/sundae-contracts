#!/usr/bin/env python3
from __future__ import annotations

import argparse
import asyncio

from common import (  # noqa: E402
    DEFAULT_POOL_IDENT,
    DEFAULT_USDM,
    add_common_preview_args,
    make_preview_client,
    parse_asset,
    payment_and_stake_key_hashes,
    require_api_key_and_user,
    resolve_or_submit,
)
from sundae import SubmitSwapParams  # noqa: E402


async def main() -> int:
    parser = argparse.ArgumentParser(description="Resolve or submit a Sundae swap order.")
    add_common_preview_args(parser)
    parser.add_argument("--pool-ident", default=DEFAULT_POOL_IDENT)
    parser.add_argument("--offer", default=DEFAULT_USDM)
    parser.add_argument("--offer-amount", type=int, default=1_000_000)
    parser.add_argument("--receive", default="ada")
    parser.add_argument("--min-received", type=int, default=1)
    parser.add_argument("--order-ada", type=int, default=3_000_000)
    parser.add_argument("--max-protocol-fee", type=int, default=600_000)
    parser.add_argument("--owner-key-hash", default="")
    parser.add_argument("--destination-payment-key-hash", default="")
    parser.add_argument("--destination-stake-key-hash", default="")
    args = parser.parse_args()

    require_api_key_and_user(args)
    client, can_sign = make_preview_client(args)

    offer_policy, offer_name = parse_asset(args.offer)
    receive_policy, receive_name = parse_asset(args.receive)
    payment_kh, stake_kh = payment_and_stake_key_hashes(args.user)

    builder = client.submit_swap(
        SubmitSwapParams(
            pool_ident=args.pool_ident,
            owner_key_hash=args.owner_key_hash or stake_kh,
            destination_payment_key_hash=args.destination_payment_key_hash or payment_kh,
            destination_stake_key_hash=args.destination_stake_key_hash or stake_kh,
            order_ada=args.order_ada,
            max_protocol_fee=args.max_protocol_fee,
            offer_policy=offer_policy,
            offer_name=offer_name,
            offer_amount=args.offer_amount,
            min_received_policy=receive_policy,
            min_received_name=receive_name,
            min_received_amount=args.min_received,
        )
    )

    await resolve_or_submit(builder, args.submit, can_sign)
    return 0


if __name__ == "__main__":
    raise SystemExit(asyncio.run(main()))
