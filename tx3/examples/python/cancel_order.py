#!/usr/bin/env python3
from __future__ import annotations

import argparse
import asyncio

from common import (  # noqa: E402
    CancelOrderParams,
    DEFAULT_PREVIEW_ORDER_SCRIPT_REF,
    add_common_preview_args,
    make_preview_client,
    payment_and_stake_key_hashes,
    require_api_key_and_user,
    resolve_or_submit,
)


async def main() -> int:
    parser = argparse.ArgumentParser(
        description="Resolve or submit a Sundae cancel_order transaction."
    )
    add_common_preview_args(parser)
    parser.add_argument("--order-utxo", required=True)
    parser.add_argument("--owner-key-hash", default="")
    parser.add_argument(
        "--order-script-ref",
        default=DEFAULT_PREVIEW_ORDER_SCRIPT_REF,
        help="Reference script UTxO for the order script.",
    )
    parser.add_argument(
        "--min-collateral-ada",
        type=int,
        default=5_000_000,
        help="Collateral floor override used during resolve.",
    )
    args = parser.parse_args()

    require_api_key_and_user(args)
    client, can_sign = make_preview_client(args)
    _, stake_kh = payment_and_stake_key_hashes(args.user)

    builder = client.cancel_order(
        CancelOrderParams(
            order_utxo=args.order_utxo,
            owner_key_hash=args.owner_key_hash or stake_kh,
        )
    ).env(
        {
            "order_script_ref": args.order_script_ref,
            "min_collateral_ada": args.min_collateral_ada,
        }
    )

    await resolve_or_submit(builder, args.submit, can_sign, args.out)
    return 0


if __name__ == "__main__":
    raise SystemExit(asyncio.run(main()))
