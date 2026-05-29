#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

trix check
trix build -p local >/dev/null
trix build -p preview >/dev/null
trix codegen -p preview >/dev/null

for tx in \
  submit_swap \
  submit_swap_any_pool \
  submit_deposit \
  submit_deposit_any_pool \
  submit_withdrawal \
  submit_withdrawal_any_pool \
  cancel_order
  do
  trix inspect tir --tx "$tx" -p preview >/dev/null
 done

trix inspect tir --tx submit_swap -p preview | grep -q '"many":true'
trix inspect tir --tx submit_deposit -p preview | grep -q '"many":true'
trix inspect tir --tx submit_withdrawal -p preview | grep -q '"many":true'
trix inspect tir --tx cancel_order -p preview | grep -q 'owner_key_hash'

python - <<'PY'
import sys
from pathlib import Path
sys.path.append(str(Path('gen/python').resolve()))
from sundae import (  # noqa: F401
    CancelOrderParams,
    Client,
    Profile,
    SubmitDepositAnyPoolParams,
    SubmitDepositParams,
    SubmitSwapAnyPoolParams,
    SubmitSwapParams,
    SubmitWithdrawalAnyPoolParams,
    SubmitWithdrawalParams,
)
print('ok: generated python SDK imports')
PY

python scripts/preview_resolve.py --help >/dev/null

echo "ok: Sundae Tx3 regressions passed"
