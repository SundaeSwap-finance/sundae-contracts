#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

trix check
trix build -p local >/dev/null
trix build -p preview >/dev/null
trix codegen -p preview >/dev/null

for tx in \
  submit_swap \
  submit_swap_for_pool \
  submit_deposit \
  submit_deposit_for_pool \
  submit_withdrawal \
  submit_withdrawal_for_pool \
  cancel_order
  do
  trix inspect tir --tx "$tx" -p preview >/dev/null
 done

trix inspect tir --tx submit_swap_for_pool -p preview | grep -q '"many":true'
trix inspect tir --tx submit_deposit_for_pool -p preview | grep -q '"many":true'
trix inspect tir --tx submit_withdrawal_for_pool -p preview | grep -q '"many":true'
trix inspect tir --tx cancel_order -p preview | grep -q 'owner_key_hash'

python - <<'PY'
import sys
from pathlib import Path
sys.path.append(str(Path('gen/python').resolve()))
from sundae import (  # noqa: F401
    CancelOrderParams,
    Client,
    Profile,
    SubmitDepositForPoolParams,
    SubmitSwapForPoolParams,
    SubmitWithdrawalForPoolParams,
)
print('ok: generated python SDK imports')
PY

python scripts/preview_resolve.py --help >/dev/null

echo "ok: Sundae Tx3 regressions passed"
