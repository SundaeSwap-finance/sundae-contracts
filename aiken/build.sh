aiken build --keep-traces

POOL_SCRIPT_HASH="581c$(aiken blueprint policy -v pool.mint)"
aiken blueprint apply -v stake.stake $POOL_SCRIPT_HASH > tmp
mv tmp plutus.json

STAKE_SCRIPT_HASH="581c$(aiken blueprint policy -v stake.stake)"
aiken blueprint apply -v order.spend $STAKE_SCRIPT_HASH > tmp
mv tmp plutus.json
