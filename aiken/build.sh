aiken() {
  ./result/bin/aiken $*
}

set -e

aiken build --keep-traces

PROTOCOL_BOOT_UTXO="d8799fd8799f58200000000000000000000000000000000000000000000000000000000000000000ff00ff"
aiken blueprint apply -v settings.spend $PROTOCOL_BOOT_UTXO > tmp
mv tmp plutus.json

aiken blueprint apply -v settings.mint $PROTOCOL_BOOT_UTXO > tmp
mv tmp plutus.json

SETTINGS_SCRIPT_HASH="581c$(aiken blueprint policy -v settings.mint)"
aiken blueprint apply -v pool.spend $SETTINGS_SCRIPT_HASH > tmp
mv tmp plutus.json

aiken blueprint apply -v pool.mint $SETTINGS_SCRIPT_HASH > tmp
mv tmp plutus.json

POOL_SCRIPT_HASH="581c$(aiken blueprint policy -v pool.mint)"
aiken blueprint apply -v stake.stake $POOL_SCRIPT_HASH > tmp
mv tmp plutus.json

STAKE_SCRIPT_HASH="581c$(aiken blueprint policy -v stake.stake)"
aiken blueprint apply -v order.spend $STAKE_SCRIPT_HASH > tmp
mv tmp plutus.json
