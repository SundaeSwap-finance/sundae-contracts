aiken() {
  /run/current-system/sw/bin/aiken $*
}

set -e

aiken build

PROTOCOL_BOOT_UTXO="d8799fd8799f5820b1bde7a1acab047ebbdc71cdfcb53ab3dbecc6b0dbe7c4732227d9709efa190eff02ff"
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
