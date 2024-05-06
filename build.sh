aiken() {
  /home/pi/proj/aiken/target/release/aiken $*
}

set -e

aiken build

PROTOCOL_BOOT_UTXO="d8799fd8799f5820770479c27e14d2bb378b73e7363d2cc13888d2cb33e1001038afd82b5de843fdff02ff"
aiken blueprint apply -v settings.spend $PROTOCOL_BOOT_UTXO > tmp
mv tmp plutus.json

aiken blueprint apply -v settings.mint $PROTOCOL_BOOT_UTXO > tmp
mv tmp plutus.json

SETTINGS_SCRIPT_HASH="581c$(aiken blueprint policy -v settings.mint)"
aiken blueprint apply -v pool.manage $SETTINGS_SCRIPT_HASH > tmp
mv tmp plutus.json

MANAGE_STAKE_SCRIPT_HASH="581c$(aiken blueprint policy -v pool.manage)"
aiken blueprint apply -v pool.spend $MANAGE_STAKE_SCRIPT_HASH > tmp
mv tmp plutus.json
aiken blueprint apply -v pool.spend $SETTINGS_SCRIPT_HASH > tmp
mv tmp plutus.json

aiken blueprint apply -v pool.mint $MANAGE_STAKE_SCRIPT_HASH > tmp
mv tmp plutus.json
aiken blueprint apply -v pool.mint $SETTINGS_SCRIPT_HASH > tmp
mv tmp plutus.json

POOL_SCRIPT_HASH="581c$(aiken blueprint policy -v pool.mint)"
aiken blueprint apply -v stake.stake $POOL_SCRIPT_HASH > tmp
mv tmp plutus.json

aiken blueprint apply -v pool_stake.stake $SETTINGS_SCRIPT_HASH > tmp
mv tmp plutus.json
aiken blueprint apply -v pool_Stake.stake "00" > tmp
mv tmp plutus.json

STAKE_SCRIPT_HASH="581c$(aiken blueprint policy -v stake.stake)"
aiken blueprint apply -v order.spend $STAKE_SCRIPT_HASH > tmp
mv tmp plutus.json
