AIKEN=$1

aiken() {
  ${AIKEN} $*
}

set -e

echo "Software versions:"
echo "  Git commit       = $(git rev-parse HEAD)"
echo "  Aiken Version    = $(aiken --version)"


echo
echo "File hashes:"
SHA256=$(cat validators/oracle.ak | sha256sum | cut -f 1 -d ' ')
echo "  validators/oracle.ak              = ${SHA256}"
SHA256=$(cat validators/order.ak | sha256sum | cut -f 1 -d ' ')
echo "  validators/order.ak               = ${SHA256}"
SHA256=$(cat validators/pool_stake.ak | sha256sum | cut -f 1 -d ' ')
echo "  validators/pool_stake.ak          = ${SHA256}"
SHA256=$(cat validators/pool.ak | sha256sum | cut -f 1 -d ' ')
echo "  validators/pool.ak                = ${SHA256}"
SHA256=$(cat validators/settings.ak | sha256sum | cut -f 1 -d ' ')
echo "  validators/settings.ak            = ${SHA256}"
SHA256=$(cat validators/stake.ak | sha256sum | cut -f 1 -d ' ')
echo "  validators/stake.ak               = ${SHA256}"
echo
SHA256=$(cat lib/shared.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/shared.ak                     = ${SHA256}"
SHA256=$(cat lib/types/oracle.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/types/oracle.ak               = ${SHA256}"
SHA256=$(cat lib/types/order.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/types/order.ak                = ${SHA256}"
SHA256=$(cat lib/types/pool.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/types/pool.ak                 = ${SHA256}"
SHA256=$(cat lib/types/settings.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/types/settings.ak             = ${SHA256}"
SHA256=$(cat lib/calculation/deposit.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/calculation/deposit.ak        = ${SHA256}"
SHA256=$(cat lib/calculation/donation.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/calculation/donation.ak       = ${SHA256}"
SHA256=$(cat lib/calculation/process.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/calculation/process.ak        = ${SHA256}"
SHA256=$(cat lib/calculation/record.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/calculation/record.ak         = ${SHA256}"
SHA256=$(cat lib/calculation/shared.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/calculation/shared.ak         = ${SHA256}"
SHA256=$(cat lib/calculation/strategy.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/calculation/strategy.ak       = ${SHA256}"
SHA256=$(cat lib/calculation/swap.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/calculation/swap.ak           = ${SHA256}"
SHA256=$(cat lib/calculation/withdrawal.ak | sha256sum | cut -f 1 -d ' ')
echo "  lib/calculation/withdrawal.ak     = ${SHA256}"

aiken build &> /dev/null

PROTOCOL_BOOT_TX="fad11baadca1e52bf34599746fb0152d9d10b31c2591b79deab34536a7998ea0"
PROTOCOL_BOOT_IX="01"
PROTOCOL_BOOT_UTXO="d8799fd8799f5820${PROTOCOL_BOOT_TX}ff${PROTOCOL_BOOT_IX}ff"

aiken blueprint apply -v settings.spend $PROTOCOL_BOOT_UTXO 2> /dev/null > tmp
mv tmp plutus.json

aiken blueprint apply -v settings.mint $PROTOCOL_BOOT_UTXO 2> /dev/null > tmp
mv tmp plutus.json

SETTINGS_SCRIPT_HASH="$(aiken blueprint policy -v settings.mint 2> /dev/null)"
aiken blueprint apply -v pool.manage "581c${SETTINGS_SCRIPT_HASH}" 2> /dev/null > tmp
mv tmp plutus.json

MANAGE_STAKE_SCRIPT_HASH="$(aiken blueprint policy -v pool.manage 2> /dev/null)"
aiken blueprint apply -v pool.spend "581c${MANAGE_STAKE_SCRIPT_HASH}" 2> /dev/null > tmp
mv tmp plutus.json
aiken blueprint apply -v pool.spend "581c${SETTINGS_SCRIPT_HASH}" 2> /dev/null > tmp
mv tmp plutus.json

aiken blueprint apply -v pool.mint "581c${MANAGE_STAKE_SCRIPT_HASH}" 2> /dev/null > tmp
mv tmp plutus.json
aiken blueprint apply -v pool.mint "581c${SETTINGS_SCRIPT_HASH}" 2> /dev/null > tmp
mv tmp plutus.json

POOL_SCRIPT_HASH="$(aiken blueprint policy -v pool.mint 2> /dev/null)"
aiken blueprint apply -v stake.stake "581c${POOL_SCRIPT_HASH}" 2> /dev/null > tmp
mv tmp plutus.json

aiken blueprint apply -v pool_stake.stake "581c${SETTINGS_SCRIPT_HASH}" 2> /dev/null > tmp
mv tmp plutus.json
aiken blueprint apply -v pool_stake.stake "00" 2> /dev/null > tmp
mv tmp plutus.json

aiken blueprint apply -v oracle.spend "581c${POOL_SCRIPT_HASH}" 2> /dev/null > tmp
mv tmp plutus.json
aiken blueprint apply -v oracle.mint "581c${POOL_SCRIPT_HASH}" 2> /dev/null > tmp
mv tmp plutus.json

STAKE_SCRIPT_HASH="$(aiken blueprint policy -v stake.stake 2> /dev/null)"
aiken blueprint apply -v order.spend "581c${STAKE_SCRIPT_HASH}" 2> /dev/null > tmp
mv tmp plutus.json

ORACLE_SCRIPT_HASH="$(aiken blueprint policy -v oracle.mint 2> /dev/null)"
POOL_STAKE_SCRIPT_HASH="$(aiken blueprint policy -v pool_stake.stake 2> /dev/null)"
ORDER_SCRIPT_HASH="$(aiken blueprint hash -v order.spend 2> /dev/null)"

echo
echo "Parameters:"
echo -e "  PROTOCOL_BOOT_UTXO                = \e[32m ${PROTOCOL_BOOT_TX}#${PROTOCOL_BOOT_IX} \e[0m"

echo
echo "Script Hashes:"
echo -e "  Settings Script Hash / Policy     = \e[32m ${SETTINGS_SCRIPT_HASH} \e[0m"
echo -e "  Pool Script Hash / Policy         = \e[32m ${POOL_SCRIPT_HASH} \e[0m"
echo -e "  Pool Stake Script Hash            = \e[32m ${POOL_STAKE_SCRIPT_HASH} \e[0m"
echo -e "  Manage Stake Script Hash          = \e[32m ${MANAGE_STAKE_SCRIPT_HASH} \e[0m"
echo -e "  Treasury Stake Script Hash        = \e[32m ${STAKE_SCRIPT_HASH} \e[0m"
echo -e "  Order Script Hash                 = \e[32m ${ORDER_SCRIPT_HASH} \e[0m"
echo -e "  Oracle Script Hash                = \e[32m ${ORACLE_SCRIPT_HASH} \e[0m"

echo
echo
