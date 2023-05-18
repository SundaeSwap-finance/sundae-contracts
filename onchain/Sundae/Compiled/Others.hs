module Sundae.Compiled.Others where

import Prelude qualified
import PlutusTx.Prelude
import qualified PlutusTx

import PlutusLedgerApi.V3

import Sundae.Contracts.Common
import Sundae.Contracts.Others
import Sundae.Utilities

treasuryScript
  :: UpgradeSettings
  -> TreasuryBootCurrencySymbol
  -> SundaeCurrencySymbol
  -> PoolCurrencySymbol
  -> SerialisedScript
treasuryScript upgradeSettings tbcs tcs pcs =
  let
    x =
      pure $$(PlutusTx.compile [|| treasuryContract ||])
        >>= flip apCode upgradeSettings
        >>= flip apCode tbcs
        >>= flip apCode tcs
        >>= flip apCode pcs
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile treasury script"

deadFactoryScript
  :: FactoryBootCurrencySymbol
  -> PoolScriptHash
  -> DeadPoolScriptHash
  -> PoolCurrencySymbol
  -> SerialisedScript
deadFactoryScript fbcs psh dpsh pcs =
  let
    x = pure $$(PlutusTx.compile [|| \fbcs' psh' dpsh' pcs' datum redeemer ctx -> check $ deadFactoryContract fbcs' psh' dpsh' pcs' (PlutusTx.unsafeFromBuiltinData datum) (PlutusTx.unsafeFromBuiltinData redeemer) (PlutusTx.unsafeFromBuiltinData ctx) ||])
      >>= flip apCode fbcs
      >>= flip apCode psh
      >>= flip apCode dpsh
      >>= flip apCode pcs
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile dead factory script"

scooperFeeScript
  :: ScooperFeeSettings
  -> GiftScriptHash
  -> FactoryBootCurrencySymbol
  -> SerialisedScript
scooperFeeScript sfs gsh fbcs =
  let
    x = pure $$(PlutusTx.compile [|| \sfs' gsh' fbcs' d r c -> check $ scooperFeeContract sfs' gsh' fbcs' (PlutusTx.unsafeFromBuiltinData d) (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData c) ||])
      >>= flip apCode sfs
      >>= flip apCode gsh
      >>= flip apCode fbcs
    in
      case x of
        Just x' -> serialiseCompiledCode x'
        Nothing -> Prelude.error "Couldn't compile scooper fee script"

proposalScript
  :: UpgradeSettings
  -> SerialisedScript
proposalScript upgradeSettings =
    let
      x = pure $$(PlutusTx.compile [|| proposalContract ||])
        >>= flip apCode upgradeSettings
    in
      case x of
        Just x' -> serialiseCompiledCode x'
        Nothing -> Prelude.error "Couldn't compile proposal script"

giftScript
  :: TreasuryBootCurrencySymbol
  -> SerialisedScript
giftScript tbcs =
  let
    x = pure $$(PlutusTx.compile [|| giftContract ||])
      >>= flip apCode tbcs
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile gift script"
