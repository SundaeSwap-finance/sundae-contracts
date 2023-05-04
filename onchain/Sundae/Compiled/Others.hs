module Sundae.Compiled.Others where

import PlutusTx.Prelude
import Data.Coerce
import qualified PlutusTx
import Ledger
import Ledger.Typed.Scripts (DatumType, RedeemerType, TypedValidator)
import qualified Ledger.Typed.Scripts as Scripts

import Sundae.Contracts.Common
import Sundae.Contracts.Others
import Sundae.Utilities

treasuryScript
  :: UpgradeSettings
  -> TreasuryBootCurrencySymbol
  -> SundaeCurrencySymbol
  -> PoolCurrencySymbol
  -> TypedValidator Treasury
treasuryScript upgradeSettings tbcs tcs pcs =
  Scripts.mkTypedValidator @Treasury
    ($$(PlutusTx.compile [|| treasuryContract ||])
      `apCode` upgradeSettings
      `apCode` tbcs
      `apCode` tcs
      `apCode` pcs)
    $$(PlutusTx.compile [|| wrap ||])
  where
  wrap = Scripts.mkUntypedValidator @(DatumType Treasury) @(RedeemerType Treasury)

deadFactoryScript
  :: FactoryBootCurrencySymbol
  -> PoolScriptHash
  -> DeadPoolScriptHash
  -> PoolCurrencySymbol
  -> TypedValidator DeadFactory
deadFactoryScript fbcs psh dpsh pcs =
  coerce $ Scripts.unsafeMkTypedValidator $ mkValidatorScript $
    ($$(PlutusTx.compile [|| \fbcs' psh' dpsh' pcs' datum redeemer ctx -> check $ deadFactoryContract fbcs' psh' dpsh' pcs' (PlutusTx.unsafeFromBuiltinData datum) (PlutusTx.unsafeFromBuiltinData redeemer) (PlutusTx.unsafeFromBuiltinData ctx) ||])
      `apCode` fbcs
      `apCode` psh
      `apCode` dpsh
      `apCode` pcs)
  where
  wrap = Scripts.mkUntypedValidator @(DatumType DeadFactory) @(RedeemerType DeadFactory)

scooperFeeScript
  :: ScooperFeeSettings
  -> GiftScriptHash
  -> FactoryBootCurrencySymbol
  -> TypedValidator ScooperFeeHolder
scooperFeeScript sfs gsh fbcs =
  coerce $ Scripts.unsafeMkTypedValidator $ mkValidatorScript
    ($$(PlutusTx.compile [|| \sfs' gsh' fbcs' d r c -> check $ scooperFeeContract sfs' gsh' fbcs' (PlutusTx.unsafeFromBuiltinData d) (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData c) ||])
      `apCode` sfs
      `apCode` gsh
      `apCode` fbcs)

proposalScript
  :: UpgradeSettings
  -> TypedValidator Proposal
proposalScript upgradeSettings =
  Scripts.mkTypedValidator @Proposal
    ($$(PlutusTx.compile [|| proposalContract ||])
      `apCode` upgradeSettings)
    $$(PlutusTx.compile [|| wrap ||])
  where
  wrap = Scripts.mkUntypedValidator @(DatumType Proposal) @(RedeemerType Proposal)

giftScript
  :: TreasuryBootCurrencySymbol
  -> TypedValidator Gift
giftScript tbcs =
  Scripts.mkTypedValidator @Gift
    ($$(PlutusTx.compile [|| giftContract ||])
      `apCode` tbcs)
    $$(PlutusTx.compile [|| wrap ||])
  where
  wrap = Scripts.mkUntypedValidator @(DatumType Gift) @(RedeemerType Gift)
