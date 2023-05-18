module Sundae.Compiled.Factory where

{-
import qualified PlutusTx
import Ledger.Typed.Scripts (DatumType, RedeemerType, TypedValidator)
import qualified Ledger.Typed.Scripts as Scripts

import Sundae.Contracts.Common
import Sundae.Contracts.Factory
import Sundae.Utilities

factoryScript
  :: UpgradeSettings
  -> FactoryBootCurrencySymbol
  -> DeadFactoryScriptHash
  -> PoolScriptHash
  -> PoolCurrencySymbol
  -> TypedValidator Factory
factoryScript settings fbcs propSh psh pcs =
  Scripts.mkTypedValidator @Factory
    ($$(PlutusTx.compile [|| factoryContract ||])
      `apCode` settings
      `apCode` fbcs
      `apCode` propSh
      `apCode` psh
      `apCode` pcs)
    $$(PlutusTx.compile [|| wrap ||])
  where
  wrap = Scripts.mkUntypedValidator @(DatumType Factory) @(RedeemerType Factory)
-}
