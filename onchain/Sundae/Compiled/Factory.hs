module Sundae.Compiled.Factory where

import Prelude qualified
import PlutusTx.Prelude
import qualified PlutusTx

import PlutusLedgerApi.V3

import Sundae.Contracts.Common
import Sundae.Contracts.Factory
import Sundae.Utilities

factoryScript
  :: UpgradeSettings
  -> FactoryBootCurrencySymbol
  -> PoolScriptHash
  -> PoolCurrencySymbol
  -> SerialisedScript
factoryScript settings fbcs psh pcs =
  let
    x =
      pure $$(PlutusTx.compile [|| factoryContract ||])
        >>= flip apCode settings
        >>= flip apCode fbcs
        >>= flip apCode psh
        >>= flip apCode pcs
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile factory script"
