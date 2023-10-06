module Sundae.Compiled.Factory where

import Prelude qualified
import PlutusTx.Prelude
import qualified PlutusTx

import PlutusLedgerApi.V2

import Sundae.Contracts.Common
import Sundae.Contracts.Factory
import Sundae.Utilities

factoryScript
  :: FactoryBootCurrencySymbol
  -> SerialisedScript
factoryScript fbcs =
  let
    x =
      pure $$(PlutusTx.compile [|| factoryContract ||])
        >>= flip apCode fbcs
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile factory script"
