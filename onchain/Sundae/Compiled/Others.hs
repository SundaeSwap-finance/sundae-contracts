module Sundae.Compiled.Others where

import Prelude qualified
import PlutusTx.Prelude
import qualified PlutusTx

import PlutusLedgerApi.V3

import Sundae.Contracts.Common
import Sundae.Contracts.Others
import Sundae.Utilities

scooperFeeScript
  :: ScooperFeeSettings
  -> FactoryBootCurrencySymbol
  -> SerialisedScript
scooperFeeScript sfs fbcs =
  let
    x = pure $$(PlutusTx.compile [|| \sfs' fbcs' d r c -> check $ scooperFeeContract sfs' fbcs' (PlutusTx.unsafeFromBuiltinData d) (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData c) ||])
      >>= flip apCode sfs
      >>= flip apCode fbcs
    in
      case x of
        Just x' -> serialiseCompiledCode x'
        Nothing -> Prelude.error "Couldn't compile scooper fee script"
