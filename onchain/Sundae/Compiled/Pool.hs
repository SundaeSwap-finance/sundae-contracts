module Sundae.Compiled.Pool where

import Prelude qualified
import PlutusTx.Prelude
import qualified PlutusTx

import PlutusLedgerApi.V3

import Sundae.Contracts.Common
import Sundae.Contracts.Pool
import Sundae.Utilities

poolScript
  :: FactoryBootCurrencySymbol
  -> PoolCurrencySymbol
  -> EscrowScriptHash
  -> SerialisedScript
poolScript fbcs pcs esh =
  let
    x =
      pure $$(PlutusTx.compile [|| \fbcs' pcs' esh' datum redeemer ctx ->
        check $ poolContract fbcs' pcs' esh' (PlutusTx.unsafeFromBuiltinData datum) (PlutusTx.unsafeFromBuiltinData redeemer) (PlutusTx.unsafeFromBuiltinData ctx) ||])
        >>= flip apCode fbcs
        >>= flip apCode pcs
        >>= flip apCode esh
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile pool script"

escrowScript
  :: PoolCurrencySymbol
  -> SerialisedScript
escrowScript pcs =
  let
    x =
      pure $$(PlutusTx.compile [|| \pcs' d r p -> check $ escrowContract pcs' (PlutusTx.unsafeFromBuiltinData d) (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData p) ||])
        >>= flip apCode pcs
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile escrow script"
