module Sundae.Compiled.Pool where

import Prelude qualified
import PlutusTx.Prelude
import qualified PlutusTx

import PlutusLedgerApi.V2

import Sundae.Contracts.Common
import Sundae.Contracts.Pool
import Sundae.Utilities

poolScript
  :: FactoryBootCurrencySymbol
  -> EscrowScriptHash
  -> SerialisedScript
poolScript fbcs esh =
  let
    x =
      pure $$(PlutusTx.compile [|| \fbcs' esh' datum redeemer ctx ->
        check $ poolContract fbcs' esh'
          (PlutusTx.unsafeFromBuiltinData datum)
          (PlutusTx.unsafeFromBuiltinData redeemer)
          (PlutusTx.unsafeFromBuiltinData ctx) ||])
        >>= flip apCode fbcs
        >>= flip apCode esh
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile pool script"

escrowScript
  :: SteakScriptHash
  -> SerialisedScript
escrowScript ssh =
  let
    x =
      pure $$(PlutusTx.compile [|| \ssh' d r ctx -> check $ escrowContract ssh' d r ctx ||])
        >>= flip apCode ssh
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile escrow script"

steakScript
  :: PoolCurrencySymbol
  -> SerialisedScript
steakScript pcs =
  let
    x =
      pure $$(PlutusTx.compile [|| \pcs' r ctx -> check $ steakContract pcs' r ctx ||])
        >>= flip apCode pcs
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile steak script"
