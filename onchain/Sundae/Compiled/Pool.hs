module Sundae.Compiled.Pool where

import PlutusTx.Prelude
import Data.Coerce
import qualified PlutusTx
import Ledger
import Ledger.Typed.Scripts (DatumType, RedeemerType, TypedValidator)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Scripts as UntypedScripts

import Sundae.Contracts.Common
import Sundae.Contracts.Pool
import Sundae.Utilities

poolScript
  :: FactoryBootCurrencySymbol
  -> PoolCurrencySymbol
  -> ScooperFeeHolderScriptHash
  -> EscrowScriptHash
  -> TypedValidator Pool
poolScript fbcs pcs slsh esh =
  coerce $ Scripts.unsafeMkTypedValidator $ mkValidatorScript
    ($$(PlutusTx.compile [|| \fbcs' pcs' slsh' esh' datum redeemer ctx ->
      check $ poolContract fbcs' pcs' slsh' esh' (PlutusTx.unsafeFromBuiltinData datum) (PlutusTx.unsafeFromBuiltinData redeemer) (PlutusTx.unsafeFromBuiltinData ctx) ||])
      `apCode` fbcs
      `apCode` pcs
      `apCode` slsh
      `apCode` esh)

escrowScript
  :: PoolCurrencySymbol
  -> TypedValidator Escrow
escrowScript pcs = coerce $ Scripts.unsafeMkTypedValidator $ UntypedScripts.mkValidatorScript $
  $$(PlutusTx.compile
    [|| \pcs' d r p -> check $ escrowContract pcs' (PlutusTx.unsafeFromBuiltinData d) (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData p) ||]) `PlutusTx.applyCode` PlutusTx.liftCode pcs

deadPoolScript
  :: PoolCurrencySymbol
  -> EscrowScriptHash
  -> TypedValidator DeadPool
deadPoolScript pcs esh =
  Scripts.mkTypedValidator @DeadPool
    ($$(PlutusTx.compile [|| deadPoolContract ||])
      `apCode` pcs
      `apCode` esh)
    $$(PlutusTx.compile [|| wrap ||])
  where
  wrap = Scripts.mkUntypedValidator @(DatumType DeadPool) @(RedeemerType DeadPool)
