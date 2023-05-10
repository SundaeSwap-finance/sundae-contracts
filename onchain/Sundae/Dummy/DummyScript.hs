module Sundae.Dummy.DummyScript where

import Prelude as Prelude
import qualified PlutusTx
import Plutus.V1.Ledger.Api
import Ledger.Typed.Scripts (DatumType, RedeemerType, TypedValidator)

import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as Scripts

data Dummy

newtype DummyScriptHash = DummyScriptHash ValidatorHash
  deriving stock Prelude.Show

instance Scripts.ValidatorTypes Dummy where
  type instance DatumType Dummy = ()
  type instance RedeemerType Dummy = ()

{-# inlinable dummyScript #-}
dummyContract
  :: ()
  -> ()
  -> ScriptContext
  -> Bool
dummyContract _ _ _ = True

dummyScript :: TypedValidator Dummy
dummyScript =
  Scripts.mkTypedValidator @Dummy
    ($$(PlutusTx.compile [|| dummyContract ||]))
    $$(PlutusTx.compile [|| wrap ||])
  where
  wrap :: (() -> () -> ScriptContext -> Bool) -> Scripts.UntypedValidator
  wrap = Scripts.mkUntypedValidator @ScriptContext @(DatumType Dummy) @(RedeemerType Dummy)
