module Sundae.Contracts.Others where

import PlutusTx.Prelude
import qualified PlutusTx.AssocMap as Map

import PlutusLedgerApi.V3
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2.Contexts (findOwnInput)

import Sundae.Contracts.Common
import Sundae.Utilities
import qualified Sundae.ShallowData as SD
import Sundae.ShallowData (unwrap)

{-# INLINABLE ownHashes #-}
-- | Get the validator and datum hashes of the output that is curently being validated
ownHashes :: ScriptContext -> (ScriptHash, DatumHash)
ownHashes (findOwnInput -> Just TxInInfo{txInInfoResolved=o@TxOut{txOutAddress=Address (ScriptCredential s) _ }}) =
  case txOutDatumHash o of
    Just dh -> (s,dh)
    Nothing -> traceError "Lg"

{-# INLINABLE ownHash #-}
-- | Get the hash of the validator script that is currently being validated.
ownHash :: ScriptContext -> ScriptHash
ownHash p = fst (ownHashes p)

{-# inlinable toTreasuryNft #-}
toTreasuryNft :: CurrencySymbol -> AssetClass
toTreasuryNft cs = assetClass cs treasuryToken
