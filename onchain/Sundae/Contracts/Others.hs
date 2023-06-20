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

-- Scooper Fee Contract
--  Scoopers get to collect any surplus ada from a scoop; however, we timelock it so that governance has a chance to revoke.
--  Parameterized by:
--    ScooperFeeSettings, including the number of weeks to lock up the rewards
--    FactoryBootCurrencySymbol, to ensure we check that you haven't been revoked
--  Uses Datum:
--    ScooperFeeDatum, which describes the scooper entitled to it, and the week of the license that was used to earn it
--  Allows (via Redeemer):
--    - Claiming the rewards, after the timelock
--  Details:
--  The factory is also included as an input, to ensure that the scooper is
--  still legal.
{-# inlinable scooperFeeContract #-}
scooperFeeContract
  :: ScooperFeeSettings
  -> FactoryBootCurrencySymbol
  -> ScooperFeeDatum
  -> ScooperFeeRedeemer
  -> ScriptContext
  -> Bool
scooperFeeContract
  ScooperFeeSettings {..}
  (FactoryBootCurrencySymbol fbcs)
  ScooperFeeDatum{..}
  ScooperCollectScooperFees
  ctx =
  debug "owner licensee has not signed the transaction"
    (atLeastOne (== scooperLicensee) txInfoSignatories) &&
  debug "valid range too large"
    (validRangeSize txInfoValidRange <= fourDaysMillis) &&
  debug "scooper is not licensed anymore"
    (elem scooperLicensee legalScoopers)
  where
  txInfo@TxInfo{..} = scriptContextTxInfo ctx
  UpperBound (Finite !latest) _ = ivTo txInfoValidRange
  legalScoopers = uniqueElement'
    [ scooperSet dat
    | txIn <- txInfoInputs
    , valueContains (txOutValue $ txInInfoResolved txIn) fbcs factoryToken
    , let Just dat = datumOf txInfo (txInInfoResolved txIn)
    ]
