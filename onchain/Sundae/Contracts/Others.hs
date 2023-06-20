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

-- Treasury Contract
--  Locks the "Treasury NFT", and any funds owned collectively by the DAO
--  Parameterized by:
--    TreasuryBootCurrencySymbol, to make the protocol unique
--    The Sundae token currency symbol
--  Uses Datum:
--    The amount of issued sundae (may be deleted)
--  Allows (via Redeemer):
--    - Upgrading to a new treasury
--  Notes:
--    The treasury contract lets us have a stable policy ID for Sundae, while
--    varying the way it can be minted, via governance.
{-# inlinable treasuryContract #-}
treasuryContract :: UpgradeSettings -> TreasuryBootCurrencySymbol -> SundaeCurrencySymbol -> PoolCurrencySymbol -> TreasuryDatum -> TreasuryRedeemer -> ScriptContext -> Bool
treasuryContract
  UpgradeSettings{..} (TreasuryBootCurrencySymbol treasuryBoot) (SundaeCurrencySymbol treasury) (PoolCurrencySymbol _) datum redeemer
  ctx@ScriptContext {scriptContextTxInfo = txInfo@TxInfo {..} } =
    debug "treasury requires limited valid range size"
      (validRangeSize txInfoValidRange <= hourMillis) &&
    debug "didn't get treasury token in input"
      (valueOf (txOutValue $ txInInfoResolved i) treasuryBoot treasuryToken == 1) &&
    debug "attempted to mint with treasury token"
      (null $ getValue txInfoMint) &&
    case redeemer of
      MakeTreasuryProposal
        | let
            !ownOutput = uniqueElement' $ getContinuingOutputs ctx
            !proposalInput = uniqueElement' [ txIn | txIn <- txInfoInputs, assetClassValueContains (txOutValue $ txInInfoResolved txIn) upgradeAuthentication ]
            Just (UpgradeScripts (proposal@ScriptUpgradeProposal {..})) = datumOf txInfo (txInInfoResolved proposalInput)
        ->
        debug "proposal applies to wrong treasury"
          (ownHash ctx == proposedOldTreasury) &&
        debug "upgrade already in progress"
          (treasuryProposalState datum == NoProposal) &&
        debug "incorrect datum while making treasury upgrade proposal"
          (isDatumUnsafe txInfo ownOutput (datum { treasuryProposalState = PendingProposal latest proposal })) &&
        debug "value taken from treasury output when making upgrade proposal"
          (txOutValue ownOutput == txOutValue (txInInfoResolved i))
      UpgradeTreasury
        -> case treasuryProposalState datum of
          PendingProposal proposalStart ScriptUpgradeProposal {..}
            | POSIXTime gap <- latest - proposalStart
            , let !newTreasuryOut = uniqueElement' [ o | o <- txInfoOutputs, txOutAddress o == scriptHashAddress proposedNewTreasury ]
            ->
            gap >= upgradeTimeLockPeriod &&
            debug "didn't pay all treasury assets to new treasury script"
              (txOutValue newTreasuryOut == txOutValue (txInInfoResolved i)) &&
            debug "datum altered during treasury upgrade"
              (isDatumUnsafe txInfo newTreasuryOut (datum { treasuryProposalState = NoProposal })) &&
            debug "output back to the treasury during treasury upgrade"
              (null (getContinuingOutputs ctx))
          _ -> error ()
      SpendIntoTreasury
        | let !ownOutput = uniqueElement' $ getContinuingOutputs ctx
        ->
        debug "unfamiliar tokens added or treasury NFT removed during spend into treasury"
          (all' correctTokens $ flattenValue (txOutValue ownOutput)) &&
        debug "datum changed when spending into treasury"
          (txOutDatumHash ownOutput == txOutDatumHash (txInInfoResolved i)) &&
        debug "tokens removed, or not added, when spending into treasury"
          (txOutValue ownOutput `valueGT` txOutValue (txInInfoResolved i))
  where
  knownAssetClasses =
    [ assetClass adaSymbol adaToken
    , assetClass treasury sundaeToken
    , assetClass treasuryBoot treasuryToken
    ]
  correctTokens (cs,tk,_) = elem (assetClass cs tk) knownAssetClasses
  Just i = findOwnInput ctx
  UpperBound (Finite latest) _ = ivTo txInfoValidRange

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

-- The proposal contract holds an upgrade authorization token as well as the
-- accepted upgrade proposal that token authorizes; as such, its only constraints
-- are that its token isn't stolen, and its datum is not changed.
{-# inlinable proposalContract #-}
proposalContract
  :: UpgradeSettings
  -> UpgradeProposal
  -> ()
  -> ScriptContext
  -> Bool
proposalContract UpgradeSettings{..} up () ctx =
  debug "upgrade authentication token stolen"
    (assetClassValueContains (txOutValue ownOutput) upgradeAuthentication) &&
  debug "multiple tokens input from self"
    (assetClassValueOf ownInputValue upgradeAuthentication == 1) &&
  debug "upgrade proposal datum changed"
    (datumOf txInfo ownOutput == Just up)
  where
  !ownOutput = uniqueElement' (getContinuingOutputs ctx)
  txInfo = scriptContextTxInfo ctx
  !(Just !ownInput) = findOwnInput ctx
  !ownInputValue = fold
    [ txOutValue (txInInfoResolved i)
    | i <- txInfoInputs txInfo
    , txOutAddress (txInInfoResolved i) == txOutAddress (txInInfoResolved ownInput)
    ]
