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

{-# inlinable giftContract #-}
giftContract
  :: TreasuryBootCurrencySymbol
  -> ()
  -> ()
  -> ScriptContext
  -> Bool
giftContract
  (TreasuryBootCurrencySymbol tbcs)
  _
  _
  ctx =
  ((treasuryOutValue + ownOutputValue) `geq` (treasuryInValue + ownInputValue)) &&
  treasuryOutValue `geq` treasuryInValue
  where
  !ownAddress = txOutAddress (txInInfoResolved ownInput)
  -- we're careful here to grab the value from *all* gift inputs. If we
  -- didn't, spending multiple gifts allows stealing some.
  ownInputValue = fold
    [ txOutValue (txInInfoResolved i)
    | i <- txInfoInputs
    , txOutAddress (txInInfoResolved i) == ownAddress
    ]
  !ownOutputValue = atMostOne
    [ txOutValue o
    | o <- txInfoOutputs
    , txOutAddress o == ownAddress
    ]
  atMostOne xs = case xs of
    [] -> mempty
    [x] -> x
    _ -> error ()
  !(Just !ownInput) = findOwnInput ctx
  TxInfo{..} = scriptContextTxInfo ctx
  !treasuryInValue = uniqueElement'
    [ txOutValue (txInInfoResolved i)
    | i <- txInfoInputs
    , valueContains (txOutValue (txInInfoResolved i)) tbcs treasuryToken
    ]
  !treasuryOutValue = uniqueElement'
    [ txOutValue o
    | o <- txInfoOutputs
    , valueContains (txOutValue o) tbcs treasuryToken
    ]

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
  -> GiftScriptHash
  -> FactoryBootCurrencySymbol
  -> ScooperFeeDatum
  -> ScooperFeeRedeemer
  -> ScriptContext
  -> Bool
scooperFeeContract
  ScooperFeeSettings {..}
  (GiftScriptHash _)
  (FactoryBootCurrencySymbol fbcs)
  ScooperFeeDatum{..}
  ScooperCollectScooperFees
  ctx =
  debug "owner licensee has not signed the transaction"
    (atLeastOne (== scooperLicensee) txInfoSignatories) &&
  debug "valid range too large"
    (validRangeSize txInfoValidRange <= fourDaysMillis) &&
  debug "scooper reward being redeemed too early"
    (getWeek (toWeek latest) - identToInt scooperLicenseIssued >= scooperRewardRedeemDelayWeeks) &&
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
scooperFeeContract
  ScooperFeeSettings {..}
  (GiftScriptHash gsh)
  _
  ScooperFeeDatum{..}
  SpendScooperFeesIntoTreasury
  ctx =
  debug "gift output doesn't have all the fees"
    (txOutValue giftOutput `geq` ownInputValue) &&
  debug "valid range too large"
    (validRangeSize txInfoValidRange <= fourDaysMillis) &&
  debug "scooper reward being spent into treasury too early"
    (getWeek (toWeek latest) - identToInt scooperLicenseIssued >= scooperRewardRedeemDelayWeeks * 3) &&
  -- though this datum isn't used by the gift script, bloating the datum is annoying.
  debug "gift output datum incorrect"
    (isDatumUnsafe txInfo giftOutput ())
  where
  !(UpperBound (Finite !latest) _) = ivTo txInfoValidRange
  !(Just !ownInput) = findOwnInput ctx
  !ownInputValue = fold
    [ txOutValue (txInInfoResolved i)
    | i <- txInfoInputs
    , txOutAddress (txInInfoResolved i) == txOutAddress (txInInfoResolved ownInput)
    ]
  txInfo@TxInfo{..} = scriptContextTxInfo ctx
  !giftOutput = uniqueElement'
    [ o
    | o <- txInfoOutputs
    , txOutAddress o == scriptHashAddress gsh
    ]

-- Dead Factory Contract
--   Contract locking the factory NFT after it's upgraded, and brokering the upgrade of each liquidity pool.
-- Parameterized by:
--   FactoryBootCurrencySymbol, to ensure the proposal token stays locked with the correct datum
--   DeadPoolScriptHash, the script that will allow users to upgrade to the new LP token
--   PoolCurrencySymbol, to identify the old pool token
-- Uses Datum:
--   an upgrade proposal, which describes the new script hashes for everything
-- Allows (via Redeemer):
--   - Spending the proposal token and a liquidity pool, to create a new pool, and a "dead pool" to let users exchange LP tokens
-- Details:
--   This functionality used to be part of the factory contract, but was split out for the sake of a smaller script size.
--   Upgrading pools would *particularly* benefit from Plutus V2 exposing redeemers in TxInfo.
{-# inlinable deadFactoryContract #-}
deadFactoryContract
  :: FactoryBootCurrencySymbol
  -> PoolScriptHash
  -> DeadPoolScriptHash
  -> PoolCurrencySymbol
  -> DeadFactoryDatum
  -> ProposalRedeemer
  -> SD.ScriptContext
  -> Bool
deadFactoryContract
  (FactoryBootCurrencySymbol _)
  (PoolScriptHash psh)
  (DeadPoolScriptHash dpsh)
  (PoolCurrencySymbol pcs)
  (DeadFactoryDatum p)
  (UpgradePool poolIdent)
  (unwrap -> ctx) =
    debug "CoinA value not preserved"
      (valueOfAC newPoolValue coinA == valueOfAC oldPoolValue coinA) &&
    debug "CoinB value not preserved"
      (valueOfAC newPoolValue coinB == valueOfAC oldPoolValue coinB) &&
    debug "New Pool nft not paid back out or has too many coins"
      (valueSizeLimited 3 newPoolValue && valueContains newPoolValue newPoolCs (computePoolTokenName poolIdent)) &&
    debug "Dead pool datum is not right"
      (shallowDatumOf deadPoolOutput == toBuiltinData expectedDeadPoolDatum) &&
    debug "Factory token not spent back into dead factory contract"
      (unwrap (SD.txOutValue ownOutput) == unwrap (SD.txOutValue ownInput)) &&
    debug "Proposal changed"
      (SD.txOutDatumHash ownOutput == SD.txOutDatumHash ownInput) &&
    debug "Mint /= New pool token + new liquidity"
      (unwrap (SD.txInfoMint txInfo) == Value
        (Map.singleton (proposedNewPoolMint p) (Map.fromList
          [ (computePoolTokenName poolIdent, 1)
          , (computeLiquidityTokenName poolIdent, newLiquidityTokens)
          ]
        ))
      )
  where
  !txInfo = unwrap $ SD.scriptContextTxInfo ctx
  shallowDatumOf o = uniqueElement' [ d | (unwrap -> SD.DeferredPair (unwrap -> h) (unwrap -> Datum d)) <- SD.txInfoData txInfo, Just h == SD.txOutDatumHash o ]
  SD.Spending ownRef = unwrap $ SD.scriptContextPurpose ctx
  !outputs' = unwrap <$> SD.txInfoOutputs txInfo
  !inputs = unwrap <$> SD.txInfoInputs txInfo
  !ownInput = unwrap $ SD.txInInfoResolved $ uniqueElement' [ i | i <- inputs, SD.txInInfoOutRef i == ownRef ]
  !ownOutput = uniqueElement' [ o | o <- outputs', SD.txOutAddress o == SD.txOutAddress ownInput ]
  !poolInput = uniqueElement'
    [ i
    | i <- inputs
    , unwrap (SD.txOutAddress (unwrap $ SD.txInInfoResolved i)) == scriptHashAddress psh
    ]
  !oldPoolValue = unwrap $ SD.txOutValue $ unwrap $ SD.txInInfoResolved poolInput
  PoolDatum (AB coinA coinB) _ oldCirculatingLP _ = unsafeFromBuiltinData $
    shallowDatumOf (unwrap $ SD.txInInfoResolved poolInput)
  !newLiquidityTokens = scaleInteger (proposedOldToNewLiquidityRatio p) oldCirculatingLP
  !deadPoolOutput = uniqueElement'
    [ o
    | o <- outputs'
    , let !v = unwrap $ SD.txOutValue o
    , valueContains v pcs (computePoolTokenName poolIdent)
    , unwrap (SD.txOutAddress o) == scriptHashAddress dpsh
    , valueOf v (proposedNewPoolMint p) (computeLiquidityTokenName poolIdent) == newLiquidityTokens
    ]
  !newPoolOutput = uniqueElement' [txOut | txOut <- outputs', unwrap (SD.txOutAddress txOut) == scriptHashAddress (proposedNewPool p) ]
  newPoolCs = proposedNewPoolMint p
  !newPoolValue = sansRider $ unwrap $ SD.txOutValue newPoolOutput
  expectedDeadPoolDatum = DeadPoolDatum newPoolCs poolIdent (proposedOldToNewLiquidityRatio p)

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
