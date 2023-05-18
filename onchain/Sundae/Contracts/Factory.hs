module Sundae.Contracts.Factory where

import PlutusTx.Prelude

import PlutusLedgerApi.V3
import PlutusLedgerApi.V1.Value

import qualified PlutusTx.AssocMap as Map

import Sundae.Contracts.Common
import Sundae.Utilities

-- Factory Contract
--  Locks the "Factory NFT" and brokers protocol operations relating to pools.
--  Parameterized by:
--    Factory Settings, including time-lock period for upgrades
--    FactoryBootCurrencySymbol, identifying the NFT
--    ProposalScriptHash, the script which will broker a protocol upgrade
--    PoolScriptHash, the contract governing each liquidity pool
--    PoolCurrencySymbol, the NFT identifying each pool
--  Uses Datum:
--    nextPoolIdent, proposalState, scooperIdent, and a list of valid scoopers
--  Allows (via Redeemer):
--    - Creating new pools
--    - Starting a protocol upgrade
--    - Finalizing a protocol upgrade
--    - Issuing scooper licenses
--    - Updating list of valid scoopers
--  Details:
--    The factory contract acts as a supply of unique names, which pools are
--    identified by, and validates the initial data of pools. Those uniques are
--    also included in the token names of tokens that have to be associated with
--    particular pools, like liquidity tracking tokens and pool NFTs.
--
--    It also dispenses scooper license tokens, which are required to do scoops,
--    and maintains a list of public keys that belong to legal scoopers. That list can be
--    updated through a governance action.
--
--    The factory can be "killed" by a full governance upgrade, after which its
--    NFT is passed to the "dead factory contract".
--
{-# inlinable factoryContract #-}
factoryContract
  :: UpgradeSettings
  -> FactoryBootCurrencySymbol
  -> DeadFactoryScriptHash
  -> PoolScriptHash
  -> PoolCurrencySymbol
  -> FactoryDatum
  -> FactoryRedeemer
  -> ScriptContext
  -> Bool
factoryContract
  UpgradeSettings {..}
  (FactoryBootCurrencySymbol fbcs)
  (DeadFactoryScriptHash deadFactorySh)
  (PoolScriptHash poolScriptHash)
  (PoolCurrencySymbol pcs)
  datum@FactoryDatum {..}
  redeemer
  ctx =
  (if redeemer /= UpgradeFactory then
    debug "factory token not spent back, and not upgrading factory"
      (hasFactoryLimited fbcs (txOutValue ownOutput)) &&
    debug "factory output not equal to input factory"
      (ownInputValue == txOutValue ownOutput)
  else True) &&
  debug "valid range too large to be useful"
    (validRangeSize txInfoValidRange <= maxValidRangeSize) &&
  case redeemer of
    CreatePool coinA coinB ->
      let
        !poolOutput = uniqueElement' $
          filter (\case
            TxOut{txOutAddress, txOutValue}
              | valueContains txOutValue pcs (computePoolTokenName nextPoolIdent)
              , txOutAddress == scriptHashAddress poolScriptHash -> True
            _ -> False
            ) txInfoOutputs
        poolOutputValue = txOutValue poolOutput
        -- Subtract out the rider, so that liquidity is calculated on actual assets tradable in pool
        poolOutputValueSansRider = sansRider poolOutputValue
        !initialLiquidityTokens =
          computeInitialLiquidityTokens
            (valueOfAC poolOutputValueSansRider coinA)
            (valueOfAC poolOutputValueSansRider coinB)
      in
        debug "coin pair not in canonical ordering, alphanumeric by policyID and assetName"
          (coinA < coinB) &&

        debug "new factory datum incorrect: should only increment nextPoolIdent" (
          let newFactoryDatum = datum { nextPoolIdent = succIdent nextPoolIdent }
          in isDatumUnsafe txInfo ownOutput newFactoryDatum) &&

        debug "minted something other than: a single pool token + correct amount of initial liquidity" (
          txInfoMint == Value (
            Map.singleton pcs $ Map.fromList
              [ (computePoolTokenName nextPoolIdent, 1)
              , (computeLiquidityTokenName nextPoolIdent, initialLiquidityTokens)
              ]
          )) &&

        debug "liquidity and/or pool NFT not spent to pool"
          ( valueOfAC poolOutputValueSansRider coinA >= 1 &&
            valueOfAC poolOutputValueSansRider coinB >= 1 &&
            hasLimitedNft 3 (toPoolNft pcs nextPoolIdent) poolOutputValueSansRider ) &&

        debug "pool datum not properly initialized"
          (case datumOf txInfo poolOutput of
            Just PoolDatum{..} ->
              _pool'coins == AB coinA coinB &&
              _pool'poolIdent == nextPoolIdent &&
              _pool'circulatingLP == initialLiquidityTokens &&
              elem _pool'swapFees legalSwapFees
            Nothing -> error ()
          )

    MakeProposal
      | let
          !proposalInput = uniqueElement' [ i | i <- txInfoInputs, assetClassValueContains (txOutValue $ txInInfoResolved i) upgradeAuthentication ]
          Just (proposal :: UpgradeProposal) = datumOf txInfo (txInInfoResolved proposalInput)
      ->
        debug "proposal applies to wrong factory"
          (txOutAddress ownInput == scriptHashAddress (proposedOldFactory proposal)) &&
        debug "making a proposal does not allow minting"
          (txInfoMint == mempty) &&
        case proposal of
          UpgradeScripts scriptProp ->
            debug "proposal already ongoing or adopted"
              (proposalState == NoProposal) &&
            debug "new datum incorrect: should only update proposalState to PendingProposal"
              (isDatumUnsafe txInfo ownOutput (datum { proposalState = PendingProposal latest scriptProp }))
          UpgradeScooperSet (ScooperUpgradeProposal {..}) ->
            debug "minting a scooper with the wrong identifier"
              (scooperIdent == proposedOldScooperIdent) &&
            debug "datum not updated correctly; should increment scooperIdent, and set the new set of allowed scoopers"
              (isDatumUnsafe txInfo ownOutput (datum { scooperIdent = succIdent scooperIdent, scooperSet = proposedNewScooperSet}))
    UpgradeFactory ->
      case proposalState of
        PendingProposal proposalStart proposal@ScriptUpgradeProposal{..}
          | POSIXTime gap <- latest - proposalStart
          , let !newFactoryOut = uniqueElement' [ o | o <- txInfoOutputs, txOutAddress o == scriptHashAddress proposedNewFactory ]
          ->
          debug "too few or too many new factory tokens"
            (hasFactoryLimited proposedNewFactoryBoot (txOutValue newFactoryOut)) &&
          debug "new factory datum not initialized correctly"
            (isDatumUnsafe txInfo newFactoryOut (datum { proposalState = NoProposal, scooperIdent = initialIdent })) &&
          debug "continuing factory datum not updated correctly: should only set proposalState to adopted"
            (null continuingOutputs) &&
          debug "time-lock hasn't expired"
            (gap >= upgradeTimeLockPeriod) &&
          debug "dead factory not paid into or initialized correctly"
            ( let !deadFactoryOutput = uniqueElement' $ getAddressOutputs ctx (scriptHashAddress deadFactorySh)
              in  isDatumUnsafe txInfo deadFactoryOutput (DeadFactoryDatum proposal) &&
                  onlyHas (withoutLovelace (txOutValue deadFactoryOutput)) fbcs factoryToken (== 1)
            ) &&
          debug "minting something other than the new factory token"
            (txInfoMint == singleton proposedNewFactoryBoot factoryToken 1)
        _ -> debug "no pending proposal to upgrade, or not paid into new factory script" False

    IssueScooperLicense pkh ->
      debug "scooper key is not a signatory"
        (elem pkh txInfoSignatories) &&
      debug "signer is not a registered scooper"
        (elem pkh scooperSet) &&
      debug "datum altered"
        (rawDatumOf txInfo ownOutput == fromData (toData datum)) &&
      debug "minting other things than scooper tokens"
        (txInfoMint == mempty ||
          onlyHas txInfoMint fbcs (computeScooperTokenName (intToIdent $ getWeek $ toWeek latest)) (const True))
  where
  !maxValidRangeSize = case redeemer of
    IssueScooperLicense _ -> fourDaysMillis
    _ -> hourMillis
  UpperBound (Finite !latest) _ = ivTo txInfoValidRange
  txInfo@TxInfo{..} = scriptContextTxInfo ctx
  ownOutput = uniqueElement' continuingOutputs
  !continuingOutputs = getContinuingOutputs ctx
  !ownInput = scriptInput ctx
  ownInputValue = txOutValue ownInput

{-# inlinable toFactoryNft #-}
toFactoryNft :: CurrencySymbol -> AssetClass
toFactoryNft cs = assetClass cs factoryToken

{-# inlinable hasFactoryLimited #-}
-- | Factory value should contain factory NFt and no more than 2 items in the value:
-- Ada, NFT
hasFactoryLimited :: CurrencySymbol -> Value -> Bool
hasFactoryLimited cs val = hasLimitedNft 2 (toFactoryNft cs) val
