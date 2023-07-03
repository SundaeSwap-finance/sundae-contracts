module Sundae.Contracts.Mints where

import PlutusTx.Prelude
import PlutusTx.Builtins

import qualified PlutusTx.AssocMap as Map

import Sundae.Contracts.Common
import Sundae.Utilities

import PlutusLedgerApi.V2

ownCurrencySymbol :: ScriptContext -> CurrencySymbol
ownCurrencySymbol (ScriptContext _ purpose) =
  case purpose of
    Minting cs -> cs
    Spending _ -> die "ownCurrencySymbol"

-- Factory Boot Minting Contract
--  Controls creation of tokens used for the factory script
--  Parameterized by:
--    Factory Boot Settings, including the protocol boot UTXO and the initial set of legal scoopers
--  Allows (via Redeemer):
--    - Starting the protocol from scratch
--    - Initializing the new "post-upgrade" factory token
--    - Minting scooper license tokens
{-# inlinable factoryBootMintingContract #-}
factoryBootMintingContract :: FactoryBootSettings -> BuiltinData -> BuiltinData -> Bool
factoryBootMintingContract fbs (unsafeFromBuiltinData -> redeemer) (unsafeFromBuiltinData -> ctx) = case redeemer of
  MakeFactory ->
    debug "not minting a factory token"
      (Map.lookup ocs (getValue txInfoMint) == Just (Map.singleton factoryToken 1)) &&
    case fbs of
      BrandNewFactoryBootSettings{..} ->
        debug "not spending protocol boot UTXO"
          (atLeastOne (\input -> txInInfoOutRef input == unProtocolBootUTXO factory'protocolBootUTXO) txInfoInputs)
        -- Because we control this transaction, some non-security-critical checks are omitted:
          -- Not checking Factory datum
          -- Not checking that it's spent into the factory script
      UpgradedFactoryBootSettings (OldFactoryBootCurrencySymbol oldFbcs) ->
        debug "not spending the old factory token" -- TODO: Think about redeemer of factory script
          (atLeastOne (\input -> valueContains (txOutValue $ txInInfoResolved input) oldFbcs factoryToken) txInfoInputs)
  MakeScooperToken ->
    debug "only scooper tokens minted"
      (onlyHas txInfoMint ocs (computeScooperTokenName (intToIdent $ getWeek week)) (\n -> if n > 0 then checkLicenseIssuable else True))
  where
  checkLicenseIssuable =
    debug "not spending factory token"
      (atLeastOne (\input -> valueContains (txOutValue (txInInfoResolved input)) ocs factoryToken) txInfoInputs)
  UpperBound (Finite latest) _ = ivTo txInfoValidRange
  week = toWeek latest
  ocs = ownCurrencySymbol ctx
  TxInfo{..} = scriptContextTxInfo ctx

-- Pool minting contract
--  Controls creation of liquidity pools and liquidity tracking tokens
--  Parameterized by:
--    FactoryBootCurrencySymbol, which points to the Factory NFT
--    OldPoolCurrencySymbol, the old pool, if we've gone through an upgrade
--  Allows:
--    - Creation of liquidity pool NFTs and liquidity tracking tokens, provided the factory is cooperating
--  Details:
--    Effectively, this script has the factory token, and pool tokens, as
--    permission slips for arbitrary minting. That means we have to be careful
--    in the scripts that hold those to check that we don't mint anything unexpected.
{-# inlinable poolMintingContract #-}
poolMintingContract
  :: FactoryBootCurrencySymbol
  -> BuiltinData -- PoolMintRedeemer
  -> BuiltinData -- ScriptContext
  -> ()
poolMintingContract
  (FactoryBootCurrencySymbol fbcs)
  (unsafeFromBuiltinData -> redeemer)
  rawCtx = check $
    case redeemer of
      MintLP poolIdent ->
        let
          poolTokenName = computePoolTokenName poolIdent
          allowsToSpend !v =
            if valueContains v ocs poolTokenName then
              True
            else
              valueContains v fbcs factoryToken
        in
        -- The below is sufficient condition, *provided that*
        -- Each of the following script/redeemer combinations check that they are minting the correct token
        --   factoryContract / CreatePool     -> Only Mint Pool Tokens + Liquidity Tokens  (Checked)
        --   factoryContract / ISL            -> Only Mint or Burn Scooper License Tokens  (Checked)
        --   factoryContract / MakeProposal   -> None                                      (Checked)
        --   factoryContract / UpgradeFactory -> Only Mint New Factory Tokens              (Checked)
        --   deadFactoryContract / _          -> Only Mint New Pool + New Liquidity        (Checked)
        --   poolContract    / Scoop          -> Only Mint or Burn Liquidity Tokens        (Checked)
        --   poolContract    / Upgrade        -> Must have dead factory first              (Checked)
        --   deadPoolContract / _             -> Only Burn Liquidity Tokens                (Checked)
        debug "can only mint: lp tokens with the pool token, an upgraded pool with the old pool, or a pool token with the factory token"
          (atLeastOne (allowsToSpend.txOutValue.txInInfoResolved) ins)
      CreatePool coinA coinB ->
        let
          getIdent (Ident i) = i
          !firstInput = txInInfoOutRef (ins !! 0)
          -- A pool ident is 31 bytes in order to make it fit in the LP / pool
          -- NFT token names with an extra byte for labeling. So we truncate the
          -- blake2 hash.
          !newPoolIdent = dropByteString 1 $ blake2b_256 $
            getTxId (txOutRefId firstInput) <> "#" <> getIdent (intToIdent (txOutRefIdx firstInput))
          !poolOutput = uniqueElement' $
            filter (\case
              TxOut{txOutAddress, txOutValue}
                | valueContains txOutValue ocs (computePoolTokenName newPoolIdent)
                -- , txOutAddress == scriptHashAddress poolSH
                -> True
              _ -> False
              ) (txInfoOutputs txInfo)
          !poolOutputValue = txOutValue poolOutput
          !poolOutputValueSansRider = sansRider poolOutputValue
          {-
          !initialLiquidityTokens =
            computeInitialLiquidityTokens
            (valueOfAC poolOutputValueSansRider coinA)
            (valueOfAC poolOutputValueSansRider coinB)
          -}
        in
        True {-
        debug "coin pair not in canonical ordering, alphanumeric by policyID and assetName"
          (coinA < coinB) &&
        debug "minted something other than: a single pool token + correct amount of initial liquidity" (
          txInfoMint txInfo == Value (
            Map.singleton ocs $ Map.fromList
              [ (computePoolTokenName newPoolIdent, 1)
              , (computeLiquidityTokenName newPoolIdent, initialLiquidityTokens)
              ]
        )) &&
        debug "liquidity and/or pool NFT not spent to pool"
          ( valueOfAC poolOutputValueSansRider coinA >= 1 &&
            valueOfAC poolOutputValueSansRider coinB >= 1 &&
            hasLimitedNft 3 (toPoolNft ocs newPoolIdent) poolOutputValueSansRider ) &&
        debug "pool datum not properly initialized"
          (case datumOf txInfo poolOutput of
            Just PoolDatum{..} ->
              _pool'coins == AB coinA coinB &&
              _pool'poolIdent == newPoolIdent &&
              _pool'circulatingLP == initialLiquidityTokens &&
              elem _pool'swapFees legalSwapFees
            Nothing -> error ()
          )-}
  where
  ScriptContext txInfo (Minting ocs) = unsafeFromBuiltinData rawCtx
  ins = txInfoInputs txInfo
  !factoryReference = uniqueElement'
    [ o
    | o <- txInfoReferenceInputs txInfo
    , isFactory fbcs (txInInfoResolved o)
    ]
  !factoryReferenceDatum =
    case datumOf txInfo (txInInfoResolved factoryReference) of
      Just fac -> fac
      Nothing -> traceError "factory reference must have a factory datum"
  !(FactoryDatum !poolSH _poolCS _ _) = factoryReferenceDatum
