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
factoryBootMintingContract :: FactoryBootSettings -> FactoryBootMintRedeemer -> ScriptContext -> Bool
factoryBootMintingContract fbs redeemer ctx = case redeemer of
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
  -> PoolMintRedeemer
  -> ScriptContext
  -> ()
poolMintingContract
  (FactoryBootCurrencySymbol fbcs)
  redeemer
  (ScriptContext txInfo purpose) = check $ True
  where
  {-
  factoryReference = uniqueElement'
    [ o
    | o <- txInfoReferenceInputs txInfo
    , isFactory fbcs (txInInfoResolved o)
    ]
  factoryReferenceDatum =
    case datumOf txInfo (txInInfoResolved factoryReference) of
      Just fac -> fac
      Nothing -> traceError "factory reference must have a factory datum"
  (FactoryDatum _poolSH poolCS _ _) = factoryReferenceDatum
  -}
