module Sundae.Contracts.Mints where

import PlutusTx.Prelude
import PlutusTx.Builtins

import PlutusLedgerApi.V3

import qualified PlutusTx.AssocMap as Map

import Sundae.Contracts.Common
import Sundae.Utilities

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

-- Treasury Boot Minting Contract
--  Controls creation of the treasury, owned by the SundaeSwap DAO
--  Parameterized by:
--    Treasury Boot Settings, including the protocol boot UTXO
--  Allows:
--    - Minting the treasury token, an NFT that tracks and brokers access to protocol funds (such as the remaining supply of Sundae)
--  Details:
--  We do this with these two layers of contracts because we want the Sundae
--  policy ID to be stable across upgrades for usability reasons.
{-# inlinable treasuryBootMintingContract #-}
treasuryBootMintingContract :: TreasuryBootSettings -> () -> ScriptContext -> Bool
treasuryBootMintingContract TreasuryBootSettings{..} () ctx =
  debug "not spending protocol boot UTXO"
    (atLeastOne (\input -> txInInfoOutRef input == unProtocolBootUTXO treasury'protocolBootUTXO) ins) &&
  debug "not minting a single treasury token"
    (Map.lookup ocs (getValue $ txInfoMint txInfo) == Just (Map.singleton treasuryToken 1)) &&
  -- For same reasons as above, we could technically omit this check; it's not security critical because we control the TX here
  debug "not spending treasury token with correct datum"
    (atLeastOne (\output ->
      valueContains (txOutValue output) ocs treasuryToken &&
      rawDatumOf txInfo output == fromBuiltinData (toBuiltinData (TreasuryDatum sundaeMaxSupply NoProposal))
      ) outs)
  where
  ocs = ownCurrencySymbol ctx
  txInfo = scriptContextTxInfo ctx
  ins = txInfoInputs txInfo
  outs = txInfoOutputs txInfo

-- Sundae minting contract
--  Controls creation of the Sundae token; defers entirely to the treasury NFT
--  Parameterized by:
--    TreasuryBootCurrencySymbol, the NFT locked by the treasury script
--  Allows:
--    - Minting and burning of the Sundae token, so long as the treasury NFT is in the output
{-# inlinable sundaeMintingContract #-}
sundaeMintingContract :: TreasuryBootCurrencySymbol -> () -> ScriptContext -> Bool
sundaeMintingContract (TreasuryBootCurrencySymbol treasuryBoot) ()
  ScriptContext { scriptContextTxInfo = TxInfo {..} } =
  debug "can only mint sundae if spending (or minting) treasury NFT" $
    atLeastOne (\output ->
      valueContains (txOutValue output) treasuryBoot treasuryToken
      ) txInfoOutputs

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
  -> OldPoolCurrencySymbol
  -> BuiltinData
  -> BuiltinData
  -> ()
poolMintingContract
  (FactoryBootCurrencySymbol fbcs)
  (OldPoolCurrencySymbol oldPcs)
  (unsafeFromBuiltinData -> poolIdent)
  rawCtx = check $
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
      (atLeastOne (allowsToSpend.unsafeTxInValue) ins)
  where
  poolTokenName = computePoolTokenName poolIdent
  allowsToSpend !v =
    if valueContains v ocs poolTokenName then
      True
    else if valueContains v oldPcs poolTokenName then
      True
    else
      valueContains v fbcs factoryToken
  -- Drill into txInfo to get our own currency symbol from the script purpose
  (unsafeDataAsConstr -> (_, [
    (unsafeDataAsConstr -> (_, [
      unsafeDataAsList -> ins,
      _, _, _, _, _, _, _, _, _, _, _
    ])), unsafeFromBuiltinData -> !(Minting ocs)])) = rawCtx
