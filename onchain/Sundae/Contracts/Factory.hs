module Sundae.Contracts.Factory where

import PlutusTx.Prelude

import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Value

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
  :: FactoryBootCurrencySymbol
  -> FactoryDatum
  -> FactoryRedeemer
  -> ScriptContext
  -> Bool
factoryContract
  (FactoryBootCurrencySymbol fbcs)
  datum
  FactoryRedeemer
  ctx =
  debug "factory token not spent back"
    (hasFactoryLimited fbcs (txOutValue ownOutput)) &&
  debug "factory output not equal to input factory"
    (ownInputValue == txOutValue ownOutput) &&
  debug "datum altered"
    (rawDatumOf txInfo ownOutput == fromBuiltinData (toBuiltinData datum)) &&
  debug "minting tokens"
    (txInfoMint == mempty)
  where
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
