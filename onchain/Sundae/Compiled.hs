{-# LANGUAGE IncoherentInstances #-}

-- | Wrappers for compiled scripts
module Sundae.Compiled(module X, AllScripts(..), scriptsExample, makeAllScripts, testEvalEscrowScr, hashScript) where

import PlutusLedgerApi.V3 (SerialisedScript, TxOutRef(..), toBuiltin, OutputDatum(..))
import PlutusLedgerApi.Common (mkDynEvaluationContext, evaluateScriptCounting, PlutusLedgerLanguage(..))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults
import PlutusLedgerApi.Common (ProtocolVersion(..), VerboseMode(..))
import Data.Default (def)
import Data.Either (fromRight)
import PlutusLedgerApi.V3 qualified as Plutus
import PlutusLedgerApi.V1.Value (AssetClass(..), CurrencySymbol(..))
import PlutusLedgerApi.V1.Value qualified as Plutus
import Data.Text
import Prelude
import GHC.Generics
import Data.Aeson
import Data.Text.Encoding qualified as Text
import Data.ByteString.Hash qualified as Hash
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Base16 qualified as Base16
import Data.Coerce (coerce)

import Codec.Serialise (deserialise)

import Sundae.Contracts.Common (EscrowRedeemer(..), EscrowAction(..), EscrowDatum(..), EscrowAddress(..), EscrowDestination(..), FactoryBootSettings(..), ProtocolBootUTXO(..), ScooperFeeSettings(..), FactoryBootSettings, UpgradeSettings(..), FactoryBootCurrencySymbol(..), OldFactoryBootCurrencySymbol(..), TreasuryBootSettings(..), OldPoolCurrencySymbol(..), factoryToken, treasuryToken, sundaeToken, TreasuryBootCurrencySymbol(..), SundaeCurrencySymbol(..), PoolCurrencySymbol(..), GiftScriptHash(..), PoolScriptHash(..), DeadPoolScriptHash(..), ScooperFeeHolderScriptHash(..), EscrowScriptHash(..), DeadFactoryScriptHash(..), TreasuryScriptHash(..))

import Sundae.Utilities (Ident(..), Coin(..))

import Sundae.Compiled.Factory as X
import Sundae.Compiled.Mints as X
import Sundae.Compiled.Others as X
import Sundae.Compiled.Pool as X

import System.IO.Unsafe (unsafePerformIO)

loadOrUse :: (FilePath -> IO a) -> OrPath a -> IO a
loadOrUse _ (InLine a) = return a
loadOrUse l (AsPath p) = l p

{-
plutusScriptHash :: Serialise a => a -> Ledger.ScriptHash Crypto.StandardCrypto
plutusScriptHash script =
  Ledger.hashScript @(ShelleyLedgerEra BabbageEra) $
    Alonzo.PlutusScript Alonzo.PlutusV1 (case serialiseScript script of PlutusScriptSerialised scr -> scr) --Alonzo.PlutusScript _ {- Cardano.PlutusV1 -} (case serialiseScript script of PlutusScriptSerialised scr -> scr)
-}

currencySymbolOf :: SerialisedScript -> CurrencySymbol
currencySymbolOf _ = CurrencySymbol "00000000000000000000000000000000000000000000000000000000"

txOutRefFromStr :: Text -> Word -> TxOutRef
txOutRefFromStr txid txix =
  TxOutRef
    (Plutus.TxId (Plutus.toBuiltin (Text.encodeUtf8 txid)))
    (fromIntegral txix)

data AllScripts = AllScripts
  { factoryBootMintScr :: SerialisedScript
  , treasuryBootMintScr :: SerialisedScript
  , sundaeMintScr :: SerialisedScript
  , factoryBootCS :: FactoryBootCurrencySymbol
  , treasuryBootCS :: TreasuryBootCurrencySymbol
  , sundaeCS :: SundaeCurrencySymbol
  , factoryScr :: SerialisedScript
  , treasuryScr :: SerialisedScript
  , treasurySH :: TreasuryScriptHash
  , giftScr :: SerialisedScript
  , giftSH :: GiftScriptHash
  , poolMintScr :: SerialisedScript
  , poolCS :: PoolCurrencySymbol
  , poolScr :: SerialisedScript
  , poolSH :: PoolScriptHash
  , deadPoolScr :: SerialisedScript
  , deadPoolSH :: DeadPoolScriptHash
  , deadFactoryScr :: SerialisedScript
  , deadFactorySH :: DeadFactoryScriptHash
  , proposalScr :: SerialisedScript
  , scooperFeeHolderScr :: SerialisedScript
  , scooperFeeHolderSH :: ScooperFeeHolderScriptHash
  , escrowScr :: SerialisedScript
  , escrowSH :: EscrowScriptHash
  , factoryAssetClass :: AssetClass
  , treasuryAssetClass :: AssetClass
  , sundaeAssetClass :: AssetClass
  } deriving (Generic, Show, ToJSON)

instance ToJSON AssetClass where
  toJSON (AssetClass (symbol, token)) =
    object
      [ "symbol" .= symbol
      , "token" .= token
      ]

instance ToJSON EscrowScriptHash where
instance ToJSON ScooperFeeHolderScriptHash where
instance ToJSON DeadFactoryScriptHash where
instance ToJSON DeadPoolScriptHash where
instance ToJSON PoolScriptHash where
instance ToJSON PoolCurrencySymbol where
instance ToJSON GiftScriptHash where
instance ToJSON TreasuryScriptHash where
instance ToJSON SundaeCurrencySymbol where
instance ToJSON TreasuryBootCurrencySymbol where
instance ToJSON FactoryBootCurrencySymbol where

deriving instance Generic EscrowScriptHash
deriving instance Generic ScooperFeeHolderScriptHash
deriving instance Generic DeadFactoryScriptHash
deriving instance Generic DeadPoolScriptHash
deriving instance Generic PoolScriptHash
deriving instance Generic PoolCurrencySymbol
deriving instance Generic GiftScriptHash
deriving instance Generic TreasuryScriptHash
deriving instance Generic SundaeCurrencySymbol
deriving instance Generic TreasuryBootCurrencySymbol
deriving instance Generic FactoryBootCurrencySymbol

instance ToJSON SerialisedScript where
  toJSON s = String $ Text.decodeUtf8 $ Base16.encode (SBS.fromShort s)

instance ToJSON Plutus.TokenName where
  toJSON (Plutus.TokenName bs) = String (Text.decodeUtf8 $ Plutus.fromBuiltin bs)

data OrPath a = InLine !a | AsPath FilePath
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type CLIBareUTXO = (Text, Word)

data CLIAssetClass = CLIAssetClass
  { currencySymbol :: Text
  , tokenName :: Text
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

fromCLIAssetClass :: CLIAssetClass -> Plutus.AssetClass
fromCLIAssetClass (CLIAssetClass policyId tokenName) = Plutus.assetClass
  (either undefined (Plutus.CurrencySymbol . Plutus.toBuiltin) $ Base16.decode $ Text.encodeUtf8 policyId)
  (either undefined (Plutus.TokenName . Plutus.toBuiltin) $ Base16.decode $ Text.encodeUtf8 tokenName)

data CLIFactoryBootSettings
  = CLIBrandNewFactoryBootSettings [Text]
  | CLIUpgradedFactoryBootSettings (OrPath CurrencySymbol) (OrPath CurrencySymbol)
  deriving (Show, Generic, ToJSON, FromJSON)

convertFBSettings :: CLIBareUTXO -> CLIFactoryBootSettings -> FactoryBootSettings
convertFBSettings (txId, txIx) = \case
  CLIBrandNewFactoryBootSettings addrs ->
    BrandNewFactoryBootSettings
      (ProtocolBootUTXO $ TxOutRef (Plutus.TxId . toBuiltin . fromRight (error "factory boot UTXO TXID not hex-encoded") . Base16.decode . Text.encodeUtf8 $ txId) (fromIntegral txIx))
      (readHexPubKey <$> addrs)
  CLIUpgradedFactoryBootSettings oldFactoryBootSymbol _ ->
    UpgradedFactoryBootSettings $
      OldFactoryBootCurrencySymbol $
      unsafePerformIO $
      loadOrUse (fmap (currencySymbolOf . deserialise) . LBS.readFile) oldFactoryBootSymbol
  where
  readHexPubKey k =
    case Base16.decode (Text.encodeUtf8 k) of
      Right bytes -> Plutus.PubKeyHash (toBuiltin bytes)
      Left err -> error err

data CLIUpgradeSettings = CLIUpgradeSettings
  { cliUpgradeTimeLockSeconds :: Integer
  , cliUpgradeAuthentication :: CLIAssetClass
  }
  deriving (Show, Generic, ToJSON, FromJSON)

scriptsExample :: AllScripts
scriptsExample =
  let
    boot = ("80070000000000000000000000000000", 0)
    treasBoot = ("78007000000000000000000000000000", 0)
    bootSettings = CLIBrandNewFactoryBootSettings []
    upgrade = CLIUpgradeSettings 1 (CLIAssetClass "00000000000000000000000000000000000000000000000000000000" "00")
    fees = ScooperFeeSettings 1
  in
    makeAllScripts boot treasBoot bootSettings upgrade fees



makeAllScripts :: CLIBareUTXO -> CLIBareUTXO -> CLIFactoryBootSettings -> CLIUpgradeSettings -> ScooperFeeSettings -> AllScripts
makeAllScripts bootUTXO treasBootUTXO fbSettings upgradeSettings scooperFeeSettings =
  let
    convertedFBSettings = convertFBSettings bootUTXO fbSettings
    convertedTreasuryBootSettings =
      TreasuryBootSettings $ ProtocolBootUTXO $ uncurry txOutRefFromStr treasBootUTXO
    oldPoolCurrencySymbol = case fbSettings of
      CLIBrandNewFactoryBootSettings {} -> OldPoolCurrencySymbol "00000000000000000000000000000000000000000000000000000000"
      CLIUpgradedFactoryBootSettings _ oldSymbol ->
        OldPoolCurrencySymbol $ unsafePerformIO $ loadOrUse (fmap (currencySymbolOf . deserialise) . LBS.readFile) oldSymbol
    convertedUpgradeSettings = UpgradeSettings
      { upgradeTimeLockPeriod = cliUpgradeTimeLockSeconds upgradeSettings * 1000
      , upgradeAuthentication = fromCLIAssetClass $ cliUpgradeAuthentication upgradeSettings
      }
    factoryBootMintScr = factoryBootMintingScript convertedFBSettings
    treasuryBootMintScr = treasuryBootMintingScript convertedTreasuryBootSettings
    sundaeMintScr = sundaeMintingScript treasuryBootCS

    factoryBootCS = mcs factoryBootMintScr
    treasuryBootCS = mcs treasuryBootMintScr
    sundaeCS = mcs sundaeMintScr

    factoryScr = factoryScript convertedUpgradeSettings factoryBootCS deadFactorySH poolSH poolCS
    treasuryScr = treasuryScript convertedUpgradeSettings treasuryBootCS sundaeCS poolCS
    treasurySH :: TreasuryScriptHash
    treasurySH = vsh treasuryScr
    giftScr = giftScript treasuryBootCS
    giftSH = vsh giftScr

    poolMintScr = poolMintingScript factoryBootCS oldPoolCurrencySymbol
    poolCS = mcs poolMintScr
    poolScr = poolScript factoryBootCS poolCS scooperFeeHolderSH escrowSH
    poolSH = vsh poolScr

    deadPoolScr = deadPoolScript poolCS escrowSH
    deadPoolSH = vsh deadPoolScr

    deadFactoryScr = deadFactoryScript factoryBootCS poolSH deadPoolSH poolCS
    deadFactorySH :: DeadFactoryScriptHash
    deadFactorySH = vsh deadFactoryScr

    proposalScr = proposalScript convertedUpgradeSettings

    scooperFeeHolderScr = scooperFeeScript scooperFeeSettings giftSH factoryBootCS
    scooperFeeHolderSH = vsh scooperFeeHolderScr

    escrowScr = escrowScript poolCS
    escrowSH = vsh escrowScr
    factoryAssetClass = AssetClass (coerce factoryBootCS, factoryToken)
    treasuryAssetClass = AssetClass (coerce treasuryBootCS, treasuryToken)
    sundaeAssetClass = AssetClass (coerce sundaeCS, sundaeToken)
  in AllScripts {..}
  where
  mcs script = coerce $ Plutus.ScriptHash (toBuiltin (hashScript script))
  vsh script = coerce $ Plutus.ScriptHash (toBuiltin (hashScript script))

-- Reference for the implementation of script hashing:
-- https://github.com/input-output-hk/cardano-ledger/blob/d421556ef91362d13963a68a94c6f9e752d67e59/eras/babbage/impl/src/Cardano/Ledger/Babbage/Scripts.hs#L35-L42
-- https://github.com/input-output-hk/cardano-ledger/blob/d421556ef91362d13963a68a94c6f9e752d67e59/libs/cardano-ledger-core/src/Cardano/Ledger/Core.hs#L449-L456
hashScript :: SerialisedScript -> BS.ByteString
hashScript script =
  let
    -- Our scripts are plutus V3
    babbageV3ScriptPrefixTag = "\x03"
  in
    Hash.blake2b_256 (babbageV3ScriptPrefixTag <> SBS.fromShort script)

testEvalEscrowScr :: IO (Plutus.LogOutput, Either Plutus.EvaluationError Plutus.ExBudget)
testEvalEscrowScr = do
  defCostModel <-
    case defaultCostModelParams of
      Just cmp -> pure cmp
      Nothing -> Prelude.error "No default cost model params"
  ec <-
    case mkDynEvaluationContext def defCostModel of
      Right ok -> pure ok
      Left err -> Prelude.error (show err)
  let
    escrowScriptAddr = Plutus.Address
      (Plutus.ScriptCredential "1111111111111111111111111111111111111111111111111111111111111111")
      Nothing
    poolScriptAddr = Plutus.Address
      (Plutus.ScriptCredential "2222222222222222222222222222222222222222222222222222222222222222")
      Nothing
    myWallet = Plutus.Address
      (Plutus.PubKeyCredential "3333333333333333333333333333333333333333333333333333333333333333")
      Nothing
    escrowDestPkh = "9999999999999999999999999999999999999999999999999999999999999999"
    escrowAddress =
      EscrowAddress
        ( EscrowDestination
          ( Plutus.Address
            (Plutus.PubKeyCredential escrowDestPkh)
            Nothing
          )
          Nothing
        )
        Nothing
    escrowAction = EscrowSwap CoinA 1000000 Nothing
    datum = EscrowDatum "" escrowAddress 2500000 escrowAction
    redeemer = EscrowScoop
    context =
      Plutus.ScriptContext
        { scriptContextTxInfo =
            Plutus.TxInfo
              { txInfoInputs =
                  -- the escrow
                  [ Plutus.TxInInfo
                      { txInInfoOutRef =
                          ( Plutus.TxOutRef
                              { txOutRefId = "0000000000000000000000000000000000000000000000000000000000000000"
                              , txOutRefIdx = 0
                              }
                          )
                      , txInInfoResolved = Plutus.TxOut
                          { txOutAddress = escrowScriptAddr
                          , txOutValue = mempty
                          , txOutDatum = Plutus.OutputDatumHash ""
                          , txOutReferenceScript = Nothing
                          }
                      }
                  , Plutus.TxInInfo
                      { Plutus.txInInfoOutRef =
                          ( Plutus.TxOutRef
                              { Plutus.txOutRefId = "0000000000000000000000000000000000000000000000000000000000000000"
                              , Plutus.txOutRefIdx = 1
                              }
                          )
                      , Plutus.txInInfoResolved = Plutus.TxOut
                          { txOutAddress = poolScriptAddr
                          , txOutValue = Plutus.Value $ Plutus.fromList
                              [ (CurrencySymbol "", Plutus.fromList [(Plutus.TokenName "", 2_000_000)])
                              , (coerce $ poolCS scriptsExample, Plutus.fromList [(Plutus.TokenName "p \NUL", 1)])
                              ]
                          , txOutDatum = NoOutputDatum
                          , txOutReferenceScript = Nothing
                          }
                      }
                  ]
              , txInfoOutputs =
                [ Plutus.TxOut
                    { txOutAddress = myWallet
                    , txOutValue = Plutus.Value $ Plutus.fromList
                        [ (CurrencySymbol "", Plutus.fromList [(Plutus.TokenName "", 2_000_000)])
                        , (coerce $ poolCS scriptsExample, Plutus.fromList [(Plutus.TokenName "p \NUL", 1)])
                        ]
                    , txOutDatum = NoOutputDatum
                    , txOutReferenceScript = Nothing
                    }
                ]
              , txInfoFee = mempty -- Value
              , txInfoMint = mempty -- Value
              , txInfoDCert = []
              , txInfoWdrl = Plutus.fromList []
              , txInfoValidRange =
                  Plutus.Interval
                    (Plutus.LowerBound Plutus.NegInf False)
                    (Plutus.UpperBound Plutus.PosInf False)
              , txInfoSignatories = []
              , txInfoData = Plutus.fromList []
              , txInfoId = "0000000000000000000000000000000000000000000000000000000000000001"
              , txInfoReferenceInputs = []
              , txInfoRedeemers = Plutus.fromList []
              }
        , scriptContextPurpose =
            Plutus.Spending
              ( Plutus.TxOutRef
                  { txOutRefId = "0000000000000000000000000000000000000000000000000000000000000000"
                  , txOutRefIdx = 0
                  }
              )
        }
  let out@(logOutput, result) =
        evaluateScriptCounting
          PlutusV3
          (ProtocolVersion 9 0)
          Verbose
          ec
          (escrowScr scriptsExample)
          [ Plutus.toData datum
          , Plutus.toData redeemer
          , Plutus.toData context
          ]
  pure out
