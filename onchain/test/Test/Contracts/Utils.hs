{-# language TypeApplications #-}
{-# language ViewPatterns #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}

module Test.Contracts.Utils where

import Codec.Serialise hiding (decode, Fail)
import Control.Exception
import Control.Lens hiding (ix)
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.ByteString.Hash qualified as Hash
import Data.Coerce (Coercible, coerce)
import Data.Containers.ListUtils (nubOrdOn)
import Data.FileEmbed (embedFile)
import Data.Maybe (catMaybes)
import Data.String(fromString)
import PlutusLedgerApi.V3
import PlutusLedgerApi.V1.Value
import qualified PlutusTx.Ratio as Ratio
import qualified PlutusTx.Prelude as Plutus
import qualified PlutusTx.AssocMap as AssocMap

import qualified Cardano.Crypto.Hash.Class as Crypto

import Test.Tasty.HUnit
import Sundae.Contracts as Sundae
import Sundae.Utilities as Sundae
import qualified Sundae.Compiled as Sundae
import qualified Sundae.ShallowData as SD
import qualified Data.Aeson as Aeson
import System.IO.Unsafe (unsafePerformIO)

data Step
  = FromUser Address Value
  | FromScript Address Value String Cond ScriptInput
  | ToUser Address Value
  | ToScript Address Value Data
  | ReferenceInput Address Value Data
  | PoolMint Value Cond BuiltinByteString
  | TreasuryBootMint Value Cond
  | SundaeMint Value Cond
  | FactoryBootMint Value Cond FactoryBootMintRedeemer
  | CustomInterval (Interval POSIXTime)
  | CustomSignatories [PubKeyHash]

data Cond = Pass | Fail
data ScriptInput
  = PoolScriptInput PoolRedeemer PoolDatum
  | EscrowScriptInput EscrowRedeemer EscrowDatum
  | FactoryScriptInput FactoryRedeemer FactoryDatum

fromPoolScript :: Address -> Value -> String -> Cond -> PoolRedeemer -> PoolDatum -> Step
fromPoolScript a v dbg cond red dat = FromScript a v dbg cond (PoolScriptInput red dat)
fromEscrowScript :: Address -> Value -> String -> Cond -> EscrowRedeemer -> EscrowDatum -> Step
fromEscrowScript a v dbg cond red dat = FromScript a v dbg cond (EscrowScriptInput red dat)
fromFactoryScript :: Address -> Value -> String -> Cond -> FactoryRedeemer -> FactoryDatum -> Step
fromFactoryScript a v dbg cond red dat = FromScript a v dbg cond (FactoryScriptInput red dat)
referenceFactoryScript :: Address -> Value -> Data -> Step
referenceFactoryScript a v d = ReferenceInput a v d

tiInputs :: Lens' TxInfo [TxInInfo]
tiInputs f info =
  (\i' -> info{txInfoInputs = i'}) <$> f (txInfoInputs info)

tiOutputs :: Lens' TxInfo [TxOut]
tiOutputs f info =
  (\i' -> info{txInfoOutputs = i'}) <$> f (txInfoOutputs info)

tiReferenceInputs :: Lens' TxInfo [TxInInfo]
tiReferenceInputs f info =
  (\i' -> info{txInfoReferenceInputs = i'}) <$> f (txInfoReferenceInputs info)

tiMint :: Lens' TxInfo Value
tiMint f info =
  (\i' -> info{txInfoMint = i'}) <$> f (txInfoMint info)

tiData :: Lens' TxInfo (Map DatumHash Datum)
tiData f info =
  (\i' -> info{txInfoData = i'}) <$> f (txInfoData info)

tiValidRange :: Lens' TxInfo (Interval POSIXTime)
tiValidRange f info =
  (\i' -> info{txInfoValidRange = i'}) <$> f (txInfoValidRange info)

tiSignatories :: Lens' TxInfo [PubKeyHash]
tiSignatories f info =
  (\i' -> info{txInfoSignatories = i'}) <$> f (txInfoSignatories info)

user1, user2, scooperUsr :: BuiltinByteString
user1 = "usr#1"
user2 = "usr#2"
scooperUsr = "usr#scooper"

user1Pkh, user2Pkh, scooperUserPkh :: PubKeyHash
user1Pkh = mkUsrPkh user1
user2Pkh = mkUsrPkh user2
scooperUserPkh = mkUsrPkh scooperUsr

user1Addr, user2Addr, scooperUserAddr :: Address
user1Addr = mkUserAddr user1
user2Addr = mkUserAddr user2
scooperUserAddr = mkUserAddr scooperUsr

user1Dest, user2Dest, scooperUserDest :: EscrowDestination
user1Dest = EscrowDestination user1Addr Nothing
user2Dest = EscrowDestination user2Addr Nothing
scooperUserDest = EscrowDestination scooperUserAddr Nothing

sundaeCoin, swapCoin, adaCoin :: AssetClass
sundaeCoin = toCoin "SUNDAE"
swapCoin = toCoin "Swap"
adaCoin = assetClass adaSymbol adaToken

currencySymbolOf :: SerialisedScript -> CurrencySymbol
currencySymbolOf script = coerce $ ScriptHash (toBuiltin (hashScript script))

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

toDatum :: ToData a => a -> Datum
toDatum = Datum . BuiltinData . toData

vsh :: Coercible ScriptHash a => SerialisedScript -> a
vsh script = coerce $ ScriptHash (toBuiltin (hashScript script))

factoryBootSettingsFile :: ByteString
factoryBootSettingsFile = $(embedFile "test/data/factory-boot-settings.json")

testFactoryBootSettings :: FactoryBootSettings
testFactoryBootSettings =
  case Aeson.eitherDecodeStrict' factoryBootSettingsFile of
    Left bad -> error ("Could not load factory boot settings: " ++ bad)
    Right b -> b

testTreasuryBootSettings :: TreasuryBootSettings
testTreasuryBootSettings = TreasuryBootSettings $ ProtocolBootUTXO $
  TxOutRef (mkTxId "treasuryBoot") 1

testSwapFees :: SwapFees
testSwapFees = SwapFees (Ratio.fromGHC 0.01)

testScoopFee :: Integer
testScoopFee = 2_500_000

testDeadFactory :: SerialisedScript
testDeadFactory =
  Sundae.deadFactoryScript factoryBootCS poolHash deadPoolHash poolCS

testFactory :: SerialisedScript
testFactory =
  Sundae.factoryScript upgradeSettings factoryBootCS deadFactoryHash poolHash poolCS

testPoolMint :: SerialisedScript
testPoolMint =
  Sundae.poolMintingScript (coerce $ currencySymbolOf factoryBootMint) (OldPoolCurrencySymbol $ CurrencySymbol "")

testPool :: SerialisedScript
testPool =
  Sundae.poolScript factoryBootCS poolCS scooperFeeHolderHash escrowHash

testEscrow :: SerialisedScript
testEscrow =
  Sundae.escrowScript poolCS

testDeadPool :: SerialisedScript
testDeadPool =
  Sundae.deadPoolScript poolCS escrowHash

factoryBootMint :: SerialisedScript
factoryBootMint =
  Sundae.factoryBootMintingScript testFactoryBootSettings

testScooperLicense :: SerialisedScript
testScooperLicense =
  Sundae.scooperFeeScript scooperFeeSettings giftHash factoryBootCS

treasuryBootMint :: SerialisedScript
treasuryBootMint =
  Sundae.treasuryBootMintingScript testTreasuryBootSettings

sundaeMint :: SerialisedScript
sundaeMint =
  Sundae.sundaeMintingScript treasuryBootCS

testTreasury :: SerialisedScript
testTreasury =
  Sundae.treasuryScript upgradeSettings treasuryBootCS sundaeCS poolCS

testGift :: SerialisedScript
testGift =
  Sundae.giftScript treasuryBootCS

scooperFeeSettings :: ScooperFeeSettings
scooperFeeSettings = ScooperFeeSettings 0

upgradeSettings :: UpgradeSettings
upgradeSettings = UpgradeSettings 0 (AssetClass ("", ""))

factoryBootCS :: FactoryBootCurrencySymbol
factoryBootCS =
  FactoryBootCurrencySymbol $ currencySymbolOf factoryBootMint

poolCS :: PoolCurrencySymbol
poolCS =
  PoolCurrencySymbol $ currencySymbolOf testPoolMint

poolSH :: PoolScriptHash
poolSH = vsh testPool

treasuryBootCS :: TreasuryBootCurrencySymbol
treasuryBootCS =
  TreasuryBootCurrencySymbol $ currencySymbolOf treasuryBootMint

sundaeCS :: SundaeCurrencySymbol
sundaeCS =
  SundaeCurrencySymbol $ currencySymbolOf sundaeMint

toCoin :: ByteString -> AssetClass
toCoin str = AssetClass (currencySymbol str, tokenName str)

factoryAC :: AssetClass
factoryAC =
  AssetClass (coerce factoryBootCS, factoryToken)

liquidityAC :: BuiltinByteString -> AssetClass
liquidityAC poolIdent =
  AssetClass (coerce poolCS, computeLiquidityTokenName poolIdent)

poolAC :: BuiltinByteString -> AssetClass
poolAC poolIdent =
  AssetClass (coerce poolCS, computePoolTokenName poolIdent)

scooperTokenAC :: Ident -> AssetClass
scooperTokenAC week =
  AssetClass (coerce factoryBootCS, computeScooperTokenName week)

treasuryHash :: TreasuryScriptHash
treasuryHash = vsh testTreasury

giftHash :: GiftScriptHash
giftHash = vsh testGift

poolHash :: PoolScriptHash
poolHash = vsh testPool

deadFactoryHash :: DeadFactoryScriptHash
deadFactoryHash = vsh testDeadFactory

deadPoolHash :: DeadPoolScriptHash
deadPoolHash = vsh testDeadPool

scooperFeeHolderHash :: ScooperFeeHolderScriptHash
scooperFeeHolderHash = vsh testScooperLicense

escrowHash :: EscrowScriptHash
escrowHash = vsh testScooperLicense

poolAddress :: Address
poolAddress = scriptHashToAddress $ vsh testPool

escrowAddress :: Address
escrowAddress = scriptHashToAddress $ vsh testEscrow

factoryAddress :: Address
factoryAddress = scriptHashToAddress $ vsh testFactory

scooperAddress :: Address
scooperAddress = scriptHashToAddress $ vsh testScooperLicense

scriptHashToAddress :: BuiltinByteString -> Address
scriptHashToAddress bs = Address (ScriptCredential (ScriptHash bs)) Nothing

mkTxId :: BuiltinByteString -> TxId
mkTxId = TxId . Plutus.sha2_256

mkUserAddr :: BuiltinByteString -> Address
mkUserAddr = flip Address Nothing . PubKeyCredential . mkUsrPkh

mkUsrPkh :: BuiltinByteString -> PubKeyHash
mkUsrPkh = PubKeyHash . Plutus.sha2_256

mkUserInput :: BuiltinByteString -> Address -> Value -> TxInInfo
mkUserInput txName usr value =
  TxInInfo
    (TxOutRef (mkTxId txName) 1)
    (TxOut usr value NoOutputDatum Nothing)

mkScriptInput :: BuiltinByteString -> Address -> Value -> DatumHash -> TxInInfo
mkScriptInput txName scriptAddr value datumHash =
  TxInInfo
    (TxOutRef (mkTxId txName) 1)
    (TxOut scriptAddr value (OutputDatumHash datumHash) Nothing)

mkReferenceInput :: BuiltinByteString -> Address -> Value -> DatumHash -> TxInInfo
mkReferenceInput txName addr value datumHash =
  TxInInfo
    (TxOutRef (mkTxId txName) 1)
    (TxOut addr value (OutputDatumHash datumHash) Nothing)

lovelaceValue :: Integer -> Value
lovelaceValue = singleton adaSymbol adaToken

lovelaceValueOf :: Value -> Integer
lovelaceValueOf v = valueOf v adaSymbol adaToken

onlyLovelace :: Value -> Value
onlyLovelace = lovelaceValue . lovelaceValueOf

-- POSIXTime is milliseconds, so 1 hour is represented as 1000 * 60 * 60 ms
hourInterval :: POSIXTime -> Interval POSIXTime
hourInterval t =
  Interval (LowerBound (Finite t) True) (UpperBound (Finite (t + POSIXTime (1000*60*60))) True)

-- Todo: maybe `run` wants to pass in the redeemer as well.
runStep :: [Step] -> Assertion
runStep steps = do
  let info = foldr step baseTxInfo (zip [(0::Integer)..] steps)
  sequence_ $ catMaybes $ fmap (exec info) $ zip [0..] steps
  where
  run (EscrowScriptInput redeemer datum) ctx = runEscrow datum redeemer ctx
  run (PoolScriptInput redeemer datum) ctx = runPool datum redeemer ctx
  run (FactoryScriptInput redeemer datum) ctx = runFactory datum redeemer ctx
  runPool datum redeemer ctx =
    poolContract factoryBootCS poolCS scooperFeeHolderHash escrowHash datum redeemer ctx
  runEscrow datum redeemer ctx =
    escrowContract poolCS datum redeemer ctx
  runFactoryBootMint redeemer ctx =
    factoryBootMintingContract testFactoryBootSettings redeemer ctx
  runTreasuryBootMint ctx =
    treasuryBootMintingContract testTreasuryBootSettings () ctx
  runSundaeMint ctx =
    sundaeMintingContract treasuryBootCS () ctx
  runPoolMint redeemer ctx =
    poolMintingContract factoryBootCS (OldPoolCurrencySymbol $ CurrencySymbol "") poolCS poolSH redeemer ctx
  runFactory =
    factoryContract upgradeSettings factoryBootCS deadFactoryHash poolHash poolCS
  handleErrors = handle (\(_ :: SomeException) -> pure False) . evaluate
  runCond = \case
    Pass -> id
    Fail -> not
  exec :: TxInfo -> (Integer, Step) -> Maybe Assertion
  exec info (ix, FromScript _ _ dbg cond scriptInput') = do
    let txId = mkTxId (fromString ("#" <> show ix))
    pure $ do
      wentThrough <- handleErrors $ run scriptInput' (ScriptContext info (Spending $ TxOutRef txId 1))
      let passes = runCond cond wentThrough
      passes @? dbg
  exec info (_, PoolMint _ cond ident) = do
    pure $ do
      wentThrough <- handleErrors $ runPoolMint (MintLP ident) (ScriptContext info (Minting $ coerce poolCS)) `seq` True
      let passes = runCond cond wentThrough
      passes @? "pool mint failure"
  exec info (_, FactoryBootMint _ cond redeemer) = do
    pure $ do
      wentThrough <- handleErrors $ runFactoryBootMint redeemer (ScriptContext info (Minting $ coerce treasuryBootCS))
      let passes = runCond cond wentThrough
      passes @? "factory boot mint failure"
  exec info (_, TreasuryBootMint _ cond) = do
    pure $ do
      wentThrough <- handleErrors $ runTreasuryBootMint (ScriptContext info (Minting $ coerce treasuryBootCS))
      let passes = runCond cond wentThrough
      passes @? "treasury boot mint failure"
  exec info (_, SundaeMint _ cond) = do
    pure $ do
      wentThrough <- handleErrors $ runSundaeMint (ScriptContext info (Minting $ coerce sundaeCS))
      let passes = runCond cond wentThrough
      passes @? "treasury mint failure"
  exec _ _ = Nothing
  mkDatumHash :: ToData a => a -> DatumHash
  mkDatumHash x = DatumHash (toBuiltin (Hash.blake2b_256 (LBS.toStrict (serialise (toData x)))))
  scriptInputData = \case
    EscrowScriptInput _ d -> (mkDatumHash d, toDatum d)
    PoolScriptInput _ d -> (mkDatumHash d, toDatum d)
    FactoryScriptInput _ d -> (mkDatumHash d, toDatum d)
  baseTxInfo =
    TxInfo
      { txInfoInputs = []
      , txInfoOutputs = []
      , txInfoReferenceInputs = []
      , txInfoFee = lovelaceValue 1
      , txInfoMint = mempty
      , txInfoDCert = []
      , txInfoWdrl = fromList []
      , txInfoValidRange = always
      , txInfoSignatories = []
      , txInfoRedeemers = fromList []
      , txInfoData = fromList []
      , txInfoId = mkTxId "#testOut"}
  step (ix, c) info =
    let txIx = fromString ("#" <> show ix)
    in case c of
      FromScript addr v _ _ (scriptInputData -> (hash, d)) ->
       info & tiInputs %~ (mkScriptInput txIx addr v (hash):)
            & tiData %~ AssocMap.insert hash d
      FromUser addr v ->
        info & tiInputs %~ (mkUserInput txIx addr v:)
      ToUser addr v ->
        info & tiOutputs %~ (TxOut addr v NoOutputDatum Nothing:)
      ToScript addr v (BuiltinData -> d) ->
        info & tiOutputs %~ (TxOut addr v (OutputDatumHash $ mkDatumHash d) Nothing:)
             & tiData %~ AssocMap.insert (mkDatumHash d) (toDatum d)
      ReferenceInput addr v (BuiltinData -> d) ->
        info & tiReferenceInputs %~ (mkReferenceInput txIx addr v (mkDatumHash d):)
             & tiData %~ AssocMap.insert (mkDatumHash d) (toDatum d)
      PoolMint v _ _->
        info & tiMint %~ (<> v)
      FactoryBootMint v _ _ ->
        info & tiMint %~ (<> v)
      TreasuryBootMint v _ ->
        info & tiMint %~ (<> v)
      SundaeMint v _ ->
        info & tiMint %~ (<> v)
      CustomInterval v ->
        info & tiValidRange .~ v
      CustomSignatories v ->
        info & tiSignatories %~ (++ v)
