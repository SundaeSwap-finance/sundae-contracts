{-# language TypeApplications #-}
{-# language ViewPatterns #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}

module Test.Contracts.Utils where

import Codec.Serialise hiding (decode, Fail)
import Control.Exception
import Control.Lens hiding (ix)
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Coerce(coerce)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe (catMaybes)
import Data.String(fromString)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import qualified Ledger.Scripts as UTScripts
import Ledger.Typed.Scripts
import qualified PlutusTx.Ratio as Ratio
import qualified PlutusTx.Prelude as Plutus
import qualified Ledger as Plutus

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Crypto as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Hashes as Ledger

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
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
  | PoolMint Value Cond Ident
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

tiInputs :: Lens' TxInfo [TxInInfo]
tiInputs f info =
  (\i' -> info{txInfoInputs = i'}) <$> f (txInfoInputs info)

tiOutputs :: Lens' TxInfo [TxOut]
tiOutputs f info =
  (\i' -> info{txInfoOutputs = i'}) <$> f (txInfoOutputs info)

tiMint :: Lens' TxInfo Value
tiMint f info =
  (\i' -> info{txInfoMint = i'}) <$> f (txInfoMint info)

tiData :: Lens' TxInfo [(DatumHash, Datum)]
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

currencySymbolOf :: MintingPolicy -> CurrencySymbol
currencySymbolOf = transMintingScriptHash . plutusScriptHash . getMintingPolicy

toDatum :: ToData a => a -> Datum
toDatum = Datum . BuiltinData . toData

toScriptData :: ToData a => a -> Cardano.ScriptData
toScriptData d = Cardano.fromAlonzoData $ Alonzo.Data $ toData d

addressOf :: Script -> Cardano.Address Cardano.ShelleyAddr
addressOf script =
  Cardano.ShelleyAddress Ledger.Testnet (Ledger.ScriptHashObj (plutusScriptHash script)) Ledger.StakeRefNull

transMintingScriptHash :: Ledger.ScriptHash c -> CurrencySymbol
transMintingScriptHash h = case Alonzo.transScriptHash h of ValidatorHash vhsh -> CurrencySymbol vhsh

serialiseScript :: Plutus.Script -> Cardano.PlutusScript Cardano.PlutusScriptV1
serialiseScript script =
  Cardano.PlutusScriptSerialised $ SBS.toShort $ LBS.toStrict $ serialise script :: Cardano.PlutusScript Cardano.PlutusScriptV1

plutusScriptHash :: Script -> Ledger.ScriptHash Ledger.StandardCrypto
plutusScriptHash script =
  Ledger.hashScript @(Cardano.ShelleyLedgerEra Cardano.AlonzoEra) $
    Alonzo.PlutusScript Alonzo.PlutusV1 (case serialiseScript script of Cardano.PlutusScriptSerialised scr -> scr)

testFactoryBootSettings :: FactoryBootSettings
testFactoryBootSettings = unsafePerformIO $
  Aeson.decodeFileStrict' "factory-boot-settings.json" >>= \case
    Nothing -> fail "Could not load factory boot settings."
    Just b -> pure b

testTreasuryBootSettings :: TreasuryBootSettings
testTreasuryBootSettings = TreasuryBootSettings $ ProtocolBootUTXO $
  TxOutRef (mkTxId "treasuryBoot") 1

testSwapFees :: SwapFees
testSwapFees = SwapFees (Ratio.fromGHC 0.01)

testScoopFee :: Integer
testScoopFee = 2_500_000

testDeadFactory :: TypedValidator DeadFactory
testDeadFactory =
  Sundae.deadFactoryScript factoryBootCS poolHash deadPoolHash poolCS

testFactory :: TypedValidator Factory
testFactory =
  Sundae.factoryScript upgradeSettings factoryBootCS deadFactoryHash poolHash poolCS

testPoolMint :: MintingPolicy
testPoolMint =
  Sundae.poolMintingScript (coerce $ currencySymbolOf factoryBootMint) (OldPoolCurrencySymbol $ CurrencySymbol "")

testPool :: TypedValidator Pool
testPool =
  Sundae.poolScript factoryBootCS poolCS scooperFeeHolderHash escrowHash

testEscrow :: TypedValidator Escrow
testEscrow =
  Sundae.escrowScript poolCS

testDeadPool :: TypedValidator DeadPool
testDeadPool =
  Sundae.deadPoolScript poolCS escrowHash

factoryBootMint :: MintingPolicy
factoryBootMint =
  Sundae.factoryBootMintingScript testFactoryBootSettings

testScooperLicense :: TypedValidator ScooperFeeHolder
testScooperLicense =
  Sundae.scooperFeeScript scooperFeeSettings giftHash factoryBootCS

treasuryBootMint :: MintingPolicy
treasuryBootMint =
  Sundae.treasuryBootMintingScript testTreasuryBootSettings

sundaeMint :: MintingPolicy
sundaeMint =
  Sundae.sundaeMintingScript treasuryBootCS

testTreasury :: TypedValidator Treasury
testTreasury =
  Sundae.treasuryScript upgradeSettings treasuryBootCS sundaeCS poolCS

testGift :: TypedValidator Gift
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

treasuryBootCS :: TreasuryBootCurrencySymbol
treasuryBootCS =
  TreasuryBootCurrencySymbol $ currencySymbolOf treasuryBootMint

sundaeCS :: SundaeCurrencySymbol
sundaeCS =
  SundaeCurrencySymbol $ currencySymbolOf sundaeMint

toCurrencySymbol :: ValidatorHash -> CurrencySymbol
toCurrencySymbol (ValidatorHash h) = CurrencySymbol h

toCoin :: ByteString -> AssetClass
toCoin str = AssetClass (currencySymbol str, tokenName str)

mkDatumHash :: ToData a => a -> DatumHash
mkDatumHash = UTScripts.datumHash . Datum . BuiltinData . toData

factoryAC :: AssetClass
factoryAC =
  AssetClass (toCurrencySymbol $ coerce factoryBootCS, factoryToken)

liquidityAC :: Ident -> AssetClass
liquidityAC poolIdent =
  AssetClass (toCurrencySymbol $ coerce poolCS, computeLiquidityTokenName poolIdent)

poolAC :: Ident -> AssetClass
poolAC poolIdent =
  AssetClass (toCurrencySymbol $ coerce poolCS, computePoolTokenName poolIdent)

scooperTokenAC :: Ident -> AssetClass
scooperTokenAC week =
  AssetClass (coerce factoryBootCS, computeScooperTokenName week)

treasuryHash :: TreasuryScriptHash
treasuryHash =
  TreasuryScriptHash $ Alonzo.transScriptHash $ plutusScriptHash $ unValidatorScript $ validatorScript testTreasury

giftHash :: GiftScriptHash
giftHash =
  GiftScriptHash $ Alonzo.transScriptHash $ plutusScriptHash $ unValidatorScript $ validatorScript testGift

poolHash :: PoolScriptHash
poolHash =
  PoolScriptHash $ Alonzo.transScriptHash $ plutusScriptHash $ unValidatorScript $ validatorScript testPool

deadFactoryHash :: DeadFactoryScriptHash
deadFactoryHash =
  DeadFactoryScriptHash $ Alonzo.transScriptHash $ plutusScriptHash $ unValidatorScript $ validatorScript testDeadFactory

deadPoolHash :: DeadPoolScriptHash
deadPoolHash =
  DeadPoolScriptHash $ Alonzo.transScriptHash $ plutusScriptHash $ unValidatorScript $ validatorScript testDeadPool

scooperFeeHolderHash :: ScooperFeeHolderScriptHash
scooperFeeHolderHash =
  ScooperFeeHolderScriptHash $ Alonzo.transScriptHash $ plutusScriptHash $ unValidatorScript $ validatorScript testScooperLicense

escrowHash :: EscrowScriptHash
escrowHash =
  EscrowScriptHash $ Alonzo.transScriptHash $ plutusScriptHash $ unValidatorScript $ validatorScript testScooperLicense

poolAddress :: Address
poolAddress =
  toPlutusAddr $ addressOf $ unValidatorScript $ validatorScript testPool

escrowAddress :: Address
escrowAddress =
  toPlutusAddr $ addressOf $ unValidatorScript $ validatorScript testEscrow

factoryAddress :: Address
factoryAddress =
  toPlutusAddr $ addressOf $ unValidatorScript $ validatorScript testFactory

scooperAddress :: Address
scooperAddress =
  toPlutusAddr $ addressOf $ unValidatorScript $ validatorScript testScooperLicense

toPlutusAddr :: Cardano.Address Cardano.ShelleyAddr -> Address
toPlutusAddr (Cardano.ShelleyAddress _ (Ledger.KeyHashObj (Ledger.KeyHash (Crypto.UnsafeHash pk))) staking) =
  Address (PubKeyCredential $ PubKeyHash $ toBuiltin (SBS.fromShort pk)) transStaking
  where
  transStaking =
    case staking of
      Ledger.StakeRefNull ->
        Nothing
      Ledger.StakeRefBase (Ledger.KeyHashObj (Ledger.KeyHash (Crypto.UnsafeHash sk))) ->
        Just $ StakingHash $ PubKeyCredential $ PubKeyHash $ toBuiltin (SBS.fromShort sk)
      _ -> error $ "transFromAddr: mysterious staking credential: " <> show staking
toPlutusAddr (Cardano.ShelleyAddress _ (Ledger.ScriptHashObj (Ledger.ScriptHash (Crypto.UnsafeHash pk))) _) =
  Address (ScriptCredential (ValidatorHash $ toBuiltin (SBS.fromShort pk))) Nothing

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
    (TxOut usr value Nothing)

mkScriptInput :: BuiltinByteString -> Address -> Value -> Maybe DatumHash -> TxInInfo
mkScriptInput txName scriptAddr value datumHash =
  TxInInfo
    (TxOutRef (mkTxId txName) 1)
    (TxOut scriptAddr value datumHash)

lovelaceValue :: Integer -> Value
lovelaceValue = singleton adaSymbol adaToken

lovelaceValueOf :: Value -> Integer
lovelaceValueOf v = valueOf v adaSymbol adaToken

onlyLovelace :: Value -> Value
onlyLovelace = lovelaceValue . lovelaceValueOf

hourInterval :: POSIXTime -> Interval POSIXTime
hourInterval t =
  Interval (LowerBound (Finite (t - POSIXTime (1000*60*60))) True) (UpperBound (Finite t) True)

-- Todo: maybe `run` wants to pass in the redeemer as well.
runStep :: [Step] -> Assertion
runStep steps = do
  let info = nubbed $ foldr step baseTxInfo (zip [(0::Integer)..] steps)
  sequence_ $ catMaybes $ fmap (exec info) $ zip [0..] steps
  where
  run (EscrowScriptInput redeemer datum) ctx = runEscrow datum redeemer (SD.ScriptContext__ (toBuiltinData ctx))
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
    poolMintingContract factoryBootCS (OldPoolCurrencySymbol $ CurrencySymbol "") (toBuiltinData redeemer) (toBuiltinData ctx)
  runFactory =
    factoryContract upgradeSettings factoryBootCS deadFactoryHash poolHash poolCS
  nubbed info =
    info & tiData %~ nubOrdOn fst
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
      wentThrough <- handleErrors $ runPoolMint (toBuiltinData ident) (toBuiltinData $  ScriptContext info (Minting $ coerce poolCS)) `seq` True
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
  scriptInputData = \case
    EscrowScriptInput _ d -> (mkDatumHash d, toDatum d)
    PoolScriptInput _ d -> (mkDatumHash d, toDatum d)
    FactoryScriptInput _ d -> (mkDatumHash d, toDatum d)
  baseTxInfo =
    TxInfo
      { txInfoInputs = []
      , txInfoOutputs = []
      , txInfoFee = lovelaceValue 1
      , txInfoMint = mempty
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoValidRange = always
      , txInfoSignatories = []
      , txInfoData = []
      , txInfoId = mkTxId "#testOut"}
  step (ix, c) info =
    let txIx = fromString ("#" <> show ix)
    in case c of
      FromScript addr v _ _ (scriptInputData -> (hash, d)) ->
       info & tiInputs %~ (mkScriptInput txIx addr v (Just hash):)
            & tiData %~ ((hash, d):)
      FromUser addr v ->
        info & tiInputs %~ (mkUserInput txIx addr v:)
      ToUser addr v ->
        info & tiOutputs %~ (TxOut addr v Nothing:)
      ToScript addr v (BuiltinData -> d) ->
        info & tiOutputs %~ (TxOut addr v (Just $ mkDatumHash d):)
             & tiData %~ ((mkDatumHash d, toDatum d):)
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
