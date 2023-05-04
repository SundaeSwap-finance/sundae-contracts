{-# options_ghc -Wincomplete-patterns #-}
{-# options_ghc -Wpartial-type-signatures #-}
{-# options_ghc -Wmissing-signatures #-}

{-# language CPP #-}
{-# language ViewPatterns #-}
{-# language TypeApplications #-}
{-# language NumericUnderscores #-}
{-# language PartialTypeSignatures #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
{-# language GADTs #-}
{-# language FlexibleContexts #-}
{-# language ApplicativeDo #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language StandaloneDeriving #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language TemplateHaskell #-}
module Sundae.GeneratePoisonPill
  ( genPoisonPill
  , genPoisonPill_
  , seqToFun  -- for testing, eventually
  )  where

import Prelude
import Codec.Serialise (Serialise, serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Data.ByteString.Base16 qualified as BS
import Cardano.Api.Shelley as Cardano (AlonzoEra, ShelleyLedgerEra, PlutusScript (..), PlutusScriptV1)
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import qualified Cardano.Ledger.Era as Ledger
import qualified Ledger as Plutus
import Ledger (ValidatorHash (..))
import qualified Cardano.Ledger.Hashes as Hashes
import Cardano.Ledger.Crypto (StandardCrypto)
import PlutusTx.Builtins.Class
import qualified PlutusTx.Builtins
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import System.Random (genByteString)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import Control.Monad.IO.Class
import qualified PlutusTx.Prelude
import System.Random.Internal (StdGen (..))
import qualified System.Random.SplitMix as SM
import qualified Data.Sequence as Seq
import Data.Word (Word8)
import Language.Haskell.TH.Syntax (Q, Lift, addDependentFile)
import Language.Haskell.TH.Syntax.Compat (Splice, liftSplice)
import qualified Language.Haskell.TH.Syntax.Compat as Compat
import Data.Aeson (decodeFileStrict', encodeFile, FromJSON(..), ToJSON(..), withText, Value(String))

import Sundae.Contracts.Common
import Sundae.Compiled.Mints
import System.Directory (makeAbsolute)
import Control.Exception


serialiseScript :: Serialise a => a -> PlutusScript PlutusScriptV1
serialiseScript script =
  PlutusScriptSerialised $ SBS.toShort $ LBS.toStrict $ serialise script :: PlutusScript PlutusScriptV1

plutusScriptHash :: Plutus.Script -> Hashes.ScriptHash StandardCrypto
plutusScriptHash script =
  Ledger.hashScript @(ShelleyLedgerEra AlonzoEra) $
    Alonzo.PlutusScript Alonzo.PlutusV1 (case serialiseScript script of PlutusScriptSerialised scr -> scr)

hashPlutusScript :: Plutus.Script -> ByteString
hashPlutusScript = transMintingScriptHash . plutusScriptHash
  where
    transMintingScriptHash h = case Alonzo.transScriptHash h of ValidatorHash vhsh -> fromBuiltin vhsh

hashMintingPolicy :: Scripts.MintingPolicy -> ByteString
hashMintingPolicy = hashPlutusScript . Scripts.getMintingPolicy

-- Get an StdGen using /dev/urandom if available.
initStdGen :: MonadIO m => m StdGen
initStdGen = liftIO (StdGen <$> SM.initSMGen)

-- | Given the hash of a (factory) minting policy and a sequence of 16 random
-- bytes, produce a randomized poison pill, represented as a sequence of
-- integers. If the number of initial bytes is wrong, this returns Nothing.
-- The implementation is a lousy translation of Javascript. I hope
-- it's right.
genPoisonPill_ :: ByteString -> Seq.Seq Word8 -> Maybe (Seq.Seq PlutusTx.Prelude.Integer)
genPoisonPill_ factory initialBytes
  -- We check the length of the initial random bytes in case we loaded
  -- them from a file and we've since changed the number of bytes
  -- we use.
  | Seq.length initialBytes == 16 = Just $ fmap fromIntegral $ go 0 10 initialBytes
  | otherwise = Nothing
  where
    go :: Int -> Int -> Seq.Seq Word8 -> Seq.Seq Word8
    go i idx bytes
      | i == 7 = bytes
      | idx == 1 = bytes'
      | even idx = go (i + 1) (idx `quot` 2) bytes'
      | otherwise = go (i + 1) (idx * 3 + 1) bytes'
      where
        -- Note: the sum is taken mod 256
        bytes' = Seq.update (idx - 1) ((factory `BS.index` i) + fromIntegral idx) bytes

-- | Generate 16 random bytes
getInitialBytes :: MonadIO m => m (Seq.Seq Word8)
getInitialBytes = do
  gen <- initStdGen
  (bytes, _gen) <- pure $ genByteString 16 gen
  pure (Seq.fromList (BS.unpack bytes))

-- | Given a sequence @s@, produce a splice for a function
-- equivalent to @'Seq.index' s . fromIntegral@. The function
-- uses binary search to produce the appropriate value.
seqToFun :: Lift a => Seq.Seq a -> Splice Q (PlutusTx.Prelude.Integer -> a)
seqToFun = go (0 :: Int)
  where
    -- go :: Int -> Seq.Seq a -> Splice Q (Integer -> a)
    go start s = case Seq.length s of
      0 -> [|| \_ -> PlutusTx.Prelude.error () ||]
      1 -> let v = Seq.index s 0
           in [|| \_ -> v ||]
      n -> case Seq.splitAt (n `quot` 2) s of
             (l, r) -> [|| \i -> if i `PlutusTx.Builtins.lessThanInteger` n'
                                 then $$(go start l) i
                                 else $$(go (start + n `quot` 2) r) i ||]
        where n' = fromIntegral (start + n `quot` 2)

getFactoryBootSettings :: Q FactoryBootSettings
getFactoryBootSettings = do
  let path = "factory-boot-settings.json"
  liftIO (makeAbsolute path) >>= addDependentFile
  liftIO (decodeFileStrict' path) >>= \case
    Nothing -> fail "Could not load factory boot settings."
    Just b -> pure b

newtype BytesHex = BytesHex ByteString

instance FromJSON BytesHex where
  parseJSON = withText "base-16 encoded bytes" \s ->
    case BS.decodeBase16 (Text.encodeUtf8 s) of
      Right ok -> pure (BytesHex ok)
      Left err -> fail ("FromJSON BytesHex: decodeBase16: " ++ Text.unpack err)

instance ToJSON BytesHex where
  toJSON (BytesHex bs) = String (BS.encodeBase16 bs)

-- | Create a splice for a poison pill using factory boot settings
-- in a file.
genPoisonPill :: Splice Q (PlutusTx.Prelude.Integer -> PlutusTx.Prelude.Integer)
genPoisonPill = liftSplice $ do
  mph <- hashMintingPolicy . factoryBootMintingScript <$> getFactoryBootSettings
  let path = "cached_poison_randoms.json"
  -- We depend on this file too, just so we have the option of forcing
  -- recompilation (and rerandomization) by deleting it. I don't think this
  -- will happen much.
  liftIO (makeAbsolute path) >>= addDependentFile
  maybe_old_poison_randoms <-
    liftIO (try (decodeFileStrict' path)) >>= \case
      Left (_ :: IOException)
        -> do
          logWarning "Couldn't find cached_poison_randoms.json"
          pure Nothing  -- Couldn't read file
      Right Nothing -> do
        logWarning "Found cached_poison_randoms.json but could not decode"
        pure Nothing -- Decoding failed
      Right (Just (BytesHex old_factory_hash, old_poison_randoms))
        | old_factory_hash == mph
        -> do
          logInfo "INFO: Found cached_poison_randoms.json, will reuse"
          pure (Just old_poison_randoms) -- Bingo
        | otherwise
        -> do
          logWarning "WARNING: Found cached_poison_randoms.json, but "
          logWarning "it does not match the boot settings:"
          logWarning ("WARNING:  old hash: " ++ show old_factory_hash)
          logWarning ("WARNING:  new hash: " ++ show mph)
          pure Nothing -- Factory settings have changed
  maybe_old_poison_pill <- pure $ maybe_old_poison_randoms >>= genPoisonPill_ mph
  case maybe_old_poison_pill of
    Just old_poison_pill
         -- Use the old poison pill
      -> Compat.examineSplice (seqToFun old_poison_pill)
    Nothing -> do
      logInfo "INFO: Overwriting new randoms to cached_poison_randoms.json"
      new_randoms <- getInitialBytes
      Just pill <- pure $ genPoisonPill_ mph new_randoms
      -- Cache the new poison pill randoms
      liftIO (encodeFile path (BytesHex mph, new_randoms))
      Compat.examineSplice (seqToFun pill)
  where
  logWarning msg = liftIO (putStrLn ("WARNING: " ++ msg))
  logInfo msg = liftIO (putStrLn ("INFO: " ++ msg))
