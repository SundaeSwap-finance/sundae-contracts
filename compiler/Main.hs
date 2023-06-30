{-# LANGUAGE ApplicativeDo              #-}

module Main (main) where

import Options.Applicative qualified as O
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as Short
import Data.ByteString.Base16 qualified as Hex
import Data.Map qualified as Map
import Data.Text.Encoding qualified as Text
import Data.Coerce (coerce, Coercible)
import Data.Foldable (for_)
import Data.String (fromString)
import System.Directory
import System.FilePath

import Sundae.Contracts.Common
import Sundae.Compiled

import PlutusLedgerApi.V2 qualified as Plutus
import PlutusLedgerApi.V1.Value (AssetClass(..), assetClass)

data Format = Raw | Hex | Json

data Strip = Strip | Don'tStrip

data CompilationSettingsSource = StdIn | InFile FilePath

data Script
    = FactoryMint
    | FactoryValidator
    | FactoryBootCS
    | PoolMint
    | PoolValidator
    | EscrowValidator
    deriving (Enum, Bounded)

data CompilationTarget = All | Script Script

data Destination
  = OutDirectory FilePath
  | OutFile FilePath
  | StdOut

data CompileConfig = CompileConfig
    { compileTarget :: CompilationTarget
    , compileSettings :: CompilationSettingsSource
    , compileFormat :: Format
    , compileStrip :: Strip
    , compileDestination :: Destination
    }

data Request
    = Compile CompileConfig

getConfig :: IO Request
getConfig = O.execParser $ O.info parser $ mconcat
    [ O.fullDesc
    , O.progDesc "Compile sundae contracts to disk"
    , O.header "compiler - Compile SundaeSwap contracts"
    ]
    where
        parser :: O.Parser Request
        parser = O.subparser $ mconcat
            [ O.command "compile" (O.info (fmap Compile pCompile) (O.progDesc "compile a single script"))
            ]
        pCompile :: O.Parser CompileConfig
        pCompile = do
            compilationTarget <- pTarget
            settingsSource <- pSettings
            format <- pFormat
            strip <- pStrip
            dest <- pDestination
            pure $ CompileConfig compilationTarget settingsSource format strip dest

        pTarget :: O.Parser CompilationTarget
        pTarget
            =     (O.flag' (Script FactoryValidator) (O.long "factory" <> O.help "compile the factory script"))
            O.<|> (O.flag' (Script FactoryBootCS) (O.long "factory-boot-cs" <> O.help "compile the factory boot currency symbol"))
            O.<|> (O.flag' (Script FactoryMint) (O.long "factory-mint" <> O.help "compile the factory mint script"))
            O.<|> (O.flag' (Script PoolValidator) (O.long "pool" <> O.help "compile the pool script"))
            O.<|> (O.flag' (Script PoolMint) (O.long "pool-mint" <> O.help "compile the pool mint script"))
            O.<|> (O.flag' (Script EscrowValidator) (O.long "escrow" <> O.help "compile the escrow script"))
            O.<|> (O.flag' All (O.long "all" <> O.help "compile the all scripts"))

        pSettings :: O.Parser CompilationSettingsSource
        pSettings = InFile <$> (O.strOption $ mconcat
            [ O.long "factory-boot-settings"
            , O.metavar "FACTORY-BOOT"
            , O.help "The factory boot settings to parameterize the contracts with"
            ]) O.<|> pure StdIn

        pFormat = pRaw O.<|> pHex O.<|> pJson O.<|> pure Raw
        pRaw = O.flag' Raw (O.long "raw" <> O.help "output the script as raw binary")
        pHex = O.flag' Hex (O.long "hex" <> O.help "output the script hex-encoded")
        pJson = O.flag' Json (O.long "json" <> O.help "output the script as a JSON file")

        pStrip = O.flag' Strip (O.long "strip" <> O.help "strip the cbor metadata")
          O.<|> pure Don'tStrip

        pDestination :: O.Parser Destination
        pDestination = OutFile <$> (O.strOption $ mconcat
            [ O.long "out"
            , O.metavar "DEST"
            , O.help "The output file or directory to write compilation results to"
            ]) O.<|> pure StdOut

readFactoryBootSettings :: CompilationSettingsSource -> IO (Either String FactoryBootSettings)
readFactoryBootSettings StdIn = do
    contents <- BL.getContents
    return $ Aeson.eitherDecode contents

makeValidatorScriptHash :: Coercible Plutus.ScriptHash a => Plutus.SerialisedScript -> a
makeValidatorScriptHash script = coerce $ Plutus.ScriptHash (Plutus.toBuiltin (hashScript script))

makeCurrencySymbol :: Coercible Plutus.ScriptHash a => Plutus.SerialisedScript -> a
makeCurrencySymbol = makeValidatorScriptHash

main :: IO ()
main = do
    getConfig >>= \case
        Compile (CompileConfig target source format strip destination) -> do
            settings <- readFactoryBootSettings source >>= \case
                Left e -> fail e -- todo(pi): Error messages etc.
                Right s -> pure s
            let
                -- Hard coding these until they gets ripped out
                oldPoolCurrencySymbol = OldPoolCurrencySymbol "00000000000000000000000000000000000000000000000000000000"
                upgradeSettings = UpgradeSettings
                    { upgradeTimeLockPeriod = 0
                    , upgradeAuthentication = AssetClass ("00000000000000000000000000000000000000000000000000000000", "00")
                    }

                -- Factory related scripts
                factoryMintScript = factoryBootMintingScript settings
                factoryCurrencySymbol = makeCurrencySymbol factoryMintScript :: FactoryBootCurrencySymbol
                factoryValidatorScript = factoryScript factoryCurrencySymbol

                -- Pool related scripts
                poolMintScript = poolMintingScript
                poolCurrencySymbol = makeCurrencySymbol poolMintScript :: PoolCurrencySymbol
                poolValidatorScript = poolScript factoryCurrencySymbol escrowScriptHash

                -- Escrow related scripts
                escrowValidatorScript = escrowScript poolCurrencySymbol
                escrowScriptHash = makeValidatorScriptHash escrowValidatorScript

                encodeMany :: Map.Map String BS8.ByteString -> BS8.ByteString
                encodeMany = case format of
                  Raw -> BS8.intercalate "\n" . Map.elems
                  Hex -> BS8.intercalate "\n" . map Hex.encode . Map.elems
                  Json -> BL.toStrict . Aeson.encode . Map.map (Text.decodeUtf8 . Hex.encode)

                encode :: Aeson.Key -> BS8.ByteString -> BS8.ByteString
                encode name script = case format of
                  Raw -> script
                  Hex -> Hex.encode script
                  Json -> BL.toStrict . Aeson.encode . Aeson.object $
                    [ (name, Aeson.String $ Text.decodeUtf8 $ Hex.encode script)
                    ]

                stripCbor :: BS8.ByteString -> BS8.ByteString
                stripCbor bytes = do
                  let lenSize = \case
                        0x58 -> Just 1
                        0x59 -> Just 2
                        0x5a -> Just 4
                        0x5b -> Just 8
                  case BS.uncons bytes of
                    Just (hd, tl) ->
                      case lenSize hd of
                        Just n -> BS.drop n tl
                        Nothing -> error "stripCbor: Unexpected CBOR format"
                    Nothing -> error "stripCbor: Empty bytestring"

                doStrip :: Strip -> BS8.ByteString -> BS8.ByteString
                doStrip s bytes =
                  case s of
                    Strip -> stripCbor bytes
                    Don'tStrip -> bytes

                infoForTarget :: Script -> (String, BS8.ByteString)
                infoForTarget = \case
                  FactoryMint -> ("factory-mint", doStrip strip $ Short.fromShort factoryMintScript)
                  FactoryValidator -> ("factory-validator", doStrip strip $ Short.fromShort factoryValidatorScript)
                  FactoryBootCS -> ("factory-boot-cs", Plutus.fromBuiltin @Plutus.BuiltinByteString $ coerce factoryCurrencySymbol)
                  PoolMint -> ("pool-mint", doStrip strip $ Short.fromShort poolMintScript)
                  PoolValidator -> ("pool-validator", doStrip strip $ Short.fromShort poolValidatorScript)
                  EscrowValidator -> ("escrow-validator", doStrip strip $ Short.fromShort escrowValidatorScript)

                targets :: CompilationTarget -> [(String, BS8.ByteString)]
                targets = \case
                  Script s -> [infoForTarget s]
                  All -> map infoForTarget [minBound..maxBound]

                output :: [(String, BS8.ByteString)] -> Destination -> IO ()
                output files = \case
                  OutDirectory file -> do
                    let targetDirectory = dropFileName file
                    createDirectoryIfMissing True targetDirectory
                    for_ files \(f, scr) -> do
                      let path = targetDirectory ++ f
                      BS.writeFile path (encode (fromString f) scr)
                  OutFile file -> BS.writeFile file (encodeMany (Map.fromList files))
                  StdOut -> BS8.putStrLn (encodeMany (Map.fromList files))

            -- If --out was passed, but target is All, use OutDirectory instead
            -- of OutFile. We can't do this in the parser itself since it's not
            -- in a monad, but explicitly distinguishing OutFile and
            -- OutDirectory in the destination type lets us decouple encoding
            -- and output
            output (targets target) $ case (target, destination) of
              (All, OutFile f) -> OutDirectory f
              (_, other) -> other
