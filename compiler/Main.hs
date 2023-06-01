{-# LANGUAGE ApplicativeDo              #-}

module Main (main) where

import Options.Applicative qualified as O
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BS
import Data.ByteString.Base16 qualified as Hex
import Data.Coerce (coerce, Coercible)
import Data.Foldable (traverse_)
import System.Directory
import System.FilePath

import Sundae.Contracts.Common
import Sundae.Compiled

import PlutusLedgerApi.V3 qualified as Plutus
import PlutusLedgerApi.V1.Value (AssetClass(..), assetClass)

data Format = Raw | Hex | Json

data CompilationSettingsSource = StdIn | InFile FilePath

data CompilationTarget
    = FactoryMint
    | FactoryValidator
    | PoolMint
    | PoolValidator
    | EscrowValidator
    | All

data Destination = OutFile FilePath | StdOut

data CompileConfig = CompileConfig
    { compileTarget :: CompilationTarget
    , compileSettings :: CompilationSettingsSource
    , compileFormat :: Format
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
            dest <- pDestination
            pure $ CompileConfig compilationTarget settingsSource format dest
        
        pTarget :: O.Parser CompilationTarget
        pTarget
            =     (O.flag' FactoryValidator (O.long "factory" <> O.help "compile the factory script"))
            O.<|> (O.flag' FactoryMint (O.long "factory-mint" <> O.help "compile the factory mint script"))
            O.<|> (O.flag' PoolValidator (O.long "pool" <> O.help "compile the pool script"))
            O.<|> (O.flag' PoolMint (O.long "pool-mint" <> O.help "compile the pool mint script"))
            O.<|> (O.flag' EscrowValidator (O.long "escrow" <> O.help "compile the escrow script"))
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
        Compile (CompileConfig target source format destination) -> do
            settings <- readFactoryBootSettings source >>= \case
                Left e -> fail e -- todo(pi): Error messages etc.
                Right s -> pure s
            let
                -- Hard coding these until they gets ripped out
                oldPoolCurrencySymbol = OldPoolCurrencySymbol "00000000000000000000000000000000000000000000000000000000"
                scooperFeeHolder = ScooperFeeHolderScriptHash $ Plutus.ScriptHash "00000000000000000000000000000000000000000000000000000000"
                deadFactoryScriptHash = DeadFactoryScriptHash $ Plutus.ScriptHash "00000000000000000000000000000000000000000000000000000000"
                upgradeSettings = UpgradeSettings
                    { upgradeTimeLockPeriod = 0
                    , upgradeAuthentication = AssetClass ("00000000000000000000000000000000000000000000000000000000", "00")
                    }

                -- Factory related scripts
                factoryMintScript = factoryBootMintingScript settings
                factoryCurrencySymbol = makeCurrencySymbol factoryMintScript :: FactoryBootCurrencySymbol
                factoryValidatorScript = factoryScript upgradeSettings factoryCurrencySymbol deadFactoryScriptHash poolScriptHash poolCurrencySymbol

                -- Pool related scripts
                poolMintScript = poolMintingScript factoryCurrencySymbol oldPoolCurrencySymbol
                poolCurrencySymbol = makeCurrencySymbol poolMintScript :: PoolCurrencySymbol
                poolValidatorScript = poolScript factoryCurrencySymbol poolCurrencySymbol scooperFeeHolder escrowScriptHash
                poolScriptHash = makeValidatorScriptHash poolValidatorScript

                -- Escrow related scripts
                escrowValidatorScript = escrowScript poolCurrencySymbol 
                escrowScriptHash = makeValidatorScriptHash escrowValidatorScript

                encode = case format of
                    Raw -> id
                    Hex -> Hex.encode

                targetDirectory = case destination of
                    StdOut -> ""
                    OutFile file -> (dropFileName file)

                targetFilename = case destination of
                    StdOut -> ""
                    OutFile file -> takeFileName file

                -- This is a little messy:
                --  In the case of a single-script compilation, --out refers to the file we should write (path + filename)
                --  In the case of a compiling all scripts, --out should refer to a directory, with appropriate filenames appended
                -- I struggled with how to do this for a bit, but settled on this:
                --  We use targetDirectory above, which always gets the directory to save to (make sure you end with a trailing slash for --all)
                --  then, for --all we can pass in specific filenames as part of the tuple, and for individual scripts, we can use targetFilename from above
                -- Feel free to restructure if you see a better way
                output (filename, scr) = case destination of
                    StdOut -> BS.putStrLn scr
                    OutFile file -> do
                        let path = targetDirectory ++ filename
                        createDirectoryIfMissing True targetDirectory
                        BS.writeFile path scr
                
            traverse_ (\(f, scr) -> output $ (f, encode . BS.fromShort $ scr)) $ case target of
                All -> [ ("factory-mint", factoryMintScript)
                       , ("factory", factoryValidatorScript)
                       , ("pool-mint", poolMintScript)
                       , ("pool", poolValidatorScript)
                       , ("escrow", escrowValidatorScript)
                       ]
                FactoryMint -> [(targetFilename, factoryMintScript)]
                FactoryValidator -> [(targetFilename, factoryValidatorScript)]
                PoolMint -> [(targetFilename, poolMintScript)]
                PoolValidator -> [(targetFilename, poolValidatorScript)]
                EscrowValidator -> [(targetFilename, escrowValidatorScript)]
