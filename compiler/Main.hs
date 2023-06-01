{-# LANGUAGE ApplicativeDo              #-}

module Main (main) where

import Sundae.Contracts.Common (FactoryBootSettings)
import Options.Applicative qualified as O

data Format = Raw | Hex | Json

data FactoryBootFile = StdIn | InFile FilePath

data Script
    = FactoryValidator FactoryBootFile
    | FactoryMint FactoryBootFile
    | PoolValidator FactoryBootFile 
    | PoolMint FactoryBootFile
    | EscrowValidator FactoryBootFile

data Destination = OutFile FilePath | StdOut

data CompileConfig = CompileConfig
    { compileScript :: Script
    , compileFormat :: Format
    , compileDestination :: Destination
    }

data CompileAllConfig = CompileAllConfig
    { compileAllBootSettings :: FactoryBootFile
    , compileAllFormat :: Format
    , compileAllDestination :: Destination
    }
data Request
    = Compile CompileConfig
    | CompileAll CompileAllConfig

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
            , O.command "compileAll" (O.info (fmap CompileAll pCompileAll) (O.progDesc "compile all scripts"))
            ]
        pCompile :: O.Parser CompileConfig
        pCompile = do
            constr <- pScript
            factoryBoot <- pFactoryBootFile
            format <- pFormat
            dest <- pDestination
            pure $ CompileConfig (constr factoryBoot) format dest
        pCompileAll :: O.Parser CompileAllConfig
        pCompileAll = do
            factoryBoot <- pFactoryBootFile
            format <- pFormat
            dest <- pDestination
            pure $ CompileAllConfig factoryBoot format dest

        pScript :: O.Parser (FactoryBootFile -> Script)
        pScript
            =     (O.flag' FactoryValidator (O.long "factory" <> O.help "compile the factory script"))
            O.<|> (O.flag' FactoryMint (O.long "factory-mint" <> O.help "compile the factory mint script"))
            O.<|> (O.flag' PoolValidator (O.long "pool" <> O.help "compile the pool script"))
            O.<|> (O.flag' PoolMint (O.long "pool-mint" <> O.help "compile the pool mint script"))
            O.<|> (O.flag' EscrowValidator (O.long "escrow" <> O.help "compile the escrow script"))

        pFactoryBootFile :: O.Parser FactoryBootFile
        pFactoryBootFile = InFile <$> (O.strOption $ mconcat
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


main :: IO ()
main = do
    getConfig >>= \case
        Compile _ -> putStrLn "Compile!"
        CompileAll _ -> putStrLn "Compile All!"