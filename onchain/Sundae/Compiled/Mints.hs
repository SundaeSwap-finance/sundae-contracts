module Sundae.Compiled.Mints where

import Prelude qualified
import PlutusTx.Prelude
import qualified PlutusTx

import PlutusLedgerApi.V3

import Sundae.Contracts.Common
import Sundae.Contracts.Mints
import Sundae.Utilities

factoryBootMintingScript :: FactoryBootSettings -> SerialisedScript
factoryBootMintingScript fbs =
  let
    x =
      pure $$(PlutusTx.compile [|| \fbs' -> factoryBootMintingContract fbs' ||])
        >>= flip apCode fbs
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile factory boot minting script"


treasuryBootMintingScript :: TreasuryBootSettings -> SerialisedScript
treasuryBootMintingScript tbs =
  let
    x =
      pure $$(PlutusTx.compile [|| \tbs' -> treasuryBootMintingContract tbs' ||])
        >>= flip apCode tbs
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile treasury boot minting script"


sundaeMintingScript :: TreasuryBootCurrencySymbol -> SerialisedScript
sundaeMintingScript tbcs =
  let
    x =
      pure $$(PlutusTx.compile [|| \tbcs' -> sundaeMintingContract tbcs' ||])
        >>= flip apCode tbcs
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile sundae minting script"

-- | Pool minting script
poolMintingScript :: FactoryBootCurrencySymbol -> OldPoolCurrencySymbol -> SerialisedScript
poolMintingScript fbcs opcs =
  let
    x =
      pure $$(PlutusTx.compile [|| poolMintingContract ||])
        >>= flip apCode fbcs
        >>= flip apCode opcs
  in
    case x of
      Just x' -> serialiseCompiledCode x'
      Nothing -> Prelude.error "Couldn't compile pool minting script"
