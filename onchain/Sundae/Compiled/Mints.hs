module Sundae.Compiled.Mints where

import PlutusTx.Prelude
import qualified PlutusTx

import Sundae.Contracts.Common
import Sundae.Contracts.Mints
import Sundae.Utilities

{-
factoryBootMintingScript :: FactoryBootSettings -> MintingPolicy
factoryBootMintingScript fbs = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \fbs' -> Scripts.mkUntypedMintingPolicy $ factoryBootMintingContract fbs' ||])
    `apCode` fbs

treasuryBootMintingScript :: TreasuryBootSettings -> MintingPolicy
treasuryBootMintingScript tbs = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \tbs' -> Scripts.mkUntypedMintingPolicy $ treasuryBootMintingContract tbs' ||])
    `apCode` tbs

sundaeMintingScript :: TreasuryBootCurrencySymbol -> MintingPolicy
sundaeMintingScript tbcs = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \tbcs' -> Scripts.mkUntypedMintingPolicy $ sundaeMintingContract tbcs' ||])
    `apCode` tbcs

-- | Pool minting script
poolMintingScript :: FactoryBootCurrencySymbol -> OldPoolCurrencySymbol -> MintingPolicy
poolMintingScript fbcs opcs = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| poolMintingContract ||])
    `apCode` fbcs
    `apCode` opcs
-}
