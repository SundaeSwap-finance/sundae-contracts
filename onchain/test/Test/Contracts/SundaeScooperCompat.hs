module Test.Contracts.SundaeScooperCompat
  ( tests
  ) where

import Sundae.Utilities
import Sundae.Contracts.Common
import Sundae.Contracts.Pool

import PlutusLedgerApi.V3
import PlutusLedgerApi.V1.Value

import Test.Tasty
import Test.Tasty.HUnit

-- This test mirrors the sundae-scooper test here:
-- https://github.com/SundaeSwap-finance/sundae-scooper/blob/beca2e3de965bde25189262ff3e6866b1abffb26/cpp/cpp_test.go#L98-L107
tests :: TestTree
tests = testCase "sundae-scooper compat tests" $ do
  let pkh = "00000000000000000000000000000000"
  let escrows =
        [ ( EscrowDestination (Address (PubKeyCredential (PubKeyHash pkh)) Nothing) Nothing
          , EscrowDeposit "00" (DepositMixed (AB 105 200))
          )
        ]
  let poolA = 1000000
  let poolB = 2000000
  let isqrt = floor . sqrt @Double . fromIntegral
  let initialLiquidity = isqrt (poolA * poolB)
  let coinA = AssetClass ("", "")
  let coinB = AssetClass ("", "")
  let (ScoopResult _cons _a _b newLiquidity) =
        doEscrows
          "00"
          coinA
          coinB
          (SwapFees (1 % 2000))
          (ABL poolA poolB initialLiquidity)
          escrows
  putStr "liquidity before scoop:"
  print initialLiquidity
  putStr "liquidity after scoop:"
  print newLiquidity
  (newLiquidity - initialLiquidity) @?= 141
