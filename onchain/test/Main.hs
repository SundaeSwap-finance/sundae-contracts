-- | Unit tests
module Main where

import Test.Tasty
import qualified Test.Contracts.Pool as Pool
import qualified Test.Contracts.SundaeScooperCompat as SundaeScooperCompat

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ Pool.tests
  , SundaeScooperCompat.tests
  ]
