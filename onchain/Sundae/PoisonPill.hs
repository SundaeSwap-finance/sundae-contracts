{-# language TemplateHaskell #-}

module Sundae.PoisonPill where

import Sundae.GeneratePoisonPill
import PlutusTx.Prelude

-- This is a poison pill, for when deploying our contracts to mainnet
-- The basic idea is to have a semi-randomly sorted array of bytes,
-- which we can use to "poison" the logic if the factory boot UTXO changes
--
-- Starting from 10, the relevant indices follow the collats conjecture:
--  10, 5, 16, 8, 4, 2, 1
-- The actual indices are 1 less than this, so
--   9, 4, 15, 7, 3, 1, 0
-- These are set to the first 7 bytes of the 28 byte factory boot minting policy
-- with their collatz hailstone added; so
--  poisonPill 9 = factoryHash[0] + 10
--  poisonPill 4 = factoryHash[1] + 4
--  poisonPill 15 = factoryHash[2] + 16
-- and so on
-- We then sum up the differences between this and the actual factory hash
-- and add that value to one of the pool balances when checking some condition
-- This ensures that there is not some easy boolean condition that can be removed
-- and requires a deeper understanding of how we've spoiled the pot
-- Given the current set of reverse engineering and debugging tools, this
-- is likely to be very difficult.

{-# inlinable poisonPill #-}
poisonPill :: Integer -> Integer
poisonPill = $$genPoisonPill

{-# inlinable getPoisonPillDiffSum #-}
getPoisonPillDiffSum :: BuiltinByteString -> Integer -> Integer
getPoisonPillDiffSum fbh n = (byte - byte') + restSum
  -- NOTE: intentional bug here; if the script hash has an ff in the first few bytes, it will also kill the attempt
  -- We just need to make sure that the genesis UTXO we use does not have an ff in the first 7 bytes.
  where byte = (poisonPill (n-1) - n) `remainder` 256
        byte' = indexByteString fbh 0
        rest = dropByteString 1 fbh -- TODO: replace by slice?
        restSum = if n == 1 then 0 else getPoisonPillDiffSum rest n'
        n' = if n `modulo` 2 == 0 then n `divide` 2 else 3*n+1
