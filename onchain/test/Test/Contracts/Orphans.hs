module Test.Contracts.Orphans where

import Control.Lens
import Data.Coerce
import Data.Maybe

import PlutusLedgerApi.V3
import PlutusLedgerApi.V1.Value

import PlutusTx.AssocMap (Map)

import qualified PlutusTx.AssocMap as Map
import qualified PlutusTx.Prelude as Plutus

type instance Index (Map k v) = k
type instance IxValue (Map k v) = v

instance Plutus.Eq k => Ixed (Map k v) where
  ix k f m = case Map.lookup k m of
     Just v  -> f v <&> \v' -> Map.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}

instance Plutus.Eq k => At (Map k v) where
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (Map.delete k m)) mv
    Just v' -> Map.insert k v' m
    where mv = Map.lookup k m

type instance Index Value = AssetClass
type instance IxValue Value = Integer

instance Ixed Value where
  ix (AssetClass (cs,tk)) f v =
    coerce <$> ix cs (ix tk f) (getValue v)

instance At Value where
  at (AssetClass (cs,tk)) f (getValue -> v) =
    f mv <&> \r -> case r of
      Nothing -> Value $ Map.insert cs (fromMaybe Map.empty (Map.delete tk <$> tks)) v
      Just n' -> Value $ Map.insert cs (Map.insert tk n' $ fromMaybe Map.empty tks) v
    where
    tks = Map.lookup cs v
    mv = Map.lookup tk =<< tks
