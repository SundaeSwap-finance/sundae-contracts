{-# Language InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Sundae.Utilities where

import PlutusTx.Prelude
import PlutusTx.Builtins
import qualified PlutusTx
import qualified Prelude hiding (foldMap)

import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V1.Time

import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16

import PlutusCore qualified as Core
import PlutusTx.AssocMap(Map)
import qualified PlutusTx.AssocMap as Map

import Control.DeepSeq
import Data.Aeson hiding (Value)
import Data.Coerce
import GHC.Generics

import Data.Text.Encoding qualified as Encoding

import qualified System.Random as Random

import PlutusLedgerApi.V3
import PlutusLedgerApi.V2.Contexts

{-# inlinable scriptHashAddress #-}
scriptHashAddress :: ScriptHash -> Address
scriptHashAddress sh = Address (ScriptCredential sh) Nothing

{-
findOwnInput :: ScriptContext -> Maybe TxInInfo
findOwnInput (ScriptContext t_info (Spending o_ref)) =
  let
    go (this@(TxInInfo tref ot) : tl) o_ref
      | tref == o_ref = Just this
      | otherwise = go tl o_ref
    go [] _ = Nothing
  in
    go (txInfoInputs t_info) o_ref
-}

{-# inlinable getContinuingOutputs #-}
getContinuingOutputs :: ScriptContext -> [TxOut]
getContinuingOutputs ctx =
  case findOwnInput ctx of
    Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} -> filter (isPaymentKeyEq txOutAddress) (txInfoOutputs $ scriptContextTxInfo ctx)
    Nothing -> traceError "Lf"
  where
    -- check only payment key, not staking, so that the staking address for a pool may be changed
    isPaymentKeyEq addr TxOut{txOutAddress=otherAddress} = addressCredential addr == addressCredential otherAddress

-- These instances were dropped, so we now have to implement them
-- but they won't be used in contracts
instance ToJSON BuiltinByteString where
  toJSON bs = toJSON (Encoding.decodeUtf8 (Base16.encode (fromBuiltin bs)))
instance FromJSON BuiltinByteString where
  parseJSON = withText "BuiltinByteString" $ \s ->
    case Base16.decode (Encoding.encodeUtf8 s) of
      Right rawBytes -> Prelude.pure (toBuiltin rawBytes)
      Left err -> Prelude.fail err

{-# inlinable atLeastOne #-}
atLeastOne :: (a -> Bool) -> [a] -> Bool
atLeastOne f (x:xs) = if f x then True else atLeastOne f xs
atLeastOne _ [] = False

{-# inlinable toDiffMilliSeconds #-}
toDiffMilliSeconds :: POSIXTime -> DiffMilliSeconds
toDiffMilliSeconds = coerce

{-# inlinable lovelaceOf #-}
lovelaceOf :: Value -> Integer
lovelaceOf v = valueOf v adaSymbol adaToken

{-# inlinable hasLimitedNft #-}
hasLimitedNft :: Integer -> AssetClass -> Value -> Bool
hasLimitedNft size coin val =
  valueOfAC val coin == 1 && valueSizeLimited size val

{-# inlinable length' #-}
length' :: [a] -> Integer
length' l = go l 0
 where
   go [] acc = acc
   go (_: tl) acc = go tl (acc + 1)


{-# inlinable flattenValue' #-}
flattenValue' :: Value -> [(CurrencySymbol, TokenName, Integer)]
flattenValue' v = go (Map.toList $ getValue v) []
  where
    go [] acc = acc
    go ((cs, m) : tl) acc = go tl $ flatten_l cs (Map.toList m) acc

    flatten_l _ [] acc = acc
    flatten_l cs ((tn, a) : tl) acc
      | a /= 0 = flatten_l cs tl ((cs, tn, a) : acc)
      | otherwise = flatten_l cs tl acc

{-# inlinable valueSizeLimited #-}
valueSizeLimited :: Integer -> Value -> Bool
valueSizeLimited size val =
  length' (flattenValue' val) <= size

{-# inlinable valueOfAC #-}
valueOfAC :: Value -> AssetClass -> Integer
valueOfAC = assetClassValueOf

{-# inlinable noSymbol #-}
-- | Value does not contain currency symbol
noSymbol :: CurrencySymbol -> Value -> Bool
noSymbol cs (Value val) =
  maybe True (== Map.empty) (Map.lookup cs val)

{-# inlinable withoutLovelace #-}
withoutLovelace :: Value -> Value
withoutLovelace v =
  Value $ Map.fromList $ filter (\(k, _) -> k /= adaSymbol) $ Map.toList $ getValue v

{-# inlinable datumOf #-}
-- | It only succeeds if there's a valid datum, or fails on a missing datum.
-- If there's an invalid datum, you get a crash.
datumOf :: FromData a => TxInfo -> TxOut -> Maybe a
datumOf txInfo txOut = do
  d <- getDatum <$> rawDatumOf txInfo txOut
  fromBuiltinData d

{-# inlinable rawDatumOf #-}
rawDatumOf :: TxInfo -> TxOut -> Maybe Datum
rawDatumOf txInfo txOut =
  case txOutDatum txOut of
    OutputDatumHash d -> Map.lookup d $ txInfoData txInfo
    OutputDatum _ -> Nothing
    NoOutputDatum -> Nothing

txOutDatumHash :: TxOut -> Maybe DatumHash
txOutDatumHash txOut =
  case txOutDatum txOut of
    OutputDatumHash d -> Just d
    OutputDatum _ -> Nothing
    NoOutputDatum -> Nothing

-- More efficient alternative to findDatum from Plutus V1. Consider changing
-- back to findDatum if we upgrade to Plutus V2
{-# inlinable searchDatum #-}
searchDatum :: DatumHash -> [(DatumHash, Datum)] -> Maybe BuiltinData
searchDatum _ [] = Nothing
searchDatum dsh ((dsh', Datum d) : tl)
  | dsh == dsh' = Just d
  | otherwise = searchDatum dsh tl

{-# inlinable isDatumUnsafe #-}
isDatumUnsafe :: ToData a => TxInfo -> TxOut -> a -> Bool
isDatumUnsafe txInfo txOut expectedDat =
  rawDatumOf txInfo txOut == Just (Datum (toBuiltinData expectedDat))

{-# INLINABLE getAddressOutputs #-}
getAddressOutputs :: ScriptContext -> Address -> [TxOut]
getAddressOutputs ctx addr = filter f (txInfoOutputs $ scriptContextTxInfo ctx)
    where
    f TxOut{txOutAddress=otherAddress} = addr == otherAddress

{-# inlinable debug #-}
debug :: BuiltinString -> Bool -> Bool
debug =
  -- const id
  traceIfFalse

{-# inlinable die #-}
die :: BuiltinString -> a
die =
  -- const (error ())
  traceError

-- the Plutus version of this makes little sense, because it requires that *all*
-- assets are greater in number, rather than just requiring that just one is,
-- and the rest are greater or equal. this may be possible to optimize.
{-# inlinable valueGT #-}
valueGT :: Value -> Value -> Bool
valueGT v1 v2 =
  atLeastOne (\(cs,tk,n) -> n > valueOf v2 cs tk) (flattenValue v1) && v1 `geq` v2

{-# inlinable all' #-}
all' :: _
all' f = go
  where
  go (x:xs) = if f x then go xs else False
  go [] = True

{-# inlinable foldl' #-}
foldl' :: _
foldl' f = go
  where
  go !z (x:xs) = go (f z x) xs
  go z [] = z

-- An optimization over `uniqueElement` from the Plutus stdlib.
{-# inlinable uniqueElement' #-}
uniqueElement' :: [a] -> a
uniqueElement' [x] = x
uniqueElement' _ = error ()

-- `Ident`, named for its most frequent use as an identifier, is basically an
-- `Integer` encoded as a `ByteString`, because Plutus offers no way to do this
-- in its standard library.
newtype Ident = Ident BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (NFData)
  deriving newtype (ToJSON, FromJSON)
  deriving anyclass (ToJSONKey, FromJSONKey)
  deriving newtype (Prelude.Eq, Prelude.Show, Prelude.Ord)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

--deriving instance NFData Rational
instance NFData Rational where
  rnf !_ = ()

deriving newtype instance FromJSON DiffMilliSeconds
deriving newtype instance ToJSON DiffMilliSeconds

instance Eq Ident where
  {-# inlinable (==) #-}
  Ident i == Ident i' = i == i'

PlutusTx.makeLift ''Ident

{-# inlinable initialIdent #-}
initialIdent :: Ident
initialIdent = Ident $ consByteString 0 ""

{-# inline identBase #-}
identBase :: Integer
identBase = 256

{-# inlinable succIdent #-}
succIdent :: Ident -> Ident
succIdent = succsIdent 1

{-# inlinable intToIdent #-}
intToIdent :: Integer -> Ident
intToIdent n = succsIdent n initialIdent

-- | Ident is chain of bytes written in reverse. It grows like this:
-- > 0, 1, ...
-- > 01, 11, 21 ...
-- > 001, 101, 201, 301 ...
--
-- only it uses bytes not 10-digit base system. So the base is 256.
{-# inlinable succsIdent #-}
succsIdent :: Integer -> Ident -> Ident
succsIdent !count (Ident ident) = Ident $ add (lengthOfByteString ident) ident count
  where
    add len str n
      | n <= 0    = str
      | len <= 0  = consByteString m (addRest d)
      | otherwise = let cur  = m + indexByteString str 0
                        rest = dropByteString 1 str -- OPTIMIZATION: Slicing?
                        (d', m') = divMod cur identBase
                    in  consByteString m' (add (len - 1) rest (d + d'))
      where
      (d, m) = divMod n identBase

    -- if string is over and we need to append number to the tail of the identifier
    addRest n
      | n == 0    = ""
      | otherwise = consByteString m (addRest d)
      where
      (d, m) = divMod n identBase

{-# inlineable identToInt #-}
identToInt :: Ident -> Integer
identToInt (Ident ident) = go 0 1 0
  where
    go ix degree res
      | ix >= len  = res
      | otherwise  =
          let cur  = indexByteString ident ix
          in  go (ix + 1) (identBase * degree) (res + cur * degree)

    len = lengthOfByteString ident

{-# inlinable assetClassValueContains #-}
assetClassValueContains :: Value -> AssetClass -> Bool
assetClassValueContains v (AssetClass (cs,tk)) = valueContains v cs tk

{-# inlinable valueContains #-}
valueContains :: Value -> CurrencySymbol -> TokenName -> Bool
valueContains v cs tk =
  case Map.lookup cs (getValue v) of
    Nothing -> False
    Just tks -> case Map.lookup tk tks of
      Just n -> n /= 0
      Nothing -> False

-- | Lots of datatypes of ours contain information relating to the assets in a
-- pool; `Coin` can act as an index into those datatypes through the `Pairlike` class.
data Coin = CoinA | CoinB
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic)
  deriving anyclass (NFData, FromJSON, ToJSON)
  deriving anyclass (Random.Finite, Random.Uniform)

instance Eq Coin where
  CoinA == CoinA = True
  CoinB == CoinB = True
  _ == _ = False

-- e `ofCoin` c $$ c == e
class Pairlike c x e | x -> e, x -> c where
  infixl 9 $$
  ($$) :: x -> Coin -> e
  infixl 8 `ofCoin`
  ofCoin :: c => e -> Coin -> x

instance Pairlike (AdditiveMonoid a) (AB a) a where
  {-# inlinable ($$) #-}
  AB a _ $$ CoinA = a
  AB _ b $$ CoinB = b
  {-# inline conlike ofCoin #-}
  a `ofCoin` CoinA = AB a zero
  b `ofCoin` CoinB = AB zero b

{-# inline conlike memo #-}
memo :: (Coin -> a) -> AB a
memo f = AB (f CoinA) (f CoinB)

data AB a = AB !a !a
  deriving stock (Generic, Prelude.Eq, Prelude.Functor, Prelude.Show, Prelude.Ord)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance Eq a => Eq (AB a) where
  {-# inlinable (==) #-}
  AB a b == AB a' b' = a == a' && b == b'

instance Functor AB where
  {-# inlinable fmap #-}
  fmap f (AB a b) = AB (f a) (f b)

instance Foldable AB where
  {-# inlinable foldr #-}
  foldr f nil (AB a b) = f a (f b nil)

instance AdditiveSemigroup a => AdditiveSemigroup (AB a) where
  {-# inlinable (+) #-}
  AB a b + AB a' b' = AB (a+a') (b+b')

instance AdditiveGroup a => AdditiveGroup (AB a) where
  {-# inlinable (-) #-}
  AB a b - AB a' b' = AB (a-a') (b-b')

instance AdditiveMonoid a => AdditiveMonoid (AB a) where
  {-# inline conlike zero #-}
  zero = AB zero zero

data SwapAggregate
  = SwapAggregate !Integer !Integer !Integer !Integer

instance AdditiveSemigroup SwapAggregate where
  {-# inlinable (+) #-}
  SwapAggregate a b c d + SwapAggregate a' b' c' d' =
    let (!a'',!b'',!c'',!d'') = (a+a',b+b',c+c',d+d')
    in SwapAggregate a'' b'' c'' d''

instance AdditiveMonoid SwapAggregate where
  {-# inline conlike zero #-}
  zero = SwapAggregate 0 0 0 0

data ABL a
  = ABL !a !a !a
  deriving (Prelude.Show, Prelude.Functor)
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)

{-# inline conlike liquidity #-}
liquidity :: ABL a -> a
liquidity (ABL _ _ l) = l

{-# inline conlike ofLiquidity #-}
ofLiquidity :: AdditiveMonoid a => a -> ABL a
ofLiquidity l = ABL zero zero l

instance Pairlike (AdditiveMonoid a) (ABL a) a where
  {-# inlinable ($$) #-}
  ABL a _ _ $$ CoinA = a
  ABL _ b _ $$ CoinB = b
  {-# inlinable ofCoin #-}
  a `ofCoin` CoinA = ABL a zero zero
  b `ofCoin` CoinB = ABL zero b zero

{-# inline conlike noLiquidity #-}
noLiquidity :: AB Integer -> ABL Integer
noLiquidity (AB a b) = ABL a b zero

instance AdditiveSemigroup a => AdditiveSemigroup (ABL a) where
  {-# inlinable (+) #-}
  ABL a b l + ABL a' b' l' =
    ABL (a+a') (b+b') (l+l')

instance AdditiveGroup a => AdditiveGroup (ABL a) where
  {-# inlinable (-) #-}
  ABL a b l - ABL a' b' l' =
    ABL (a-a') (b-b') (l-l')

instance AdditiveMonoid a => AdditiveMonoid (ABL a) where
  {-# inline conlike zero #-}
  zero = ABL zero zero zero

instance (Eq k, AdditiveSemigroup a) => AdditiveSemigroup (Map k a) where
  {-# inlinable (+) #-}
  (+) = Map.unionWith (+)

instance (Eq k, AdditiveGroup a) => AdditiveGroup (Map k a) where
  {-# inlinable (-) #-}
  (-) = Map.unionWith (-)

instance (Eq k, AdditiveMonoid a) => AdditiveMonoid (Map k a) where
  {-# inline conlike zero #-}
  zero = Map.empty

{-# inlinable unsafeTxInValue #-}
unsafeTxInValue :: _ -> Value
unsafeTxInValue (unsafeDataAsConstr -> (_, [_, (unsafeDataAsConstr -> (_, [_, (unsafeFromBuiltinData -> v), _, _]))])) = v
unsafeTxInValue _ = error ()

{-# inlinable toWeek #-}
toWeek :: POSIXTime -> Week
toWeek (POSIXTime t) =
  Week (t `divide` 604_800_000)
  -- 1000 (millseconds / second) * 60 (seconds / minute) * 60 (minutes / hour) * 24 (hours / day) * 7 (days / week)

newtype Week = Week { getWeek :: Integer }
  deriving newtype (AdditiveMonoid, AdditiveSemigroup, AdditiveGroup, PlutusTx.UnsafeFromData, PlutusTx.FromData, PlutusTx.ToData)
  deriving newtype (ToJSON, FromJSON, NFData)
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Week
PlutusTx.makeIsDataIndexed ''Coin [('CoinA, 0), ('CoinB, 1)]
PlutusTx.makeIsDataIndexed ''AB [('AB, 0)]

apCode
  :: (PlutusTx.Lift Core.DefaultUni a)
  => PlutusTx.CompiledCode (a -> b)
  -> a
  -> Maybe (PlutusTx.CompiledCode b)
apCode p arg = p `PlutusTx.applyCode` PlutusTx.liftCodeDef arg

{-# inlinable onlyHas #-}
onlyHas :: Value -> CurrencySymbol -> TokenName -> (Integer -> Bool) -> Bool
onlyHas v cs tk p
  | [(cs', tk', n)] <- flattenValue' v
  = cs == cs' && tk == tk' && p n
  | otherwise
  = False

-- avoid use of unoptimized function findOwnInput

{-# INLINEABLE scriptInput #-}
scriptInput :: ScriptContext -> TxOut
scriptInput (ScriptContext t_info (Spending o_ref)) = getScriptInput (txInfoInputs t_info) o_ref
scriptInput _ = traceError "script input not found !!!"

{-# INLINEABLE getScriptInput #-}
getScriptInput :: [TxInInfo] -> TxOutRef -> TxOut
getScriptInput [] _ = traceError "script input not found !!!"
getScriptInput ((TxInInfo tref ot) : tl) o_ref
  | tref == o_ref = ot
  | otherwise = getScriptInput tl o_ref

{-# INLINEABLE isScriptAddress #-}
isScriptAddress :: TxOut -> ScriptHash -> Bool
isScriptAddress (TxOut (Address (ScriptCredential h) _) _ _ _) sh = h == sh
isScriptAddress _ _ = False

{-# INLINEABLE eqAddrCredential #-}
eqAddrCredential :: Address -> Address -> Bool
eqAddrCredential addr1 addr2 = addressCredential addr1 == addressCredential addr2

infixl 7 %

{-# INLINEABLE (%) #-}
(%) :: Integer -> Integer -> Rational
(%) x y = unsafeRatio x y
