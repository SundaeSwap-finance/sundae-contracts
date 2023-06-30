{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}

module Sundae.Contracts.Common where

import qualified Prelude
import PlutusTx.Prelude
import PlutusTx.Sqrt
import Data.Aeson qualified as Aeson
import Data.Aeson (FromJSON(..), ToJSON(..), withScientific, withObject, (.:), (.=))
import Data.ByteString.Base16 qualified as Base16
import Data.Text.Encoding qualified as Text

import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V2

import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Ratio

import GHC.Generics
import Control.DeepSeq
import Sundae.Utilities

import Control.Lens (makeLenses)

-- | Factory script controls creation of pools
data Factory

-- | Script that holds all live pools
data Pool

-- | Script that holds all escrows
data Escrow

instance FromJSON TxOutRef where
  parseJSON = withObject "TxOutRef" $ \o -> do
    txid <- o .: "txid"
    txix <- o .: "txix"
    Prelude.pure $ TxOutRef
      { txOutRefId = TxId txid
      , txOutRefIdx = txix
      }

instance ToJSON TxOutRef where
  toJSON (TxOutRef { txOutRefId = TxId txid, txOutRefIdx = txix }) =
    Aeson.object
      [ "txid" .= txid
      , "txix" .= txix
      ]

-- | A single UTXO that uniquely identifies / parameterizes the entire protocol
-- | This ensures that anyone who runs our scripts with a different UTXO
-- | ends up with different policy IDs / script hashes, and is a fundamentally different protocol
newtype ProtocolBootUTXO = ProtocolBootUTXO
  { unProtocolBootUTXO :: TxOutRef
  }
  deriving stock (Generic, Prelude.Show)
  deriving newtype (ToJSON)

instance FromJSON ProtocolBootUTXO where
  parseJSON v = Prelude.fmap ProtocolBootUTXO $ parseJSON v

-- | Used to make the treasury token an NFT.
newtype TreasuryBootSettings = TreasuryBootSettings
  { treasury'protocolBootUTXO :: ProtocolBootUTXO
  }
  --deriving newtype (FromJSON, ToJSON)

instance FromJSON PubKeyHash where
  parseJSON = Aeson.withText "PubKeyHash" $ \s -> do
    dec <- case Base16.decode (Text.encodeUtf8 s) of
      Right ok -> Prelude.pure ok
      Left err -> Prelude.fail err
    Prelude.pure (PubKeyHash (toBuiltin dec))

instance ToJSON PubKeyHash where
  toJSON (PubKeyHash pkh) =
    let
      bytes = fromBuiltin pkh
      hex = Base16.encode bytes
      text = Text.decodeUtf8 hex
    in
      Aeson.String text

data FactoryBootSettings
  = BrandNewFactoryBootSettings
  { factory'protocolBootUTXO :: ProtocolBootUTXO
  , initialLegalScoopers :: [PubKeyHash]
  }
  | UpgradedFactoryBootSettings
  { oldFactoryBootCurrencySymbol :: OldFactoryBootCurrencySymbol
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToJSON)

instance FromJSON FactoryBootSettings where
  parseJSON = withObject "FactoryBootSettings" $ \obj -> do
    utxo <- obj .: "protocolBootUTXO"
    scoopers <- obj .: "scoopers"
    return $ BrandNewFactoryBootSettings utxo scoopers

data UpgradeSettings = UpgradeSettings
  { upgradeTimeLockPeriod :: Integer
  , upgradeAuthentication :: AssetClass
  }
  deriving stock (Generic, Prelude.Show)
  --deriving anyclass (FromJSON, ToJSON)

-- | The destination for the results of an escrowed operation.
-- Could be a user address, but could also be a script address + datum
-- for composing with other protocols.
data EscrowDestination = EscrowDestination !Address !(Maybe DatumHash)
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic)
  --deriving anyclass (NFData, ToJSON, FromJSON)

instance Eq EscrowDestination where
  {-# inlinable (==) #-}
  (==) (EscrowDestination addr1 dh1) (EscrowDestination addr2 dh2) =
    addr1 == addr2 && dh1 == dh2

-- | An escrow's address information.
-- The first field is the destination for the results of the escrowed operation.
-- The second field can also supply auxiliary PubKeyHash which can be used to authenticate cancelling this order.
data EscrowAddress = EscrowAddress !EscrowDestination !(Maybe PubKeyHash)
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic)
  --deriving anyclass (NFData, ToJSON, FromJSON)

instance Eq EscrowAddress where
  {-# inlinable (==) #-}
  (==) (EscrowAddress dest1 pkh1) (EscrowAddress dest2 pkh2) =
    dest1 == dest2 && pkh1 == pkh2

{-# inlinable escrowPubKeyHashes #-}
escrowPubKeyHashes :: EscrowAddress -> [PubKeyHash]
escrowPubKeyHashes (EscrowAddress (EscrowDestination addr _) mPubKey) =
  mapMaybe id [toPubKeyHash addr, mPubKey]
  where
  toPubKeyHash (Address cred stakingCred) =
    case cred of
      PubKeyCredential pkh -> Just pkh
      _ -> Nothing

{-# inlinable fromEscrowAddress #-}
fromEscrowAddress :: EscrowAddress -> EscrowDestination
fromEscrowAddress (EscrowAddress dest _) = dest

newtype SwapFees = SwapFees Rational
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Eq, NFData)
  deriving (Prelude.Show)

instance FromJSON SwapFees where
  parseJSON = withScientific "SwapFees" $ \sci -> return (SwapFees (fromGHC (Prelude.toRational sci)))

instance ToJSON SwapFees where
  toJSON (SwapFees r) =
    toJSON (Prelude.fromRational (toGHC r) :: Prelude.Double)

instance ToJSON POSIXTime where
instance FromJSON POSIXTime where
instance ToJSON CurrencySymbol where
instance FromJSON CurrencySymbol where
instance ToJSON ScriptHash where
instance FromJSON ScriptHash where

data FactoryDatum = FactoryDatum
  { poolScriptHash :: ScriptHash
  , poolCurrencySymbol :: CurrencySymbol
  , scooperSet :: ![PubKeyHash]
  -- permissible staking credentials for pool
  , poolStakingCredSet :: ![StakingCredential]
  }
  deriving stock (Generic, Prelude.Show)
  --deriving anyclass (NFData, ToJSON, FromJSON)

instance Eq FactoryDatum where
  {-# inlinable (==) #-}
  FactoryDatum poolSH' poolCS' scooperSet' poolStakingCredSet' ==
    FactoryDatum poolSH'' poolCS'' scooperSet'' poolStakingCredSet'' =
      poolSH' == poolSH'' &&
      poolCS' == poolCS'' &&
      scooperSet' == scooperSet'' &&
      poolStakingCredSet' == poolStakingCredSet''

-- | Action on factory script
data FactoryRedeemer
  = FactoryRedeemer
  --deriving (Generic, ToJSON, FromJSON)

instance Eq FactoryRedeemer where
  {-# inlinable (==) #-}
  _ == _ = True

data FactoryBootMintRedeemer
  = MakeFactory
  | MakeScooperToken

data PoolMintRedeemer
  = MintLP BuiltinByteString -- Mint LP for the given pool ident
  | CreatePool AssetClass AssetClass

data ScooperFeeSettings
  = ScooperFeeSettings
  { scooperRewardRedeemDelayWeeks :: Integer
  } --deriving (Generic, NFData, Prelude.Show, ToJSON, FromJSON)

data ScooperFeeDatum
  = ScooperFeeDatum
  { scooperLicensee :: !PubKeyHash
  } --deriving (Generic, NFData, Prelude.Show, ToJSON, FromJSON)

data ScooperFeeRedeemer
  = ScooperCollectScooperFees

-- | Pool internal state
data PoolDatum
  = PoolDatum
  { _pool'coins :: !(AB AssetClass)   -- ^ pair of coins on which pool operates
  , _pool'poolIdent :: !BuiltinByteString -- ^ unique identifier of the pool.
  , _pool'circulatingLP :: !Integer    -- ^ amount of minted liquidity
  , _pool'swapFees :: !SwapFees -- ^ this pool's trading fee.
  , _pool'marketOpenTime :: !POSIXTime -- ^ time to enable swaps on this pool
  , _pool'rewards :: !Integer -- ^ ADA reserved for scooper rewards
  } deriving (Generic, NFData, Prelude.Show)

instance Eq PoolDatum where
  {-# inlinable (==) #-}
  PoolDatum coinPair ident issuedLiquidity swapFees marketOpenTime rewards ==
    PoolDatum coinPair' ident' issuedLiquidity' swapFees' marketOpenTime' rewards' =
      coinPair == coinPair' && ident == ident' && issuedLiquidity == issuedLiquidity' && swapFees == swapFees' && marketOpenTime == marketOpenTime' && rewards == rewards'

data PoolRedeemer
  = PoolScoop !PubKeyHash [Integer] -- OPTIMIZATION: PKH here is candidate for removal

-- | The escrow datum specified which pool it's intended for, what the return
-- address is for any results of the escrowed operation, and the amount of
-- lovelace intended to be paid to the scooper.
data EscrowDatum = EscrowDatum
  { _escrow'address :: EscrowAddress
  , _escrow'scoopFee :: Integer
  , _escrow'action :: EscrowAction
  }
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic)
  --deriving anyclass (NFData, ToJSON, FromJSON)

instance Eq EscrowDatum where
  EscrowDatum ret fee act == EscrowDatum ret' fee' act' =
    ret == ret' && fee == fee' && act == act'

-- | Deposits take the form of single-asset and mixed-asset deposits.
data Deposit = DepositSingle Coin Integer | DepositMixed (AB Integer)
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic)
  --deriving anyclass (NFData, ToJSON, FromJSON)

instance Eq Deposit where
  {-# inlinable (==) #-}
  DepositSingle coin n == DepositSingle coin' n' = coin == coin' && n == n'
  DepositMixed ab == DepositMixed ab' = ab == ab'
  _ == _ = False

-- | Escrow actions
data EscrowAction
  -- | Swap to address given amount of tokens (Integer) of asset (AssetClass),
  --   expecting to get at least some amount (Integer) in return.
  = EscrowSwap (AssetClass, Integer) (AssetClass, Maybe Integer)
  -- | Withdraw some amount of liquidity, by burning liquidity tracking tokens.
  | EscrowWithdraw BuiltinByteString Integer
  -- | Make a deposit, in exchange for newly-minted liquidity tracking tokens.
  | EscrowDeposit BuiltinByteString Deposit
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord, Generic)
  --deriving anyclass (NFData, ToJSON, FromJSON)

instance Eq EscrowAction where
  {-# inlinable (==) #-}
  EscrowSwap (coinGive, gives) (coinTake, minTakes) == EscrowSwap (coinGive', gives') (coinTake', minTakes') =
    coinGive == coinGive' && gives == gives' && coinTake == coinTake' && minTakes == minTakes'
  EscrowWithdraw poolId givesLiq == EscrowWithdraw poolId' givesLiq' =
    poolId == poolId' && givesLiq == givesLiq'
  EscrowDeposit poolId dep == EscrowDeposit poolId' dep' =
    poolId == poolId' && dep == dep'
  _ == _ =
    False

-- | Escrow redeemer
data EscrowRedeemer
  -- ^ scooper collects escrow actions to execute them on pool
  = EscrowScoop
  -- ^ user withdraws their escrow
  | EscrowCancel

newtype FactoryScriptHash = FactoryScriptHash ScriptHash
  deriving stock Prelude.Show
newtype TreasuryScriptHash = TreasuryScriptHash ScriptHash
  deriving stock Prelude.Show

newtype FactoryBootCurrencySymbol = FactoryBootCurrencySymbol CurrencySymbol
  deriving stock Prelude.Show
newtype OldFactoryBootCurrencySymbol = OldFactoryBootCurrencySymbol CurrencySymbol
  deriving stock Prelude.Show
  deriving newtype (FromJSON, ToJSON)
newtype TreasuryBootCurrencySymbol = TreasuryBootCurrencySymbol CurrencySymbol
  deriving stock Prelude.Show
newtype OldPoolCurrencySymbol = OldPoolCurrencySymbol CurrencySymbol
  deriving stock Prelude.Show
newtype PoolCurrencySymbol = PoolCurrencySymbol CurrencySymbol
  deriving stock Prelude.Show

newtype ScooperFeeHolderScriptHash = ScooperFeeHolderScriptHash ScriptHash
  deriving stock Prelude.Show
newtype PoolScriptHash = PoolScriptHash ScriptHash
  deriving stock Prelude.Show
newtype EscrowScriptHash = EscrowScriptHash ScriptHash
  deriving stock Prelude.Show

factoryToken :: TokenName
factoryToken = TokenName "factory"

-- Asset name can be 28 bytes; 19 bytes reserved for the week number;
-- That gives us ~5.3e45 weeks to work with.  We should be good :)
{-# inlinable computeScooperTokenName #-}
computeScooperTokenName :: Ident -> TokenName
computeScooperTokenName (Ident ident) = TokenName $ "scooper " <> ident

{-# inlinable computeLiquidityTokenName #-}
computeLiquidityTokenName :: BuiltinByteString -> TokenName
computeLiquidityTokenName poolIdent = TokenName $ "l" <> poolIdent

{-# inlinable computePoolTokenName #-}
computePoolTokenName :: BuiltinByteString -> TokenName
computePoolTokenName poolIdent = TokenName $ "p" <> poolIdent

PlutusTx.makeLift ''OldFactoryBootCurrencySymbol
PlutusTx.makeLift ''ProtocolBootUTXO
PlutusTx.makeLift ''FactoryBootSettings
PlutusTx.makeLift ''TreasuryBootSettings
PlutusTx.makeLift ''UpgradeSettings
PlutusTx.makeLift ''ScooperFeeSettings
PlutusTx.makeIsDataIndexed ''FactoryBootMintRedeemer [('MakeFactory, 0), ('MakeScooperToken, 1)]
PlutusTx.makeIsDataIndexed ''FactoryDatum [('FactoryDatum, 0)]
PlutusTx.makeIsDataIndexed ''FactoryRedeemer [('FactoryRedeemer, 0)]
PlutusTx.makeIsDataIndexed ''ScooperFeeDatum [('ScooperFeeDatum, 0)]
PlutusTx.makeIsDataIndexed ''ScooperFeeRedeemer [('ScooperCollectScooperFees, 0)]
PlutusTx.makeIsDataIndexed ''PoolRedeemer [('PoolScoop, 0)]
PlutusTx.makeIsDataIndexed ''PoolDatum [('PoolDatum, 0)]
PlutusTx.makeIsDataIndexed ''Deposit [('DepositSingle, 0), ('DepositMixed, 1)]
PlutusTx.makeIsDataIndexed ''EscrowAction [('EscrowSwap, 0), ('EscrowWithdraw, 1), ('EscrowDeposit, 2)]
PlutusTx.makeIsDataIndexed ''EscrowDestination [('EscrowDestination, 0)]
PlutusTx.makeIsDataIndexed ''EscrowAddress [('EscrowAddress, 0)]
PlutusTx.makeIsDataIndexed ''EscrowDatum [('EscrowDatum, 0)]
PlutusTx.makeIsDataIndexed ''EscrowRedeemer [('EscrowScoop, 0), ('EscrowCancel, 1)]
PlutusTx.makeIsDataIndexed ''PoolMintRedeemer [('MintLP, 0), ('CreatePool, 1)]
PlutusTx.makeLift ''FactoryBootCurrencySymbol
PlutusTx.makeLift ''TreasuryBootCurrencySymbol
PlutusTx.makeLift ''FactoryScriptHash
PlutusTx.makeLift ''PoolScriptHash
PlutusTx.makeLift ''TreasuryScriptHash
PlutusTx.makeLift ''ScooperFeeHolderScriptHash
PlutusTx.makeLift ''PoolCurrencySymbol
PlutusTx.makeLift ''OldPoolCurrencySymbol
PlutusTx.makeLift ''EscrowScriptHash

--instance Scripts.ValidatorTypes Factory where
--  type instance DatumType Factory = FactoryDatum
--  type instance RedeemerType Factory = FactoryRedeemer
--
--instance Scripts.ValidatorTypes Escrow where
--  type instance DatumType Escrow = EscrowDatum
--  type instance RedeemerType Escrow = EscrowRedeemer
--
--instance Scripts.ValidatorTypes Pool where
--  type instance DatumType Pool = PoolDatum
--  type instance RedeemerType Pool = PoolRedeemer
--

-- Here, instead of utilities, to avoid dependency cycle
{-# inlinable mergeListByKey #-}
mergeListByKey :: [(EscrowDestination, ABL Integer)] -> [(EscrowDestination, ABL Integer, Integer)]
mergeListByKey cs = go cs []
  where
    -- Fold over the constraints, accumulating a list of merged constraints
  go ((r,v):xs) acc =
    -- If we've already seen this before, add together the values and the order counts
   case valueInList r acc Nothing [] of
     Just ((v', c), acc') ->
       let !v'' = v + v'
           !c' = c + 1
       in go xs ((r, v'', c') : acc')
     Nothing -> go xs ((r,v,1) : acc)
  go [] acc = acc

  valueInList _ [] Nothing _ = Nothing
  valueInList _ [] (Just (v, c)) acc = Just ((v, c), acc)
  valueInList r (x@(r', v', c') : tl) res@Nothing acc
    | r == r' = valueInList r tl (Just (v', c')) acc
    | otherwise = valueInList r tl res (x : acc)
  valueInList r (x : tl) res@(Just _) acc = valueInList r tl res (x : acc)

-- Every UTXO in cardano must come with a minimum amount of ADA to prevent dust attacks;
-- We've been calling this the "rider".
-- Technically this is dependent on the number of bytes in the UTXO, but to avoid complications
-- we just fix a specific rider size.  Since most of our protocol is passing NFTs through, this
-- usually isn't an additional requirement for most operations, or it comes back to the user in
-- the long run.
{-# inlinable riderAmount #-}
riderAmount :: Integer
riderAmount = 2_000_000

-- If multiple orders for the same destination are in the same order, we need to
-- subtract the deposit for each, to ensure that ada isn't getting skimmed off the top
{-# inlinable sansRider' #-}
sansRider' :: Integer -> Value -> Value
sansRider' c v =
  let
    !lovelace = valueOf v adaSymbol adaToken
    !finalRider = riderAmount * c
  in
    if lovelace < finalRider
    then die "not enough Ada to cover the rider"
    else
      let v_l = removeSymbol adaSymbol (Map.toList $ getValue v) [] in
        if lovelace - finalRider /= 0 then
          Value $ Map.fromList ((adaSymbol, Map.singleton adaToken (lovelace - finalRider)) : v_l)
        else
          Value $ Map.fromList v_l

{-# inlinable removeSymbol #-}
removeSymbol _ [] acc = acc
removeSymbol sym (x@(cs, _) : tl) acc
  | sym == cs = removeSymbol sym tl acc
  | otherwise = removeSymbol sym tl (x : acc)

-- In the pool contract, we subtract off the riders so as not to affect the price calculation
{-# inlinable sansRider #-}
sansRider :: Value -> Value
sansRider v = sansRider' 1 v

{-# inlinable sansAda #-}
sansAda :: Integer -> Value -> Value
sansAda extra v =
  let
    !lovelace = valueOf v adaSymbol adaToken
  in
    if lovelace < extra
    then die "not enough ada"
    else
      let v_l = removeSymbol adaSymbol (Map.toList $ getValue v) [] in
        if lovelace - extra /= 0 then
          Value $ Map.fromList ((adaSymbol, Map.singleton adaToken (lovelace - extra)) : v_l)
        else
          Value $ Map.fromList v_l

-- Valid fees for the protocol
-- [0.05%, 0.3%, 1%]
{-# inlinable legalSwapFees #-}
legalSwapFees :: [SwapFees]
legalSwapFees = SwapFees <$> [1 % 2000, 3 % 1000, 1 % 100]

-- The largest valid range for most time-sensitive operations
-- We don't get access to an exact time (since the transaction could get accepted in many blocks)
-- so we need to bound the valid range, to ensure we have an approximate idea of the time of the transaction
-- 1000 * 60 * 60 = 3_600_000
{-# inlinable hourMillis #-}
hourMillis :: Integer
hourMillis = 3_600_000

-- In order to allow governance to revoke access to a list of scoopers, we expire the tokens on regular intervals
{-# inlinable scooperLicenseExpiryDelayWeeks #-}
scooperLicenseExpiryDelayWeeks :: Integer
scooperLicenseExpiryDelayWeeks = 1

{-# inlinable scaleInteger #-}
scaleInteger :: Rational -> Integer -> Integer
scaleInteger r n = truncate $ r * fromInteger n

{-# inlinable computeInitialLiquidityTokens #-}
computeInitialLiquidityTokens :: Integer -> Integer -> Integer
computeInitialLiquidityTokens amtA amtB =
  case rsqrt (fromInteger $ amtA * amtB) of
    Exactly n -> n
    Approximately n -> n
    Imaginary -> error ()

{-# inlinable toPoolNft #-}
toPoolNft :: CurrencySymbol -> BuiltinByteString -> AssetClass
toPoolNft cs poolIdent = assetClass cs (computePoolTokenName poolIdent)

makeLenses ''PoolDatum
makeLenses ''EscrowDatum

{-# inlinable isFactory #-}
isFactory :: CurrencySymbol -> TxOut -> Bool
isFactory fbcs o = assetClassValueOf (txOutValue o) factoryNft == 1
  where
  factoryNft = assetClass fbcs factoryToken
