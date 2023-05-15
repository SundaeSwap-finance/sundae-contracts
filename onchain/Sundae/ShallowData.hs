{-# options_ghc -fexpose-all-unfoldings #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language FunctionalDependencies #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DefaultSignatures #-}
module Sundae.ShallowData where

import qualified PlutusLedgerApi.V1.Address as Ledger
import qualified PlutusLedgerApi.V1.Credential as Ledger
import qualified PlutusLedgerApi.V1.Value as Ledger
import qualified PlutusLedgerApi.V1.Scripts as Ledger
import qualified PlutusLedgerApi.V1.Crypto as Ledger
import qualified PlutusLedgerApi.V1.Time as Ledger
import qualified PlutusLedgerApi.V1.DCert as Ledger

import PlutusTx
import Data.Coerce
import PlutusTx.Prelude
import PlutusTx.Builtins
--import Language.Haskell.TH.Syntax (Q, TExp)

-- We optimize a few scripts by only deserializing *part* of their
-- ScriptContext's from the `Data` that they're given.  This file lets us do
-- that in a type-safe way. The serialization schema of ScriptContext is fixed,
-- which justifies this optimization.

-- This stuff is kind of gross. We'd like to use a data family for the
-- unwrapped version of each type. Unfortunately, those aren't supported by
-- `makeIsDataIndexed` and such. Barring that, we'd like to use a type family to link
-- each typed to its unwrapped version. That's not supported by the Plutus
-- compiler.  We'd really like to use pattern synonyms. Their desugaring uses
-- features that, again, are not supported by Plutus. It would also be very
-- nice to be able to generate all this stuff automagically. But the Template
-- Haskell name munging sounds a little tricky, and we'd need to use manually
-- constructed `makeIsDataIndexed` instances regardless, since there's no
-- remotely simple way to work out what those should look like.

class Unwrappable a b | a -> b, b -> a

{-# INLINE unwrap #-}
unwrap :: (Goercible a, UnsafeFromData b, Unwrappable a b) => a -> b
unwrap = unsafeFromBuiltinData . goerce

class Goercible a where
  goerce :: a -> BuiltinData
  -- I can't seem to get an unfolding for the obvious goerce = coerce implementation.
  -- *sigh*

newtype Datum = Datum__ BuiltinData
  deriving newtype (UnsafeFromData, ToData, FromData)
instance Goercible Datum where goerce = coerce
makeLift ''Datum
instance Unwrappable Datum Ledger.Datum

newtype DatumHash = DatumHash__ BuiltinData
  deriving newtype (UnsafeFromData, ToData, FromData)
instance Goercible DatumHash where goerce = coerce
makeLift ''DatumHash
instance Unwrappable DatumHash Ledger.DatumHash

newtype PubKeyHash = PubKeyHash__ BuiltinData
  deriving newtype (UnsafeFromData, ToData, FromData, Eq)
instance Goercible PubKeyHash where goerce = coerce
makeLift ''PubKeyHash
instance Unwrappable PubKeyHash Ledger.PubKeyHash

newtype POSIXTimeRange = POSIXTimeRange__ BuiltinData
  deriving newtype (UnsafeFromData, ToData, FromData)
instance Goercible POSIXTimeRange where goerce = coerce
makeLift ''POSIXTimeRange
instance Unwrappable POSIXTimeRange Ledger.POSIXTimeRange

newtype DCert = DCert__ BuiltinData
  deriving newtype (UnsafeFromData, ToData, FromData)
instance Goercible DCert where goerce = coerce
makeLift ''DCert
instance Unwrappable DCert Ledger.DCert

newtype Value = Value__ BuiltinData
  deriving newtype (Eq, UnsafeFromData, ToData, FromData)
instance Goercible Value where goerce = coerce
makeLift ''Value
instance Unwrappable Value Ledger.Value

newtype Credential = Credential__ BuiltinData
  deriving newtype (UnsafeFromData, ToData, FromData)
instance Goercible Credential where goerce = coerce
makeLift ''Credential
instance Unwrappable Credential Ledger.Credential

newtype StakingCredential = StakingCredential__ BuiltinData
  deriving newtype (Eq, UnsafeFromData, ToData, FromData)
instance Goercible StakingCredential where goerce = coerce
instance Unwrappable StakingCredential Ledger.StakingCredential

newtype Address = Address__ BuiltinData
  deriving newtype (Eq, UnsafeFromData, ToData, FromData)
instance Goercible Address where goerce = coerce
-- data Address__U = Address{ addressCredential :: Credential, addressStakingCredential :: Maybe StakingCredential }
-- PlutusTx.makeIsDataIndexed ''Address__U [('Address,0)]
instance Unwrappable Address Ledger.Address

newtype TxId = TxId__ BuiltinData
  deriving newtype (Eq, UnsafeFromData, ToData, FromData)
instance Goercible TxId where goerce = coerce
newtype TxId__U = TxId { getTxId :: BuiltinByteString }
makeIsDataIndexed ''TxId__U [('TxId,0)]

newtype TxOutRef = TxOutRef__ BuiltinData
  deriving newtype (Eq, UnsafeFromData, ToData, FromData)
instance Goercible TxOutRef where goerce = coerce
makeLift ''TxOutRef

data TxOutRef__U = TxOutRef {
    txOutRefId  :: TxId,
    txOutRefIdx :: Integer -- ^ Index into the referenced transaction's outputs
    }
PlutusTx.makeIsDataIndexed ''TxOutRef__U [('TxOutRef,0)]

instance Eq TxOutRef__U where
  {-# inlinable (==) #-}
  TxOutRef i idx == TxOutRef i' idx'
    | i == i'
    , idx == idx' = True
    | otherwise = False
instance Unwrappable TxOutRef TxOutRef__U

newtype TxOut = TxOut__ BuiltinData
  deriving newtype (Eq, UnsafeFromData, ToData, FromData)
instance Goercible TxOut where goerce = coerce
data TxOut__U = TxOut {
    txOutAddress   :: Address,
    txOutValue     :: Value,
    txOutDatumHash :: Maybe Ledger.DatumHash
    }
instance Unwrappable TxOut TxOut__U
PlutusTx.makeIsDataIndexed ''TxOut__U [('TxOut,0)]

newtype TxInInfo = TxInInfo__ BuiltinData
  deriving newtype (Eq, UnsafeFromData, ToData, FromData)
instance Goercible TxInInfo where goerce = coerce

data TxInInfo__U = TxInInfo
    { txInInfoOutRef   :: TxOutRef
    , txInInfoResolved :: TxOut
    }
instance Unwrappable TxInInfo TxInInfo__U
makeIsDataIndexed ''TxInInfo__U [('TxInInfo,0)]

newtype DeferredPair a b = DeferredPair__ BuiltinData
  deriving newtype (UnsafeFromData, ToData, FromData)
instance Goercible (DeferredPair a b) where goerce = coerce
data DeferredPair__U a b = DeferredPair a b
PlutusTx.makeIsDataIndexed ''DeferredPair__U [('DeferredPair,0)]
instance Unwrappable (DeferredPair a b) (DeferredPair__U a b)

newtype TxInfo = TxInfo__ BuiltinData
  deriving newtype (UnsafeFromData, ToData, FromData)
instance Goercible TxInfo where goerce = coerce
data TxInfo__U = TxInfo
    { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
    , txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
    , txInfoFee         :: Value -- ^ The fee paid by this transaction.
    , txInfoMint        :: Value -- ^ The 'Value' minted by this transaction.
    , txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        :: [DeferredPair StakingCredential Integer] -- ^ Withdrawals
    , txInfoValidRange  :: POSIXTimeRange -- ^ The valid range for the transaction.
    , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        :: [DeferredPair DatumHash Datum]
    , txInfoId          :: TxId
    -- ^ Hash of the pending transaction (excluding witnesses)
    }
instance Unwrappable TxInfo TxInfo__U
instance UnsafeFromData TxInfo__U where
  unsafeFromBuiltinData (unsafeDataAsConstr ->
    (_, [inputs, outputs, fee, mint, dcert, wdrl, vrng, signts, infdat, infid]))
    = TxInfo (coerce (unsafeDataAsList inputs)) (coerce (unsafeDataAsList outputs)) (coerce fee) (coerce mint) (coerce (unsafeDataAsList dcert)) (coerce (unsafeDataAsList wdrl))
             (coerce vrng) (coerce (unsafeDataAsList signts)) (coerce (unsafeDataAsList infdat)) (coerce infid)
  unsafeFromBuiltinData _ = traceError "Unexpected TxInfo construction."

newtype ScriptPurpose = ScriptPurpose__ BuiltinData
  deriving newtype (Eq, UnsafeFromData, ToData, FromData)

instance Goercible ScriptPurpose where goerce = coerce

data ScriptPurpose__U
  = Minting Ledger.CurrencySymbol
  | Spending TxOutRef
  -- | ...

instance Unwrappable ScriptPurpose ScriptPurpose__U
makeIsDataIndexed ''ScriptPurpose__U [('Minting,0), ('Spending,1)]

newtype ScriptContext = ScriptContext__ BuiltinData
  deriving newtype (Eq, UnsafeFromData, ToData,FromData)
instance Goercible ScriptContext where goerce = coerce
makeLift ''ScriptContext

data ScriptContext__U
  = ScriptContext
  { scriptContextTxInfo :: TxInfo
  , scriptContextPurpose :: ScriptPurpose
  }
instance Unwrappable ScriptContext ScriptContext__U
makeIsDataIndexed ''ScriptContext__U [('ScriptContext,0)]

grum :: BuiltinData -> (TxInfo, ScriptPurpose)
grum (unwrap . unsafeFromBuiltinData -> ScriptContext info purpose) = (info, purpose)

potato :: CompiledCode (BuiltinData -> (TxInfo, ScriptPurpose))
potato = $$(compile [|| grum ||])
