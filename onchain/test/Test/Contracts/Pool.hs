{-# language TypeApplications #-}
{-# language ViewPatterns #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}

module Test.Contracts.Pool(tests) where

import Prelude
import qualified PlutusTx.Prelude as Plutus
import qualified PlutusTx.Ratio as Plutus
import Control.Exception
import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit

import PlutusLedgerApi.V3
import PlutusLedgerApi.V1.Value

import Sundae.Contracts as Sundae
import Sundae.Utilities

import System.IO.Unsafe

import Test.Contracts.Orphans
import Test.Contracts.Utils
import PlutusLedgerApi.V1 (Credential(PubKeyCredential))

data ScoopTest
  = ScoopTest
  { poolCoins :: AB AssetClass
  , poolCond :: Cond
  , escrow1Cond :: Cond
  , escrow2Cond :: Cond
  , poolMintCond :: Cond
  , editMinted :: Value -> Value
  , editDisbursed :: [(Address, Value)] -> [(Address, Value)]
  , editScooperInputValue :: Value -> Value
  , editEscrow1Value :: Value -> Value
  , editEscrow1Datum :: EscrowDatum -> EscrowDatum
  , editEscrow2Value :: Value -> Value
  , editEscrow2Datum :: EscrowDatum -> EscrowDatum
  , editOldPoolValue :: Value -> Value
  , editPoolOutputValue :: Value -> Value
  , editPoolAddress :: Address -> Address
  , editMinTakes :: Integer -> Integer
  , editValidRange :: Interval POSIXTime -> Interval POSIXTime
  , editNewPoolDatum :: PoolDatum -> PoolDatum
  , editPoolRedeemer :: PoolRedeemer -> PoolRedeemer
  , editEscrowRedeemer :: EscrowRedeemer -> EscrowRedeemer
  , editFee :: Value -> Value
  }

defaultValidScoopParams :: ScoopTest
defaultValidScoopParams =
  ScoopTest
  { poolCoins = AB sundaeCoin swapCoin
  , poolCond = Pass
  , escrow1Cond = Pass
  , escrow2Cond = Pass
  , poolMintCond = Pass
  , editMinted = id
  , editDisbursed = id
  , editScooperInputValue = id
  , editEscrow1Value = id
  , editEscrow1Datum = id
  , editEscrow2Value = id
  , editEscrow2Datum = id
  , editOldPoolValue = id
  , editPoolOutputValue = id
  , editMinTakes = id
  , editValidRange = id
  , editNewPoolDatum = id
  , editPoolRedeemer = id
  , editEscrowRedeemer = id
  , editPoolAddress = id
  , editFee = id
  }

getIdent :: Ident -> BuiltinByteString
getIdent (Ident i) = i

mkScoopTest :: ScoopTest -> IO ()
mkScoopTest ScoopTest{..} = do
  let (AB coin1 coin2) = poolCoins
      (poolAmt1, poolAmt2) = (2000, 3000)
      initialLiquidityTokenCount = computeInitialLiquidityTokens poolAmt1 poolAmt2
      (depositAmt1, depositAmt2) = (200, 300)
      swapAmt = 100
      swapAmtReceived = 142 -- Calculated on repl
      extraLiquidity = (depositAmt1 * initialLiquidityTokenCount) `div` poolAmt1
      poolIdent = case intToIdent 0 of { Ident i -> i }
      minAda = lovelaceValue 2_000_000
      scooperInputValue = editScooperInputValue $ assetClassValue (scooperTokenAC $ intToIdent 0) 1
      escrow1Value = editEscrow1Value $ (assetClassValue coin1 depositAmt1 <> assetClassValue coin2 depositAmt2 <> minAda <> lovelaceValue testScoopFee)
      escrow1Datum = editEscrow1Datum $ EscrowDatum (EscrowAddress user1Dest Nothing) testScoopFee (EscrowDeposit poolIdent (DepositMixed (AB depositAmt1 depositAmt2)))
      escrow2Value = editEscrow2Value $ (assetClassValue coin1 swapAmt <> minAda <> lovelaceValue testScoopFee)
      escrow2Datum = editEscrow2Datum $ EscrowDatum (EscrowAddress user2Dest Nothing) testScoopFee (EscrowSwap (coin1, swapAmt) (coin2, Just (editMinTakes swapAmtReceived)))
      oldPoolValue = editOldPoolValue $ assetClassValue coin1 poolAmt1 <> assetClassValue coin2 poolAmt2 <> assetClassValue (poolAC poolIdent) 1 <> minAda
      disbursed = editDisbursed $ [(user1Addr, assetClassValue (liquidityAC poolIdent) extraLiquidity <> minAda), (user2Addr, assetClassValue coin2 swapAmtReceived <> minAda)]
      newAmtA = poolAmt1 + depositAmt1 + swapAmt
      newAmtB = poolAmt2 + depositAmt2 - swapAmtReceived
      newIssued = initialLiquidityTokenCount + extraLiquidity
      minted = editMinted $ assetClassValue (liquidityAC poolIdent) (newIssued - initialLiquidityTokenCount)
      txFee = editFee (lovelaceValue 1)
      scooperFee = 2_500_000
      rewards = max 0 (2 * scooperFee - lovelaceOf txFee)
      newPoolValue = editPoolOutputValue $ assetClassValue coin1 newAmtA <> assetClassValue coin2 newAmtB <> assetClassValue (poolAC poolIdent) 1 <> lovelaceValue rewards <> minAda
      newPoolDatum = editNewPoolDatum (PoolDatum (AB coin1 coin2) poolIdent newIssued testSwapFees 0 rewards)
      newPoolAddr = editPoolAddress poolAddress
      poolRedeemer = editPoolRedeemer (PoolScoop scooperUserPkh [0, 1])
      escrowRedeemer = editEscrowRedeemer EscrowScoop
      interval = editValidRange (hourInterval (POSIXTime 0))
      factoryValue = assetClassValue factoryAC 1
  runStep $
    [ fromEscrow escrow1Value "Escrow1 script call with scoop" escrow1Cond escrowRedeemer
        escrow1Datum
    , fromEscrow escrow2Value "Escrow2 script call with scoop" escrow2Cond escrowRedeemer
        escrow2Datum
    , fromPool oldPoolValue "Pool script call with scoop" poolCond poolRedeemer
        (PoolDatum (AB coin1 coin2) poolIdent initialLiquidityTokenCount testSwapFees 0 0)
    , referenceFactory factoryValue (toData $ FactoryDatum initialIdent NoProposal initialIdent [scooperUserPkh] [poolStakingCred])
    , toPool newPoolValue newPoolDatum newPoolAddr
    , PoolMint minted poolMintCond poolIdent
    , CustomInterval interval
    , FromUser scooperUserAddr scooperInputValue
    , CustomSignatories [scooperUserPkh]
    , TxFee txFee
    ] ++ (uncurry ToUser <$> disbursed)
  where
    toPool v d a = ToScript a v (toData d)
    fromPool = fromPoolScript poolAddress
    fromEscrow = fromEscrowScript escrowAddress
    referenceFactory = referenceFactoryScript factoryAddress

tests :: TestTree
tests =
  testGroup "Pool contract"
    [ testByCoin "Non-ADA pair" (AB sundaeCoin swapCoin)
    , testByCoin "ADA pair" (AB adaCoin sundaeCoin)
    ]

testByCoin :: String -> AB AssetClass -> TestTree
testByCoin title coins@(AB coin1 coin2) =
  testGroup title [validTest, invalidTest]
  where
  validScoopParams = defaultValidScoopParams { poolCoins = coins }

  invalidTest = testGroup "Expecting failure"
    [ scoopOutputLacksLiquidity
    , scoopIssuedTooMuch
    , scoopIssuedNotEnough
    , scoopsMintedTooMuch
    , scoopsMintedNotEnough
    , scoopNotPayingUser
    , scoopNotPayingAnyUsers
    , scoopJackingUserFunds
    , invalidDepositAmount
    , invalidSwapAmount
    , invalidWithdrawAmount
    , slippageFailure
    , failedDoubleSwap
    , changingCoins
    , changingIdent
    , changingSwapFees
    , unboundedValidRangeUpwards
    , unboundedValidRangeDownwards
    , unboundedValidRangeBothWays
    , mintingPoolToken
    , upgradeRedeemer
    , cancelRedeemer
    , stolenPoolToken
    , noPoolToken
    , escrowForDifferentPool
    , hugeSwap
    , extraPoolAssets
    , escrowWithNegativeFee
    , stolenPoolToken
    , poolChangePayment
    , swapTooEarly
    , rewardsNotPaidToPool
    , poolRewardsFieldNotChanged
    , noExtraLockedRewards
    , requireSufficientLockedRewards
    , marketOrderTakeCoinDoesn'tMatchPool
    ]
  validTest = testGroup "Expecting success"
    [ validScoop
    , validScoopWithWithdraw
    , validDoubleDeposit
    , escrowExtraAda
    , escrowExtraSwapCoin
    , escrowCancellationKey
    , validateSingleDeposit
    , validDepositOnDifferentRate
    , validDoubleSwap
    , validTwoOrdersSamePerson
    , poolSetStaking
    , poolUnsetStaking
    , feeMatchesRewards
    , feeExceedsRewards
    , marketOrderTakeCoinMatchesPool
    ]
  -- User 1 is depositing.
  -- User 2 is swapping.
  testValidScoop = evaluate $ unsafePerformIO $ mkScoopTest validScoopParams
  validScoop = testCase "Should pass a valid scoop of deposit + swap" $ testValidScoop
  validDoubleDeposit = testCase "Should pass a correct double deposit" $ do
    testValidScoop
    let (depositAmt1, depositAmt2) = (200, 300)
        (poolAmt1, poolAmt2) = (2000, 3000)
        initialLiquidityTokenCount = computeInitialLiquidityTokens poolAmt1 poolAmt2
        extraLiquidity = (depositAmt1 * initialLiquidityTokenCount) `div` poolAmt1
        newAmtA = poolAmt1 + depositAmt1*2
        newAmtB = poolAmt2 + depositAmt2*2
        poolIdent = getIdent initialIdent
        minAda = lovelaceValue 2_000_000
        txFee = 1
        scooperFee = 2_500_000
        rewards = 2 * scooperFee - txFee
        invalid = validScoopParams
                  { editEscrow2Value = const $ assetClassValue coin1 depositAmt1 <> assetClassValue coin2 depositAmt2 <> minAda <> lovelaceValue testScoopFee
                  , editEscrow2Datum = const $ EscrowDatum (EscrowAddress user1Dest Nothing) testScoopFee (EscrowDeposit poolIdent (DepositMixed (AB depositAmt1 depositAmt2)))
                  , editNewPoolDatum = pool'circulatingLP .~ initialLiquidityTokenCount + extraLiquidity*2
                  , editMinted = at (liquidityAC poolIdent) .~ Just (extraLiquidity * 2)
                  , editPoolOutputValue = const $ assetClassValue coin1 newAmtA <> assetClassValue coin2 newAmtB <> assetClassValue (poolAC poolIdent) 1 <> lovelaceValue rewards <> minAda
                  , editDisbursed = const $ [(user1Addr, assetClassValue (liquidityAC poolIdent) (extraLiquidity*2) <> minAda <> minAda)] -- two orders, two riders
                  }
    mkScoopTest invalid

  validDepositOnDifferentRate = testCase "Should pass a correct double deposit with different rate" $ mkScoopTest $
    let (depositAmt1, depositAmt2) = (100, 100)
        (poolAmt1, poolAmt2) = (2000, 3000)
        initialLiquidityTokenCount = computeInitialLiquidityTokens poolAmt1 poolAmt2
        -- computed on REPL
        userLiq = 324
        change1 = 34
        newAmt1 = 2266
        newAmt2 = 3400
        poolIdent = getIdent initialIdent
        minAda = lovelaceValue 2_000_000
        txFee = 1
        scooperFee = 2_500_000
        rewards = 2 * scooperFee - txFee
    in  validScoopParams
          { editEscrow2Value = const $ assetClassValue coin1 depositAmt1 <> assetClassValue coin2 depositAmt2 <> minAda <> lovelaceValue testScoopFee
          , editEscrow2Datum = const $ EscrowDatum (EscrowAddress user1Dest Nothing) testScoopFee (EscrowDeposit poolIdent (DepositMixed (AB depositAmt1 depositAmt2)))
          , editNewPoolDatum = pool'circulatingLP .~ initialLiquidityTokenCount + userLiq
          , editMinted = at (liquidityAC poolIdent) .~ Just userLiq
          , editPoolOutputValue = const $ assetClassValue coin1 newAmt1 <> assetClassValue coin2 newAmt2 <> assetClassValue (poolAC poolIdent) 1 <> lovelaceValue rewards <> minAda
          , editDisbursed = const $ [(user1Addr, assetClassValue coin1 change1 <> assetClassValue (liquidityAC poolIdent) userLiq <> minAda <> minAda)] -- two orders, two riders
          }

  validateSingleDeposit = testCase "Should pass single deposit" $ mkScoopTest $
    let depositAmt1 = 100
        depositAmt2 = 200
        (poolAmt1, poolAmt2) = (2000, 3000)
        initialLiquidityTokenCount = computeInitialLiquidityTokens poolAmt1 poolAmt2
        -- computed on REPL
        userLiq = 141
        newAmt1 = 2100
        newAmt2 = 3200
        poolIdent = getIdent initialIdent
        minAda = lovelaceValue 2_000_000
        txFee = 1
        scooperFee = 2_500_000
        rewards = 2 * scooperFee - txFee
        userDeposit1 x = EscrowDatum (EscrowAddress user1Dest Nothing) testScoopFee (EscrowDeposit poolIdent x)
    in  validScoopParams
          { editEscrow1Value = const $ assetClassValue coin1 depositAmt1 <> minAda <> lovelaceValue testScoopFee
          , editEscrow1Datum = const $ userDeposit1 (DepositSingle CoinA depositAmt1)
          , editEscrow2Value = const $ assetClassValue coin2 depositAmt2 <> minAda <> lovelaceValue testScoopFee
          , editEscrow2Datum = const $ userDeposit1 (DepositSingle CoinB depositAmt2)
          , editNewPoolDatum = pool'circulatingLP .~ initialLiquidityTokenCount + userLiq
          , editMinted = at (liquidityAC poolIdent) .~ Just userLiq
          , editPoolOutputValue = const $ assetClassValue coin1 newAmt1 <> assetClassValue coin2 newAmt2 <> assetClassValue (poolAC poolIdent) 1 <> lovelaceValue rewards <> minAda
          , editDisbursed = const $ [(user1Addr, assetClassValue (liquidityAC poolIdent) userLiq <> minAda <> minAda)] -- two orders, two riders
          }

  validScoopWithWithdraw = testCase "Should pass a valid scoop with deposit + withdraw" $ do
    testValidScoop
    let (depositAmt1, depositAmt2) = (200, 300)
        (poolAmt1, poolAmt2) = (2000, 3000)
        initialLiquidityTokenCount = computeInitialLiquidityTokens poolAmt1 poolAmt2
        extraLiquidity = (depositAmt1 * initialLiquidityTokenCount) `div` poolAmt1
        newAmtA = poolAmt1 + depositAmt1 - withdrawReceived1
        newAmtB = poolAmt2 + depositAmt2 - withdrawReceived2
        withdrawAmount = 100
        -- computed on REPL
        (withdrawReceived1, withdrawReceived2) = (81, 122)
        poolIdent = getIdent initialIdent
        minAda = lovelaceValue 2_000_000
        txFee = 1
        scooperFee = 2_500_000
        rewards = 2 * scooperFee - txFee
        valid = validScoopParams
                  { editEscrow2Value = const $ assetClassValue (liquidityAC (getIdent initialIdent)) withdrawAmount <> minAda <> lovelaceValue testScoopFee
                  , editEscrow2Datum = const $ EscrowDatum (EscrowAddress user2Dest Nothing) testScoopFee (EscrowWithdraw poolIdent withdrawAmount)
                  , editNewPoolDatum = pool'circulatingLP .~ initialLiquidityTokenCount + extraLiquidity - withdrawAmount
                  , editMinted = at (liquidityAC (getIdent initialIdent)) .~ Just (extraLiquidity - withdrawAmount)
                  , editPoolOutputValue = const $ assetClassValue coin1 newAmtA <> assetClassValue coin2 newAmtB <> assetClassValue (poolAC poolIdent) 1 <> lovelaceValue rewards <> minAda
                  , editDisbursed = const $ [ (user1Addr, assetClassValue (liquidityAC poolIdent) extraLiquidity <> minAda)
                                            , (user2Addr, assetClassValue coin1 withdrawReceived1 <> assetClassValue coin2 withdrawReceived2 <> minAda)]
                  }
    mkScoopTest valid

  -- Pool output < expected liquidity
  scoopOutputLacksLiquidity = testCase "Fails when pool output lacks liquidity" $ do
    testValidScoop
    let invalid = validScoopParams
                  { editPoolOutputValue = (<> assetClassValue coin2 (-1))
                  , poolCond = Fail
                  }
    mkScoopTest invalid

  scoopIssuedNotEnough = testCase "Should fail on issued liquidity < correctly issued liquidity" $ do
    testValidScoop
    let invalid = validScoopParams
                  { editNewPoolDatum = pool'circulatingLP -~ 1
                  , editMinted = at (liquidityAC (getIdent initialIdent)) . _Just -~ 1
                  , poolCond = Fail
                  }
    mkScoopTest invalid

  scoopIssuedTooMuch = testCase "Should fail on issued liquidity > correctly issued liquidity" $ do
    testValidScoop
    let invalid = validScoopParams
                  { editNewPoolDatum = pool'circulatingLP +~ 1
                  , editMinted = at (liquidityAC (getIdent initialIdent)) . _Just +~ 1
                  , poolCond = Fail
                  }
    mkScoopTest invalid

  scoopsMintedNotEnough = testCase "Should fail on minted < delta issued " $ do
    testValidScoop
    let invalid = validScoopParams
                  { editMinted = at (liquidityAC (getIdent initialIdent)) . _Just -~ 1
                  , poolCond = Fail
                  }
    mkScoopTest invalid

  scoopsMintedTooMuch = testCase "Should fail on minted > delta issued " $ do
    testValidScoop
    let invalid = validScoopParams
                  { editMinted = at (liquidityAC (getIdent initialIdent)) . _Just +~ 1
                  , poolCond = Fail
                  }
    mkScoopTest invalid

  scoopNotPayingUser = testCase "Should fail on a user not paid out" $ do
    testValidScoop
    let invalid = validScoopParams
                  { editDisbursed = tail
                  , poolCond = Fail
                  }
    mkScoopTest invalid

  scoopNotPayingAnyUsers = testCase "Should fail on all users not paid out" $ do
    testValidScoop
    let invalid = validScoopParams
                  { editDisbursed = const []
                  , poolCond = Fail
                  }
    mkScoopTest invalid

  scoopJackingUserFunds = testCase "Should fail on a user trying to steal funds" $ do
    testValidScoop
    let (depositAmt1, depositAmt2) = (200, 300)
        (poolAmt1, poolAmt2) = (2000, 3000)
        initialLiquidityTokenCount = computeInitialLiquidityTokens poolAmt1 poolAmt2
        extraLiquidity = (depositAmt1 * initialLiquidityTokenCount) `div` poolAmt1
        newAmtA = poolAmt1 + depositAmt1*2
        newAmtB = poolAmt2 + depositAmt2*2
        poolIdent = getIdent initialIdent
        minAda = lovelaceValue 2_000_000
        txFee = 1
        scooperFee = 2_500_000
        rewards = 2 * scooperFee - txFee
        invalid = validScoopParams
                  { editEscrow2Value = const $ assetClassValue coin1 depositAmt1 <> assetClassValue coin2 depositAmt2 <> minAda <> lovelaceValue testScoopFee
                  , editEscrow2Datum = const $ EscrowDatum (EscrowAddress user1Dest Nothing) testScoopFee (EscrowDeposit poolIdent (DepositMixed (AB depositAmt1 depositAmt2)))
                  , editNewPoolDatum = pool'circulatingLP .~ initialLiquidityTokenCount + extraLiquidity*2
                  , editMinted = at (liquidityAC (getIdent initialIdent)) .~ Just (initialLiquidityTokenCount + extraLiquidity*2)
                  , editPoolOutputValue = const $ assetClassValue coin1 newAmtA <> assetClassValue coin2 newAmtB <> assetClassValue (poolAC poolIdent) 1 <> lovelaceValue rewards <> minAda
                  , editDisbursed = const $ [(user1Addr, assetClassValue (liquidityAC poolIdent) extraLiquidity <> minAda)
                                    , (user2Addr, assetClassValue (liquidityAC poolIdent) extraLiquidity <> minAda)]
                  , poolCond = Fail}
    mkScoopTest invalid

  invalidDepositAmount = testCase "Should fail on a deposit without the right amount of tokens" $ do
    testValidScoop
    let invalid = validScoopParams
                  { editEscrow1Value = (<> assetClassValue coin2 (-199))
                  , poolCond = Fail}
    mkScoopTest invalid

  invalidSwapAmount = testCase "Should fail on a swap without the right amount of tokens" $ do
    testValidScoop
    let invalid = validScoopParams
                  { editEscrow2Value = (<> assetClassValue coin1 (-140))
                  , poolCond = Fail}
    mkScoopTest invalid

  invalidWithdrawAmount = testCase "Should fail on a withdrawal without the right amount of tokens" $ do
    testValidScoop
    let (depositAmt1, depositAmt2) = (200, 300)
        (poolAmt1, poolAmt2) = (2000, 3000)
        initialLiquidityTokenCount = computeInitialLiquidityTokens poolAmt1 poolAmt2
        extraLiquidity = (depositAmt1 * initialLiquidityTokenCount) `div` poolAmt1
        newAmtA = poolAmt1 + depositAmt1 - withdrawReceived1
        newAmtB = poolAmt2 + depositAmt2 - withdrawReceived2
        withdrawAmount = 100
        (withdrawReceived1, withdrawReceived2) = (81, 122)
        poolIdent = getIdent initialIdent
        minAda = lovelaceValue 2_000_000
        txFee = 1
        scooperFee = 2_500_000
        rewards = 2 * scooperFee - txFee
        invalid = validScoopParams
                  { editEscrow2Value = const $ assetClassValue (liquidityAC (getIdent initialIdent)) (withdrawAmount - 100) <> minAda <> lovelaceValue testScoopFee
                  , editEscrow2Datum = const $ EscrowDatum (EscrowAddress user2Dest Nothing) testScoopFee (EscrowWithdraw poolIdent (withdrawAmount - 1))
                  , editNewPoolDatum = pool'circulatingLP .~ initialLiquidityTokenCount + extraLiquidity - (withdrawAmount - 100)
                  , editPoolOutputValue = const $ assetClassValue coin1 newAmtA <> assetClassValue coin2 newAmtB <> assetClassValue (poolAC poolIdent) 1 <> lovelaceValue rewards <> minAda
                  , editDisbursed = const $ [ (user1Addr, assetClassValue (liquidityAC poolIdent) extraLiquidity <> minAda)
                                            , (user2Addr, assetClassValue coin1 withdrawReceived1 <> assetClassValue coin2 withdrawReceived2 <> minAda)]
                  , poolCond = Fail
                  }
    mkScoopTest invalid

  slippageFailure = testCase "Should fail on a swap where the slippage amt > amt received" $ do
    testValidScoop
    let invalid = validScoopParams
                  { editMinTakes = (+ 2)
                  , poolCond = Fail}
    mkScoopTest invalid

  validDoubleSwap = testCase "Should work on a double swap" $ do
    let swapAmt = 200
        swapReceived1 = 270
        swapReceived2 = 225
        (poolAmt1, poolAmt2) = (2000, 3000)
        initialLiquidityTokenCount = computeInitialLiquidityTokens poolAmt1 poolAmt2
        newAmtA = poolAmt1 + swapAmt*2
        newAmtB = poolAmt2 - swapReceived1 - swapReceived2
        poolIdent = getIdent initialIdent
        minAda = lovelaceValue 2_000_000
        txFee = 1
        scooperFee = 2_500_000
        rewards = 2 * scooperFee - txFee
        valid = validScoopParams
                  { editEscrow1Value = const $ assetClassValue coin1 swapAmt <> minAda <> lovelaceValue testScoopFee
                  , editEscrow1Datum = const $ EscrowDatum (EscrowAddress user1Dest Nothing) testScoopFee (EscrowSwap (coin1, swapAmt) (coin2, Just swapReceived1))
                  , editEscrow2Value =  const $ assetClassValue coin1 swapAmt <> minAda <> lovelaceValue testScoopFee
                  , editEscrow2Datum = const $ EscrowDatum (EscrowAddress user2Dest Nothing) testScoopFee (EscrowSwap (coin1, swapAmt) (coin2, Just swapReceived2))
                  , editNewPoolDatum = pool'circulatingLP .~ initialLiquidityTokenCount
                  , editMinted = at (liquidityAC (getIdent initialIdent)) .~ Nothing
                  , editPoolOutputValue = const $ assetClassValue coin1 newAmtA <> assetClassValue coin2 newAmtB <> assetClassValue (poolAC poolIdent) 1 <> lovelaceValue rewards <> minAda
                  , editDisbursed = const $ [ (user1Addr, assetClassValue coin2 swapReceived1 <> minAda)
                                            , (user2Addr, assetClassValue coin2 swapReceived2 <> minAda)]
                  }
    mkScoopTest valid

  validTwoOrdersSamePerson = testCase "Should work with two orders from the same user" $ do
    let swapAmt = 200
        swapReceived1 = 270
        swapReceived2 = 225
        swapReceivedT = swapReceived1 + swapReceived2
        (poolAmt1, poolAmt2) = (2000, 3000)
        initialLiquidityTokenCount = computeInitialLiquidityTokens poolAmt1 poolAmt2
        newAmtA = poolAmt1 + swapAmt*2
        newAmtB = poolAmt2 - swapReceivedT
        poolIdent = getIdent initialIdent
        minAda = lovelaceValue 2_000_000
        txFee = 1
        scooperFee = 2_500_000
        rewards = 2 * scooperFee - txFee
        valid = validScoopParams
                  { editEscrow1Value = const $ assetClassValue coin1 swapAmt <> minAda <> lovelaceValue testScoopFee
                  , editEscrow1Datum = const $ EscrowDatum (EscrowAddress user1Dest Nothing) testScoopFee (EscrowSwap (coin1, swapAmt) (coin2, Just swapReceived1))
                  , editEscrow2Value =  const $ assetClassValue coin1 swapAmt <> minAda <> lovelaceValue testScoopFee
                  , editEscrow2Datum = const $ EscrowDatum (EscrowAddress user1Dest Nothing) testScoopFee (EscrowSwap (coin1, swapAmt) (coin2, Just swapReceived2))
                  , editNewPoolDatum = pool'circulatingLP .~ initialLiquidityTokenCount
                  , editMinted = at (liquidityAC (getIdent initialIdent)) .~ Nothing
                  , editPoolOutputValue = const $ assetClassValue coin1 newAmtA <> assetClassValue coin2 newAmtB <> assetClassValue (poolAC poolIdent) 1 <> lovelaceValue rewards <> minAda
                  , editDisbursed = const $ [ (user1Addr, assetClassValue coin2 swapReceivedT <> minAda <> minAda)]
                  }
    mkScoopTest valid

  escrowExtraAda = testCase "an escrow with extra ada" $ do
    mkScoopTest validScoopParams
      { editEscrow1Value = at (assetClass adaSymbol adaToken) . _Just +~ 10_000_000
      }

  escrowExtraSwapCoin = testCase "an escrow with extra 'swap coin'" $ do
    mkScoopTest validScoopParams
      { editEscrow1Value = at swapCoin . _Just +~ 10_000_000
      }

  escrowCancellationKey = testCase "an escrow with an extra cancellation key" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editEscrow1Datum = escrow'address %~ (\(EscrowAddress dest Nothing) -> EscrowAddress dest (Just user2Pkh))
      }

  failedDoubleSwap = testCase "Should fail on a swap where slippage causes failure" $ do
    testValidScoop
    let swapAmt = 200
        swapReceived1 = 271
        swapReceived2 = 226
        swapMinTakes = 250
        (poolAmt1, poolAmt2) = (2000, 3000)
        initialLiquidityTokenCount = computeInitialLiquidityTokens poolAmt1 poolAmt2
        newAmtA = poolAmt1 + swapAmt*2
        newAmtB = poolAmt2 - swapReceived1 - swapReceived2
        poolIdent = getIdent initialIdent
        minAda = lovelaceValue 2_000_000
        txFee = 1
        scooperFee = 2_500_000
        rewards = 2 * scooperFee - txFee
        invalid = validScoopParams
                  { editEscrow1Value = const $ assetClassValue coin1 swapAmt <> minAda
                  , editEscrow1Datum = const $ EscrowDatum (EscrowAddress user1Dest Nothing) testScoopFee (EscrowSwap (coin1, swapAmt) (coin2, Just swapMinTakes))
                  , editEscrow2Value =  const $ assetClassValue coin1 swapAmt <> minAda
                  , editEscrow2Datum = const $ EscrowDatum (EscrowAddress user2Dest Nothing) testScoopFee (EscrowSwap (coin1, swapAmt) (coin2, Just swapMinTakes))
                  , editNewPoolDatum = pool'circulatingLP .~ initialLiquidityTokenCount
                  , editMinted = at (liquidityAC (getIdent initialIdent)) .~ Nothing
                  , editPoolOutputValue = const $ assetClassValue coin1 newAmtA <> assetClassValue coin2 newAmtB <> assetClassValue (poolAC poolIdent) 1 <> lovelaceValue rewards <> minAda
                  , editDisbursed = const $ [ (user1Addr, assetClassValue coin2 swapReceived1 <> minAda)
                                            , (user2Addr, assetClassValue coin2 swapReceived2 <> minAda)]
                  , poolCond = Fail
                  }
    mkScoopTest invalid

  changingCoins = testCase "Should fail when trying to change the datum's coin pair" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editNewPoolDatum = pool'coins .~ AB (liquidityAC (getIdent (intToIdent 0))) (liquidityAC (getIdent (intToIdent 1)))
      , poolCond = Fail
      }

  changingIdent = testCase "Should fail when trying to change the pool identifier" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editNewPoolDatum = pool'poolIdent .~ getIdent (intToIdent 1)
      , poolCond = Fail
      }

  changingSwapFees = testCase "Should fail when trying to change the swap fees" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editNewPoolDatum = pool'swapFees .~ SwapFees (Plutus.unsafeRatio 1 2)
      , poolCond = Fail
      }

  unboundedValidRangeUpwards = testCase "Should fail with an upward-unbounded valid range" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editValidRange = \i -> Interval (ivFrom i) (UpperBound PosInf True)
      , poolCond = Fail
      }

  unboundedValidRangeDownwards = testCase "Should fail with a downward-unbounded valid range" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editValidRange = \i -> Interval (LowerBound NegInf True) (ivTo i)
      , poolCond = Fail
      }

  unboundedValidRangeBothWays = testCase "Should fail with a totally unbounded valid range" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editValidRange = const $ Interval (LowerBound NegInf True) (UpperBound PosInf True)
      , poolCond = Fail
      }

  mintingPoolToken = testCase "attempting to mint a pool token during a scoop" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editMinted = at (poolAC (getIdent initialIdent)) .~ Just 1
      , poolCond = Fail
      }

  upgradeRedeemer = testCase "scooping with PoolUpgrade as the pool redeemer" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editPoolRedeemer = const PoolUpgrade
      , poolCond = Fail
      }

  cancelRedeemer = testCase "scooping with EscrowCancel as the escrow redeemer" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editEscrowRedeemer = const EscrowCancel
      , escrow1Cond = Fail
      , escrow2Cond = Fail
      }

  stolenPoolToken = testCase "pool token not in pool output" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editPoolOutputValue = at (poolAC (getIdent initialIdent)) .~ Nothing
      , poolCond = Fail
      , escrow1Cond = Fail
      , escrow2Cond = Fail
      }

  noPoolToken = testCase "pool token nowhere to be seen" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editPoolOutputValue = at (poolAC (getIdent initialIdent)) .~ Nothing
      , editOldPoolValue = at (poolAC (getIdent initialIdent)) .~ Nothing
      , escrow1Cond = Fail
      , escrow2Cond = Fail
      , poolCond = Fail
      , poolMintCond = Fail
      }

  escrowForDifferentPool = testCase "an escrow for a different pool" $ do
    testValidScoop
    let (depositAmt1, depositAmt2) = (200, 300)
    mkScoopTest validScoopParams
      { editEscrow1Datum = const $
          EscrowDatum
            (EscrowAddress user1Dest Nothing)
            testScoopFee
            (EscrowDeposit (getIdent $ intToIdent 1) (DepositMixed (AB depositAmt1 depositAmt2)))
      , poolCond = Fail
      }

  hugeSwap = testCase "an escrow for a swap way too huge for the pool" $ do
    testValidScoop
    let hugeSwapAmount = 1_000_000_000_000_000
    mkScoopTest validScoopParams
      { editEscrow1Value = at (poolCoins validScoopParams $$ CoinA) .~ Just hugeSwapAmount
      , editEscrow1Datum = escrow'action .~ EscrowSwap (coin1, hugeSwapAmount) (coin2, Nothing)
      , poolCond = Fail
      }

  extraPoolAssets = testCase "the pool has more of its assets" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editPoolOutputValue = at (poolCoins validScoopParams $$ CoinA) . _Just +~ 1
      , poolCond = Fail
      }

  escrowWithNegativeFee = testCase "an escrow with a negative fee" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editEscrow1Datum = escrow'scoopFee .~ (-testScoopFee)
      , poolCond = Fail
      }

  poolSetStaking = testCase "change staking key of pool to a non-empty credential" $ do
    mkScoopTest validScoopParams
      { editPoolAddress = \(Address previousPoolPaymentCred _) ->
          Address previousPoolPaymentCred (Just (StakingHash (PubKeyCredential "1234")))
      }

  poolUnsetStaking = testCase "unset staking key of pool" $ do
    mkScoopTest validScoopParams
      { editPoolAddress = \(Address previousPoolPaymentCred _) ->
          Address previousPoolPaymentCred Nothing
      }

  poolChangePayment = testCase "change payment key of pool" $ do
    mkScoopTest validScoopParams
      { editPoolAddress = \(Address _ previousPoolStakingCred) ->
          Address (ScriptCredential "0") previousPoolStakingCred
      , poolCond = Fail
      }

  swapTooEarly = testCase "swapping before marketOpenTime" $ do
    testValidScoop
    mkScoopTest validScoopParams
      { editValidRange = \i -> Interval (LowerBound (Finite (-1)) True) (ivTo i)
      , poolCond = Fail
      }

  rewardsNotPaidToPool = testCase "rewards not paid to pool" $ do
    testValidScoop
    let
      txFee = 1
      scooperFee = 2_500_000
      rewards = 2 * scooperFee - txFee
    mkScoopTest validScoopParams
      { editPoolOutputValue = (<> Plutus.inv (lovelaceValue rewards))
      , poolCond = Fail
      }

  poolRewardsFieldNotChanged = testCase "pool rewards field not updated correctly" $ do
    mkScoopTest validScoopParams
      { editNewPoolDatum = pool'rewards .~ 0
      , poolCond = Fail
      }

  noExtraLockedRewards = testCase "may not lock extra rewards" $ do
    let extra = 10_000
    mkScoopTest validScoopParams
      { editNewPoolDatum = pool'rewards +~ extra
      , editPoolOutputValue = (<> lovelaceValue extra)
      , poolCond = Fail
      }

  requireSufficientLockedRewards = testCase "must lock sufficient rewards" $ do
    let missing = 10_000
    mkScoopTest validScoopParams
      { editNewPoolDatum = pool'rewards -~ missing
      , editPoolOutputValue = (<> Plutus.inv (lovelaceValue missing))
      , poolCond = Fail
      }

  feeExceedsRewards = testCase "fee exceeds rewards" $ do
    mkScoopTest validScoopParams
      { editFee = const $ lovelaceValue 5_000_001
      }

  feeMatchesRewards = testCase "fee matches rewards" $ do
    mkScoopTest validScoopParams
      { editFee = const $ lovelaceValue 5_000_000
      }

  marketOrderTakeCoinMatchesPool = testCase "market order expecting a coin in the pool" $ do
    let
      swapAmt = 100
    mkScoopTest validScoopParams
      { editEscrow2Datum = const $ EscrowDatum (EscrowAddress user2Dest Nothing) testScoopFee (EscrowSwap (coin1, swapAmt) (coin2, Nothing))
      }

  marketOrderTakeCoinDoesn'tMatchPool = testCase "market order expecting a coin not in the pool" $ do
    let
      coin3 = toCoin "worm"
      swapAmt = 100
    mkScoopTest validScoopParams
      { editEscrow2Datum = const $ EscrowDatum (EscrowAddress user2Dest Nothing) testScoopFee (EscrowSwap (coin1, swapAmt) (coin3, Nothing))
      , poolCond = Fail
      }
