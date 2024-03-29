{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Avoid restricted alias" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Simulation.Test where

import Prelude

import Control.Monad
    ( foldM
    , replicateM
    )
import Control.Monad.Random.Class
    ( MonadRandom
    )
import Data.Bag
    ( Count (Count)
    , (×)
    )
import Data.Distribution
    ( BarConfig (..)
    , BarLengthConfig (BarLengthScalingFactor)
    , Distribution
    , defaultBarConfig
    , fromUnaryList
    , toBars
    )
import Data.Ratio
    ( (%)
    )
import Data.Text
    ( Text
    )
import qualified Data.Text as Text
import GHC.IsList
    ( IsList (fromList)
    )
import Interval
    ( Interval
    , IntervalWidth (IntervalWidth)
    )
import qualified Interval
import Numeric.Natural
    ( Natural
    )
import Simulation.Implementation
    ( txBalancer
    )
import Simulation.Model.Basic
    ( Action (Deposit, Payment)
    , Asset (Asset, Lovelace)
    , BalanceTxError (BalanceTxError)
    , PartialTx (PartialTx, outputs)
    , Tx
    , TxBalancer (TxBalancer, balanceTx)
    , Value
    , Wallet
    , applyTxToWallet
    , valueOfAsset
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , frequency
    , generate
    , scale
    )
import Test.QuickCheck.Extra
    ( GenCount (GenCount)
    , GenSize (GenSize)
    , arbitrarySampleList
    )

genAction :: Gen Action
genAction =
    frequency
        [ (0, Deposit <$> genDepositValue)
        , (8, Payment <$> genPaymentValue)
        ]

genDepositValue :: Gen Value
genDepositValue =
    naturalToLovelace <$> genNaturalDistribution 100_000_000 1_000_000

genPaymentValue :: Gen Value
genPaymentValue =
    naturalToLovelace <$> genNaturalDistribution 10_000_000 1_000_000

genNaturalDistribution :: Natural -> Natural -> Gen Natural
genNaturalDistribution mean variance =
    positiveIntegerToNatural . (+ fromIntegral mean) <$> genOffset
  where
    genOffset :: Gen Integer
    genOffset = scale (const (fromIntegral variance)) arbitrary

    positiveIntegerToNatural :: Integer -> Natural
    positiveIntegerToNatural i
        | i < 0 = 0
        | otherwise = fromIntegral i

naturalToLovelace :: Natural -> Value
naturalToLovelace v = [ v × Lovelace ]

actionToLovelaceDelta :: Action -> Integer
actionToLovelaceDelta = \case
    Deposit v -> extract v
    Payment v -> negate $ extract v
  where
    extract :: Value -> Integer
    extract v = fromIntegral (case valueOfAsset Lovelace v of Count n _ -> n)

genActions :: Gen [Action]
genActions = replicateM 2000 genAction

performAction :: MonadRandom m => Action -> Wallet -> m (Maybe Wallet)
performAction action wallet =
    case action of
        Deposit v -> pure $ Just (wallet <> [v])
        Payment v -> do
            let partialTx = PartialTx {outputs = [v]}
            maybeBalancedTx <- balanceTx wallet partialTx
            case maybeBalancedTx of
                Left (BalanceTxError e) -> error e
                Right tx -> pure (applyTxToWallet tx wallet)
  where
    TxBalancer {balanceTx} = txBalancer

performActions
    :: forall m. MonadRandom m
    => [Action]
    -> Wallet
    -> m (Maybe Wallet)
performActions actions wallet = foldM f (Just wallet) actions
  where
    f :: Maybe Wallet -> Action -> m (Maybe Wallet)
    f maybeWallet action =
        case maybeWallet of
            Nothing -> pure Nothing
            Just w -> performAction action w

testBalancedTx :: MonadRandom m => m (Either BalanceTxError Tx)
testBalancedTx =
    balanceTx testWalletAscendingUniform testPartialTx
  where
    TxBalancer {balanceTx} = txBalancer

testPartialTx :: PartialTx
testPartialTx = PartialTx
    { outputs =
      [ [ 1000_000_000 × Lovelace ]
      ]
    }

testWalletFruit :: Wallet
testWalletFruit =
    [ [ 1_000_000 × Lovelace, 1 × Asset "🍎" ]
    , [ 1_000_000 × Lovelace, 1 × Asset "🍌" ]
    , [ 1_000_000 × Lovelace, 1 × Asset "🍌" ]
    , [ 1_000_000 × Lovelace, 1 × Asset "🍌" ]
    , [ 1_000_000 × Lovelace, 1 × Asset "🍌" ]
    , [ 1_000_000 × Lovelace, 1 × Asset "🍌" ]
    , [ 1_000_000 × Lovelace, 1 × Asset "🍌" ]
    , [ 1_000_000 × Lovelace, 1 × Asset "🥥" ]
    , [ 1_000_000 × Lovelace, 3 × Asset "🫐" ]
    ]

testWalletAscendingUniform :: Wallet
testWalletAscendingUniform = fromList $
    (\v -> [(v * 1_000_000) × Lovelace]) <$> ([1 .. 100])

testWalletBimodal :: Wallet
testWalletBimodal = fromList $
    replicate 1_000 [1_000_000 × Lovelace]
    <>
    replicate 1 [1_000_000_000 × Lovelace]

--------------------------------------------------------------------------------

testInitialWallet :: Wallet
testInitialWallet =
    fromList $
    replicate 1000 [1000_000_000 × Lovelace]

genFinalWallet :: IO Wallet
genFinalWallet = do
    actions <- generate genActions
    maybeResultWallet <- performActions actions testInitialWallet
    case maybeResultWallet of
        Nothing -> error "unable to generate wallet result"
        Just w -> pure w

--------------------------------------------------------------------------------

exampleDistribution :: Text
exampleDistribution =
    Text.unlines $
    toBars
        defaultBarConfig
            { scale = BarLengthScalingFactor (1%840)
            }
        Interval.toLabel
        distribution
  where
    distribution :: Distribution Interval
    distribution = fromUnaryList intervals

    intervals :: [Interval]
    intervals = Interval.fromNatural (IntervalWidth 20_000) <$> values

    values :: [Natural]
    values =
        fromIntegral @Int @Natural . abs . (+ 500_000)
            <$> arbitrarySampleList (GenCount 100_0000) (GenSize 500_000)
