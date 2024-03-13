{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Avoid restricted alias" #-}

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
    ( (×)
    )
import GHC.IsList
    ( IsList (fromList)
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
    )
import Test.QuickCheck
    ( frequency
    )
import Test.QuickCheck.Gen
    ( Gen
    )

genAction :: Gen Action
genAction =
    frequency
        [ (8, Deposit <$> genDepositValue)
        , (7, Payment <$> genPaymentValue)
        ]

genDepositValue :: Gen Value
genDepositValue = pure [ 50_000_000 × Lovelace ]

genPaymentValue :: Gen Value
genPaymentValue = pure [ 50_000_000 × Lovelace ]

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
    , [ 1_000_000 × Lovelace, 1 × Asset "🥥" ]
    , [ 1_000_000 × Lovelace, 1 × Asset "🫐" ]
    ]

testWalletAscendingUniform :: Wallet
testWalletAscendingUniform = fromList $
    (\v -> [(v * 1_000_000) × Lovelace]) <$> ([1 .. 100])

testWalletBimodal :: Wallet
testWalletBimodal = fromList $
    replicate 1_000 [1_000_000 × Lovelace]
    <>
    replicate 1 [1_000_000_000 × Lovelace]
