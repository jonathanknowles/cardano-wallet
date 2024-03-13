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
import System.Random.StdGenSeed
    ( StdGenSeed (..)
    )
import Test.QuickCheck
    ( frequency
    , listOf
    )
import Test.QuickCheck.Gen
    ( Gen
    )

genAction :: Gen Action
genAction =
    frequency
        [ (1, Deposit <$> genDepositValue)
        , (5, Payment <$> genPaymentValue)
        ]

genDepositValue :: Gen Value
genDepositValue = pure [ 50_000_000 × Lovelace ]

genPaymentValue :: Gen Value
genPaymentValue = pure [ 10_000_000 × Lovelace ]

genActions :: Gen [Action]
genActions = replicateM 1000 genAction

performAction :: Action -> Wallet -> Maybe Wallet
performAction action wallet =
    case action of
        Deposit v -> Just (wallet <> [v])
        Payment v -> do
            let partialTx = PartialTx {outputs = [v]}
            case balanceTx wallet partialTx of
                Left (BalanceTxError e) -> error e
                Right tx -> applyTxToWallet tx wallet
  where
    TxBalancer {balanceTx} = txBalancer (StdGenSeed 0)

performActions :: [Action] -> Wallet -> Maybe Wallet
performActions actions wallet = foldM (flip performAction) wallet actions

testBalancedTx :: Either BalanceTxError Tx
testBalancedTx =
    balanceTx testWalletAscendingUniform testPartialTx
  where
    TxBalancer {balanceTx} = txBalancer (StdGenSeed 0)

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
