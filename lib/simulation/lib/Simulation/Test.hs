{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}

module Simulation.Test where

import Data.Either
    ( Either
    )
import Simulation.Implementation
    ( txBalancer
    )
import Simulation.Model.Basic
    ( Asset (Asset, Lovelace)
    , BalanceTxError
    , PartialTx (PartialTx, outputs)
    , Tx
    , TxBalancer (TxBalancer, balanceTx)
    , Wallet
    )

testBalancedTx :: Either BalanceTxError Tx
testBalancedTx =
    balanceTx testWallet testPartialTx
  where
    TxBalancer {balanceTx} = txBalancer

testPartialTx :: PartialTx
testPartialTx = PartialTx
    { outputs =
      [ [ (Lovelace, 1_000_000) ]
      , [ (Lovelace, 3_000_000) ]
      ]
    }

testWallet :: Wallet
testWallet =
    [ [ (Lovelace, 1_000_000) ]
    , [ (Lovelace, 2_000_000) ]
    , [ (Lovelace, 3_000_000) ]
    , [ (Lovelace, 4_000_000) ]
    , [ (Lovelace, 1_000_000), (Asset "A", 1) ]
    , [ (Lovelace, 1_000_000), (Asset "B", 2) ]
    , [ (Lovelace, 1_000_000), (Asset "C", 3) ]
    , [ (Lovelace, 1_000_000), (Asset "D", 4) ]
    , [ (Lovelace, 8_000_000)
      , (Asset "A", 1_000_000_000)
      , (Asset "B", 1_000_000_000)
      , (Asset "C", 1_000_000_000)
      , (Asset "D", 1_000_000_000)
      ]
    ]
