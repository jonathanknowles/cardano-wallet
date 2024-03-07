{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Simulation.Test where

import Data.Bag
    ( (×) )
import Data.Either
    ( Either
    )
import Simulation.Implementation
    ( txBalancer
    )
import Simulation.Model.Basic
    ( Asset (Lovelace, Asset)
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
    [ [ 1_000_000 × Lovelace ]
    , [ 3_000_000 × Lovelace ]
      ]
    }

testWallet :: Wallet
testWallet =
    [ [ 1_000_000 × Lovelace ]
    , [ 2_000_000 × Lovelace ]
    , [ 3_000_000 × Lovelace ]
    , [ 4_000_000 × Lovelace ]
    , [ 1_000_000 × Lovelace, 1 × Asset "🍎" ]
    , [ 1_000_000 × Lovelace, 2 × Asset "你好" ]
    , [ 1_000_000 × Lovelace, 3 × Asset "C" ]
    , [ 1_000_000 × Lovelace, 4 × Asset "D" ]
    , [ 8_000_000 × Lovelace, 1 × Asset "X", 1 × Asset "Y", 1 × Asset "Z" ]
    ]
