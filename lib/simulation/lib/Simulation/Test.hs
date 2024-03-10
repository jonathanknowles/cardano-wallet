{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Avoid restricted alias" #-}

module Simulation.Test where

import Prelude

import Chart
    ( Priority (Priority)
    , frames
    , hudOptions
    )
import Chart.Bar
    ( BarData (..)
    , barChart
    , defaultBarOptions
    )
import Chart.Hud
    ( FrameOptions (buffer)
    , defaultFrameOptions
    )
import Chart.Markup
    ( ChartOptions
    )
import Data.Bag
    ( (×)
    )
import qualified Data.Text as Text
import GHC.IsList
    ( IsList (fromList)
    )
import Optics.Core
    ( set
    , (%)
    , (&)
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
import System.Random.StdGenSeed
    ( StdGenSeed (..)
    )

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
    (\v -> [(v * 1_000_000) × Lovelace]) <$> [1 .. 1000]

barDataExample :: BarData
barDataExample =
    BarData
        [[1, 2, 4, 6, 10, 16, 26, 42, 68, 88, 102]]
        (("row " <>) . Text.pack . show <$> [1 .. 11 :: Int])
        (("column " <>) . Text.pack . show <$> [1 :: Int])

barExample :: ChartOptions
barExample =
  barChart defaultBarOptions barDataExample
      & set
          (#hudOptions % #frames)
          [Priority 101 (defaultFrameOptions & set #buffer 0.02)]
