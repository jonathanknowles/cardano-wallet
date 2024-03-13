{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Avoid restricted alias" #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Simulation.Test where

import Prelude

import Chart
    ( Priority (Priority)
    , frames
    , hudOptions
    , writeChartOptions
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
import Control.Monad
    ( forM_
    , replicateM
    )
import Control.Monad.Random.Class
    ( MonadRandom (getRandomR)
    )
import Data.Bag
    ( type (:×:) ((:×:))
    , (×)
    )
import Data.Distribution
    ( Distribution
    )
import qualified Data.Distribution as Distribution
import Data.List
    ( scanl'
    )
import Data.Text
    ( Text
    )
import qualified Data.Text as Text
import GHC.IsList
    ( IsList (fromList)
    )
import Numeric.Natural
    ( Natural
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
import System.Random.Shuffle
    ( shuffleM
    )
import System.Random.StdGenSeed
    ( StdGenSeed (..)
    )
import Text.Printf
    ( printf
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

barDataToChartOptions :: BarData -> ChartOptions
barDataToChartOptions barData =
  barChart defaultBarOptions barData
      & set
          (#hudOptions % #frames)
          [Priority 101 (defaultFrameOptions & set #buffer 0.02)]

randomValue :: MonadRandom m => m Natural
randomValue = do
    vs <- replicateM 4 v
    pure (sum vs)
  where
    v = fromIntegral @Integer @Natural <$>
        getRandomR (0, 4)

randomValues :: forall m. MonadRandom m => m [Natural]
randomValues = do
    xs <- shuffleM $ foldMap (replicate 256) ([1..16] :: [Natural])
    pure ((replicate 256 0) <> xs)

every :: Int -> [a] -> [a]
every n xs =
    case drop (n - 1) xs of
        y : ys -> y : every n ys
        [] -> []

randomDistribution :: MonadRandom m => m (Distribution Natural)
randomDistribution = Distribution.fromUnaryList <$> randomValues

distributionsFromValues :: [Natural] -> [Distribution Natural]
distributionsFromValues = drop 1 . scanl' (flip Distribution.insert) mempty

barDataFromDistribution :: Distribution Natural -> BarData
barDataFromDistribution d = BarData values rowLabels columnLabels
  where
    values :: [[Double]]
    values
        = (: [])
        . fmap (\(n :×: _) -> fromIntegral @Natural @Double n)
        $ Distribution.toListWithBounds 0 16 d

    rowLabels :: [Text]
    rowLabels = Text.pack . show @Natural <$> [0 .. 16]

    columnLabels :: [Text]
    columnLabels = [""]

fileNameFromIndex :: Natural -> FilePath
fileNameFromIndex n = "chart-" <> printf "%08d" n <> ".svg"

writeChartFiles :: IO ()
writeChartFiles = do
    vs <- randomValues
    let ds = every 16 $ distributionsFromValues vs
    let barDatas = barDataFromDistribution <$> ds
    forM_ (zip [0 :: Natural ..] barDatas) $ \(index, barData) ->
        writeChartOptions
            (fileNameFromIndex index)
            (barDataToChartOptions barData)
