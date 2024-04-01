{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Avoid restricted alias" #-}

module Simulation.Test.Chart
    ( writeChartFiles
    ) where

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
    )
import Control.Monad.Random.Class
    ( MonadRandom
    )
import Data.Bag
    ( Count (Count)
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
import Numeric.Natural
    ( Natural
    )
import Optics.Core
    ( set
    , (%)
    , (&)
    )
import System.Random.Shuffle
    ( shuffleM
    )
import Text.Printf
    ( printf
    )

barDataToChartOptions :: BarData -> ChartOptions
barDataToChartOptions d =
  barChart defaultBarOptions d
      & set
          (#hudOptions % #frames)
          [Priority 101 (defaultFrameOptions & set #buffer 0.02)]

randomValues :: forall m. MonadRandom m => m [Natural]
randomValues = do
    xs <- shuffleM $ foldMap (replicate 256) ([1..16] :: [Natural])
    pure ((replicate 256 0) <> xs)

every :: Int -> [a] -> [a]
every n xs =
    case drop (n - 1) xs of
        y : ys -> y : every n ys
        [] -> []

distributionsFromValues :: [Natural] -> [Distribution Natural]
distributionsFromValues = drop 1 . scanl' (flip Distribution.insert) mempty

barDataFromDistribution :: Distribution Natural -> BarData
barDataFromDistribution d = BarData values rowLabels columnLabels
  where
    values :: [[Double]]
    values
        = (: [])
        . fmap (\(Count n _) -> fromIntegral @Natural @Double n)
        $ Distribution.toList d

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
    forM_ (zip [0 :: Natural ..] barDatas) $ \(index, d) ->
        writeChartOptions
            (fileNameFromIndex index)
            (barDataToChartOptions d)
