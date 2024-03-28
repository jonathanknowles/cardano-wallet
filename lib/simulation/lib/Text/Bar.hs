{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted alias" #-}
{-# LANGUAGE LambdaCase #-}

module Text.Bar where

import Prelude

import Data.Ratio
    ( Ratio
    )
import Data.Text
    ( Text
    )
import Fraction
    ( ProperFractionOf2 (..)
    , ProperFractionOf8 (..)
    , nearestFractionOf2
    , nearestFractionOf8, FractionOf2 (FractionOf2), FractionOf8 (FractionOf8)
    )
import Numeric.Natural
    ( Natural
    )
import Rounding
    ( RoundDirection (RoundDown)
    )

import qualified Data.Text as Text

fractionOf2ToBar :: FractionOf2 -> Text
fractionOf2ToBar (FractionOf2 n f) =
    naturalToBar2 n <> properFractionOf2ToBar f

fractionOf8ToBar :: FractionOf8 -> Text
fractionOf8ToBar (FractionOf8 n f) =
    naturalToBar8 n <> properFractionOf8ToBar f

naturalToBar1 :: Natural -> Text
naturalToBar1 n = Text.replicate (fromIntegral n) "🬋"

naturalToBar2 :: Natural -> Text
naturalToBar2 n = Text.replicate (fromIntegral n) "🬋"

naturalToBar8 :: Natural -> Text
naturalToBar8 n = Text.replicate (fromIntegral n) "█"

rationalToBar1 :: Ratio Natural -> Text
rationalToBar1 r =
    naturalToBar1 n
  where
    (n, _) = properFraction r

rationalToBar2 :: Ratio Natural -> Text
rationalToBar2 r =
    naturalToBar2 n <> properFractionOf2ToBar f
  where
    FractionOf2 n f = nearestFractionOf2 RoundDown r

rationalToBar8 :: Ratio Natural -> Text
rationalToBar8 r =
    naturalToBar8 n <> properFractionOf8ToBar f
  where
    FractionOf8 n f = nearestFractionOf8 RoundDown r

properFractionOf2ToBar :: ProperFractionOf2 -> Text
properFractionOf2ToBar = \case
    Fraction_0_2 -> ""
    Fraction_1_2 -> "🬃"

properFractionOf8ToBar :: ProperFractionOf8 -> Text
properFractionOf8ToBar = \case
    Fraction_0_8 -> ""
    Fraction_1_8 -> "▏"
    Fraction_2_8 -> "▎"
    Fraction_3_8 -> "▍"
    Fraction_4_8 -> "▌"
    Fraction_5_8 -> "▋"
    Fraction_6_8 -> "▊"
    Fraction_7_8 -> "▉"
