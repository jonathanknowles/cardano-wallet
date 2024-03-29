{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted alias" #-}
{-# LANGUAGE LambdaCase #-}

module Text.Bar
    ( LengthResolution (..)
    , LengthRoundingStrategy (..)
    , fromFractionOf2
    , fromFractionOf8
    , fromNatural
    , fromRational
    )
    where

import Prelude hiding
    ( fromRational
    )

import Data.Ratio
    ( Ratio
    )
import Data.Text
    ( Text
    )
import Fraction
    ( FractionOf2 (FractionOf2)
    , FractionOf8 (FractionOf8)
    , ProperFractionOf2 (..)
    , ProperFractionOf8 (..)
    , nearestFractionOf2
    , nearestFractionOf8
    )
import Numeric.Natural
    ( Natural
    )
import Rounding
    ( RoundingStrategy (RoundDown, RoundUp)
    )

import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Public
--------------------------------------------------------------------------------

data LengthResolution
    = LengthResolution1
    | LengthResolution2
    | LengthResolution8
    deriving (Eq, Show)

data LengthRoundingStrategy
    = LengthRoundUp
    | LengthRoundDown
    deriving (Eq, Show)

fromFractionOf2 :: FractionOf2 -> Text
fromFractionOf2 (FractionOf2 n f) =
    naturalToBar2 n <> properFractionOf2ToBar f

fromFractionOf8 :: FractionOf8 -> Text
fromFractionOf8 (FractionOf8 n f) =
    naturalToBar8 n <> properFractionOf8ToBar f

fromNatural :: Natural -> Text
fromNatural = naturalToBar1

-- Perhaps this shouldn't be in this module.
fromRational
    :: LengthResolution
    -> LengthRoundingStrategy
    -> Ratio Natural
    -> Text
fromRational resolution rounding ratio = case resolution of
    LengthResolution1 -> rationalToBar1 d ratio
    LengthResolution2 -> rationalToBar2 d ratio
    LengthResolution8 -> rationalToBar8 d ratio
  where
    d = mapRoundingStrategy rounding

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

mapRoundingStrategy :: LengthRoundingStrategy -> RoundingStrategy
mapRoundingStrategy = \case
    LengthRoundUp   -> RoundUp
    LengthRoundDown -> RoundDown

naturalToBar1 :: Natural -> Text
naturalToBar1 n = Text.replicate (fromIntegral n) "🬋"

naturalToBar2 :: Natural -> Text
naturalToBar2 n = Text.replicate (fromIntegral n) "🬋"

naturalToBar8 :: Natural -> Text
naturalToBar8 n = Text.replicate (fromIntegral n) "█"

rationalToBar1 :: RoundingStrategy -> Ratio Natural -> Text
rationalToBar1 _ r =
    naturalToBar1 n
  where
    (n, _) = properFraction r

rationalToBar2 :: RoundingStrategy -> Ratio Natural -> Text
rationalToBar2 = (fromFractionOf2 .) . nearestFractionOf2

rationalToBar8 :: RoundingStrategy -> Ratio Natural -> Text
rationalToBar8 = (fromFractionOf8 .) . nearestFractionOf8

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
