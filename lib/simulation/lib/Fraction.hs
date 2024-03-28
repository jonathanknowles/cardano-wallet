{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fraction where

import Prelude
import Data.Ratio (Ratio, denominator, numerator, (%))
import Numeric.Natural (Natural)

data ProperFractionOf2
    = Fraction_0_2
    | Fraction_1_2
    deriving (Bounded, Enum, Eq, Show)

data ProperFractionOf8
    = Fraction_0_8
    | Fraction_1_8
    | Fraction_2_8
    | Fraction_3_8
    | Fraction_4_8
    | Fraction_5_8
    | Fraction_6_8
    | Fraction_7_8
    deriving (Bounded, Enum, Eq, Show)

properFractionOf2 :: Ratio Natural -> (Natural, ProperFractionOf2)
properFractionOf2 = properFractionOf 2

properFractionOf8 :: Ratio Natural -> (Natural, ProperFractionOf8)
properFractionOf8 = properFractionOf 8

properFractionOf
    :: forall fraction. Enum fraction
    => Natural
    -> Ratio Natural
    -> (Natural, fraction)
properFractionOf n r =
    (naturalPart, fractionalPart)
  where
    naturalPart :: Natural
    naturalPart = floor r

    fractionalPart :: fraction
    fractionalPart
        = toEnum
        $ fromIntegral @Natural @Int
        $ floor
        $ fromIntegral n * ((rn `mod` rd) % rd)
      where
        (rn, rd) = (numerator r, denominator r)
