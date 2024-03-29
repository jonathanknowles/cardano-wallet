{-# LANGUAGE ScopedTypeVariables #-}

module Fraction
    ( FractionOf2 (FractionOf2)
    , FractionOf8 (FractionOf8)
    , ProperFractionOf2 (..)
    , ProperFractionOf8 (..)
    , nearestFractionOf2
    , nearestFractionOf8
    )
    where

import Data.Ratio
    ( Ratio
    , denominator
    , numerator
    , (%)
    )
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( round
    )
import Rounding
    ( RoundDirection
    , round
    )

data FractionOf2 = FractionOf2 Natural ProperFractionOf2
    deriving (Eq, Show)

data FractionOf8 = FractionOf8 Natural ProperFractionOf8
    deriving (Eq, Show)

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

nearestFractionOf2
    :: RoundDirection
    -> Ratio Natural
    -> FractionOf2
nearestFractionOf2 = (uncurry FractionOf2 .) . nearestFractionOf 2

nearestFractionOf8
    :: RoundDirection
    -> Ratio Natural
    -> FractionOf8
nearestFractionOf8 = (uncurry FractionOf8 .) . nearestFractionOf 8

nearestFractionOf
    :: forall fraction. Enum fraction
    => Natural
    -> RoundDirection
    -> Ratio Natural
    -> (Natural, fraction)
nearestFractionOf n roundDirection r =
    (naturalPart, fractionalPart)
  where
    errorUnexpectedFractionalPartCount =
        error "nearestFractionOf: unexpected fractional part count"

    naturalPart :: Natural
    naturalPart
        | fractionalPartCount == n = floor r + 1
        | fractionalPartCount <  n = floor r
        | otherwise                = errorUnexpectedFractionalPartCount

    fractionalPart :: fraction
    fractionalPart
        | fractionalPartCount == n = toEnum 0
        | fractionalPartCount <  n = toEnum (fromIntegral fractionalPartCount)
        | otherwise                = errorUnexpectedFractionalPartCount

    fractionalPartCount :: Natural
    fractionalPartCount
        = round roundDirection
        $ fromIntegral n * ((rn `mod` rd) % rd)
      where
        (rn, rd) = (numerator r, denominator r)
