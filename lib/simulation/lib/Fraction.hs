{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fraction where

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

properFractionOf2
    :: RoundDirection
    -> Ratio Natural
    -> (Natural, ProperFractionOf2)
properFractionOf2 = properFractionOf 2

properFractionOf8
    :: RoundDirection
    -> Ratio Natural
    -> (Natural, ProperFractionOf8)
properFractionOf8 = properFractionOf 8

properFractionOf
    :: forall fraction. Enum fraction
    => Natural
    -> RoundDirection
    -> Ratio Natural
    -> (Natural, fraction)
properFractionOf n roundDirection r =
    (naturalPart, fractionalPart)
  where
    errorUnexpectedFractionalPartCount =
        error "properFractionOf: unexpected fractional part count"

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
