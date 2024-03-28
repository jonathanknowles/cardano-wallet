module Rounding where

import Prelude

data RoundDirection
    = RoundUp
    | RoundDown

round :: (RealFrac r, Integral i) => RoundDirection -> r -> i
round direction r =
    case direction of
        RoundUp -> ceiling r
        RoundDown -> floor r
