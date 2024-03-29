{-# LANGUAGE LambdaCase #-}

module Rounding where

import Prelude

data RoundingStrategy
    = RoundUp
    | RoundDown

round :: (RealFrac r, Integral i) => RoundingStrategy -> r -> i
round = \case
    RoundUp -> ceiling
    RoundDown -> floor
