{-# LANGUAGE DerivingStrategies #-}

module Interval where

import Prelude

import Numeric.Natural
    ( Natural
    )
import Successor
    ( Successor (successor)
    )

data Interval = Interval
    { inclusiveLowerBound :: Natural
    , exclusiveUpperBound :: Natural
    }
    deriving stock (Eq, Ord, Show)

instance Successor Interval where
    successor (Interval lo hi) = pure (Interval hi (hi + (hi - lo)))

newtype IntervalWidth = IntervalWidth Natural
    deriving stock (Eq, Show)

fromNatural :: IntervalWidth -> Natural -> Interval
fromNatural (IntervalWidth intervalWidth) n =
    Interval lo hi
  where
    lo = intervalWidth * (n `div` intervalWidth)
    hi = lo + intervalWidth
