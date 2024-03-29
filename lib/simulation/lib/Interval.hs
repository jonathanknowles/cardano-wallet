{-# HLINT ignore "Avoid restricted alias" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interval where

import Prelude

import Data.List
    ( intercalate
    )
import Data.List.Split
    ( chunksOf
    )
import qualified Data.Text as Text
import Numeric.Natural
    ( Natural
    )
import Successor
    ( Successor (successor)
    )
import Text.Label
    ( Alignment (AlignRight)
    , Label (Label)
    , LabelPart (LabelPart)
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

toLabel :: Interval -> Label
toLabel (Interval lo hi) =
    Label . fmap (LabelPart AlignRight) $ parts
  where
    parts =
        [ "["
        , Text.pack (showNatural lo)
        , ", "
        , Text.pack (showNatural hi)
        , ")"
        ]
    showNatural :: Natural -> String
    showNatural =
        reverse . intercalate "," . chunksOf 3 . reverse . show
