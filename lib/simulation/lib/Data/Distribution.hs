{-# HLINT ignore "Avoid restricted alias" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}

module Data.Distribution where

import Data.Bag
    ( Bag
    , Count (Count)
    )
import qualified Data.Bag as Bag
import Data.List.NonEmpty
    ( NonEmpty ((:|))
    )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio
    ( Ratio
    , (%)
    )
import qualified Data.Set as Set
import Data.Text
    ( Text
    )
import qualified Data.Text as Text
import Deriving
    ( AsList (AsList)
    , Prefix (Prefix)
    )
import GHC.IsList
    ( IsList (Item)
    )
import qualified GHC.IsList as IsList
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( lookup
    , maximum
    , minimum
    )
import qualified Prelude
    ( maximum
    )
import Successor
    ( Successor (successor)
    )
import qualified Text.Bar as Bar
import Text.Colour
    ( Colour (Green, Red)
    , withColour
    )
import qualified Text.Label as Label
import Text.Label
    ( Label
    )

newtype Distribution a = Distribution (Bag a)
    deriving stock Eq
    deriving Show via Prefix "Distribution" (AsList (Distribution a))
    deriving newtype (Semigroup, Monoid)

instance (Ord a, Successor a) => IsList (Distribution a) where
    type Item (Distribution a) = Count a
    fromList = fromList
    toList = toList

fromUnaryList :: Ord a => [a] -> Distribution a
fromUnaryList as = Distribution $ Bag.fromUnaryList as

fromList :: Ord a => [Count a] -> Distribution a
fromList = Distribution . Bag.fromCountList

toList :: (Ord a, Successor a) => Distribution a -> [Count a]
toList d = maybe [] (\(lo, hi) -> toListWithBounds lo hi d) (bounds d)

toListWithBounds
    :: (Ord a, Successor a)
    => a
    -> a
    -> Distribution a
    -> [Count a]
toListWithBounds lo hi d =
    (`count` d) <$> from lo
  where
    from !x
        | x > hi = []
        | otherwise = x : maybe [] from (successor x)

empty :: Ord a => Distribution a
empty = Distribution mempty

bounds :: Distribution a -> Maybe (a, a)
bounds d = do
    lo <- minimum d
    hi <- maximum d
    pure (lo, hi)

insert :: Ord a => a -> Distribution a -> Distribution a
insert a (Distribution d) = Distribution (Bag.insert a d)

minimum :: Distribution a -> Maybe a
minimum (Distribution d) = Set.lookupMin (Bag.support d)

maximum :: Distribution a -> Maybe a
maximum (Distribution d) = Set.lookupMax (Bag.support d)

count :: Ord a => a -> Distribution a -> Count a
count i (Distribution d) = Bag.count i d

topLeftCorner :: Text
topLeftCorner = "┌"

bottomLeftCorner :: Text
bottomLeftCorner = "└"

topLeftCornerRounded :: Text
topLeftCornerRounded = "╭"

bottomLeftCornerRounded :: Text
bottomLeftCornerRounded = "╰"

data BarLengthConfig
    = BarLengthScale (Ratio Natural)
    | BarLengthLimit Natural
    deriving (Eq, Show)

data BarChartOptions = BarChartOptions
    { colours :: NonEmpty Colour
    , resolution :: BarChartResolution
    , scale :: BarLengthConfig
    }

data BarChartResolution
    = BarChartResolution1
    | BarChartResolution2
    | BarChartResolution8
    deriving (Eq, Show)

defaultBarChartOptions :: BarChartOptions
defaultBarChartOptions = BarChartOptions
    { colours = Green :| [Red]
    , resolution = BarChartResolution2
    , scale = BarLengthScale (1 % 2)
    }

convertResolution :: BarChartResolution -> Bar.LengthResolution
convertResolution = \case
    BarChartResolution1 -> Bar.LengthResolution1
    BarChartResolution2 -> Bar.LengthResolution2
    BarChartResolution8 -> Bar.LengthResolution8

toBars
    :: forall a. (Ord a, Successor a)
    => BarChartOptions
    -> (a -> Label)
    -> Distribution a
    -> [Text]
toBars BarChartOptions {colours, resolution, scale} toLabel d = mconcat
    [ [ Text.replicate (labelColumnWidth + 1) " " <> topLeftCorner ]
    , [ toBar colour label n
      | (colour, label, n) <- zip3 colourSequence labelsPadded counts
      ]
    , [ Text.replicate (labelColumnWidth + 1) " " <> bottomLeftCorner ]
    ]
  where
    colourSequence :: [Colour]
    colourSequence = NonEmpty.toList $ NonEmpty.cycle colours

    counts :: [Natural]
    counts = (\(Count n _) -> n) <$> labelCountPairs

    labelCountPairs :: [Count a]
    labelCountPairs = toList d

    labels :: [Text]
    labels
        = Label.renderManyAsColumn
        $ fmap (toLabel . (\(Count _ a) -> a)) labelCountPairs

    labelsPadded :: [Text]
    labelsPadded = pad <$> labels
      where
        pad :: Text -> Text
        pad label =
            Text.replicate (labelColumnWidth - Text.length label) " " <> label

    labelColumnWidth :: Int
    labelColumnWidth = Prelude.maximum (0 : (Text.length <$> labels))

    toBar :: Colour -> Text -> Natural -> Text
    toBar colour label n =
        label
        <> " ┤"
        <> withColour colour
            (Bar.fromRational barResolution Bar.LengthRoundDown barWidth)
        <> " "
        <> Text.pack (show n)
      where
        barResolution :: Bar.LengthResolution
        barResolution = convertResolution resolution

        barWidth :: Ratio Natural
        barWidth = case scale of
            BarLengthLimit a -> (a % 1)
            BarLengthScale r -> (n % 1) * r
