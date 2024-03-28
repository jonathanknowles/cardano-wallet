{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted alias" #-}

module Data.Distribution where

import Data.Bag
    ( Bag
    , Count (Count)
    )
import qualified Data.Bag as Bag
import Data.List
    ( foldl'
    , intercalate
    , transpose
    )
import Data.List.NonEmpty
    ( NonEmpty ((:|))
    )
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.Split
    ( chunksOf
    )
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
import Fraction
    ( ProperFractionOf2 (..)
    , ProperFractionOf8 (..)
    , properFractionOf2
    , properFractionOf8
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
import Rounding
    ( RoundDirection (RoundDown)
    )
import Test.QuickCheck.Extra
    ( GenCount (GenCount)
    , GenSize (GenSize)
    , arbitrarySampleList
    )
import Text.Colour
    ( Colour (Green, Red)
    , withColour
    )

newtype Distribution a = Distribution (Bag a)
    deriving stock Eq
    deriving Show via Prefix "Distribution" (AsList (Distribution a))
    deriving newtype (Semigroup, Monoid)

instance (Ord a, Successor a) => IsList (Distribution a) where
    type Item (Distribution a) = Count a
    fromList = fromList
    toList = toList

class Successor a where
    successor :: a -> Maybe a

instance Successor Integer where
    successor = Just . succ

instance Successor Natural where
    successor = Just . succ

instance Successor Char where
    successor a
        | a >= maxBound = Nothing
        | otherwise = Just (succ a)

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

data BarResolution
    = BarResolution1
    | BarResolution2
    | BarResolution8
    deriving (Eq, Show)

data BarScale
    = BarScaleRelative (Ratio Natural)
    | BarScaleAbsolute Natural
    deriving (Eq, Show)

data BarConfig = BarConfig
    { colours :: NonEmpty Colour
    , resolution :: BarResolution
    , scale :: BarScale
    }

defaultBarConfig :: BarConfig
defaultBarConfig = BarConfig
    { colours = Green :| [Red]
    , resolution = BarResolution2
    , scale = BarScaleRelative (1 % 2)
    }

data Alignment
    = AlignLeft
    | AlignRight
    deriving stock (Eq, Show)

newtype Label = Label [LabelPart]
    deriving stock (Eq, Show)

labelParts :: Label -> [LabelPart]
labelParts (Label parts) = parts

data LabelPart = LabelPart Alignment Text
    deriving stock (Eq, Show)

data Interval = Interval
    { inclusiveLowerBound :: Natural
    , exclusiveUpperBound :: Natural
    }
    deriving stock (Eq, Ord, Show)

instance Successor Interval where
    successor (Interval lo hi) = pure (Interval hi (hi + (hi - lo)))

newtype IntervalWidth = IntervalWidth Natural
    deriving stock (Eq, Show)

naturalToInterval :: IntervalWidth -> Natural -> Interval
naturalToInterval (IntervalWidth intervalWidth) n =
    Interval lo hi
  where
    lo = intervalWidth * (n `div` intervalWidth)
    hi = lo + intervalWidth

intervalToLabel :: Interval -> Label
intervalToLabel (Interval lo hi) =
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

renderLabelPart :: Int -> LabelPart -> Text
renderLabelPart paddedWidth (LabelPart alignment text) =
    case alignment of
        AlignLeft ->
            text <> padding
        AlignRight ->
            padding <> text
  where
    labelPartWidth :: Int
    labelPartWidth = Text.length text

    padding :: Text
    padding = Text.replicate paddingWidth " "

    paddingWidth :: Int
    paddingWidth = max 0 (paddedWidth - labelPartWidth)

renderLabels :: (a -> Label) -> [a] -> [Text]
renderLabels toLabel as =
    mconcat <$> transpose renderedColumns
  where
    renderedColumns :: [[Text]]
    renderedColumns = renderColumn <$> columns

    columns :: [[LabelPart]]
    columns = transpose $ labelParts . toLabel <$> as

    columnWidth :: [LabelPart] -> Int
    columnWidth = foldl' max 0 . fmap labelPartWidth

    labelPartWidth :: LabelPart -> Int
    labelPartWidth (LabelPart _ t) = Text.length t

    renderColumn :: [LabelPart] -> [Text]
    renderColumn parts =
        renderLabelPart width <$> parts
      where
        width :: Int
        width = columnWidth parts

toBars
    :: forall a. (Ord a, Successor a)
    => BarConfig
    -> (a -> Label)
    -> Distribution a
    -> [Text]
toBars BarConfig {colours, resolution, scale} toLabel d = mconcat
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
    labels = renderLabels toLabel $ (\(Count _ a) -> a) <$> labelCountPairs

    labelsPadded :: [Text]
    labelsPadded = pad <$> labels
      where
        pad :: Text -> Text
        pad label =
            Text.replicate (labelColumnWidth - Text.length label) " " <> label

    labelColumnWidth :: Int
    labelColumnWidth = Prelude.maximum (0 : (Text.length <$> labels))

    rationalToBar :: Ratio Natural -> Text
    rationalToBar =
        case resolution of
            BarResolution1 -> rationalToBar1
            BarResolution2 -> rationalToBar2
            BarResolution8 -> rationalToBar8

    toBar :: Colour -> Text -> Natural -> Text
    toBar colour label n =
        label
        <> " ┤"
        <> withColour colour (rationalToBar barWidth)
        <> " "
        <> Text.pack (show n)
      where
        barWidth :: Ratio Natural
        barWidth = case scale of
            BarScaleAbsolute a -> (a % 1)
            BarScaleRelative r -> (n % 1) * r

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
    (n, f) = properFractionOf2 RoundDown r

rationalToBar8 :: Ratio Natural -> Text
rationalToBar8 r =
    naturalToBar8 n <> properFractionOf8ToBar f
  where
    (n, f) = properFractionOf8 RoundDown r

example :: Text
example =
    Text.unlines $
    toBars
        defaultBarConfig
            { scale = BarScaleRelative (1%840)
            }
        intervalToLabel
        distribution
  where
    distribution :: Distribution Interval
    distribution = fromUnaryList intervals

    intervals :: [Interval]
    intervals = naturalToInterval (IntervalWidth 20_000) <$> values

    values :: [Natural]
    values =
        fromIntegral @Int @Natural . abs . (+ 500_000)
            <$> arbitrarySampleList (GenCount 100_0000) (GenSize 500_000)
