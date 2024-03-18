{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted alias" #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Distribution where

import Data.Bag
    ( Bag
    , (:×:) ((:×:))
    )
import qualified Data.Bag as Bag
import Data.List.NonEmpty
    ( NonEmpty ((:|))
    )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio
    ( Ratio
    , denominator
    , numerator
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

newtype Distribution a = Distribution (Bag a)
    deriving stock Eq
    deriving Show via Prefix "Distribution" (AsList (Distribution a))
    deriving newtype (Semigroup, Monoid)

instance (Ord a, Successor a) => IsList (Distribution a) where
    type Item (Distribution a) = Natural :×: a
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

fromList :: Ord a => [Natural :×: a] -> Distribution a
fromList = Distribution . Bag.fromCountList

toList :: (Ord a, Successor a) => Distribution a -> [Natural :×: a]
toList d = maybe [] (\(lo, hi) -> toListWithBounds lo hi d) (bounds d)

toListWithBounds
    :: (Ord a, Successor a)
    => a
    -> a
    -> Distribution a
    -> [Natural :×: a]
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

count :: Ord a => a -> Distribution a -> Natural :×: a
count i (Distribution d) = Bag.count i d

topLeftCorner :: Text
topLeftCorner = "┌"

bottomLeftCorner :: Text
bottomLeftCorner = "└"

topLeftCornerRounded :: Text
topLeftCornerRounded = "╭"

bottomLeftCornerRounded :: Text
bottomLeftCornerRounded = "╰"

colourDefault :: Text
colourDefault = "\ESC[0m"

colourGreen :: Text
colourGreen = "\ESC[32m"

colourRed :: Text
colourRed = "\ESC[31m"

colourYellow :: Text
colourYellow = "\ESC[33m"

colourBlue :: Text
colourBlue = "\ESC[34m"

newtype BarScale = BarScale (Ratio Natural)
    deriving (Eq, Show, Num)

data Colour
    = Red
    | Green
    | Blue
    | Yellow
    deriving (Bounded, Enum)

withColour :: Colour -> Text -> Text
withColour colour text =
    prefix <> text <> suffix
  where
    prefix = case colour of
        Red -> colourRed
        Green -> colourGreen
        Blue -> colourBlue
        Yellow -> colourYellow
    suffix = colourDefault

data BarResolution
    = BarResolution1
    | BarResolution2
    | BarResolution8
    deriving (Eq, Show)

data BarConfig = BarConfig
    { colours :: NonEmpty Colour
    , resolution :: BarResolution
    , scale :: Ratio Natural
    }

defaultBarConfig :: BarConfig
defaultBarConfig = BarConfig
    { colours = Green :| [Red]
    , resolution = BarResolution2
    , scale = 1 % 2
    }

toBars
    :: forall a. (Ord a, Show a, Successor a)
    => BarConfig
    -> Distribution a
    -> [Text]
toBars BarConfig {colours, resolution, scale} d = mconcat
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
    counts = (\(n :×: _) -> n) <$> labelCountPairs

    labelCountPairs :: [Natural :×: a]
    labelCountPairs = toList d

    labels :: [Text]
    labels = (\(_ :×: a) -> Text.pack $ show a) <$> labelCountPairs

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
        <> withColour colour (rationalToBar ((n % 1) * scale))
        <> " "
        <> Text.pack (show n)

data FractionOf2
    = Fraction_0_2
    | Fraction_1_2
    | Fraction_2_2
    deriving (Bounded, Enum, Eq, Show)

data FractionOf8
    = Fraction_0_8
    | Fraction_1_8
    | Fraction_2_8
    | Fraction_3_8
    | Fraction_4_8
    | Fraction_5_8
    | Fraction_6_8
    | Fraction_7_8
    | Fraction_8_8
    deriving (Bounded, Enum, Eq, Show)

fraction2ToBar :: FractionOf2 -> Text
fraction2ToBar = \case
    Fraction_0_2 -> ""
    Fraction_1_2 -> "🬃"
    Fraction_2_2 -> "🬋"

fraction8ToBar :: FractionOf8 -> Text
fraction8ToBar = \case
    Fraction_0_8 -> ""
    Fraction_1_8 -> "▏"
    Fraction_2_8 -> "▎"
    Fraction_3_8 -> "▍"
    Fraction_4_8 -> "▌"
    Fraction_5_8 -> "▋"
    Fraction_6_8 -> "▊"
    Fraction_7_8 -> "▉"
    Fraction_8_8 -> "█"

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
    naturalToBar2 n <> fraction2ToBar f
  where
    (n, f) = properFractionOf2 r

rationalToBar8 :: Ratio Natural -> Text
rationalToBar8 r =
    naturalToBar8 n <> fraction8ToBar f
  where
    (n, f) = properFractionOf8 r

properFractionOf2 :: Ratio Natural -> (Natural, FractionOf2)
properFractionOf2 = properFractionOf 2

properFractionOf8 :: Ratio Natural -> (Natural, FractionOf8)
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
        $ ceiling
        $ fromIntegral n * ((rn `mod` rd) % rd)
      where
        (rn, rd) = (numerator r, denominator r)
