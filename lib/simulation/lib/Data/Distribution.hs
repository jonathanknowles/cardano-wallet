{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted alias" #-}

module Data.Distribution where

import Data.Bag
    ( Bag
    , (:×:)
    )
import qualified Data.Bag as Bag
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

data Fraction8
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

fraction8ToBar :: Fraction8 -> Text
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

naturalToBar :: Natural -> Text
naturalToBar n = Text.replicate (fromIntegral n) "█"

rationalToBar :: Ratio Natural -> Text
rationalToBar r =
    naturalToBar n <> fraction8ToBar f
  where
    (n, f) = properFraction8 r

properFraction8 :: Ratio Natural -> (Natural, Fraction8)
properFraction8 r =
    (naturalPart, fractionalPart)
  where
    naturalPart :: Natural
    naturalPart = floor r

    fractionalPart :: Fraction8
    fractionalPart
        = toEnum
        $ fromIntegral @Natural @Int
        $ ceiling
        $ 8 * ((n `mod` d) % d)
      where
        (n, d) = (numerator r, denominator r)
