{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distribution where

import Data.Bag
    ( Bag
    , (:×:)
    )
import qualified Data.Bag as Bag
import qualified Data.Set as Set
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

instance (Ord a, Successor a) => IsList (Distribution a) where
    type Item (Distribution a) = Natural :×: a
    fromList = fromList
    toList = toList

class Successor a where
    successor :: a -> a

instance Successor Integer where
    successor = succ

instance Successor Natural where
    successor = succ

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
        | otherwise = x : from (successor x)

empty :: Ord a => Distribution a
empty = Distribution mempty

bounds :: Distribution a -> Maybe (a, a)
bounds d = do
    lo <- minimum d
    hi <- maximum d
    pure (lo, hi)

minimum :: Distribution a -> Maybe a
minimum (Distribution d) = Set.lookupMin (Bag.support d)

maximum :: Distribution a -> Maybe a
maximum (Distribution d) = Set.lookupMax (Bag.support d)

count :: Ord a => a -> Distribution a -> Natural :×: a
count i (Distribution d) = Bag.count i d
