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
toList d = (`lookup` d) <$> range d

empty :: Ord a => Distribution a
empty = Distribution mempty

limits :: Distribution a -> Maybe (a, a)
limits d = do
    lo <- minimum d
    hi <- maximum d
    pure (lo, hi)

minimum :: Distribution a -> Maybe a
minimum (Distribution d) = Set.lookupMin (Bag.support d)

maximum :: Distribution a -> Maybe a
maximum (Distribution d) = Set.lookupMax (Bag.support d)

range :: forall a. (Eq a, Successor a) => Distribution a -> [a]
range d = maybe [] enumerateRange (limits d)
  where
    enumerateRange :: (a, a) -> [a]
    enumerateRange (lowerBound, upperBound) = loop lowerBound
      where
        loop !x
            | x == upperBound = [x]
            | otherwise = x : loop (successor x)

lookup :: Ord a => a -> Distribution a -> Natural :×: a
lookup i (Distribution d) = Bag.count i d
