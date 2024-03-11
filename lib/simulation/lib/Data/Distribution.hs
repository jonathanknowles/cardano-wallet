{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Data.Distribution where
import Data.Bag (Bag, CountList, (:×:) ((:×:)))
import Prelude
import Deriving (Prefix(Prefix), AsList(AsList))
import GHC.IsList (IsList)
import qualified Data.Bag as Bag
import qualified Data.Set as Set
import Numeric.Natural (Natural)

newtype Distribution a = Distribution (Bag (Interval a))

class HasInterval a where
    type Interval a
    toInterval :: a -> Interval a

class Least a where
    least :: a

class Successor a where
    successor :: a -> a

fromList :: HasInterval a => Ord (Interval a) => [a] -> Distribution a
fromList as = Distribution $ Bag.fromUnaryList $ toInterval <$> as

toDenseList
    :: Eq (Interval a)
    => Least (Interval a)
    => Successor (Interval a)
    => Distribution a
    -> [Natural :×: Interval a]
toDenseList d = undefined
  where
    intervals = undefined

empty :: Ord (Interval a) => Distribution a
empty = Distribution mempty

min :: Distribution a -> Maybe (Interval a)
min (Distribution d) = Set.lookupMin (Bag.support d)

max :: Distribution a -> Maybe (Interval a)
max (Distribution d) = Set.lookupMax (Bag.support d)

lookup
    :: Ord (Interval a)
    => Interval a
    -> Distribution a
    -> Natural :×: Interval a
lookup i (Distribution d) = Bag.count i d :×: i
