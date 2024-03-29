{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Bag where

import Prelude hiding
    ( maximum
    , minimum
    )

import Data.Coerce
    ( coerce
    )
import Data.List
    ( intercalate
    )
import Data.List.Split
    ( chunksOf
    )
import Data.Monoid
    ( Sum (Sum)
    )
import Data.Monoid.GCD
    ( OverlappingGCDMonoid
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Data.Monoid.Null
    ( MonoidNull
    )
import Data.MonoidMap
    ( MonoidMap
    )
import qualified Data.MonoidMap as MonoidMap
import Data.Semigroup.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive
    , Reductive
    , RightCancellative
    , RightReductive
    )
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import GHC.IsList
    ( IsList (Item, fromList, toList)
    )
import Numeric.Natural
    ( Natural
    )
import Successor
    ( Successor (successor)
    )

newtype Bag a = Bag (MonoidMap a (Sum Natural))
    deriving stock Eq
    deriving newtype (Semigroup, Commutative, Monoid, MonoidNull)
    deriving newtype (LeftReductive, RightReductive, Reductive)
    deriving newtype (LeftCancellative, RightCancellative, Cancellative)
    deriving newtype (OverlappingGCDMonoid, Monus)

(×) :: Natural -> a -> Count a
(×) = Count

data Count a = Count !Natural !a
    deriving stock (Eq, Ord, Functor)

instance Show a => Show (Count a) where
    show (Count n a) = showNatural n <> " × " <> show a
      where
        showNatural :: Natural -> String
        showNatural =
            reverse . intercalate "_" . chunksOf 3 . reverse . show

newtype CountList a = CountList a
newtype UnaryList a = UnaryList a

instance Ord a => IsList (CountList (Bag a)) where
    type Item (CountList (Bag a)) = Count a
    fromList = coerce fromCountList
    toList = coerce toCountList

instance Ord a => IsList (UnaryList (Bag a)) where
    type Item (UnaryList (Bag a)) = a
    fromList = coerce fromUnaryList
    toList = coerce toUnaryList

instance Show a => Show (CountList (Bag a)) where
    show (CountList m) =
        "fromCountList " <> show (toCountList m)

instance Show a => Show (UnaryList (Bag a)) where
    show (UnaryList m) =
        "fromUnaryList " <> show (toUnaryList m)

empty :: Ord a => Bag a
empty = Bag mempty

map :: Ord b => (a -> b) -> Bag a -> Bag b
map f (Bag m) = Bag (MonoidMap.mapKeys f m)

delete :: Ord a => a -> Bag a -> Bag a
delete a (Bag m) = Bag $ MonoidMap.adjust (<\> Sum 1) a m

insert :: Ord a => a -> Bag a -> Bag a
insert a (Bag m) = Bag $ MonoidMap.adjust (<> Sum 1) a m

deleteCount :: Ord a => Count a -> Bag a -> Bag a
deleteCount (Count n a) (Bag m) = Bag $ MonoidMap.adjust (<\> Sum n) a m

insertCount :: Ord a => Count a -> Bag a -> Bag a
insertCount (Count n a) (Bag m) = Bag $ MonoidMap.adjust (<> Sum n) a m

count :: Ord a => a -> Bag a -> Count a
count a (Bag m) = Count (coerce $ MonoidMap.get a m) a

fromCount :: Ord a => Count a -> Bag a
fromCount (Count n a) = Bag (MonoidMap.singleton a (Sum n))

fromCountList :: Ord a => [Count a] -> Bag a
fromCountList = Bag . MonoidMap.fromList . fmap fromMultiple
  where
    fromMultiple (Count n a) = (a, Sum n)

toCountList :: Bag a -> [Count a]
toCountList (Bag s) = fmap toMultiple . MonoidMap.toList $ s
  where
    toMultiple (a, Sum n) = Count n a

toDenseCountListWithBounds
    :: (Ord a, Successor a)
    => a
    -> a
    -> Bag a
    -> [Count a]
toDenseCountListWithBounds lo hi b =
    (`count` b) <$> from lo
  where
    from !x
        | x > hi = []
        | otherwise = x : maybe [] from (successor x)

fromUnaryList :: Ord a => [a] -> Bag a
fromUnaryList = Bag . MonoidMap.fromList . fmap (, Sum 1)

toUnaryList :: Bag a -> [a]
toUnaryList s = f =<< toCountList s
  where
    f :: (Count a) -> [a]
    f (Count n a)
        | n == 0 = []
        | otherwise = a : f (Count (n - 1) a)

support :: Bag a -> Set a
support (Bag m) = MonoidMap.nonNullKeys m

bounds :: Bag a -> Maybe (a, a)
bounds d = (,) <$> minimum d <*> maximum d

minimum :: Bag a -> Maybe a
minimum = Set.lookupMin . support

maximum :: Bag a -> Maybe a
maximum = Set.lookupMax . support
