{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Bag where

import Prelude

import Data.Coerce
    ( coerce
    )
import Data.Monoid
    ( Sum (Sum)
    )
import Data.Monoid.GCD
    ( OverlappingGCDMonoid
    )
import Data.Monoid.Monus
    ( Monus
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
import GHC.IsList
    ( IsList (Item, fromList, toList)
    )
import Numeric.Natural
    ( Natural
    )

newtype Bag a = Bag (MonoidMap a (Sum Natural))
    deriving stock Eq
    deriving newtype (Semigroup, Commutative, Monoid, MonoidNull)
    deriving newtype (LeftReductive, RightReductive, Reductive)
    deriving newtype (LeftCancellative, RightCancellative, Cancellative)
    deriving newtype (OverlappingGCDMonoid, Monus)

data n :×: a = n :×: a
    deriving stock Eq

instance (Show n, Show a) => Show (n :×: a) where
    show (n :×: a) = show n <> " × " <> show a

(×) :: Natural -> a -> Natural :×: a
n × a = n :×: a

-- MultipleList
newtype MultiplicityList a = MultiplicityList a
newtype UnaryList a = UnaryList a

instance Ord a => IsList (MultiplicityList (Bag a)) where
    type Item (MultiplicityList (Bag a)) = Natural :×: a
    fromList = coerce fromMultiplicityList
    toList = coerce toMultiplicityList

instance Ord a => IsList (UnaryList (Bag a)) where
    type Item (UnaryList (Bag a)) = a
    fromList = coerce fromUnaryList
    toList = coerce toUnaryList

instance Show a => Show (MultiplicityList (Bag a)) where
    show (MultiplicityList m) =
        "fromMultiplicityList " <> show (toMultiplicityList m)

instance Show a => Show (UnaryList (Bag a)) where
    show (UnaryList m) =
        "fromUnaryList " <> show (toUnaryList m)

fromMultiplicityList :: Ord a => [Natural :×: a] -> Bag a
fromMultiplicityList = Bag . MonoidMap.fromList . fmap fromMultiple
  where
    fromMultiple (n :×: a) = (a, Sum n)

toMultiplicityList :: Bag a -> [Natural :×: a]
toMultiplicityList (Bag s) = fmap toMultiple . MonoidMap.toList $ s
  where
    toMultiple (a, Sum n) = (n :×: a)

fromUnaryList :: Ord a => [a] -> Bag a
fromUnaryList = Bag . MonoidMap.fromList . fmap (, Sum 1)

toUnaryList :: Bag a -> [a]
toUnaryList s = f =<< toMultiplicityList s
  where
    f :: (Natural :×: a) -> [a]
    f (n :×: a)
        | n == 0 = []
        | otherwise = a : f ((n - 1) :×: a)
