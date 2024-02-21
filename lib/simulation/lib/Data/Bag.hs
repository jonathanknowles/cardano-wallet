{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

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

newtype MultiplicityList a = MultiplicityList a
newtype UnaryList a = UnaryList a

instance Ord a => IsList (MultiplicityList (Bag a)) where
    type Item (MultiplicityList (Bag a)) = (a, Natural)
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

fromMultiplicityList :: Ord a => [(a, Natural)] -> Bag a
fromMultiplicityList = Bag . MonoidMap.fromList . coerce

toMultiplicityList :: Bag a -> [(a, Natural)]
toMultiplicityList (Bag s) = coerce . MonoidMap.toList $ s

fromUnaryList :: Ord a => [a] -> Bag a
fromUnaryList = Bag . MonoidMap.fromList . fmap (, Sum 1)

toUnaryList :: Bag a -> [a]
toUnaryList s = f =<< toMultiplicityList s
  where
    f :: (a, Natural) -> [a]
    f (a, n)
        | n == 0 = []
        | otherwise = a : f (a, n - 1)
