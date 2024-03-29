module Successor where

import Prelude

import Numeric.Natural
    ( Natural
    )

class Successor a where
    successor :: a -> Maybe a

instance Successor Char where
    successor = boundedEnumSuccessor

instance Successor Int where
    successor = boundedEnumSuccessor

instance Successor Integer where
    successor = unboundedNumSuccessor

instance Successor Natural where
    successor = unboundedNumSuccessor

boundedEnumSuccessor :: (Ord a, Bounded a, Enum a) => a -> Maybe a
boundedEnumSuccessor a
    | a >= maxBound = Nothing
    | otherwise = Just (succ a)

unboundedNumSuccessor :: Num a => a -> Maybe a
unboundedNumSuccessor = Just . (+ 1)
