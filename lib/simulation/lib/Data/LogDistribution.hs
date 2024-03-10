{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.LogDistribution where

import Data.Bag
    ( Bag
    , CountList
    , (:×:) ((:×:))
    )
import qualified Data.Bag as Bag
import Data.Semigroup
    ( Semigroup (stimes)
    )
import Deriving
    ( AsList (AsList)
    , Prefix (Prefix)
    )
import GHC.IsList
    ( IsList
    )
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( lookup
    , maximum
    )

newtype LogDistribution = LogDistribution (Bag LogInterval)
    deriving newtype Eq
    deriving (Semigroup, Monoid)
    deriving IsList via CountList (Bag LogInterval)
    deriving Show via Prefix "LogDistribution" (AsList LogDistribution)

newtype LogInterval = LogInterval Natural
    deriving newtype (Eq, Ord)

logInterval :: Natural -> LogInterval
logInterval n = LogInterval . fromIntegral $ length (show n) - 1

instance Show LogInterval where
    show (LogInterval n)
        | n == 0 = "0–9"
        | otherwise = lo <> "–" <> hi
      where
        lo :: String
        lo = "1" <> stimes n "0"

        hi :: String
        hi = stimes (n + 1) "9"

fromList :: [Natural] -> LogDistribution
fromList ns = LogDistribution $ Bag.fromUnaryList $ logInterval <$> ns

toList :: LogDistribution -> [(Natural :×: LogInterval)]
toList d =
    case maximum d of
        Nothing -> []
        Just (LogInterval n) ->
            (`lookup` d) . LogInterval <$> [0 .. n]

insert :: Natural -> LogDistribution -> LogDistribution
insert n (LogDistribution d) = LogDistribution $ Bag.insert (logInterval n) d

lookup :: LogInterval -> LogDistribution -> (Natural :×: LogInterval)
lookup interval (LogDistribution d) = Bag.count interval d :×: interval

maximum :: LogDistribution -> Maybe LogInterval
maximum (LogDistribution d) =
    case reverse (Bag.toCountList d) of
        [] -> Nothing
        (_ :×: i) : _ -> Just i
