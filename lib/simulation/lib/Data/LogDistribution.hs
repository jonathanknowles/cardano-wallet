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

-- 0-9     or 1-9?
-- 10–99
-- 100–999
-- 1000–9999
-- 10000–99999
-- 100000–999999
-- 1000000–9999999
-- 10000000–99999999
-- 100000000–999999999
-- 1000000000–9999999999
-- 10000000000–99999999999
-- 100000000000–999999999999
-- 1000000000000–9999999999999

-- 10^0 <= x < 10^1
-- 10^1 <= x < 10^2
-- 10^2 <= x < 10^3
--
-- 10^1 -- (10^2 - 1)
-- 10^2 -- (10^3 - 1)
-- 10^3 -- (10^4 - 1)

instance Show LogInterval where
    show (LogInterval n) = lo <> "–" <> hi
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
