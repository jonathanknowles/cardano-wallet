{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distribution.Log where

import Data.Bag
    ( Bag
    , CountList
    , (:×:) ((:×:))
    )
import qualified Data.Bag as Bag
import Data.List
    ( intercalate
    )
import Data.List.Split
    ( chunksOf
    )
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

--    0 <= x < 10^1
-- 10^1 <= x < 10^2
-- 10^2 <= x < 10^3
-- 10^3 <= x < 10^4
-- 10^4 <= x < 10^5
-- 10^5 <= x < 10^6
-- 10^6 <= x < 10^7
-- 10^7 <= x < 10^8
-- 10^8 <= x < 10^9
-- 10^9  <= x < 10^10
-- 10^10 <= x < 10^11
--
-- [10^0 .. 10^1)
--
--
-- 10^1 -- (10^2 - 1)
-- 10^2 -- (10^3 - 1)
-- 10^3 -- (10^4 - 1)

instance Show LogInterval where
    show (LogInterval n) = lo <> "–" <> hi
      where
        lo :: String
        lo = addNumericUnderscores $ "1" <> stimes n "0"

        hi :: String
        hi = addNumericUnderscores $ stimes (n + 1) "9"

        addNumericUnderscores :: String -> String
        addNumericUnderscores = reverse . intercalate "_" . chunksOf 3 . reverse

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

lookup :: LogInterval -> LogDistribution -> Natural :×: LogInterval
lookup interval (LogDistribution d) = Bag.count interval d

maximum :: LogDistribution -> Maybe LogInterval
maximum (LogDistribution d) =
    case reverse (Bag.toCountList d) of
        [] -> Nothing
        (_ :×: i) : _ -> Just i
