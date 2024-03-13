{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Deriving where

import Prelude

import Data.Function
    ( on
    )
import Data.Proxy
    ( Proxy (Proxy)
    )
import GHC.IsList
    ( IsList (toList)
    )
import qualified GHC.IsList as IsList
    ( Item
    )
import GHC.TypeLits
    ( KnownSymbol
    , Symbol
    , symbolVal
    )

newtype AsList a = AsList a

instance
    ( IsList a
    , Eq (IsList.Item a)
    ) =>
    Eq (AsList a)
  where
    AsList a1 == AsList a2 = toList a1 == toList a2

instance
    ( IsList a
    , Ord (IsList.Item a)
    ) =>
    Ord (AsList a)
  where
    AsList a1 <= AsList a2 = toList a1 <= toList a2

instance
    ( IsList a
    , Show (IsList.Item a)
    ) =>
    Show (AsList a)
  where
    show (AsList a) = show (toList a)

newtype AsShow a = AsShow a

instance Show a => Eq (AsShow a) where
    (==) = (==) `on` (show . (\(AsShow a) -> a))

instance Show a => Ord (AsShow a) where
    (<=) = (<=) `on` (show . (\(AsShow a) -> a))

newtype Prefix (prefix :: Symbol) a = Prefix a

instance
    ( KnownSymbol prefix
    , Show a
    ) =>
    Show (Prefix prefix a)
  where
    show (Prefix a) = unwords
        [ symbolVal (Proxy :: Proxy prefix)
        , show a
        ]
