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
    , Show (IsList.Item a)
    ) =>
    Show (AsList a)
  where
    show (AsList a) = show (toList a)

newtype AsShown a = AsShown a

instance Show a => Eq (AsShown a) where
    (==) = (==) `on` (show . (\(AsShown a) -> a))

instance Show a => Ord (AsShown a) where
    (<=) = (<=) `on` (show . (\(AsShown a) -> a))

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
