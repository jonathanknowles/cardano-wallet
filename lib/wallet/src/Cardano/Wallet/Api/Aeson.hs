{-# LANGUAGE DerivingVia #-}

module Cardano.Wallet.Api.Aeson
  ( eitherToParser,
    toTextJSON,
    fromTextJSON,
  )
where

import Cardano.Wallet.Util
  ( ShowFmt (..),
  )
import Data.Aeson
  ( ToJSON (..),
    Value,
    withText,
  )
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor
  ( Bifunctor (..),
  )
import Data.Text.Class
  ( FromText (..),
    ToText (toText),
  )
import Prelude

eitherToParser :: Show s => Either s a -> Aeson.Parser a
eitherToParser = either (fail . show) pure

toTextJSON :: ToText a => a -> Value
toTextJSON = toJSON . toText

fromTextJSON :: FromText a => String -> Value -> Aeson.Parser a
fromTextJSON n = withText n (eitherToParser . first ShowFmt . fromText)
