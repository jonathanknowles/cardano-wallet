{-# LANGUAGE DerivingVia #-}

module Cardano.Wallet.Api.Hex
  ( hexText,
    fromHexText,
  )
where

import Cardano.Wallet.Primitive.AddressDerivation
  ( fromHex,
    hex,
  )
import Data.ByteString
  ( ByteString,
  )
import Data.Text
  ( Text,
  )
import qualified Data.Text.Encoding as T
import Prelude

hexText :: ByteString -> Text
hexText = T.decodeLatin1 . hex

fromHexText :: Text -> Either String ByteString
fromHexText = fromHex . T.encodeUtf8
