{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted alias" #-}

module Text.Colour where

import Prelude

import Data.Text (Text)
import Numeric.Natural (Natural)
import qualified Data.Text as Text

data Colour
    = Red
    | Green
    | Blue
    | Yellow
    deriving (Bounded, Enum)

newtype ColourCode = ColourCode Natural

colourToCode :: Colour -> ColourCode
colourToCode = ColourCode . \case
    Red     -> 31
    Green   -> 32
    Yellow  -> 33
    Blue    -> 34

colourCodeDefault :: ColourCode
colourCodeDefault = ColourCode 0

colourCodeToText :: ColourCode -> Text
colourCodeToText (ColourCode n) =
    "\ESC[" <> Text.pack (show n) <> "m"

withColour :: Colour -> Text -> Text
withColour colour text =
    prefix <> text <> suffix
  where
    prefix = colourCodeToText (colourToCode colour)
    suffix = colourCodeToText colourCodeDefault
