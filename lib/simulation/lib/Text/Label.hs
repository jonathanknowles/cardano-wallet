{-# HLINT ignore "Avoid restricted alias" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.Label where

import Prelude

import Data.Foldable
    ( foldl'
    )
import Data.List
    ( transpose
    )
import Data.Text
    ( Text
    )
import qualified Data.Text as Text

data Alignment
    = AlignLeft
    | AlignRight
    deriving stock (Eq, Show)

newtype Label = Label [LabelPart]
    deriving stock (Eq, Show)

data LabelPart = LabelPart Alignment Text
    deriving stock (Eq, Show)

toParts :: Label -> [LabelPart]
toParts (Label parts) = parts

renderAsColumnWith :: (a -> Label) -> [a] -> [Text]
renderAsColumnWith toLabel as =
    mconcat <$> transpose renderedColumns
  where
    renderedColumns :: [[Text]]
    renderedColumns = renderColumn <$> columns

    columns :: [[LabelPart]]
    columns = transpose $ toParts . toLabel <$> as

    columnWidth :: [LabelPart] -> Int
    columnWidth = foldl' max 0 . fmap labelPartWidth

    labelPartWidth :: LabelPart -> Int
    labelPartWidth (LabelPart _ t) = Text.length t

    renderColumn :: [LabelPart] -> [Text]
    renderColumn parts =
        renderPart width <$> parts
      where
        width :: Int
        width = columnWidth parts

renderPart :: Int -> LabelPart -> Text
renderPart paddedWidth (LabelPart alignment text) =
    case alignment of
        AlignLeft ->
            text <> padding
        AlignRight ->
            padding <> text
  where
    labelPartWidth :: Int
    labelPartWidth = Text.length text

    padding :: Text
    padding = Text.replicate paddingWidth " "

    paddingWidth :: Int
    paddingWidth = max 0 (paddedWidth - labelPartWidth)
