{-# HLINT ignore "Avoid restricted alias" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Text.Label
    ( Alignment (..)
    , Label (..)
    , LabelPart (..)
    , toParts
    , renderManyAsColumn
    )
    where

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
import Numeric.Natural
    ( Natural
    )

data Alignment
    = AlignLeft
    | AlignRight
    deriving stock (Eq, Show)

newtype Label = Label [LabelPart]
    deriving stock (Eq, Show)

data LabelPart = LabelPart Alignment Text
    deriving stock (Eq, Show)

newtype LabelPartLength = LabelPartLength Natural
    deriving stock (Eq, Ord, Show)
    deriving newtype Num

toParts :: Label -> [LabelPart]
toParts (Label parts) = parts

renderManyAsColumn :: [Label] -> [Text]
renderManyAsColumn labels =
    mconcat <$> transpose renderedColumns
  where
    renderedColumns :: [[Text]]
    renderedColumns = renderColumn <$> columns

    columns :: [[LabelPart]]
    columns = transpose $ toParts <$> labels

    columnWidth :: [LabelPart] -> LabelPartLength
    columnWidth = foldl' max 0 . fmap labelPartWidth

    labelPartWidth :: LabelPart -> LabelPartLength
    labelPartWidth (LabelPart _ t)
        = LabelPartLength
        $ fromIntegral @Int @Natural
        $ Text.length t

    renderColumn :: [LabelPart] -> [Text]
    renderColumn parts =
        renderPart width <$> parts
      where
        width :: LabelPartLength
        width = columnWidth parts

renderPart :: LabelPartLength -> LabelPart -> Text
renderPart (LabelPartLength paddedWidth) (LabelPart alignment text) =
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
    paddingWidth =
        max 0 (fromIntegral @Natural @Int paddedWidth - labelPartWidth)
