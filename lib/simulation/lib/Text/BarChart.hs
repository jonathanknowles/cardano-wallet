{-# HLINT ignore "Avoid restricted alias" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.BarChart
    ( Config (..)
    , Resolution (..)
    , Scale (..)
    , defaultConfig
    , render
    ) where

import Data.Bag
    ( Bag
    , Count (Count)
    )
import qualified Data.Bag as Bag
import Data.List.NonEmpty
    ( NonEmpty ((:|))
    )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio
    ( Ratio
    , (%)
    )
import Data.Text
    ( Text
    )
import qualified Data.Text as Text
import Numeric.Natural
    ( Natural
    )
import Prelude hiding
    ( lookup
    , maximum
    , minimum
    )
import qualified Prelude
    ( maximum
    )
import Successor
    ( Successor
    , successor
    )
import qualified Text.Bar as Bar
import Text.Colour
    ( Colour (Green, Red)
    , withColour
    )
import qualified Text.Label as Label
import Text.Label
    ( Label
    )

--------------------------------------------------------------------------------
-- Public
--------------------------------------------------------------------------------

data Config = Config
    { barColours :: NonEmpty Colour
    , barResolution :: Resolution
    , barScale :: Scale
    }

data Resolution
    = Resolution1
    | Resolution2
    | Resolution8
    deriving (Eq, Show)

data Scale
    = ScaleMultiplier (Ratio Natural)
    | ScaleLimit Natural
    deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { barColours = Green :| [Red]
    , barResolution = Resolution2
    , barScale = ScaleMultiplier (1 % 2)
    }

render
    :: forall a. (Ord a, Successor a)
    => Config
    -> (a -> Label)
    -> Bag a
    -> [Text]
render Config {barColours, barResolution, barScale} toLabel d = mconcat
    [ [ Text.replicate (labelColumnWidth + 1) " " <> topLeftCorner ]
    , [ toBar colour label n
      | (colour, label, n) <- zip3 colourSequence labelsPadded counts
      ]
    , [ Text.replicate (labelColumnWidth + 1) " " <> bottomLeftCorner ]
    ]
  where
    colourSequence :: [Colour]
    colourSequence = NonEmpty.toList $ NonEmpty.cycle barColours

    counts :: [Natural]
    counts = (\(Count n _) -> n) <$> labelCountPairs

    labelCountPairs :: [Count a]
    labelCountPairs = toDenseCountList d

    labels :: [Text]
    labels
        = Label.renderManyAsColumn
        $ fmap (toLabel . (\(Count _ a) -> a)) labelCountPairs

    labelsPadded :: [Text]
    labelsPadded = pad <$> labels
      where
        pad :: Text -> Text
        pad label =
            Text.replicate (labelColumnWidth - Text.length label) " " <> label

    labelColumnWidth :: Int
    labelColumnWidth = Prelude.maximum (0 : (Text.length <$> labels))

    toBar :: Colour -> Text -> Natural -> Text
    toBar colour label n =
        label
        <> " ┤"
        <> withColour colour
            (Bar.fromRational lengthResolution Bar.LengthRoundDown barWidth)
        <> " "
        <> Text.pack (show n)
      where
        lengthResolution :: Bar.LengthResolution
        lengthResolution = convertResolution barResolution

        barWidth :: Ratio Natural
        barWidth = case barScale of
            ScaleLimit a -> (a % 1)
            ScaleMultiplier r -> (n % 1) * r

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

toDenseCountList :: (Ord a, Successor a) => Bag a -> [Count a]
toDenseCountList b =
    maybe [] (uncurry toDenseCountListWithBounds) (Bag.bounds b)
  where
    toDenseCountListWithBounds lo hi =
        (`Bag.count` b) <$> from lo
      where
        from !x
            | x > hi = []
            | otherwise = x : maybe [] from (successor x)

topLeftCorner :: Text
topLeftCorner = "┌"

bottomLeftCorner :: Text
bottomLeftCorner = "└"

_topLeftCornerRounded :: Text
_topLeftCornerRounded = "╭"

_bottomLeftCornerRounded :: Text
_bottomLeftCornerRounded = "╰"

convertResolution :: Resolution -> Bar.LengthResolution
convertResolution = \case
    Resolution1 -> Bar.LengthResolution1
    Resolution2 -> Bar.LengthResolution2
    Resolution8 -> Bar.LengthResolution8
