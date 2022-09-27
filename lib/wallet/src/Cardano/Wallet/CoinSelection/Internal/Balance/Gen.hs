{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.CoinSelection.Internal.Balance.Gen
  ( genSelectionLimit,
    genSelectionSkeleton,
    genSelectionStrategy,
    shrinkSelectionLimit,
    shrinkSelectionSkeleton,
    shrinkSelectionStrategy,
  )
where

import Cardano.Wallet.CoinSelection.Internal.Balance
  ( SelectionLimit,
    SelectionLimitOf (..),
    SelectionSkeleton (..),
    SelectionStrategy (..),
  )
import Cardano.Wallet.CoinSelection.Internal.Context
  ( SelectionContext (..),
  )
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (..),
  )
import Cardano.Wallet.Primitive.Types.TokenBundle
  ( TokenBundle,
  )
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
  ( genTokenBundleSmallRange,
    shrinkTokenBundleSmallRange,
  )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
  ( genAssetId,
    shrinkAssetId,
  )
import qualified Data.Set as Set
import Generics.SOP
  ( NP (..),
  )
import Test.QuickCheck
  ( Gen,
    NonNegative (..),
    arbitrary,
    arbitraryBoundedEnum,
    listOf,
    oneof,
    shrink,
    shrinkList,
    shrinkMapBy,
    suchThat,
  )
import Test.QuickCheck.Extra
  ( genericRoundRobinShrink,
    (<:>),
    (<@>),
  )
import Prelude

--------------------------------------------------------------------------------
-- Selection limits
--------------------------------------------------------------------------------

genSelectionLimit :: Gen SelectionLimit
genSelectionLimit =
  oneof
    [ MaximumInputLimit . getNonNegative <$> arbitrary,
      pure NoLimit
    ]

shrinkSelectionLimit :: SelectionLimit -> [SelectionLimit]
shrinkSelectionLimit = \case
  MaximumInputLimit n ->
    MaximumInputLimit . getNonNegative <$> shrink (NonNegative n)
  NoLimit ->
    []

--------------------------------------------------------------------------------
-- Selection skeletons
--------------------------------------------------------------------------------

genSelectionSkeleton :: Gen (Address ctx) -> Gen (SelectionSkeleton ctx)
genSelectionSkeleton genAddress =
  SelectionSkeleton
    <$> genSkeletonInputCount
    <*> genSkeletonOutputs
    <*> genSkeletonChange
  where
    genSkeletonInputCount =
      getNonNegative <$> arbitrary @(NonNegative Int)
    genSkeletonOutputs =
      listOf genSkeletonOutput
    genSkeletonOutput =
      (,)
        <$> genAddress
        <*> genTokenBundleSmallRange `suchThat` tokenBundleHasNonZeroCoin
    genSkeletonChange =
      listOf (Set.fromList <$> listOf genAssetId)

shrinkSelectionSkeleton ::
  (Address ctx -> [Address ctx]) ->
  (SelectionSkeleton ctx -> [SelectionSkeleton ctx])
shrinkSelectionSkeleton shrinkAddress =
  genericRoundRobinShrink
    <@> shrinkSkeletonInputCount
    <:> shrinkSkeletonOutputs
    <:> shrinkSkeletonChange
    <:> Nil
  where
    shrinkSkeletonInputCount =
      shrink @Int
    shrinkSkeletonOutputs =
      shrinkList shrinkSkeletonOutput
    shrinkSkeletonOutput =
      genericRoundRobinShrink
        <@> shrinkAddress
        <:> filter tokenBundleHasNonZeroCoin . shrinkTokenBundleSmallRange
        <:> Nil
    shrinkSkeletonChange =
      shrinkList $
        shrinkMapBy Set.fromList Set.toList (shrinkList shrinkAssetId)

tokenBundleHasNonZeroCoin :: TokenBundle -> Bool
tokenBundleHasNonZeroCoin b = TokenBundle.getCoin b /= Coin 0

--------------------------------------------------------------------------------
-- Selection strategies
--------------------------------------------------------------------------------

genSelectionStrategy :: Gen SelectionStrategy
genSelectionStrategy = arbitraryBoundedEnum

shrinkSelectionStrategy :: SelectionStrategy -> [SelectionStrategy]
shrinkSelectionStrategy = \case
  -- Shrinking from "optimal" to "minimal" should increase the likelihood of
  -- making a successful selection, as the "minimal" strategy is designed to
  -- generate smaller selections.
  SelectionStrategyMinimal -> []
  SelectionStrategyOptimal -> [SelectionStrategyMinimal]
