{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- Data type that represents a collection of checkpoints.
-- Each checkpoints is associated with a 'Slot'.
module Cardano.Wallet.Checkpoints
  ( -- * Checkpoints
    Checkpoints,
    checkpoints,
    loadCheckpoints,
    fromGenesis,
    getLatest,
    findNearestPoint,

    -- * Delta types
    DeltaCheckpoints (..),
    DeltasCheckpoints,

    -- * Checkpoint hygiene
    SparseCheckpointsConfig (..),
    defaultSparseCheckpointsConfig,
    sparseCheckpoints,
    gapSize,
  )
where

import qualified Cardano.Wallet.Primitive.Types as W
import Data.Delta
  ( Delta (..),
  )
import Data.Generics.Internal.VL.Lens
  ( over,
    view,
  )
import qualified Data.List as L
import Data.Map.Strict
  ( Map,
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe,
  )
import Data.Quantity
  ( Quantity (..),
  )
import qualified Data.Set as Set
import Data.Word
  ( Word32,
    Word8,
  )
import Fmt
  ( Buildable (..),
    listF,
  )
import GHC.Generics
  ( Generic,
  )
import Prelude

{- NOTE [PointSlotNo]

'SlotNo' cannot represent the genesis point.

Historical hack. The DB layer can't represent 'Origin' in the database,
instead we have mapped it to 'SlotNo 0', which is wrong.

Rolling back to SlotNo 0 instead of Origin is fine for followers starting
from genesis (which should be the majority of cases). Other, non-trivial
rollbacks to genesis cannot occur on mainnet (genesis is years within
stable part, and there were no rollbacks in byron).

Could possibly be problematic in the beginning of a testnet without a
byron era. /Perhaps/ this is what is happening in the
>>> [cardano-wallet.pools-engine:Error:1293] [2020-11-24 10:02:04.00 UTC]
>>> Couldn't store production for given block before it conflicts with
>>> another block. Conflicting block header is:
>>> 5bde7e7b<-[f1b35b98-4290#2008]
errors observed in the integration tests.

The issue has been partially fixed in that 'rollbackTo' now takes
a 'Slot' as argument, which can represent the 'Origin'.
However, the database itself mostly stores slot numbers.

FIXME LATER during ADP-1043: As we move towards in-memory data,
all slot numbers in the DB file will either be replaced by
the 'Slot' type, or handled slightly differently when it
is clear that the data cannot exist at the genesis point
(e.g. for TxHistory).

-}

{-------------------------------------------------------------------------------
    Checkpoints
-------------------------------------------------------------------------------}

-- | Collection of checkpoints indexed by 'Slot'.
newtype Checkpoints a = Checkpoints
  { -- | Map of checkpoints. Always contains the genesis checkpoint.
    checkpoints :: Map W.Slot a
  }
  deriving (Eq, Show, Generic)

-- FIXME LATER during ADP-1043:
--  Use a more sophisticated 'Checkpoints' type that stores deltas.

-- | Turn the list of checkpoints into a map of checkpoints.
--
-- FIXME LATER during ADP-1043:
--   The database actually does not store the checkpoint at genesis,
--   but the checkpoint after that.
--   Hence, this function does not check whether the genesis checkpoint
--   is in the list of checkpoints.
loadCheckpoints :: [(W.Slot, a)] -> Checkpoints a
loadCheckpoints = Checkpoints . Map.fromList

-- | Begin with the genesis checkpoint.
fromGenesis :: a -> Checkpoints a
fromGenesis a = Checkpoints $ Map.singleton W.Origin a

-- | Get the checkpoint with the largest 'SlotNo'.
getLatest :: Checkpoints a -> (W.Slot, a)
getLatest = from . Map.lookupMax . view #checkpoints
  where
    from = fromMaybe (error "getLatest: there should always be at least a genesis checkpoint")

-- | Find the nearest 'Checkpoint' that is either at the given point or before.
findNearestPoint :: Checkpoints a -> W.Slot -> Maybe W.Slot
findNearestPoint m key = fst <$> Map.lookupLE key (view #checkpoints m)

{-------------------------------------------------------------------------------
    Delta type for Checkpoints
-------------------------------------------------------------------------------}
type DeltasCheckpoints a = [DeltaCheckpoints a]

data DeltaCheckpoints a
  = PutCheckpoint W.Slot a
  | RollbackTo W.Slot
  | -- Rolls back to the latest checkpoint at or before this slot.

    -- | Restrict to the intersection of this list with
    -- the checkpoints that are already present.
    -- The genesis checkpoint will always be present.
    RestrictTo [W.Slot]

instance Delta (DeltaCheckpoints a) where
  type Base (DeltaCheckpoints a) = Checkpoints a
  apply (PutCheckpoint pt a) = over #checkpoints $ Map.insert pt a
  apply (RollbackTo pt) =
    over #checkpoints $
      Map.filterWithKey (\k _ -> k <= pt)
  apply (RestrictTo pts) = over #checkpoints $ \m ->
    Map.restrictKeys m $ Set.fromList (W.Origin : pts)

instance Buildable (DeltaCheckpoints a) where
  build (PutCheckpoint slot _) = "PutCheckpoint " <> build slot
  build (RollbackTo slot) = "RollbackTo " <> build slot
  build (RestrictTo slots) = "RestrictTo " <> listF slots

{-------------------------------------------------------------------------------
    Checkpoint hygiene
-------------------------------------------------------------------------------}

-- | Storing EVERY checkpoints in the database is quite expensive and useless.
-- We make the following assumptions:
--
-- - We can't rollback for more than `k=epochStability` blocks in the past
-- - It is pretty fast to re-sync a few hundred blocks
-- - Small rollbacks may occur more often than deep ones
--
-- So, as we insert checkpoints, we make sure to:
--
-- - Prune any checkpoint that more than `k` blocks in the past
-- - Keep only one checkpoint every 100 blocks
-- - But still keep ~10 most recent checkpoints to cope with small rollbacks
--
-- __Example 1__: Inserting `cp153`
--
--  ℹ: `cp142` is discarded and `cp153` inserted.
--
--  @
--  Currently in DB:
-- ┌───┬───┬───┬─  ──┬───┐
-- │cp000 │cp100 │cp142 │..    ..│cp152 │
-- └───┴───┴───┴─  ──┴───┘
--  Want in DB:
-- ┌───┬───┬───┬─  ──┬───┐
-- │cp000 │cp100 │cp143 │..    ..│cp153 │
-- └───┴───┴───┴─  ──┴───┘
--  @
--
--
--  __Example 2__: Inserting `cp111`
--
--  ℹ: `cp100` is kept and `cp111` inserted.
--
--  @
--  Currently in DB:
-- ┌───┬───┬───┬─  ──┬───┐
-- │cp000 │cp100 │cp101 │..    ..│cp110 │
-- └───┴───┴───┴─  ──┴───┘
--  Want in DB:
-- ┌───┬───┬───┬─  ──┬───┐
-- │cp000 │cp100 │cp101 │..    ..│cp111 │
-- └───┴───┴───┴─  ──┴───┘
--  @
--
-- NOTE: There might be cases where the chain following "fails" (because, for
-- example, the node has switched to a different chain, different by more than k),
-- and in such cases, we have no choice but rolling back from genesis.
-- Therefore, we need to keep the very first checkpoint in the database, no
-- matter what.
sparseCheckpoints ::
  -- | Parameters for the function.
  SparseCheckpointsConfig ->
  -- | A given block height
  Quantity "block" Word32 ->
  -- | The list of checkpoint heights that should be kept in DB.
  [Word32]
sparseCheckpoints cfg blkH =
  let SparseCheckpointsConfig {edgeSize, epochStability} = cfg
      g = gapSize cfg
      h = getQuantity blkH
      e = fromIntegral edgeSize

      minH =
        let x = if h < epochStability + g then 0 else h - epochStability - g
         in g * (x `div` g)

      initial = 0
      longTerm = [minH, minH + g .. h]
      shortTerm =
        if h < e
          then [0 .. h]
          else [h - e, h - e + 1 .. h]
   in L.sort (L.nub $ initial : (longTerm ++ shortTerm))

-- | Captures the configuration for the `sparseCheckpoints` function.
--
-- NOTE: large values of 'edgeSize' aren't recommended as they would mean
-- storing many unnecessary checkpoints. In Ouroboros Praos, there's a
-- reasonable probability for small forks each a few blocks deep so it makes sense to
-- maintain a small part that is denser near the edge.
data SparseCheckpointsConfig = SparseCheckpointsConfig
  { edgeSize :: Word8,
    epochStability :: Word32
  }
  deriving (Show)

-- | A sensible default to use in production. See also 'SparseCheckpointsConfig'
defaultSparseCheckpointsConfig :: Quantity "block" Word32 -> SparseCheckpointsConfig
defaultSparseCheckpointsConfig (Quantity epochStability) =
  SparseCheckpointsConfig
    { edgeSize = 5,
      epochStability
    }

-- | A reasonable gap size used internally in 'sparseCheckpoints'.
--
-- 'Reasonable' means that it's not _too frequent_ and it's not too large. A
-- value that is too small in front of k would require generating much more
-- checkpoints than necessary.
--
-- A value that is larger than `k` may have dramatic consequences in case of
-- deep rollbacks.
--
-- As a middle ground, we current choose `k / 3`, which is justified by:
--
-- - The current speed of the network layer (several thousands blocks per seconds)
-- - The current value of k = 2160
--
-- So, `k / 3` = 720, which should remain around a second of time needed to catch
-- up in case of large rollbacks.
gapSize :: SparseCheckpointsConfig -> Word32
gapSize SparseCheckpointsConfig {epochStability} =
  epochStability `div` 3
