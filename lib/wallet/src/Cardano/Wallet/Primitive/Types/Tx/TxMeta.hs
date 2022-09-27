{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the `TxMeta` data types used by the wallet.
module Cardano.Wallet.Primitive.Types.Tx.TxMeta
  ( TxMeta (..),
    TxStatus (..),
    Direction (..),
    WithDirection (..),
    isPending,
  )
where

import Cardano.Slotting.Slot
  ( SlotNo (..),
  )
import Cardano.Wallet.Orphans
  (
  )
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (..),
  )
import Control.DeepSeq
  ( NFData (..),
  )
import Data.Quantity
  ( Quantity (..),
  )
import Data.Text.Class
  ( CaseStyle (..),
    FromText (..),
    ToText (..),
    fromTextToBoundedEnum,
    toTextFromBoundedEnum,
  )
import qualified Data.Text.Lazy.Builder as Builder
import Data.Word
  ( Word32,
  )
import Fmt
  ( Buildable (..),
  )
import GHC.Generics
  ( Generic,
  )
import Prelude

-- | Additional information about a transaction, derived from the transaction
-- and ledger state. This should not be confused with 'TxMetadata' which is
-- application-specific data included with the transaction.
--
-- TODO: TxProperties or TxProps would be a good name for this type.
data TxMeta = TxMeta
  { status :: !TxStatus,
    direction :: !Direction,
    slotNo :: !SlotNo,
    blockHeight :: !(Quantity "block" Word32),
    -- | Amount seen from the perspective of the wallet. Refers either to a
    -- spent value for outgoing transaction, or a received value on incoming
    -- transaction.
    amount :: !Coin,
    -- | The slot at which a pending transaction will no longer be accepted
    -- into mempools.
    expiry :: !(Maybe SlotNo)
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData TxMeta

instance Buildable TxMeta where
  build (TxMeta s d sl (Quantity bh) c mex) =
    mempty
      <> build (WithDirection d c)
      <> " "
      <> build s
      <> " since "
      <> build sl
      <> "#"
      <> build bh
      <> maybe mempty (\ex -> " (expires slot " <> build ex <> ")") mex

data TxStatus
  = -- | Created, but not yet in a block.
    Pending
  | -- | Has been found in a block.
    InLedger
  | -- | Time to live (TTL) has passed.
    Expired
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance NFData TxStatus

instance Buildable TxStatus where
  build = Builder.fromText . toTextFromBoundedEnum SpacedLowerCase

instance FromText TxStatus where
  fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText TxStatus where
  toText = toTextFromBoundedEnum SnakeLowerCase

-- | The effect of a @Transaction@ on the wallet balance.
data Direction
  = -- | The wallet balance decreases.
    Outgoing
  | -- | The wallet balance increases or stays the same.
    Incoming
  deriving (Show, Bounded, Enum, Eq, Ord, Generic)

instance NFData Direction

instance Buildable Direction where
  build = Builder.fromText . toTextFromBoundedEnum SpacedLowerCase

instance FromText Direction where
  fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText Direction where
  toText = toTextFromBoundedEnum SnakeLowerCase

data WithDirection a = WithDirection Direction a

instance Buildable a => Buildable (WithDirection a) where
  build (WithDirection d a) =
    mempty
      <> (case d of Incoming -> "+"; Outgoing -> "-")
      <> build a

-- | True if the given metadata refers to a pending transaction
isPending :: TxMeta -> Bool
isPending = (== Pending) . (status :: TxMeta -> TxStatus)
