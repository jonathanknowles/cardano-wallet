{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the `TransactionInfo` data types used by the wallet.
module Cardano.Wallet.Primitive.Types.Tx.TransactionInfo
  ( TransactionInfo (..),
    fromTransactionInfo,
    toTxHistory,
  )
where

import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (..),
  )
import Cardano.Wallet.Primitive.Types.Hash
  ( Hash (..),
  )
import Cardano.Wallet.Primitive.Types.RewardAccount
  ( RewardAccount (..),
  )
import Cardano.Wallet.Primitive.Types.Tx.Tx
  ( Tx (..),
    TxIn,
    TxMetadata,
    TxOut,
    TxScriptValidity,
  )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
  ( TxMeta,
  )
import Cardano.Wallet.Read.Tx.CBOR
  ( TxCBOR,
  )
import Control.DeepSeq
  ( NFData (..),
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Quantity
  ( Quantity (..),
  )
import Data.Time.Clock
  ( UTCTime,
  )
import GHC.Generics
  ( Generic,
  )
import Numeric.Natural
  ( Natural,
  )
import Prelude

-- | Full expanded and resolved information about a transaction, suitable for
-- presentation to the user.
data TransactionInfo = TransactionInfo
  { -- | Transaction ID of this transaction
    txInfoId :: Hash "Tx",
    -- | Serialization of this transaction
    txInfoCBOR :: Maybe TxCBOR,
    -- | Explicit transaction fee
    txInfoFee :: Maybe Coin,
    -- | Transaction inputs and (maybe) corresponding outputs of the
    -- source. Source information can only be provided for outgoing payments.
    txInfoInputs :: [(TxIn, Coin, Maybe TxOut)],
    -- | Collateral inputs and (maybe) corresponding outputs.
    txInfoCollateralInputs :: [(TxIn, Coin, Maybe TxOut)],
    -- | Payment destination.
    txInfoOutputs :: [TxOut],
    -- | An output that is only created if a transaction script fails.
    txInfoCollateralOutput :: Maybe TxOut,
    -- | Withdrawals on this transaction.
    txInfoWithdrawals :: Map RewardAccount Coin,
    -- | Other information calculated from the transaction.
    txInfoMeta :: TxMeta,
    -- | Number of slots since the transaction slot.
    txInfoDepth :: Quantity "block" Natural,
    -- | Creation time of the block including this transaction.
    txInfoTime :: UTCTime,
    -- | Application-specific extension data.
    txInfoMetadata :: Maybe TxMetadata,
    -- | Tag indicating whether non-native scripts in this transaction passed
    -- validation. This is added by the block creator when constructing the
    -- block. May be 'Nothing' for pre-Alonzo and pending transactions.
    txInfoScriptValidity :: Maybe TxScriptValidity
  }
  deriving (Generic, Show, Eq)

instance NFData TransactionInfo

-- | Reconstruct a transaction info from a transaction.
fromTransactionInfo :: TransactionInfo -> Tx
fromTransactionInfo info =
  Tx
    { txId = txInfoId info,
      txCBOR = txInfoCBOR info,
      fee = txInfoFee info,
      resolvedInputs = drop3rd <$> txInfoInputs info,
      resolvedCollateralInputs = drop3rd <$> txInfoCollateralInputs info,
      outputs = txInfoOutputs info,
      collateralOutput = txInfoCollateralOutput info,
      withdrawals = txInfoWithdrawals info,
      metadata = txInfoMetadata info,
      scriptValidity = txInfoScriptValidity info
    }
  where
    drop3rd :: (a, b, c) -> (a, b)
    drop3rd (a, b, _) = (a, b)

-- | Drop time-specific information
toTxHistory :: TransactionInfo -> (Tx, TxMeta)
toTxHistory info =
  (fromTransactionInfo info, txInfoMeta info)
