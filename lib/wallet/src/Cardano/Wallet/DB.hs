{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Database / Persistence layer for the wallet backend. This is where we define
-- the interface allowing us to store and fetch various data on our wallets.
module Cardano.Wallet.DB
  ( -- * Interface
    DBLayer (..),
    DBFactory (..),
    cleanDB,

    -- * Errors
    ErrBadFormat (..),
    ErrWalletAlreadyExists (..),
    ErrNoSuchTransaction (..),
    ErrRemoveTx (..),
    ErrPutLocalTxSubmission (..),
  )
where

import Cardano.Address.Derivation
  ( XPrv,
  )
import Cardano.Wallet.DB.WalletState
  ( DeltaMap,
    DeltaWalletState,
    ErrNoSuchWallet (..),
  )
import Cardano.Wallet.Primitive.AddressDerivation
  ( Depth (..),
  )
import Cardano.Wallet.Primitive.Model
  ( Wallet,
  )
import Cardano.Wallet.Primitive.Passphrase
  ( PassphraseHash,
  )
import Cardano.Wallet.Primitive.Types
  ( ChainPoint,
    DelegationCertificate,
    GenesisParameters,
    Range (..),
    Slot,
    SlotNo (..),
    SortOrder (..),
    WalletId,
    WalletMetadata,
  )
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin,
  )
import Cardano.Wallet.Primitive.Types.Hash
  ( Hash,
  )
import Cardano.Wallet.Primitive.Types.Tx
  ( LocalTxSubmissionStatus,
    SealedTx,
    TransactionInfo,
    Tx (..),
    TxMeta,
    TxStatus,
  )
import Control.Monad.IO.Class
  ( MonadIO,
  )
import Control.Monad.Trans.Except
  ( ExceptT,
    runExceptT,
  )
import Data.DBVar
  ( DBVar,
  )
import Data.Quantity
  ( Quantity (..),
  )
import Data.Word
  ( Word32,
  )
import UnliftIO.Exception
  ( Exception,
  )
import Prelude

-- | Instantiate database layers at will
data DBFactory m s k = DBFactory
  { -- | Creates a new or use an existing database, maintaining an open
    -- connection so long as necessary
    withDatabase :: forall a. WalletId -> (DBLayer m s k -> IO a) -> IO a,
    -- | Erase any trace of the database
    removeDatabase :: WalletId -> IO (),
    -- | List existing wallet database found on disk.
    listDatabases :: IO [WalletId]
  }

-- | A Database interface for storing various things in a DB. In practice,
-- we'll need some extra constraints on the wallet state that allows us to
-- serialize and unserialize it (e.g. @forall s. (Serialize s) => ...@)
--
-- NOTE:
--
-- We can't use record accessors on the DBLayer as it carries an existential
-- within its constructor. We are forced to pattern-match on the `DBLayer`
-- record type in order to be able to use its methods in any context. With
-- NamedFieldPuns, or RecordWildCards, this can be quite easy:
--
-- @
-- myFunction DBLayer{..} = do
--     ...
--
-- myOtherFunction DBLayer{atomically,initializeWallet} = do
--     ...
-- @
--
-- Alternatively, in some other context where the database may not be a function
-- argument but come from a different source, it is possible to simply rely on
-- 'Data.Function.(&)' to easily pattern match on it:
--
-- @
-- myFunction arg0 arg1 = db & \DBLayer{..} -> do
--     ...
--   where
--     db = ...
-- @
--
-- Note that it isn't possible to simply use a @where@ clause or a @let@ binding
-- here as the semantic for those are slightly different: we really need a
-- pattern match here!
data DBLayer m s k = forall stm.
  (MonadIO stm, MonadFail stm) =>
  DBLayer
  { -- | Initialize a database entry for a given wallet. 'putCheckpoint',
    -- 'putWalletMeta', 'putTxHistory' or 'putProtocolParameters' will
    -- actually all fail if they are called _first_ on a wallet.
    initializeWallet ::
      WalletId ->
      Wallet s ->
      WalletMetadata ->
      [(Tx, TxMeta)] ->
      GenesisParameters ->
      ExceptT ErrWalletAlreadyExists stm (),
    -- | Remove a given wallet and all its associated data (checkpoints,
    -- metadata, tx history ...)
    removeWallet ::
      WalletId ->
      ExceptT ErrNoSuchWallet stm (),
    -- | Get the list of all known wallets in the DB, possibly empty.
    listWallets ::
      stm [WalletId],
    -- | 'DBVar' containing the 'WalletState' of each wallet in the database.
    -- Currently contains all 'Checkpoints' of the 'UTxO' and the
    -- 'Discoveries', as well as the 'Prologue' of the address discovery state.
    --
    -- Intended to replace 'putCheckpoint' and 'readCheckpoint' in the short-term,
    -- and all other functions in the long-term.
    walletsDB ::
      DBVar stm (DeltaMap WalletId (DeltaWalletState s)),
    -- | Replace the current checkpoint for a given wallet. We do not handle
    -- rollbacks yet, and therefore only stores the latest available
    -- checkpoint.
    --
    -- If the wallet doesn't exist, this operation returns an error.
    putCheckpoint ::
      WalletId ->
      Wallet s ->
      ExceptT ErrNoSuchWallet stm (),
    -- | Fetch the most recent checkpoint of a given wallet.
    --
    -- Return 'Nothing' if there's no such wallet.
    readCheckpoint ::
      WalletId ->
      stm (Maybe (Wallet s)),
    -- | List all known checkpoint tips, ordered by slot ids from the oldest
    -- to the newest.
    listCheckpoints ::
      WalletId ->
      stm [ChainPoint],
    -- | Replace an existing wallet metadata with the given one.
    --
    -- If the wallet doesn't exist, this operation returns an error
    putWalletMeta ::
      WalletId ->
      WalletMetadata ->
      ExceptT ErrNoSuchWallet stm (),
    -- | Fetch a wallet metadata, if they exist.
    --
    -- Return 'Nothing' if there's no such wallet.
    readWalletMeta ::
      WalletId ->
      stm (Maybe WalletMetadata),
    isStakeKeyRegistered ::
      WalletId ->
      ExceptT ErrNoSuchWallet stm Bool,
    -- | Binds a stake pool id to a wallet. This will have an influence on
    -- the wallet metadata: the last known certificate will indicate to
    -- which pool a wallet is currently delegating.
    --
    -- This is done separately from 'putWalletMeta' because certificate
    -- declarations are:
    --
    -- 1. Stored on-chain.
    -- 2. Affected by rollbacks (or said differently, tied to a 'SlotNo').
    putDelegationCertificate ::
      WalletId ->
      DelegationCertificate ->
      SlotNo ->
      ExceptT ErrNoSuchWallet stm (),
    -- | Store the latest known reward account balance.
    --
    -- This is separate from checkpoints because the data corresponds to the
    -- node tip.
    -- This is separate from putWalletMeta because it's not meta data
    putDelegationRewardBalance ::
      WalletId ->
      Coin ->
      ExceptT ErrNoSuchWallet stm (),
    -- | Get the reward account balance.
    --
    -- Returns zero if the wallet isn't found or if wallet hasn't delegated
    -- stake.
    readDelegationRewardBalance ::
      WalletId ->
      stm Coin,
    -- | Augments the transaction history for a known wallet.
    --
    -- If an entry for a particular transaction already exists it is not
    -- altered nor merged (just ignored).
    --
    -- If the wallet doesn't exist, this operation returns an error.
    putTxHistory ::
      WalletId ->
      [(Tx, TxMeta)] ->
      ExceptT ErrNoSuchWallet stm (),
    -- | Fetch the current transaction history of a known wallet, ordered by
    -- descending slot number.
    --
    -- Returns an empty list if the wallet isn't found.
    readTxHistory ::
      WalletId ->
      Maybe Coin ->
      SortOrder ->
      Range SlotNo ->
      Maybe TxStatus ->
      stm [TransactionInfo],
    -- | Fetch the latest transaction by id, returns Nothing when the
    -- transaction isn't found.
    --
    -- If the wallet doesn't exist, this operation returns an error.
    getTx ::
      WalletId ->
      Hash "Tx" ->
      ExceptT ErrNoSuchWallet stm (Maybe TransactionInfo),
    -- | Add or update a transaction in the local submission pool with the
    -- most recent submission slot.
    putLocalTxSubmission ::
      WalletId ->
      Hash "Tx" ->
      SealedTx ->
      SlotNo ->
      ExceptT ErrPutLocalTxSubmission stm (),
    -- | List all transactions from the local submission pool which are
    -- still pending as of the latest checkpoint of the given wallet. The
    -- slot numbers for first submission and most recent submission are
    -- included.
    readLocalTxSubmissionPending ::
      WalletId ->
      stm [LocalTxSubmissionStatus SealedTx],
    -- | Removes any expired transactions from the pending set and marks
    -- their status as expired.
    updatePendingTxForExpiry ::
      WalletId ->
      SlotNo ->
      ExceptT ErrNoSuchWallet stm (),
    -- | Manually remove a pending transaction.
    removePendingOrExpiredTx ::
      WalletId ->
      Hash "Tx" ->
      ExceptT ErrRemoveTx stm (),
    -- | Store or replace a private key for a given wallet. Note that wallet
    -- _could_ be stored and manipulated without any private key associated
    -- to it. A private key is only seldomly required for very specific
    -- operations (like transaction signing).
    putPrivateKey ::
      WalletId ->
      (k 'RootK XPrv, PassphraseHash) ->
      ExceptT ErrNoSuchWallet stm (),
    -- | Read a previously stored private key and its associated passphrase
    -- hash.
    readPrivateKey ::
      WalletId ->
      stm (Maybe (k 'RootK XPrv, PassphraseHash)),
    -- | Read the *Byron* genesis parameters.
    readGenesisParameters ::
      WalletId ->
      stm (Maybe GenesisParameters),
    -- | Drops all checkpoints and transaction data which
    -- have appeared after the given 'ChainPoint'.
    --
    -- Returns the actual 'ChainPoint' to which the database has rolled back.
    -- Its slot is guaranteed to be earlier than (or identical to) the given
    -- point of rollback but can't be guaranteed to be exactly the same
    -- because the database may only keep sparse checkpoints.
    rollbackTo ::
      WalletId ->
      Slot ->
      ExceptT ErrNoSuchWallet stm ChainPoint,
    -- | Prune database entities and remove entities that can be discarded.
    --
    -- The second argument represents the stability window, or said
    -- length of the deepest rollback.
    prune ::
      WalletId ->
      Quantity "block" Word32 ->
      ExceptT ErrNoSuchWallet stm (),
    -- | Execute operations of the database in isolation and atomically.
    atomically ::
      forall a.
      stm a ->
      m a
  }

-- | Can't read the database file because it's in a bad format
-- (corrupted, too old, …)
data ErrBadFormat
  = ErrBadFormatAddressPrologue
  | ErrBadFormatCheckpoints
  deriving (Eq, Show)

instance Exception ErrBadFormat

-- | Can't add a transaction to the local tx submission pool.
data ErrPutLocalTxSubmission
  = ErrPutLocalTxSubmissionNoSuchWallet ErrNoSuchWallet
  | ErrPutLocalTxSubmissionNoSuchTransaction ErrNoSuchTransaction
  deriving (Eq, Show)

-- | Can't remove pending or expired transaction.
data ErrRemoveTx
  = ErrRemoveTxNoSuchWallet ErrNoSuchWallet
  | ErrRemoveTxNoSuchTransaction ErrNoSuchTransaction
  | ErrRemoveTxAlreadyInLedger (Hash "Tx")
  deriving (Eq, Show)

-- | Indicates that the specified transaction hash is not found in the
-- transaction history of the given wallet.
data ErrNoSuchTransaction
  = ErrNoSuchTransaction WalletId (Hash "Tx")
  deriving (Eq, Show)

-- | Forbidden operation was executed on an already existing wallet
newtype ErrWalletAlreadyExists
  = ErrWalletAlreadyExists WalletId -- Wallet already exists in db
  deriving (Eq, Show)

-- | Clean a database by removing all wallets.
cleanDB :: DBLayer m s k -> m ()
cleanDB DBLayer {..} =
  atomically $
    listWallets >>= mapM_ (runExceptT . removeWallet)
