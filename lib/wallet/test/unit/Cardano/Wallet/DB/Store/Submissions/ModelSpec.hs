module Cardano.Wallet.DB.Store.Submissions.ModelSpec (spec, genDeltas, genDeltasConstrained) where

import Cardano.Wallet.DB.Arbitrary
  (
  )
import Cardano.Wallet.DB.Sqlite.Schema
  ( LocalTxSubmission (LocalTxSubmission),
  )
import Cardano.Wallet.DB.Sqlite.Types
  ( TxId (..),
  )
import Cardano.Wallet.DB.Store.Submissions.Model
  ( DeltaTxLocalSubmission (..),
    TxLocalSubmissionHistory (..),
  )
import Cardano.Wallet.Primitive.Types
  ( WalletId,
  )
import Cardano.Wallet.Primitive.Types.Hash
  ( Hash (..),
  )
import Cardano.Wallet.Primitive.Types.Tx
  ( mockSealedTx,
  )
import Control.Monad
  ( forM,
  )
import qualified Data.Map.Strict as Map
import Test.Hspec
  ( Spec,
  )
import Test.QuickCheck
  ( Gen,
    arbitrary,
    frequency,
    shuffle,
    sized,
    sublistOf,
    vectorOf,
  )
import Prelude

spec :: Spec
spec = pure ()

genDeltas ::
  -- | wallet holding the submissions
  WalletId ->
  -- | submitted ones
  TxLocalSubmissionHistory ->
  Gen (DeltaTxLocalSubmission)
genDeltas wid old = genDeltasConstrained wid old Nothing

genDeltasConstrained ::
  -- | wallet holding the submissions
  WalletId ->
  -- | submitted ones
  TxLocalSubmissionHistory ->
  -- | possible pool of txids
  Maybe [TxId] ->
  Gen (DeltaTxLocalSubmission)
genDeltasConstrained wid (TxLocalSubmissionHistory old) txids =
  frequency $
    [ ( 1,
        sized $ \n -> do
          tids <- genTxIds n txids
          locals <- forM tids $ \txId'@(TxId txId) -> do
            let sealed = mockSealedTx . getHash $ txId
            slot <- arbitrary
            pure (txId', LocalTxSubmission txId' wid slot sealed)

          pure $ Expand . TxLocalSubmissionHistory $ Map.fromList locals
      )
    ]
      <> [(2, Prune <$> sublistOf (Map.keys old)) | not (null old)]

genTxIds :: Int -> Maybe [TxId] -> Gen [TxId]
genTxIds n Nothing = fmap TxId <$> vectorOf n arbitrary
genTxIds n (Just base) = take n <$> shuffle base
