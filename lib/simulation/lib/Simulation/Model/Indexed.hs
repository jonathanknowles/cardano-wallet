{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Simulation.Model.Indexed where

import Simulation.Model.Basic

import Prelude hiding
    ( null
    )

import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( mapMaybe
    )
import Deriving
    ( AsList (AsList)
    , Prefix (Prefix)
    )
import GHC.IsList
    ( IsList (fromList, toList)
    )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Address
    = AddressInternal
    | AddressExternal
    deriving stock (Eq, Ord, Show)

data IndexedTx = IndexedTx
    { inputs :: [TxIn]
    , outputs :: [TxOut]
    , fee :: Value
    }
    deriving stock (Eq, Show)

newtype IndexedTxBalancer = IndexedTxBalancer
    { balanceIndexedTx :: UTxO -> IndexedTx -> Either BalanceTxError IndexedTx }

newtype TxIn = TxIn Int
    deriving stock (Eq, Ord, Show)

data TxOut = TxOut Address Value
    deriving stock (Eq, Show)

newtype UTxO = UTxO (Map TxIn TxOut)
    deriving stock Eq
    deriving Show via Prefix "UTxO" (AsList UTxO)
    deriving newtype IsList

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

indexedTxBalancerToTxBalancer :: IndexedTxBalancer -> TxBalancer
indexedTxBalancerToTxBalancer IndexedTxBalancer {balanceIndexedTx} =
    TxBalancer {balanceTx}
  where
    balanceTx :: Wallet -> PartialTx -> Either BalanceTxError Tx
    balanceTx wallet (PartialTx ptxOutputs) =
        indexedTxToTx <$> balanceIndexedTx (UTxO utxoMap) unbalancedIndexedTx
      where
        unbalancedIndexedTx :: IndexedTx
        unbalancedIndexedTx = IndexedTx
            { inputs = mempty
            , outputs = TxOut AddressExternal <$> ptxOutputs
            , fee = mempty
            }

        utxoMap :: Map TxIn TxOut
        utxoMap = fromList $ zip
            (TxIn <$> [0 ..])
            (TxOut AddressInternal <$> toList wallet)

        indexedTxToTx :: IndexedTx -> Tx
        indexedTxToTx (IndexedTx itxInputs itxOutputs itxFee) =
            Tx
                { inputs = mapMaybe lookupInput itxInputs
                , outputs = mapMaybe toExternalOutput itxOutputs
                , change = mapMaybe toInternalOutput itxOutputs
                , fee = itxFee
                }
          where
            lookupInput i =
                (\(TxOut _ v) -> v) <$> Map.lookup i utxoMap
            toExternalOutput = \case
                TxOut AddressExternal v -> Just v
                TxOut AddressInternal _ -> Nothing
            toInternalOutput = \case
                TxOut AddressInternal v -> Just v
                TxOut AddressExternal _ -> Nothing
