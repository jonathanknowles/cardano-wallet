{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Simulation.Model.Basic where

import Prelude hiding
    ( null
    )

import Data.Bag
    ( Bag
    , MultiplicityList (MultiplicityList)
    , UnaryList (UnaryList)
    )
import Data.Foldable
    ( Foldable (fold)
    )
import Data.Monoid
    ( Sum (Sum)
    )
import Data.Monoid.GCD
    ( OverlappingGCDMonoid
    )
import Data.Monoid.Monus
    ( Monus ((<\>))
    )
import Data.Monoid.Null
    ( MonoidNull (null)
    )
import Data.Semigroup.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive
    , Reductive ((</>))
    , RightCancellative
    , RightReductive
    )
import Data.Text
    ( Text
    )
import Deriving
    ( AsList (AsList)
    , AsShown (AsShown)
    , Prefix (Prefix)
    )
import GHC.IsList
    ( IsList (fromList)
    )
import Numeric.Natural
    ( Natural
    )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Asset = Lovelace | Asset Text
    deriving stock (Eq, Ord, Show)

newtype Fee = Fee Natural
    deriving stock (Eq, Ord, Show)
    deriving newtype Num
    deriving (Semigroup, Monoid, MonoidNull) via Sum Natural

data PartialTx = PartialTx
    { outputs :: [Value]
    }
    deriving stock (Eq, Show)

data Tx = Tx
    { inputs :: [Value]
    , outputs :: [Value]
    , change :: [Value]
    , fee :: Fee
    }
    deriving stock (Eq, Show)

newtype TxBalancer = TxBalancer
    { balanceTx :: Wallet -> PartialTx -> Either BalanceTxError Tx }

newtype Value = Value (Bag Asset)
    deriving (Eq, Ord) via AsShown (MultiplicityList (Bag Asset))
    deriving IsList via MultiplicityList (Bag Asset)
    deriving Show via Prefix "Value" (AsList Value)
    deriving newtype
        ( Cancellative
        , Commutative
        , LeftCancellative
        , LeftReductive
        , Monoid
        , MonoidNull
        , Monus
        , OverlappingGCDMonoid
        , Reductive
        , RightCancellative
        , RightReductive
        , Semigroup
        )

newtype Wallet = Wallet (Bag Value)
    deriving (Eq, Ord) via AsShown (MultiplicityList (Bag Value))
    deriving IsList via UnaryList (Bag Value)
    deriving Show via Prefix "Wallet" (AsList Wallet)
    deriving newtype
        ( Cancellative
        , Commutative
        , LeftCancellative
        , LeftReductive
        , Monoid
        , MonoidNull
        , Monus
        , OverlappingGCDMonoid
        , Reductive
        , RightCancellative
        , RightReductive
        , Semigroup
        )

newtype BalanceTxError = BalanceTxError String
    deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

applyTxToWallet :: Tx -> Wallet -> Maybe Wallet
applyTxToWallet tx wallet =
    receiveChange tx <$> consumeInputs tx wallet
  where
    consumeInputs :: Tx -> Wallet -> Maybe Wallet
    consumeInputs Tx {inputs} = (</> fromList inputs)

    receiveChange :: Tx -> Wallet -> Wallet
    receiveChange Tx {change} = (<> fromList change)

feeToValue :: Fee -> Value
feeToValue (Fee n) = [(Lovelace, n)]

txValueIn :: Tx -> Value
txValueIn Tx {inputs} = fold inputs

txValueOut :: Tx -> Value
txValueOut Tx {outputs, change, fee} =
    fold outputs <> fold change <> feeToValue fee

txValueDeficit :: Tx -> Value
txValueDeficit tx = txValueOut tx <\> txValueIn tx

txValueSurplus :: Tx -> Value
txValueSurplus tx = txValueIn tx <\> txValueOut tx

txIsBalanced :: Tx -> Bool
txIsBalanced tx =
    null (txValueDeficit tx) &&
    null (txValueSurplus tx)
