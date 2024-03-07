{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Avoid restricted alias" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Simulation.Model.Basic where

import Prelude hiding
    ( null
    )

import Data.Bag
    ( Bag
    , CountList (CountList)
    , UnaryList (UnaryList)
    )
import Data.Foldable
    ( Foldable (fold)
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
import Data.String
    ( IsString (fromString)
    )
import Data.Text
    ( Text
    )
import qualified Data.Text as Text
import Deriving
    ( AsList (AsList)
    , AsShow (AsShow)
    , Prefix (Prefix)
    )
import GHC.IsList
    ( IsList (fromList)
    )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Asset = Lovelace | Asset Text
    deriving stock (Eq, Ord, Show)

instance IsString Asset where
    fromString = Asset . Text.pack

data PartialTx = PartialTx
    { outputs :: [Value]
    }
    deriving stock (Eq, Show)

data Tx = Tx
    { inputs :: [Value]
    , outputs :: [Value]
    , change :: [Value]
    , fee :: Value
    }
    deriving stock (Eq, Show)

newtype TxBalancer = TxBalancer
    { balanceTx :: Wallet -> PartialTx -> Either BalanceTxError Tx }

newtype Value = Value (Bag Asset)
    deriving (Eq, Ord) via AsShow (CountList (Bag Asset))
    deriving IsList via CountList (Bag Asset)
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
    deriving (Eq, Ord) via AsShow (CountList (Bag Value))
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

txValueIn :: Tx -> Value
txValueIn Tx {inputs} = fold inputs

txValueOut :: Tx -> Value
txValueOut Tx {outputs, change, fee} =
    fold outputs <> fold change <> fee

txValueDeficit :: Tx -> Value
txValueDeficit tx = txValueOut tx <\> txValueIn tx

txValueSurplus :: Tx -> Value
txValueSurplus tx = txValueIn tx <\> txValueOut tx

txIsBalanced :: Tx -> Bool
txIsBalanced tx =
    null (txValueDeficit tx) &&
    null (txValueSurplus tx)
