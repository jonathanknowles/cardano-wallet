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
{-# LANGUAGE TypeOperators #-}

module Simulation.Model.Basic where

import Prelude hiding
    ( null
    )

import Data.Bag
    ( Bag
    , CountList (CountList)
    , UnaryList (UnaryList)
    , (:×:) ((:×:))
    )
import qualified Data.Bag as Bag
import Data.Distribution
    ( Distribution
    , Interval
    , IntervalWidth (IntervalWidth)
    , defaultBarConfig
    , intervalToLabel
    , naturalToInterval
    , toBars
    )
import qualified Data.Distribution as Distribution
import Data.Distribution.Log
    ( LogDistribution
    )
import qualified Data.Distribution.Log as LogDistribution
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
import qualified Data.Text.IO as Text
import Deriving
    ( AsList (AsList)
    , Prefix (Prefix)
    )
import GHC.IsList
    ( IsList (fromList, toList)
    )
import Numeric.Natural
    ( Natural
    )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Action
    = Deposit Value
    | Payment Value
    deriving stock (Eq, Show)

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

newtype TxBalancer m = TxBalancer
    { balanceTx :: Wallet -> PartialTx -> m (Either BalanceTxError Tx) }

newtype Value = Value (Bag Asset)
    deriving Ord via AsList Value
    deriving IsList via CountList (Bag Asset)
    deriving Show via Prefix "Value" (AsList Value)
    deriving newtype
        ( Cancellative
        , Commutative
        , Eq
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
    deriving IsList via UnaryList (Bag Value)
    deriving Show via Prefix "Wallet" (AsList Wallet)
    deriving newtype
        ( Cancellative
        , Commutative
        , Eq
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
    insertChange tx <$> removeInputs tx wallet
  where
    removeInputs :: Tx -> Wallet -> Maybe Wallet
    removeInputs Tx {inputs} = (</> fromList inputs)

    insertChange :: Tx -> Wallet -> Wallet
    insertChange Tx {change} = (<> fromList change)

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

valueOfAsset :: Asset -> Value -> Natural :×: Asset
valueOfAsset a (Value v) = Bag.count a v

walletDistribution :: IntervalWidth -> Wallet -> Distribution Interval
walletDistribution intervalWidth w =
    Distribution.fromUnaryList intervals
  where
    intervals :: [Interval]
    intervals = naturalToInterval intervalWidth <$> lovelaceValues

    lovelaceValues :: [Natural]
    lovelaceValues = (\(n :×: _) -> n) . valueOfAsset Lovelace <$> toList w

printWalletDistribution :: IntervalWidth -> Wallet -> IO ()
printWalletDistribution intervalWidth w =
    mapM_ Text.putStrLn $
    toBars defaultBarConfig intervalToLabel $
    walletDistribution intervalWidth w

walletLogDistribution :: Wallet -> LogDistribution
walletLogDistribution w =
    LogDistribution.fromList lovelaceValues
  where
    lovelaceValues :: [Natural]
    lovelaceValues = (\(n :×: _) -> n) . valueOfAsset Lovelace <$> toList w

walletValue :: Wallet -> Value
walletValue = fold . toList
