{-# LANGUAGE NumericUnderscores #-}

{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Defines generators and shrinkers for the 'MinimumUTxO' data type.
module Cardano.Wallet.Primitive.Types.MinimumUTxO.Gen
  ( -- * Generators and shrinkers
    genMinimumUTxO,
    genMinimumUTxOForShelleyBasedEra,
    shrinkMinimumUTxO,
    shrinkMinimumUTxOForShelleyBasedEra,

    -- * Test protocol parameter values
    testParameter_minUTxOValue_Shelley,
    testParameter_minUTxOValue_Allegra,
    testParameter_minUTxOValue_Mary,
    testParameter_coinsPerUTxOWord_Alonzo,
    testParameter_coinsPerUTxOByte_Babbage,
  )
where

import Cardano.Api
  ( ShelleyBasedEra (..),
  )
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (..),
  )
import Cardano.Wallet.Primitive.Types.Coin.Gen
  ( chooseCoin,
  )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
  ( MinimumUTxO (..),
    MinimumUTxOForShelleyBasedEra (..),
  )
import Data.Default
  ( Default (..),
  )
import Data.Semigroup
  ( stimes,
  )
import Test.QuickCheck
  ( Gen,
    chooseInteger,
    frequency,
    oneof,
  )
import Prelude

--------------------------------------------------------------------------------
-- Generating 'MinimumUTxO' values
--------------------------------------------------------------------------------

genMinimumUTxO :: Gen MinimumUTxO
genMinimumUTxO =
  frequency
    [ (1, genMinimumUTxONone),
      (1, genMinimumUTxOConstant),
      (8, MinimumUTxOForShelleyBasedEraOf <$> genMinimumUTxOForShelleyBasedEra)
    ]
  where
    genMinimumUTxONone :: Gen MinimumUTxO
    genMinimumUTxONone = pure MinimumUTxONone

    genMinimumUTxOConstant :: Gen MinimumUTxO
    genMinimumUTxOConstant =
      MinimumUTxOConstant
        <$>
        -- The 'MinimumUTxOConstant' constructor is only used for testing.
        genCoinOfSimilarMagnitude (Coin 1_000_000)

shrinkMinimumUTxO :: MinimumUTxO -> [MinimumUTxO]
shrinkMinimumUTxO = const []

--------------------------------------------------------------------------------
-- Generating 'MinimumUTxOForShelleyBasedEra' values
--------------------------------------------------------------------------------

genMinimumUTxOForShelleyBasedEra ::
  Gen MinimumUTxOForShelleyBasedEra
genMinimumUTxOForShelleyBasedEra =
  oneof
    [ genShelley,
      genAllegra,
      genMary,
      genAlonzo,
      genBabbage
    ]
  where
    genShelley :: Gen MinimumUTxOForShelleyBasedEra
    genShelley = do
      minUTxOValue <-
        genLedgerCoinOfSimilarMagnitude
          testParameter_minUTxOValue_Shelley
      pure $
        MinimumUTxOForShelleyBasedEra
          ShelleyBasedEraShelley
          def {Shelley._minUTxOValue = minUTxOValue}

    genAllegra :: Gen MinimumUTxOForShelleyBasedEra
    genAllegra = do
      minUTxOValue <-
        genLedgerCoinOfSimilarMagnitude
          testParameter_minUTxOValue_Allegra
      pure $
        MinimumUTxOForShelleyBasedEra
          ShelleyBasedEraAllegra
          def {Shelley._minUTxOValue = minUTxOValue}

    genMary :: Gen MinimumUTxOForShelleyBasedEra
    genMary = do
      minUTxOValue <-
        genLedgerCoinOfSimilarMagnitude
          testParameter_minUTxOValue_Mary
      pure $
        MinimumUTxOForShelleyBasedEra
          ShelleyBasedEraMary
          def {Shelley._minUTxOValue = minUTxOValue}

    genAlonzo :: Gen MinimumUTxOForShelleyBasedEra
    genAlonzo = do
      coinsPerUTxOWord <-
        genLedgerCoinOfSimilarMagnitude
          testParameter_coinsPerUTxOWord_Alonzo
      pure $
        MinimumUTxOForShelleyBasedEra
          ShelleyBasedEraAlonzo
          def {Alonzo._coinsPerUTxOWord = coinsPerUTxOWord}

    genBabbage :: Gen MinimumUTxOForShelleyBasedEra
    genBabbage = do
      coinsPerUTxOByte <-
        genLedgerCoinOfSimilarMagnitude
          testParameter_coinsPerUTxOByte_Babbage
      pure $
        MinimumUTxOForShelleyBasedEra
          ShelleyBasedEraBabbage
          def {Babbage._coinsPerUTxOByte = coinsPerUTxOByte}

shrinkMinimumUTxOForShelleyBasedEra ::
  MinimumUTxOForShelleyBasedEra -> [MinimumUTxOForShelleyBasedEra]
shrinkMinimumUTxOForShelleyBasedEra = const []

--------------------------------------------------------------------------------
-- Test protocol parameter values
--------------------------------------------------------------------------------

-- | A test value of the Shelley-era 'minUTxOValue' parameter.
--
-- Value derived from 'mainnet-shelley-genesis.json'.
testParameter_minUTxOValue_Shelley :: Ledger.Coin
testParameter_minUTxOValue_Shelley = Ledger.Coin 1_000_000

-- | A test value of the Allegra-era 'minUTxOValue' parameter.
--
-- Value derived from 'mainnet-shelley-genesis.json'.
testParameter_minUTxOValue_Allegra :: Ledger.Coin
testParameter_minUTxOValue_Allegra = Ledger.Coin 1_000_000

-- | A test value of the Mary-era 'minUTxOValue' parameter.
--
-- Value derived from 'mainnet-shelley-genesis.json'.
testParameter_minUTxOValue_Mary :: Ledger.Coin
testParameter_minUTxOValue_Mary = Ledger.Coin 1_000_000

-- | A test value of the Alonzo-era 'coinsPerUTxOWord' parameter.
--
-- Value derived from 'mainnet-alonzo-genesis.json'.
testParameter_coinsPerUTxOWord_Alonzo :: Ledger.Coin
testParameter_coinsPerUTxOWord_Alonzo = Ledger.Coin 34_482

-- | A test value of the Babbage-era 'coinsPerUTxOByte' parameter.
--
-- Value derived from 'mainnet-alonzo-genesis.json':
-- >>> 34_482 `div` 8 == 4_310
testParameter_coinsPerUTxOByte_Babbage :: Ledger.Coin
testParameter_coinsPerUTxOByte_Babbage = Ledger.Coin 4_310

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

-- | Chooses a 'Ledger.Coin' value from within the given range.
chooseLedgerCoin :: (Ledger.Coin, Ledger.Coin) -> Gen Ledger.Coin
chooseLedgerCoin (Ledger.Coin lo, Ledger.Coin hi) =
  Ledger.Coin <$> chooseInteger (lo, hi)

-- | Generates a wallet 'Coin' value that has a similar magnitude to the given
--   value.
genCoinOfSimilarMagnitude :: Coin -> Gen Coin
genCoinOfSimilarMagnitude coin =
  chooseCoin (mempty, stimes (2 :: Int) coin)

-- | Generates a 'Ledger.Coin' value that has a similar magnitude to the given
--   value.
genLedgerCoinOfSimilarMagnitude :: Ledger.Coin -> Gen Ledger.Coin
genLedgerCoinOfSimilarMagnitude coin =
  chooseLedgerCoin (mempty, stimes (2 :: Int) coin)
