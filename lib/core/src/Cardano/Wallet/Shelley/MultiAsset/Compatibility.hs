{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- This module provides glue code between shelley-ma ledger spec
-- types and functions and the wallet. Types from shelley-ma should
-- generally not leak into the rest of the codebase.
--(HashAlgorithm (ADDRHASH (Crypto era)))
module Cardano.Wallet.Shelley.MultiAsset.Compatibility where

import Prelude

import Cardano.Crypto.Hash
    ( HashAlgorithm, hashFromBytes )
import Cardano.Ledger.Crypto
    ( ADDRHASH )
import Cardano.Ledger.Era
    ( Crypto )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Ledger.Mary.Value
    ( AssetName (..), PolicyID (..), Value (..) )
import Cardano.Ledger.Val
    ( scaledMinDeposit )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( getHash )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.TokenMap
    ( TokenMap (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Data.Bits
    ( toIntegralSized )
import Data.Map.NonEmpty.Strict
    ( toMap )
import Data.Text.Encoding
    ( encodeUtf8 )
import Shelley.Spec.Ledger.Scripts
    ( ScriptHash (..) )

import qualified Data.Map.Strict as Map
import qualified Shelley.Spec.Ledger.Coin as SL

-- | Convert our own @TokenBundle@ type to the corresponding
-- @Value era@ ledger type. This should be very low in cost,
-- because it just converts strict Map to strict Map.
toLedgerSpecValue
    :: forall era. (HashAlgorithm (ADDRHASH (Crypto era)))
    => TokenBundle
    -> Value era
toLedgerSpecValue = \(TokenBundle ada (TokenMap tb)) ->
    Value (fromIntegral $ unCoin ada) . Map.foldrWithKey' outer Map.empty $ tb
  where
    outer (TokenPolicyId h) a m = case hashFromBytes $ getHash h of
        Just sbs -> Map.insert
            (PolicyID (ScriptHash sbs))
            (Map.foldrWithKey inner mempty (toMap a))
            m
        Nothing -> m -- TODO: do we want to hard-fail here?
    inner (TokenName k) (TokenQuantity a) m = Map.insert (AssetName $ encodeUtf8 k) a m

-- | Calculate the minimal UTxO value for a token bundle.
--
-- Returns @Nothing@ if Coin conversion fails (Integer to Word64).
calculateMinCoin
    :: forall era . (Era era)
    => Coin        -- ^ the protocol minUTxOValue
    -> TokenBundle
    -> Maybe Coin
calculateMinCoin = \c t -> fromLedgerCoin $ scaledMinDeposit
    (toLedgerSpecValue @era t) (fromWalletCoin c)
  where
    fromLedgerCoin :: SL.Coin -> Maybe Coin
    fromLedgerCoin (SL.Coin i) = Coin <$> (toIntegralSized i)

    fromWalletCoin :: Coin -> SL.Coin
    fromWalletCoin (Coin i) = SL.Coin (fromIntegral i)
