{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Shelley.Network
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiNetworkParameters (..) )
import Data.Quantity
    ( Quantity (..), mkPercentage )
import Data.Ratio
    ( (%) )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldNotBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , epochLengthValue
    , eventually
    , expectField
    , expectResponseCode
    , minUTxOValue
    , request
    , slotLengthValue
    , verify
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: SpecWith Context
spec = describe "SHELLEY_NETWORK" $ do
    it "NETWORK_PARAMS - Able to fetch network parameters" $ \ctx ->
        eventually "hardfork is detected in network parameters " $ do
        r <- request @ApiNetworkParameters ctx Link.getNetworkParams Default Empty
        expectResponseCode @IO HTTP.status200 r
        let Right d = Quantity <$> mkPercentage (3 % 4)
        -- for Shelley desiredPoolNumber is node's nOpt protocol parameter
        -- in integration test setup it is 3
        let nOpt = 3
        verify r
            [ expectField #decentralizationLevel (`shouldBe` d)
            , expectField #desiredPoolNumber (`shouldBe` nOpt)
            , expectField #minimumUtxoValue (`shouldBe` Quantity minUTxOValue)
            , expectField #hardforkAt (`shouldNotBe` Nothing)
            , expectField #slotLength (`shouldBe` Quantity slotLengthValue)
            , expectField #epochLength (`shouldBe` Quantity epochLengthValue)
            -- TODO: ADP-554 query activeSlotCoefficient from ledger
            -- This value is hardcoded for mainnet (1.0 = 100%).
            -- The integration test cluster value is (0.5 = 50%).
            , expectField #activeSlotCoefficient (`shouldBe` Quantity 100.0)
            ]
