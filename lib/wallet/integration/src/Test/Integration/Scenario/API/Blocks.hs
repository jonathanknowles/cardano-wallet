{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Blocks
  ( spec,
  )
where

import qualified Cardano.Wallet.Api.Link as Link
import Cardano.Wallet.Api.Types.BlockHeader
  ( ApiBlockHeader,
  )
import qualified Network.HTTP.Types.Status as HTTP
import Test.Hspec
  ( SpecWith,
    describe,
  )
import Test.Hspec.Extra
  ( it,
  )
import Test.Integration.Framework.DSL
  ( Context (..),
    Headers (..),
    Payload (..),
    expectResponseCode,
    expectSuccess,
    request,
  )
import Prelude

spec :: SpecWith Context
spec = describe "BLOCKS" $ do
  it "LATEST_BLOCK Current tip is reported" $ \ctx -> do
    r <-
      request
        @ApiBlockHeader
        ctx
        Link.getBlocksLatestHeader
        Default
        Empty
    expectSuccess r
    expectResponseCode @IO HTTP.status200 r
