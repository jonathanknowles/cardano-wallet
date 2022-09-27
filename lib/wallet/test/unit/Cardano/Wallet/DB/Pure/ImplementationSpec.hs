{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.Pure.ImplementationSpec
  ( spec,
  )
where

import Cardano.Wallet.DB.Properties
  ( properties,
  )
import qualified Cardano.Wallet.DB.Pure.Layer as PureLayer
import Cardano.Wallet.DummyTarget.Primitive.Types
  ( dummyTimeInterpreter,
  )
import Cardano.Wallet.Primitive.AddressDerivation
  ( NetworkDiscriminant (..),
  )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
  ( ShelleyKey,
  )
import Cardano.Wallet.Primitive.AddressDiscovery
  ( IsOurs (..),
  )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
  ( SeqState (..),
  )
import Cardano.Wallet.Primitive.Types.Address
  ( Address,
  )
import Control.DeepSeq
  ( NFData,
  )
import Test.Hspec
  ( Spec,
    before,
    describe,
  )
import Test.QuickCheck
  ( Arbitrary (..),
  )
import Test.Utils.Platform
  ( pendingOnMacOS,
  )
import Prelude

spec :: Spec
spec =
  before (pendingOnMacOS "#2472: timeouts in hydra mac builds") $
    before (PureLayer.newDBLayer @IO @(SeqState 'Mainnet ShelleyKey) ti) $
      describe "PureLayer" properties
  where
    ti = dummyTimeInterpreter

newtype DummyStatePureLayer = DummyStatePureLayer Int
  deriving (Show, Eq)

instance Arbitrary DummyStatePureLayer where
  shrink _ = []
  arbitrary = DummyStatePureLayer <$> arbitrary

deriving instance NFData DummyStatePureLayer

instance IsOurs DummyStatePureLayer Address where
  isOurs _ num = (Nothing, num)
