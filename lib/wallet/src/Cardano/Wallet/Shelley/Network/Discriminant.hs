{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Wallet.Shelley.Network.Discriminant
  ( SomeNetworkDiscriminant (..),
    networkDiscriminantToId,
  )
where

import Cardano.Wallet.Api.Types
  ( DecodeAddress,
    DecodeStakeAddress,
    EncodeAddress,
    EncodeStakeAddress,
  )
import Cardano.Wallet.Primitive.AddressDerivation
  ( DelegationAddress,
    Depth (..),
    NetworkDiscriminant,
    NetworkDiscriminantVal,
    PaymentAddress,
  )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
  ( ByronKey,
  )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
  ( IcarusKey,
  )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
  ( ShelleyKey,
  )
import Cardano.Wallet.Shelley.Compatibility
  ( HasNetworkId (..),
    NetworkId,
  )
import Data.Proxy
  ( Proxy,
  )
import Data.Typeable
  ( Typeable,
  )
import Prelude

-- | Encapsulate a network discriminant and the necessary constraints it should
-- satisfy.
data SomeNetworkDiscriminant where
  SomeNetworkDiscriminant ::
    forall (n :: NetworkDiscriminant).
    ( NetworkDiscriminantVal n,
      PaymentAddress n IcarusKey 'CredFromKeyK,
      PaymentAddress n ByronKey 'CredFromKeyK,
      PaymentAddress n ShelleyKey 'CredFromKeyK,
      DelegationAddress n ShelleyKey 'CredFromKeyK,
      HasNetworkId n,
      DecodeAddress n,
      EncodeAddress n,
      DecodeStakeAddress n,
      EncodeStakeAddress n,
      Typeable n
    ) =>
    Proxy n ->
    SomeNetworkDiscriminant

deriving instance Show SomeNetworkDiscriminant

networkDiscriminantToId :: SomeNetworkDiscriminant -> NetworkId
networkDiscriminantToId (SomeNetworkDiscriminant proxy) = networkIdVal proxy
