{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.Shelley.Network
  ( -- * Top-Level Interface
    withNetworkLayer,

    -- * Logging
    NetworkLayerLog (..),
  )
where

import Cardano.BM.Tracing
  ( HasPrivacyAnnotation,
    HasSeverityAnnotation (..),
    Tracer,
  )
import Cardano.Wallet.Network
  ( NetworkLayer,
  )
import Cardano.Wallet.Primitive.Types
  ( NetworkParameters,
  )
import Cardano.Wallet.Shelley.BlockchainSource
  ( BlockchainSource (..),
  )
import Cardano.Wallet.Shelley.Compatibility
  ( CardanoBlock,
    StandardCrypto,
  )
import qualified Cardano.Wallet.Shelley.Network.Blockfrost as Blockfrost
import Cardano.Wallet.Shelley.Network.Discriminant
  ( SomeNetworkDiscriminant,
  )
import qualified Cardano.Wallet.Shelley.Network.Node as Node
import Control.Monad.Trans.Cont
  ( ContT (ContT),
  )
import Data.Functor.Contravariant
  ( (>$<),
  )
import Data.Text.Class
  ( ToText (toText),
  )
import GHC.Stack
  ( HasCallStack,
  )
import Ouroboros.Network.Client.Wallet
  ( PipeliningStrategy,
  )
import Prelude

data NetworkLayerLog
  = NodeNetworkLog Node.Log
  | BlockfrostNetworkLog Blockfrost.Log

instance ToText NetworkLayerLog where
  toText = \case
    NodeNetworkLog l -> toText l
    BlockfrostNetworkLog l -> toText l

instance HasPrivacyAnnotation NetworkLayerLog

instance HasSeverityAnnotation NetworkLayerLog where
  getSeverityAnnotation = \case
    NodeNetworkLog l -> getSeverityAnnotation l
    BlockfrostNetworkLog l -> getSeverityAnnotation l

withNetworkLayer ::
  HasCallStack =>
  Tracer IO NetworkLayerLog ->
  PipeliningStrategy (CardanoBlock StandardCrypto) ->
  BlockchainSource ->
  SomeNetworkDiscriminant ->
  NetworkParameters ->
  ContT r IO (NetworkLayer IO (CardanoBlock StandardCrypto))
withNetworkLayer tr pipeliningStrategy blockchainSrc net netParams =
  ContT $ case blockchainSrc of
    NodeSource nodeConn ver tol ->
      let tr' = NodeNetworkLog >$< tr
       in Node.withNetworkLayer
            tr'
            pipeliningStrategy
            netParams
            nodeConn
            ver
            tol
    BlockfrostSource project ->
      let tr' = BlockfrostNetworkLog >$< tr
       in Blockfrost.withNetworkLayer tr' net netParams project
