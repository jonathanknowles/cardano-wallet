module Cardano.Wallet.CoinSelection.Gen
  ( coarbitraryWalletUTxO,
    genWalletUTxO,
    genWalletUTxOFunction,
    genWalletUTxOLargeRange,
    shrinkWalletUTxO,
  )
where

import Cardano.Wallet.CoinSelection
  ( WalletUTxO (..),
  )
import Cardano.Wallet.Primitive.Types.Address.Gen
  ( genAddress,
    shrinkAddress,
  )
import Cardano.Wallet.Primitive.Types.Tx.Gen
  ( genTxIn,
    genTxInLargeRange,
    shrinkTxIn,
  )
import Generics.SOP
  ( NP (..),
  )
import Test.QuickCheck
  ( Gen,
    coarbitrary,
  )
import Test.QuickCheck.Extra
  ( genFunction,
    genSized2,
    genericRoundRobinShrink,
    (<:>),
    (<@>),
  )
import Prelude

--------------------------------------------------------------------------------
-- Wallet UTxO identifiers chosen according to the size parameter
--------------------------------------------------------------------------------

coarbitraryWalletUTxO :: WalletUTxO -> Gen a -> Gen a
coarbitraryWalletUTxO = coarbitrary . show

genWalletUTxO :: Gen WalletUTxO
genWalletUTxO = uncurry WalletUTxO <$> genSized2 genTxIn genAddress

shrinkWalletUTxO :: WalletUTxO -> [WalletUTxO]
shrinkWalletUTxO =
  genericRoundRobinShrink
    <@> shrinkTxIn
    <:> shrinkAddress
    <:> Nil

genWalletUTxOFunction :: Gen a -> Gen (WalletUTxO -> a)
genWalletUTxOFunction = genFunction coarbitraryWalletUTxO

--------------------------------------------------------------------------------
-- Wallet UTxO identifiers chosen from a large range
--------------------------------------------------------------------------------

genWalletUTxOLargeRange :: Gen WalletUTxO
genWalletUTxOLargeRange = WalletUTxO <$> genTxInLargeRange <*> genAddress
