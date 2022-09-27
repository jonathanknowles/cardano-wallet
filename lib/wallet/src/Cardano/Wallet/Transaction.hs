{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An extra interface for operation on transactions (e.g. creating witnesses,
-- estimating size...). This makes it possible to decouple those operations from
-- our wallet layer, keeping the implementation flexible to various backends.
module Cardano.Wallet.Transaction
  ( -- * Interface
    TransactionLayer (..),
    DelegationAction (..),
    TransactionCtx (..),
    defaultTransactionCtx,
    Withdrawal (..),
    withdrawalToCoin,
    TxUpdate (..),
    TxFeeUpdate (..),
    TokenMapWithScripts (..),
    emptyTokenMapWithScripts,
    AnyScript (..),
    PlutusScriptInfo (..),
    PlutusVersion (..),
    TxFeeAndChange (..),
    mapTxFeeAndChange,
    ValidityIntervalExplicit (..),

    -- * Errors
    ErrSignTx (..),
    ErrMkTransaction (..),
    ErrCannotJoin (..),
    ErrCannotQuit (..),
    ErrUpdateSealedTx (..),
    ErrAssignRedeemers (..),
    ErrMoreSurplusNeeded (..),
  )
where

import Cardano.Address.Derivation
  ( XPrv,
    XPub,
  )
import Cardano.Address.Script
  ( KeyHash,
    Script,
  )
import Cardano.Api
  ( AnyCardanoEra,
  )
import qualified Cardano.Api as Cardano
import Cardano.Api.Extra
  (
  )
import qualified Cardano.Api.Shelley as Cardano
import Cardano.Ledger.Alonzo.TxInfo
  ( TranslationError (..),
  )
import Cardano.Ledger.Crypto
  ( StandardCrypto,
  )
import Cardano.Wallet.CoinSelection
  ( SelectionCollateralRequirement (..),
    SelectionLimit,
    SelectionOf (..),
    SelectionSkeleton,
  )
import Cardano.Wallet.Primitive.AddressDerivation
  ( Depth (..),
    DerivationIndex,
  )
import Cardano.Wallet.Primitive.Passphrase
  ( Passphrase,
  )
import Cardano.Wallet.Primitive.Slotting
  ( PastHorizonException,
    TimeInterpreter,
  )
import Cardano.Wallet.Primitive.Types
  ( Certificate,
    FeePolicy,
    PoolId,
    ProtocolParameters,
    SlotNo (..),
    TokenBundleMaxSize (..),
    WalletId,
  )
import Cardano.Wallet.Primitive.Types.Address
  ( Address (..),
  )
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (..),
  )
import Cardano.Wallet.Primitive.Types.Hash
  ( Hash,
  )
import Cardano.Wallet.Primitive.Types.Redeemer
  ( Redeemer,
  )
import Cardano.Wallet.Primitive.Types.RewardAccount
  ( RewardAccount,
  )
import Cardano.Wallet.Primitive.Types.TokenMap
  ( AssetId,
    TokenMap,
  )
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import Cardano.Wallet.Primitive.Types.TokenPolicy
  ( TokenPolicyId,
  )
import Cardano.Wallet.Primitive.Types.Tx
  ( Tx (..),
    TxIn,
    TxMetadata,
    TxOut,
  )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
  ( TokenBundleSizeAssessor,
    TxConstraints,
    TxSize,
  )
import Cardano.Wallet.Primitive.Types.UTxO
  ( UTxO,
  )
import Cardano.Wallet.Util
  ( ShowFmt (..),
  )
import Control.DeepSeq
  ( NFData (..),
  )
import Control.Monad
  ( (>=>),
  )
import Data.Aeson.Types
  ( FromJSON (..),
    Parser,
    ToJSON (..),
    camelTo2,
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor
  ( bimap,
  )
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.Map.Strict
  ( Map,
  )
import qualified Data.Map.Strict as Map
import Data.Quantity
  ( Quantity (..),
  )
import Data.Text
  ( Text,
  )
import Data.Text.Class
  ( FromText (..),
    TextDecodingError (..),
    ToText (..),
  )
import Data.Word
  ( Word64,
  )
import Fmt
  ( Buildable (..),
    genericF,
  )
import GHC.Generics
  ( Generic,
  )
import Prelude

data TransactionLayer k ktype tx = TransactionLayer
  { -- | Construct a standard transaction
    --
    -- " Standard " here refers to the fact that we do not deal with redemption,
    -- multisignature transactions, etc.
    --
    -- This expects as a first argument a mean to compute or lookup private
    -- key corresponding to a particular address.
    mkTransaction ::
      AnyCardanoEra ->
      -- Era for which the transaction should be created.
      (XPrv, Passphrase "encryption") ->
      -- Reward account
      (Address -> Maybe (k 'CredFromKeyK XPrv, Passphrase "encryption")) ->
      -- Key store
      ProtocolParameters ->
      -- Current protocol parameters
      TransactionCtx ->
      -- An additional context about the transaction
      SelectionOf TxOut ->
      -- A balanced coin selection where all change addresses have been
      -- assigned.
      Either ErrMkTransaction (Tx, tx),
    -- | Add Vk witnesses to a transaction for known inputs.
    --
    -- If inputs can't be resolved, they are simply skipped, hence why this
    -- function cannot fail.
    addVkWitnesses ::
      AnyCardanoEra ->
      -- Preferred latest era
      (XPrv, Passphrase "encryption") ->
      -- Reward account
      (KeyHash, XPrv, Passphrase "encryption") ->
      -- policy public and private key
      (Address -> Maybe (k ktype XPrv, Passphrase "encryption")) ->
      -- Key store / address resolution
      (TxIn -> Maybe Address) ->
      -- Input resolution
      tx ->
      -- The transaction to sign
      tx,
    -- | Construct a standard unsigned transaction
    --
    -- " Standard " here refers to the fact that we do not deal with redemption,
    -- multisignature transactions, etc.
    --
    -- The function returns CBOR-ed transaction body to be signed in another step.
    mkUnsignedTransaction ::
      AnyCardanoEra ->
      -- Era for which the transaction should be created.
      XPub ->
      -- Reward account public key
      ProtocolParameters ->
      -- Current protocol parameters
      TransactionCtx ->
      -- An additional context about the transaction
      SelectionOf TxOut ->
      -- A balanced coin selection where all change addresses have been
      -- assigned.
      Either ErrMkTransaction tx,
    -- | Compute a minimal fee amount necessary to pay for a given selection
    -- This also includes necessary deposits.
    calcMinimumCost ::
      AnyCardanoEra ->
      -- Era for which the transaction should be created.
      ProtocolParameters ->
      -- Current protocol parameters
      TransactionCtx ->
      -- Additional information about the transaction
      SelectionSkeleton ->
      -- An intermediate representation of an ongoing selection
      Coin,
    -- | Compute the maximum execution cost of scripts in a given transaction.
    maxScriptExecutionCost ::
      ProtocolParameters ->
      -- Current protocol parameters
      [Redeemer] ->
      -- Redeemers for this transaction
      Coin,
    -- | Evaluate a minimal fee amount necessary to pay for a given tx
    -- using ledger's functionality
    --
    -- Will estimate how many witnesses there /should be/, so it works even
    -- for unsigned transactions.
    evaluateMinimumFee ::
      forall era.
      Cardano.IsShelleyBasedEra era =>
      Cardano.ProtocolParameters ->
      -- Current protocol parameters
      Cardano.Tx era ->
      -- The sealed transaction
      Coin,
    -- | Estimate the size of the transaction when fully signed.
    estimateSignedTxSize ::
      forall era.
      Cardano.IsShelleyBasedEra era =>
      Cardano.ProtocolParameters ->
      Cardano.Tx era ->
      TxSize,
    -- | Evaluate the balance of a transaction using the ledger. The balance
    -- is defined as @(value consumed by transaction) - (value produced by
    -- transaction)@. For a transaction to be valid, it must have a balance
    -- of zero.
    --
    -- Note that the fee-field of the transaction affects the balance, and
    -- is not automatically the minimum fee.
    --
    -- The function takes two UTxOs of different types and merges them. The
    -- reason is to workaround that wallet 'UTxO' type doesn't support
    -- Datum hashes
    evaluateTransactionBalance ::
      forall era.
      Cardano.IsShelleyBasedEra era =>
      Cardano.Tx era ->
      Cardano.ProtocolParameters ->
      Cardano.UTxO era ->
      Cardano.Value,
    -- | Distributes a surplus transaction balance between the given change
    -- outputs and the given fee. This function is aware of the fact that
    -- any increase in a 'Coin' value could increase the size and fee
    -- requirement of a transaction.
    --
    -- When comparing the original fee and change outputs to the adjusted
    -- fee and change outputs, this function guarantees that:
    --
    --    - The number of the change outputs remains constant;
    --
    --    - The fee quantity either remains the same or increases.
    --
    --    - For each change output:
    --        - the ada quantity either remains constant or increases.
    --        - non-ada quantities remain the same.
    --
    --    - The surplus is conserved:
    --        The total increase in the fee and change ada quantities is
    --        exactly equal to the surplus.
    --
    --    - Any increase in cost is covered:
    --        If the total cost has increased by 𝛿c, then the fee value
    --        will have increased by at least 𝛿c.
    --
    -- If the cost of distributing the provided surplus is greater than the
    -- surplus itself, the function will return 'ErrMoreSurplusNeeded'. If
    -- the provided surplus is greater or equal to
    -- @maximumCostOfIncreasingCoin feePolicy@, the function will always
    -- return 'Right'.
    distributeSurplus ::
      FeePolicy ->
      Coin ->
      -- Surplus transaction balance to distribute.
      TxFeeAndChange [TxOut] ->
      -- Original fee and change outputs.
      Either ErrMoreSurplusNeeded (TxFeeAndChange [TxOut]),
    --

    computeSelectionLimit ::
      AnyCardanoEra ->
      ProtocolParameters ->
      TransactionCtx ->
      [TxOut] ->
      SelectionLimit,
    -- | A function to assess the size of a token bundle.
    tokenBundleSizeAssessor ::
      TokenBundleMaxSize ->
      TokenBundleSizeAssessor,
    constraints ::
      AnyCardanoEra ->
      -- Era for which the transaction should be created.
      ProtocolParameters ->
      -- Current protocol parameters.
      TxConstraints,
    -- The set of constraints that apply to all transactions.

    -- | Decode an externally-created transaction.
    decodeTx ::
      AnyCardanoEra ->
      tx ->
      ( Tx,
        TokenMapWithScripts,
        TokenMapWithScripts,
        [Certificate],
        Maybe ValidityIntervalExplicit
      ),
    -- | Update tx by adding additional inputs and outputs
    updateTx ::
      forall era.
      Cardano.IsShelleyBasedEra era =>
      Cardano.Tx era ->
      TxUpdate ->
      Either ErrUpdateSealedTx (Cardano.Tx era),
    assignScriptRedeemers ::
      forall era.
      Cardano.IsShelleyBasedEra era =>
      Cardano.ProtocolParameters ->
      -- Current protocol parameters
      TimeInterpreter (Either PastHorizonException) ->
      Cardano.UTxO era ->
      [Redeemer] ->
      -- A list of redeemers to set on the transaction.
      (Cardano.Tx era) ->
      -- Transaction containing scripts
      (Either ErrAssignRedeemers (Cardano.Tx era)),
    -- | Temporary hack to allow access to conversion in balanceTransaction
    toCardanoUTxO ::
      forall era.
      Cardano.IsShelleyBasedEra era =>
      UTxO ->
      [(TxIn, TxOut, Maybe (Hash "Datum"))] ->
      Cardano.UTxO era,
    -- | Temporary hack to allow access to conversion in balanceTransaction
    fromCardanoTxIn ::
      Cardano.TxIn ->
      TxIn,
    -- | Temporary hack to allow access to conversion in balanceTransaction
    fromCardanoTxOut ::
      forall era ctx.
      Cardano.IsCardanoEra era =>
      Cardano.TxOut ctx era ->
      TxOut
  }

-- | Method to use when updating the fee of a transaction.
data TxFeeUpdate
  = -- | Instead of updating the fee, just use the old fee of the
    -- Tx (no-op for fee update).
    UseOldTxFee
  | -- | Specify a new fee to use instead.
    UseNewTxFee Coin
  deriving (Eq, Show)

-- | Describes modifications that can be made to a `Tx` using `updateTx`.
data TxUpdate = TxUpdate
  { extraInputs :: [(TxIn, TxOut)],
    -- | Only used in the Alonzo era and later. Will be silently ignored in
    -- previous eras.
    extraCollateral :: [TxIn],
    extraOutputs :: [TxOut],
    -- | Set a new fee or use the old one.
    feeUpdate :: TxFeeUpdate
  }

-- | Some additional context about a transaction. This typically contains
-- details that are known upfront about the transaction and are used to
-- construct it from inputs selected from the wallet's UTxO.
data TransactionCtx = TransactionCtx
  { -- | Withdrawal amount from a reward account, can be zero.
    txWithdrawal :: Withdrawal,
    -- | User or application-defined metadata to embed in the transaction.
    txMetadata :: Maybe TxMetadata,
    -- | Transaction optional starting slot and expiry (TTL) slot for which the
    -- transaction is valid.
    txValidityInterval :: (Maybe SlotNo, SlotNo),
    -- | An additional delegation to take.
    txDelegationAction :: Maybe DelegationAction,
    -- | Total execution cost of plutus scripts, determined by their execution units
    -- and prices obtained from network.
    txPlutusScriptExecutionCost :: Coin,
    -- | The assets to mint.
    txAssetsToMint :: (TokenMap, Map AssetId (Script KeyHash)),
    -- | The assets to burn.
    txAssetsToBurn :: (TokenMap, Map AssetId (Script KeyHash)),
    -- | A map of script hashes related to inputs. Only for multisig wallets
    txNativeScriptInputs :: Map TxIn (Script KeyHash),
    -- | The collateral requirement.
    txCollateralRequirement :: SelectionCollateralRequirement,
    -- | Extra fees. Some parts of a transaction are not representable using
    -- cardano-wallet types, which makes it useful to account for them like
    -- this. For instance: datums.
    txFeePadding :: !Coin
  }
  deriving (Show, Generic, Eq)

data Withdrawal
  = WithdrawalSelf RewardAccount (NonEmpty DerivationIndex) Coin
  | WithdrawalExternal RewardAccount (NonEmpty DerivationIndex) Coin
  | NoWithdrawal
  deriving (Show, Eq)

withdrawalToCoin :: Withdrawal -> Coin
withdrawalToCoin = \case
  WithdrawalSelf _ _ c -> c
  WithdrawalExternal _ _ c -> c
  NoWithdrawal -> Coin 0

-- | A default context with sensible placeholder. Can be used to reduce
-- repetition for changing only sub-part of the default context.
defaultTransactionCtx :: TransactionCtx
defaultTransactionCtx =
  TransactionCtx
    { txWithdrawal = NoWithdrawal,
      txMetadata = Nothing,
      txValidityInterval = (Nothing, maxBound),
      txDelegationAction = Nothing,
      txPlutusScriptExecutionCost = Coin 0,
      txAssetsToMint = (TokenMap.empty, Map.empty),
      txAssetsToBurn = (TokenMap.empty, Map.empty),
      txNativeScriptInputs = Map.empty,
      txCollateralRequirement = SelectionCollateralNotRequired,
      txFeePadding = Coin 0
    }

-- | Whether the user is attempting any particular delegation action.
data DelegationAction = RegisterKeyAndJoin PoolId | Join PoolId | Quit
  deriving (Show, Eq, Generic)

instance Buildable DelegationAction where
  build = genericF

data PlutusVersion
  = PlutusVersionV1
  | PlutusVersionV2
  deriving (Eq, Generic, Show)
  deriving anyclass (NFData)

instance ToText PlutusVersion where
  toText PlutusVersionV1 = "v1"
  toText PlutusVersionV2 = "v2"

instance FromText PlutusVersion where
  fromText txt = case txt of
    "v1" -> Right PlutusVersionV1
    "v2" -> Right PlutusVersionV2
    _ ->
      Left $
        TextDecodingError $
          unwords
            [ "I couldn't parse the given plutus version.",
              "I am expecting one of the words 'v1' or",
              "'v2'."
            ]

newtype PlutusScriptInfo = PlutusScriptInfo
  { languageVersion :: PlutusVersion
  }
  deriving (Eq, Generic, Show)
  deriving anyclass (NFData)

instance FromJSON PlutusScriptInfo where
  parseJSON =
    parseJSON
      >=> eitherToParser . bimap ShowFmt PlutusScriptInfo . fromText
    where
      eitherToParser :: Show s => Either s a -> Parser a
      eitherToParser = either (fail . show) pure

instance ToJSON PlutusScriptInfo where
  toJSON (PlutusScriptInfo v) = toJSON $ toText v

data AnyScript
  = NativeScript !(Script KeyHash)
  | PlutusScript !PlutusScriptInfo
  deriving (Eq, Generic, Show)
  deriving anyclass (NFData)

data TokenMapWithScripts = TokenMapWithScripts
  { txTokenMap :: !TokenMap,
    txScripts :: !(Map TokenPolicyId AnyScript)
  }
  deriving (Show, Generic, Eq)

emptyTokenMapWithScripts :: TokenMapWithScripts
emptyTokenMapWithScripts =
  TokenMapWithScripts
    { txTokenMap = mempty,
      txScripts = Map.empty
    }

data ErrMkTransaction
  = ErrMkTransactionNoSuchWallet WalletId
  | -- | We failed to construct a transaction for some reasons.
    ErrMkTransactionTxBodyError Text
  | -- | Should never happen, means that that we have programmatically provided
    -- an invalid era.
    ErrMkTransactionInvalidEra AnyCardanoEra
  | ErrMkTransactionJoinStakePool ErrCannotJoin
  | ErrMkTransactionQuitStakePool ErrCannotQuit
  | ErrMkTransactionIncorrectTTL PastHorizonException
  deriving (Generic, Eq, Show)

data ErrAssignRedeemers
  = ErrAssignRedeemersScriptFailure Redeemer String
  | -- | The given redeemer target couldn't be located in the transaction.
    ErrAssignRedeemersTargetNotFound Redeemer
  | -- | Redeemer's data isn't a valid Plutus' data.
    ErrAssignRedeemersInvalidData Redeemer String
  | ErrAssignRedeemersTranslationError (TranslationError StandardCrypto)
  deriving (Generic, Eq, Show)

-- | Possible signing error
data ErrSignTx
  = -- | We tried to sign a transaction with inputs that are unknown to us?
    ErrSignTxAddressUnknown TxIn
  | -- | TODO: [ADP-919] Remove ErrSignTxUnimplemented
    ErrSignTxUnimplemented
  deriving (Generic, Eq, Show)

data ErrCannotJoin
  = ErrAlreadyDelegating PoolId
  | ErrNoSuchPool PoolId
  deriving (Generic, Eq, Show)

data ErrCannotQuit
  = ErrNotDelegatingOrAboutTo
  | ErrNonNullRewards Coin
  deriving (Eq, Show)

newtype ErrUpdateSealedTx
  = -- | The `SealedTx` couldn't not be updated because the *n* existing
    -- key-witnesses would have been rendered invalid.
    ErrExistingKeyWitnesses Int
  deriving (Generic, Eq, Show)

-- | Error for when its impossible for 'distributeSurplus' to distribute the
-- surplus. As long as the surplus is larger than 'costOfIncreasingCoin', this
-- should never happen.
newtype ErrMoreSurplusNeeded = ErrMoreSurplusNeeded Coin
  deriving (Generic, Eq, Show)

-- | Small helper record to disambiguate between a fee and change Coin values.
-- Used by 'distributeSurplus'.
data TxFeeAndChange change = TxFeeAndChange
  { fee :: Coin,
    change :: change
  }
  deriving (Eq, Show)

-- | Manipulates a 'TxFeeAndChange' value.
mapTxFeeAndChange ::
  -- | A function to transform the fee
  (Coin -> Coin) ->
  -- | A function to transform the change
  (change1 -> change2) ->
  -- | The original fee and change
  TxFeeAndChange change1 ->
  -- | The transformed fee and change
  TxFeeAndChange change2
mapTxFeeAndChange mapFee mapChange TxFeeAndChange {fee, change} =
  TxFeeAndChange (mapFee fee) (mapChange change)

data ValidityIntervalExplicit = ValidityIntervalExplicit
  { invalidBefore :: !(Quantity "slot" Word64),
    invalidHereafter :: !(Quantity "slot" Word64)
  }
  deriving (Generic, Eq, Show)
  deriving anyclass (NFData)

instance ToJSON ValidityIntervalExplicit where
  toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ValidityIntervalExplicit where
  parseJSON = genericParseJSON defaultRecordTypeOptions

defaultRecordTypeOptions :: Aeson.Options
defaultRecordTypeOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = camelTo2 '_' . dropWhile (== '_'),
      Aeson.omitNothingFields = True
    }
