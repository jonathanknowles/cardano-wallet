{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Avoid restricted alias" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Simulation.Implementation.Internal where

import Simulation.Model.Basic
import Simulation.Model.Indexed

--------------------------------------------------------------------------------
-- Write
--------------------------------------------------------------------------------

import qualified Internal.Cardano.Write.Tx as Write
    ( IsRecentEra (recentEra)
    , RecentEra (RecentEraBabbage, RecentEraConway)
    , shelleyBasedEra
    )
import qualified Internal.Cardano.Write.Tx.Balance as Write
    ( ChangeAddressGen (..)
    , PartialTx (PartialTx)
    , UTxOIndex
    , balanceTransaction
    , constructUTxOIndex
    )
import qualified Internal.Cardano.Write.Tx.TimeTranslation as Write
    ( TimeTranslation
    , timeTranslationFromEpochInfo
    )
import qualified Internal.Cardano.Write.UTxOAssumptions as Write
    ( UTxOAssumptions (AllKeyPaymentCredentials)
    )

--------------------------------------------------------------------------------
-- CardanoApi
--------------------------------------------------------------------------------

import qualified Cardano.Api.Shelley as CardanoApi
    ( EpochNo (EpochNo)
    , ExecutionUnitPrices (ExecutionUnitPrices)
    , ExecutionUnits (ExecutionUnits)
    , Lovelace (Lovelace)
    , ProtocolParameters (..)
    , fromShelleyLovelace
    , toLedgerPParams
    )

--------------------------------------------------------------------------------
-- Ledger
--------------------------------------------------------------------------------

import qualified Cardano.Ledger.Address as Ledger
    ( Addr
    , Withdrawals (Withdrawals)
    )
import qualified Cardano.Ledger.Api as Ledger
    ( BabbageEra
    , ConwayEra
    )
import qualified Cardano.Ledger.Api.PParams as Ledger
    ( PParams
    )
import qualified Cardano.Ledger.Api.Tx as Ledger
    ( EraTx (Tx)
    , EraTxBody (TxBody)
    , EraTxOut (TxOut)
    )
import qualified Cardano.Ledger.BaseTypes as Ledger
    ( StrictMaybe (SNothing)
    , natVersion
    )
import qualified Cardano.Ledger.Binary as Ledger
    ( Sized (sizedValue)
    , mkSized
    )
import qualified Cardano.Ledger.Coin as Ledger
    ( Coin (Coin)
    )
import qualified Cardano.Ledger.Crypto as Ledger
    ( StandardCrypto
    )
import qualified Cardano.Ledger.TxIn as Ledger
    ( TxIn
    )
import qualified Cardano.Ledger.UTxO as Ledger
    ( UTxO (UTxO)
    )
import Test.Cardano.Ledger.Core.Arbitrary
    ()

--------------------------------------------------------------------------------
-- Ledger.Allegra
--------------------------------------------------------------------------------

import qualified Cardano.Ledger.Allegra.Scripts as Ledger.Allegra
    ( ValidityInterval (ValidityInterval, invalidBefore, invalidHereafter)
    )

--------------------------------------------------------------------------------
-- Ledger.Alonzo
--------------------------------------------------------------------------------

import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
    ( AlonzoTx (AlonzoTx, auxiliaryData, body, isValid, wits)
    )

--------------------------------------------------------------------------------
-- Ledger.Babbage
--------------------------------------------------------------------------------

import qualified Cardano.Ledger.Babbage.Core as Ledger.Babbage
    ( CoinPerByte (CoinPerByte, unCoinPerByte)
    )
import qualified Cardano.Ledger.Babbage.Tx as Ledger.Babbage
    ( IsValid (IsValid)
    )
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
    ( BabbageTxBody (..)
    , BabbageTxOut (BabbageTxOut)
    , Datum (NoDatum)
    )

--------------------------------------------------------------------------------
-- Ledger.Mary
--------------------------------------------------------------------------------

import qualified Cardano.Ledger.Mary.Value as Ledger.Mary
    ( AssetName (AssetName)
    , MaryValue (MaryValue)
    , PolicyID
    , flattenMultiAsset
    , valueFromList
    )
import Test.Cardano.Ledger.Mary.Arbitrary
    ()

--------------------------------------------------------------------------------
-- Slotting
--------------------------------------------------------------------------------

import qualified Cardano.Slotting.EpochInfo as Slotting
    ( fixedEpochInfo
    )
import qualified Cardano.Slotting.Slot as Slotting
    ( EpochSize (EpochSize)
    )
import qualified Cardano.Slotting.Time as Slotting
    ( SystemStart (SystemStart)
    , slotLengthFromSec
    )

--------------------------------------------------------------------------------
-- Standard imports
--------------------------------------------------------------------------------

import Prelude

import Control.Monad.Random.Class
    ( MonadRandom
    )
import Control.Monad.Trans.Except
    ( runExceptT
    )
import Data.Bag
    ( Count (Count)
    , (×)
    )
import Data.Bifunctor
    ( Bifunctor (bimap)
    )
import Data.Bimap
    ( Bimap
    )
import qualified Data.Bimap as Bimap
    ( fromList
    , lookup
    , lookupR
    )
import qualified Data.ByteString.Short as ByteString
    ( fromShort
    , toShort
    )
import qualified Data.Foldable as F
import Data.List.NonEmpty
    ( NonEmpty ((:|))
    )
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( mapMaybe
    )
import Data.Ratio
    ( (%)
    )
import qualified Data.Sequence.Strict as StrictSeq
    ( fromList
    )
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import Data.Text
    ( Text
    )
import qualified Data.Text.Encoding as Text
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import GHC.IsList
    ( IsList (fromList, toList)
    )
import qualified Internal.Cardano.Write.Tx.Balance
import Test.QuickCheck.Extra
    ( GenCount (GenCount)
    , GenSize (GenSize)
    , arbitrarySample
    )

--------------------------------------------------------------------------------
-- Type synonyms
--------------------------------------------------------------------------------

newtype AssetName = AssetName Int
    deriving stock (Eq, Ord, Show)

newtype PolicyId = PolicyId Int
    deriving stock (Eq, Ord, Show)

type LedgerAddress = Ledger.Addr Ledger.StandardCrypto
type LedgerAssetId = (LedgerPolicyId, LedgerAssetName)
type LedgerAssetName = Ledger.Mary.AssetName
type LedgerEraBabbage = Ledger.BabbageEra Ledger.StandardCrypto
type LedgerEraConway = Ledger.ConwayEra Ledger.StandardCrypto
type LedgerPolicyId = Ledger.Mary.PolicyID Ledger.StandardCrypto
type LedgerProtocolParameters = Ledger.PParams
type LedgerTx era = Ledger.Tx era
type LedgerTxBody era = Ledger.TxBody era
type LedgerTxIn = Ledger.TxIn Ledger.StandardCrypto
type LedgerTxOut era = Ledger.TxOut era
type LedgerUTxO era = Ledger.UTxO era
type LedgerValue = Ledger.Mary.MaryValue Ledger.StandardCrypto

--------------------------------------------------------------------------------
-- Dummy values
--------------------------------------------------------------------------------

testLedgerAddresses :: Set LedgerAddress
testLedgerAddresses = arbitrarySample (GenCount 10_000) (GenSize 10)

testLedgerAssetNames :: Set LedgerAssetName
testLedgerAssetNames = arbitrarySample (GenCount 10_000) (GenSize 10)

testLedgerPolicyIds :: Set LedgerPolicyId
testLedgerPolicyIds = arbitrarySample (GenCount 10_000) (GenSize 10)

testLedgerTxIns :: Set LedgerTxIn
testLedgerTxIns = arbitrarySample (GenCount 10_000) (GenSize 10)

--------------------------------------------------------------------------------
-- Bidirectional conversion maps
--------------------------------------------------------------------------------

ledgerAddressMap :: Bimap Address LedgerAddress
ledgerAddressMap =
    Bimap.fromList $ zip
        [AddressInternal, AddressExternal]
        (Set.toList testLedgerAddresses)

ledgerAssetNameMap :: Bimap AssetName LedgerAssetName
ledgerAssetNameMap =
    Bimap.fromList $ mkEntry <$> zip [0 ..] (Set.toList testLedgerAssetNames)
  where
    mkEntry (i, a) = (AssetName i, a)

ledgerPolicyIdMap :: Bimap PolicyId LedgerPolicyId
ledgerPolicyIdMap =
    Bimap.fromList $ mkEntry <$> zip [0 ..] (Set.toList testLedgerPolicyIds)
  where
    mkEntry (i, a) = (PolicyId i, a)

ledgerTxInMap :: Bimap TxIn LedgerTxIn
ledgerTxInMap =
    Bimap.fromList $ mkEntry <$> zip [0 ..] (Set.toList testLedgerTxIns)
  where
    mkEntry (i, a) = (TxIn i, a)

--------------------------------------------------------------------------------
-- Bidirectional conversion functions
--------------------------------------------------------------------------------

toLedgerAddress :: Address -> LedgerAddress
toLedgerAddress a
    = fromJust ("toLedgerAddress: " <> show a)
    . flip Bimap.lookup ledgerAddressMap
    $ a

fromLedgerAddress :: LedgerAddress -> Address
fromLedgerAddress
    = fromJust "fromLedgerAddress"
    . flip Bimap.lookupR ledgerAddressMap

toLedgerAssetId :: Text -> LedgerAssetId
toLedgerAssetId t = (policyId, assetName)
  where
    policyId = toLedgerPolicyId (PolicyId 0)
    assetName = Ledger.Mary.AssetName (ByteString.toShort (Text.encodeUtf8 t))

fromLedgerAssetId :: LedgerAssetId -> Text
fromLedgerAssetId (policyId, Ledger.Mary.AssetName assetName)
    | fromLedgerPolicyId policyId /= PolicyId 0 =
        error "fromLedgerAssetId: unexpected policy identifier"
    | otherwise =
        Text.decodeUtf8 (ByteString.fromShort assetName)

toLedgerAssetName :: AssetName -> LedgerAssetName
toLedgerAssetName
    = fromJust "toLedgerAssetName"
    . flip Bimap.lookup ledgerAssetNameMap

fromLedgerAssetName :: LedgerAssetName -> AssetName
fromLedgerAssetName
    = fromJust "fromLedgerAssetName"
    . flip Bimap.lookupR ledgerAssetNameMap

toLedgerPolicyId :: PolicyId -> LedgerPolicyId
toLedgerPolicyId
    = fromJust "toLedgerPolicyId"
    . flip Bimap.lookup ledgerPolicyIdMap

fromLedgerPolicyId :: LedgerPolicyId -> PolicyId
fromLedgerPolicyId
    = fromJust "fromLedgerPolicyId"
    . flip Bimap.lookupR ledgerPolicyIdMap

toLedgerTx :: forall era. Write.IsRecentEra era => IndexedTx -> LedgerTx era
toLedgerTx IndexedTx {inputs, outputs} =
    case Write.recentEra @era of
        Write.RecentEraBabbage -> Ledger.Alonzo.AlonzoTx
            { body = txBody
            , wits = mempty
            , isValid = Ledger.Babbage.IsValid True
            , auxiliaryData = Ledger.SNothing
            }
        _ -> undefined
  where
    txBody :: LedgerTxBody era
    txBody = case Write.recentEra @era of
        Write.RecentEraBabbage ->
            Ledger.Babbage.BabbageTxBody
                { btbInputs
                    = Set.map toLedgerTxIn $ Set.fromList inputs
                , btbOutputs
                    = StrictSeq.fromList
                    $ fmap
                        ( Ledger.mkSized (Ledger.natVersion @7)
                        . toLedgerTxOut
                        )
                        outputs
                , btbCerts = mempty
                , btbWithdrawals = Ledger.Withdrawals mempty
                , btbTxFee = mempty
                , btbValidityInterval = Ledger.Allegra.ValidityInterval
                    { invalidBefore = Ledger.SNothing
                    , invalidHereafter = Ledger.SNothing
                    }
                , btbUpdate = Ledger.SNothing
                , btbAuxDataHash = Ledger.SNothing
                , btbMint = mempty
                , btbCollateral = mempty
                , btbReferenceInputs = mempty
                , btbReqSignerHashes = mempty
                , btbScriptIntegrityHash = Ledger.SNothing
                , btbTxNetworkId = Ledger.SNothing
                , btbCollateralReturn = Ledger.SNothing
                , btbTotalCollateral = Ledger.SNothing
                }
        _ ->
            undefined

fromLedgerTx :: forall era. Write.IsRecentEra era => LedgerTx era -> IndexedTx
fromLedgerTx tx = case Write.recentEra @era of
    Write.RecentEraBabbage ->
        case tx of
            Ledger.Alonzo.AlonzoTx {body} ->
                case body of
                    Ledger.Babbage.BabbageTxBody
                        {btbInputs, btbOutputs, btbTxFee = Ledger.Coin f} ->
                            IndexedTx
                                { inputs = toList $
                                    Set.map fromLedgerTxIn btbInputs
                                , outputs
                                    = fromLedgerTxOut
                                    . Ledger.sizedValue <$> F.toList btbOutputs
                                , fee = [fromIntegral f × Lovelace]
                                }
    Write.RecentEraConway ->
        undefined

toLedgerTxIn :: TxIn -> LedgerTxIn
toLedgerTxIn
    = fromJust "toLedgerTxIn"
    . flip Bimap.lookup ledgerTxInMap

fromLedgerTxIn :: LedgerTxIn -> TxIn
fromLedgerTxIn
    = fromJust "fromLedgerTxIn"
    . flip Bimap.lookupR ledgerTxInMap

toLedgerTxOut :: forall era. Write.IsRecentEra era => TxOut -> LedgerTxOut era
toLedgerTxOut (TxOut address value) = case Write.recentEra @era of
    Write.RecentEraBabbage ->
        Ledger.Babbage.BabbageTxOut
            (toLedgerAddress address)
            (toLedgerValue value)
            Ledger.Babbage.NoDatum
            Ledger.SNothing
    _ ->
        undefined

fromLedgerTxOut :: forall era. Write.IsRecentEra era => LedgerTxOut era -> TxOut
fromLedgerTxOut txOut = case Write.recentEra @era of
    Write.RecentEraBabbage ->
        case txOut of
            Ledger.Babbage.BabbageTxOut address value _ _ ->
                TxOut
                    (fromLedgerAddress address)
                    (fromLedgerValue value)
    Write.RecentEraConway ->
        undefined

toLedgerUTxO :: Write.IsRecentEra era => UTxO -> LedgerUTxO era
toLedgerUTxO (UTxO m) =
    Ledger.UTxO $ Map.mapKeys toLedgerTxIn $ Map.map toLedgerTxOut m

toLedgerValue :: Value -> LedgerValue
toLedgerValue v =
    Ledger.Mary.valueFromList
        (Ledger.Coin lovelaceQuantity)
        (transform <$> assetQuantities)
  where
    assetQuantities :: [(Text, Integer)]
    assetQuantities = mapMaybe getAssetQuantity (toList v)

    lovelaceQuantity :: Integer
    lovelaceQuantity =
        sum (mapMaybe getLovelaceQuantity (toList v))

    getAssetQuantity = \case
        (Count _ Lovelace) -> Nothing
        (Count q (Asset i)) -> Just (i, fromIntegral q)

    getLovelaceQuantity = \case
        (Count q Lovelace) -> Just (fromIntegral q)
        (Count _ (Asset _)) -> Nothing

    transform
        :: (Text, Integer)
        -> (LedgerPolicyId, LedgerAssetName, Integer)
    transform (assetId, v') = (ledgerPolicyId, ledgerAssetName, v')
      where
        (ledgerPolicyId, ledgerAssetName) = toLedgerAssetId assetId

fromLedgerValue :: LedgerValue -> Value
fromLedgerValue
    (Ledger.Mary.MaryValue (Ledger.Coin i) m) =
        fromList
            $ (fromIntegral i × Lovelace)
            : (transform <$> Ledger.Mary.flattenMultiAsset m)
  where
    transform
        :: (LedgerPolicyId, LedgerAssetName, Integer)
        -> (Count Asset)
    transform (ledgerPolicyId, ledgerAssetName, v) =
        fromIntegral v
        ×
        Asset (fromLedgerAssetId (ledgerPolicyId, ledgerAssetName))

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

testUTxOAssumptions :: Write.UTxOAssumptions
testUTxOAssumptions = Write.AllKeyPaymentCredentials

testLedgerProtocolParametersBabbage :: LedgerProtocolParameters LedgerEraBabbage
testLedgerProtocolParametersBabbage = testLedgerProtocolParameters

testLedgerProtocolParameters
    :: forall era. Write.IsRecentEra era => LedgerProtocolParameters era
testLedgerProtocolParameters =
    either (error . show) id $
        CardanoApi.toLedgerPParams
            (Write.shelleyBasedEra @era)
            testCardanoApiProtocolParameters

testCardanoApiProtocolParameters :: CardanoApi.ProtocolParameters
testCardanoApiProtocolParameters = CardanoApi.ProtocolParameters
    { CardanoApi.protocolParamTxFeeFixed = 155_381
    , CardanoApi.protocolParamTxFeePerByte = 44
    , CardanoApi.protocolParamMaxTxSize = 16_384
    , CardanoApi.protocolParamMinUTxOValue = Nothing
    , CardanoApi.protocolParamMaxTxExUnits =
        Just $ CardanoApi.ExecutionUnits 10_000_000_000 14_000_000
    , CardanoApi.protocolParamMaxValueSize = Just 4_000
    , CardanoApi.protocolParamProtocolVersion = (6, 0)
    , CardanoApi.protocolParamDecentralization = Just 0
    , CardanoApi.protocolParamExtraPraosEntropy = Nothing
    , CardanoApi.protocolParamMaxBlockHeaderSize = 100_000
    , CardanoApi.protocolParamMaxBlockBodySize = 100_000
    , CardanoApi.protocolParamStakeAddressDeposit =
        CardanoApi.Lovelace 2_000_000
    , CardanoApi.protocolParamStakePoolDeposit =
        CardanoApi.Lovelace 500_000_000
    , CardanoApi.protocolParamMinPoolCost =
        CardanoApi.Lovelace 32_000_000
    , CardanoApi.protocolParamPoolRetireMaxEpoch = CardanoApi.EpochNo 2
    , CardanoApi.protocolParamStakePoolTargetNum = 100
    , CardanoApi.protocolParamPoolPledgeInfluence = 0
    , CardanoApi.protocolParamMonetaryExpansion = 0
    , CardanoApi.protocolParamTreasuryCut  = 0
    , CardanoApi.protocolParamUTxOCostPerByte =
        Just $ CardanoApi.fromShelleyLovelace $
            Ledger.Babbage.unCoinPerByte testParameterCoinsPerUTxOByteBabbage
    , CardanoApi.protocolParamCostModels = mempty
    , CardanoApi.protocolParamPrices =
        Just $ CardanoApi.ExecutionUnitPrices (721 % 10_000_000) (577 % 10_000)
    , CardanoApi.protocolParamMaxBlockExUnits =
        Just $ CardanoApi.ExecutionUnits 10_000_000_000 14_000_000
    , CardanoApi.protocolParamCollateralPercent = Just 150
    , CardanoApi.protocolParamMaxCollateralInputs = Just 3
    }

testParameterCoinsPerUTxOByteBabbage :: Ledger.Babbage.CoinPerByte
testParameterCoinsPerUTxOByteBabbage
    = Ledger.Babbage.CoinPerByte $ Ledger.Coin 4_310

testTimeTranslation :: Write.TimeTranslation
testTimeTranslation =
    Write.timeTranslationFromEpochInfo
        (Slotting.SystemStart (posixSecondsToUTCTime 0))
        (Slotting.fixedEpochInfo
            (Slotting.EpochSize 21_600)
            (Slotting.slotLengthFromSec 1))

--------------------------------------------------------------------------------
-- Transaction balancing
--------------------------------------------------------------------------------

txBalancer :: MonadRandom m => TxBalancer m
txBalancer = indexedTxBalancerToTxBalancer indexedTxBalancer

indexedTxBalancer :: forall m. MonadRandom m => IndexedTxBalancer m
indexedTxBalancer = IndexedTxBalancer {balanceIndexedTx}
  where
    balanceIndexedTx :: UTxO -> IndexedTx -> m (Either BalanceTxError IndexedTx)
    balanceIndexedTx utxo tx =
        fmap (bimap (BalanceTxError . show) (fromLedgerTx . fst)) $
        runExceptT $
        Write.balanceTransaction
            testLedgerProtocolParametersBabbage
            testTimeTranslation
            testUTxOAssumptions
            utxoIndex
            changeAddressGen
            changeAddresses
            partialTx
      where
        changeAddresses =
            AddressInternal :| repeat AddressInternal

        changeAddressGen = Write.ChangeAddressGen
            { genChangeAddress, maxLengthChangeAddress }

        genChangeAddress
            :: NonEmpty Address
            -> (LedgerAddress, NonEmpty Address)
        genChangeAddress = \case
            a :| (b : bs) -> (toLedgerAddress a, b :| bs        )
            a :| []       -> (toLedgerAddress a, changeAddresses)

        maxLengthChangeAddress :: LedgerAddress
        maxLengthChangeAddress = toLedgerAddress AddressInternal

        partialTx :: Write.PartialTx LedgerEraBabbage
        partialTx = ledgerTxToPartialTx . toLedgerTx $ tx

        utxoIndex :: Write.UTxOIndex LedgerEraBabbage
        utxoIndex = Write.constructUTxOIndex (toLedgerUTxO utxo)

ledgerTxToPartialTx
    :: Write.IsRecentEra era => LedgerTx era -> Write.PartialTx era
ledgerTxToPartialTx tx =
    Write.PartialTx
        { tx
        , inputs = mempty
        , redeemers = mempty
        , timelockKeyWitnessCounts = mempty
        }

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

fromJust :: String -> Maybe a -> a
fromJust note = \case
    Just a -> a
    Nothing -> error $ "failed lookup: " <> note
