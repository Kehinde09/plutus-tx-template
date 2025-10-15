{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module AuctionValidator where

import GHC.Generics (Generic)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash)
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValueOf, valueOf)
import PlutusLedgerApi.V2 (
  CurrencySymbol, Datum (..), OutputDatum (..),
  ScriptContext (..), TokenName, TxInfo (..), TxOut (..),
  from, to)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs)
import PlutusTx
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.List qualified as List

-- =======================================
-- Types
-- =======================================

data AuctionParams = AuctionParams
  { apSeller         :: PubKeyHash
  , apCurrencySymbol :: CurrencySymbol
  , apTokenName      :: TokenName
  , apMinBid         :: Lovelace
  , apEndTime        :: POSIXTime
  } deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''AuctionParams
PlutusTx.makeIsDataSchemaIndexed ''AuctionParams [('AuctionParams, 0)]

data Bid = Bid
  { bAddr   :: BuiltinByteString
  , bPkh    :: PubKeyHash
  , bAmount :: Lovelace
  } deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.deriveShow ''Bid
PlutusTx.makeIsDataSchemaIndexed ''Bid [('Bid, 0)]

instance Eq Bid where
  {-# INLINEABLE (==) #-}
  b1 == b2 = bPkh b1 == bPkh b2 && bAmount b1 == bAmount b2

newtype AuctionDatum = AuctionDatum { adHighestBid :: Maybe Bid }
  deriving stock (Generic)
  deriving newtype
    ( HasBlueprintDefinition
    , ToData, FromData, UnsafeFromData
    )

data AuctionRedeemer = NewBid Bid | Payout
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''AuctionRedeemer [('NewBid, 0), ('Payout, 1)]

-- =======================================
-- Core Validator Logic
-- =======================================

{-# INLINEABLE auctionTypedValidator #-}
auctionTypedValidator ::
  AuctionParams ->
  AuctionDatum ->
  AuctionRedeemer ->
  ScriptContext ->
  Bool
auctionTypedValidator params (AuctionDatum highestBid) redeemer ctx@(ScriptContext txInfo _) =
  case redeemer of
    NewBid bid -> all id
      [ sufficientBid bid
      , validBidTime
      , refundsPreviousHighestBid
      , correctOutput bid
      ]
    Payout -> all id
      [ validPayoutTime
      , sellerGetsHighestBid
      , highestBidderGetsAsset
      ]
  where
    -- Check if bid is high enough
    {-# INLINEABLE sufficientBid #-}
    sufficientBid :: Bid -> Bool
    sufficientBid (Bid _ _ amt) = case highestBid of
      Just (Bid _ _ prevAmt) -> amt > prevAmt
      Nothing                -> amt >= apMinBid params

    -- Bid must be before end time
    {-# INLINEABLE validBidTime #-}
    validBidTime :: Bool
    validBidTime = to (apEndTime params) `contains` txInfoValidRange txInfo

    -- Payout must be after auction ends
    {-# INLINEABLE validPayoutTime #-}
    validPayoutTime :: Bool
    validPayoutTime = from (apEndTime params) `contains` txInfoValidRange txInfo

    -- Refund previous highest bidder
    {-# INLINEABLE refundsPreviousHighestBid #-}
    refundsPreviousHighestBid :: Bool
    refundsPreviousHighestBid = case highestBid of
      Nothing -> True
      Just (Bid _ bidderPkh amt) ->
        any (\o -> toPubKeyHash (txOutAddress o) == Just bidderPkh &&
                   lovelaceValueOf (txOutValue o) == amt)
            (txInfoOutputs txInfo)

    -- Check that output contains new highest bid and correct asset
    {-# INLINEABLE correctOutput #-}
    correctOutput :: Bid -> Bool
    correctOutput bid = case getContinuingOutputs ctx of
      [o] ->
        let outValue = txOutValue o
            correctDatum = case txOutDatum o of
              OutputDatum (Datum d) -> case fromBuiltinData d of
                Just (AuctionDatum (Just bid')) -> traceIfFalse "Datum mismatch" (bid == bid')
                _ -> traceError "Invalid datum"
              _ -> traceError "Invalid datum format"
            correctValue =
              lovelaceValueOf outValue == bAmount bid &&
              valueOf outValue (apCurrencySymbol params) (apTokenName params) == 1
        in correctDatum && correctValue
      _ -> traceError "Expected one continuing output"

    -- Ensure seller receives the winning bid amount
    {-# INLINEABLE sellerGetsHighestBid #-}
    sellerGetsHighestBid :: Bool
    sellerGetsHighestBid = case highestBid of
      Nothing -> True
      Just (Bid _ _ amt) ->
        any (\o -> toPubKeyHash (txOutAddress o) == Just (apSeller params) &&
                   lovelaceValueOf (txOutValue o) == amt)
            (txInfoOutputs txInfo)

    -- Ensure highest bidder receives the asset (or seller if no bids)
    {-# INLINEABLE highestBidderGetsAsset #-}
    highestBidderGetsAsset :: Bool
    highestBidderGetsAsset =
      let recipient = maybe (apSeller params) bPkh highestBid
      in any (\o -> toPubKeyHash (txOutAddress o) == Just recipient &&
                    valueOf (txOutValue o) (apCurrencySymbol params) (apTokenName params) == 1)
             (txInfoOutputs txInfo)

-- =======================================
-- Untyped wrapper
-- =======================================

{-# INLINEABLE auctionUntypedValidator #-}
auctionUntypedValidator ::
  AuctionParams ->
  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
auctionUntypedValidator params d r c =
  check $ auctionTypedValidator
    params
    (unsafeFromBuiltinData d)
    (unsafeFromBuiltinData r)
    (unsafeFromBuiltinData c)

-- =======================================
-- Compiled Script
-- =======================================

auctionValidatorScript ::
  AuctionParams ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
auctionValidatorScript params =
  $$(compile [|| auctionUntypedValidator ||])
    `unsafeApplyCode` liftCode plcVersion100 params
