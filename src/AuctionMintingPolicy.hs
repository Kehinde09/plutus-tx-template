{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module AuctionMintingPolicy where

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V2 (PubKeyHash, ScriptContext(..), TxInfo(..))
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol, txSignedBy)
import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

--------------------------------------------------------------------------------
-- | Types
--------------------------------------------------------------------------------

type AuctionMintingParams   = PubKeyHash
type AuctionMintingRedeemer = ()

--------------------------------------------------------------------------------
-- | Typed minting policy
--------------------------------------------------------------------------------

{-# INLINEABLE auctionTypedMintingPolicy #-}
auctionTypedMintingPolicy ::
  AuctionMintingParams ->             -- ^ The required PubKeyHash (e.g. owner's)
  AuctionMintingRedeemer ->          -- ^ Unused redeemer
  ScriptContext ->                   -- ^ Transaction context
  Bool
auctionTypedMintingPolicy pkh _ ctx =
  txSignedBy info pkh PlutusTx.&& mintedExactlyOneToken
  where
    info = scriptContextTxInfo ctx

    -- | Ensure exactly one token is minted with the expected currency symbol
    mintedExactlyOneToken = case flattenValue (txInfoMint info) of
      [(cs, _tn, q)] ->
        cs PlutusTx.== ownCurrencySymbol ctx PlutusTx.&& q PlutusTx.== 1
      _ -> False

--------------------------------------------------------------------------------
-- | Untyped wrapper for the minting policy
--------------------------------------------------------------------------------

{-# INLINEABLE auctionUntypedMintingPolicy #-}
auctionUntypedMintingPolicy ::
  AuctionMintingParams ->          -- ^ Parameters (PubKeyHash)
  BuiltinData ->                   -- ^ Redeemer (ignored here)
  BuiltinData ->                   -- ^ ScriptContext
  PlutusTx.BuiltinUnit
auctionUntypedMintingPolicy pkh redeemer ctx =
  PlutusTx.check $
    auctionTypedMintingPolicy
      pkh
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (PlutusTx.unsafeFromBuiltinData ctx)

--------------------------------------------------------------------------------
-- | Compiled minting policy
--------------------------------------------------------------------------------

auctionMintingPolicyScript ::
  AuctionMintingParams ->
  CompiledCode (BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
auctionMintingPolicyScript pkh =
  $$(PlutusTx.compile [|| \pkh' -> auctionUntypedMintingPolicy pkh' ||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 pkh
