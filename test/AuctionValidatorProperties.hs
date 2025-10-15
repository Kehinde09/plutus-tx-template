{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Value
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified Prelude as Haskell
import qualified PlutusTx

import qualified AuctionMintingPolicy as AMP

-- Helper to create a dummy PubKeyHash from a hex string
dummyPubKeyHash :: PubKeyHash
dummyPubKeyHash = PubKeyHash "12345678901234567890123456789012345678901234567890"

-- Another dummy PubKeyHash for negative test
wrongPubKeyHash :: PubKeyHash
wrongPubKeyHash = PubKeyHash "99999999999999999999999999999999999999999999999999"

-- The CurrencySymbol and TokenName expected by the minting policy
currencySymbol :: CurrencySymbol
currencySymbol = "currencySymbol" -- Ideally, get it dynamically from the minting policy script ID

tokenName :: TokenName
tokenName = "tokenName"

-- Helper to create a ScriptContext for minting 'amount' tokens by 'pkh'
mockScriptContext :: Integer -> PubKeyHash -> ScriptContext
mockScriptContext amount pkh = ScriptContext txInfo purpose
  where
    txInfo :: TxInfo
    txInfo = TxInfo
        { txInfoInputs = []
        , txInfoOutputs = []
        , txInfoFee = mempty
        , txInfoMint = singleton currencySymbol tokenName amount
        , txInfoDCert = []
        , txInfoWdrl = mempty
        , txInfoValidRange = always
        , txInfoSignatories = [pkh]
        , txInfoData = []
        , txInfoId = "abcd1234"
        }

    purpose :: ScriptPurpose
    purpose = Minting currencySymbol

-- Helper to run the minting policy and catch errors
runMintingPolicy :: PubKeyHash -> () -> ScriptContext -> Haskell.IO (Either Haskell.SomeException ())
runMintingPolicy pkh redeemer ctx =
    Haskell.try $ pure $! AMP.auctionTypedMintingPolicy pkh redeemer ctx

-- Tests
main :: IO ()
main = hspec $ do
    describe "Auction Minting Policy" $ do
        it "allows minting exactly one token by the correct PubKeyHash" $ do
            let ctx = mockScriptContext 1 dummyPubKeyHash
            result <- runMintingPolicy dummyPubKeyHash () ctx
            result `shouldBe` Right ()

        it "rejects minting more than one token" $ do
            let ctx = mockScriptContext 2 dummyPubKeyHash
            result <- runMintingPolicy dummyPubKeyHash () ctx
            result `shouldSatisfy` either (const True) (const False)

        it "rejects minting if signed by the wrong PubKeyHash" $ do
            let ctx = mockScriptContext 1 wrongPubKeyHash
            result <- runMintingPolicy dummyPubKeyHash () ctx
            result `shouldSatisfy` either (const True) (const False)
