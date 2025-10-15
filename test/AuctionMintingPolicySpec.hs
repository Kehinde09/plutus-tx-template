{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Hspec
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Crypto
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified Prelude as Haskell

import qualified PlutusTx
import qualified AuctionMintingPolicy as AMP

-- Helper function to create a dummy PubKeyHash
dummyPubKeyHash :: PubKeyHash
dummyPubKeyHash = PubKeyHash "12345678901234567890123456789012345678901234567890"

-- Mock a ScriptContext that allows minting exactly 1 token with correct PubKeyHash
mockScriptContext :: Integer -> PubKeyHash -> ScriptContext
mockScriptContext amount pkh = ScriptContext txInfo purpose
  where
    txInfo :: TxInfo
    txInfo = TxInfo
        { txInfoInputs = []
        , txInfoOutputs = []
        , txInfoFee = mempty
        , txInfoMint = singleton "currencySymbol" "tokenName" amount
        , txInfoDCert = []
        , txInfoWdrl = mempty
        , txInfoValidRange = always
        , txInfoSignatories = [pkh]
        , txInfoData = []
        , txInfoId = "abcd1234"
        }

    purpose :: ScriptPurpose
    purpose = Minting "currencySymbol"

-- Compile and run tests
main :: IO ()
main = hspec $ do
    describe "Auction Minting Policy" $ do
        it "should allow minting exactly one token by the correct PubKeyHash" $ do
            let redeemer = ()
            let ctx = mockScriptContext 1 dummyPubKeyHash
            AMP.auctionTypedMintingPolicy dummyPubKeyHash redeemer ctx `shouldBe` ()

        it "should reject minting more than one token" $ do
            let ctx = mockScriptContext 2 dummyPubKeyHash
            AMP.auctionTypedMintingPolicy dummyPubKeyHash () ctx `shouldThrow` anyException

        it "should reject if signed by the wrong PubKeyHash" $ do
            let wrongPKH = PubKeyHash "99999999999999999999999999999999999999999999999999"
            let ctx = mockScriptContext 1 wrongPKH
            AMP.auctionTypedMintingPolicy dummyPubKeyHash () ctx `shouldThrow` anyException
