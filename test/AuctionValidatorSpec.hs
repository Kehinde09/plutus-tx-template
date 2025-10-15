{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Test.Hspec
import           AuctionValidator

import           PlutusLedgerApi.V1.Crypto      (PubKeyHash (..))
import           PlutusLedgerApi.V1             (Lovelace (..))
import           PlutusLedgerApi.V1.Interval    (always)

import           PlutusLedgerApi.V2             ( CurrencySymbol (..)
                                               , TokenName      (..)
                                               , ScriptContext  (..)
                                               , TxInfo         (..)
                                               )
import           PlutusLedgerApi.V2.Contexts    ( ScriptPurpose (..)
                                               , TxOutRef       (..)
                                               , TxId           (..)
                                               , TxInInfo
                                               )
import qualified PlutusTx.AssocMap             as AssocMap

-- | A completely empty TxInfo / ScriptContext used only for unit tests.
mockScriptContext :: ScriptContext
mockScriptContext =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs           = []
          , txInfoReferenceInputs  = []
          , txInfoOutputs          = []
          , txInfoFee              = mempty
          , txInfoMint             = mempty
          , txInfoDCert            = []
          , txInfoWdrl             = AssocMap.empty
          , txInfoValidRange       = always
          , txInfoSignatories      = []
          , txInfoData             = AssocMap.empty
          , txInfoId               = TxId ""
          , txInfoRedeemers        = AssocMap.empty
          }
    , scriptContextPurpose = Spending (TxOutRef (TxId "") 0)
    }

main :: IO ()
main = hspec $ do
  describe "auctionTypedValidator" $ do
    it "rejects a new bid when the context has no outputs" $ do
      
      -- Auction parameters with dummy values
      let params = AuctionParams
            { apSeller         = PubKeyHash "12345678"
            , apCurrencySymbol = CurrencySymbol ""    -- dummy currency symbol
            , apTokenName      = TokenName "MY_TOKEN"
            , apMinBid         = Lovelace 100
            , apEndTime        = 1725227091000
            }
      
          -- Previous bid exists, but less than new bid
          previousBid = Just (Bid "addr" (PubKeyHash "oldBidder") (Lovelace 50))
          newBid      = Bid   "addr" (PubKeyHash "newBidder") (Lovelace 150)

          datum       = AuctionDatum previousBid
          redeemer    = NewBid newBid

          -- The mockScriptContext has no outputs, so the validator should reject this bid
          result = auctionTypedValidator params datum redeemer mockScriptContext
      
      result `shouldBe` False
