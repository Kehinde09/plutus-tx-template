{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

-- Internal modules
import           AuctionValidator  -- Assumes this defines AuctionParams, auctionValidatorScript, etc.

-- Standard libraries
import           System.Environment (getArgs)
import           qualified Data.Set as Set
import           qualified Data.ByteString.Short as Short

-- Plutus libraries
import           PlutusLedgerApi.Common          (serialiseCompiledCode)
import qualified PlutusLedgerApi.V1.Crypto       as Crypto
import qualified PlutusLedgerApi.V1.Time         as Time
import qualified PlutusLedgerApi.V1.Value        as Value
import           PlutusTx.Blueprint
import           PlutusTx.Builtins.HasOpaque     (stringToBuiltinByteStringHex)

--------------------------------------------------------------------------------
-- Dummy values for development. Replace with actual values before deployment.
--------------------------------------------------------------------------------

dummyHex28 :: BuiltinByteString
dummyHex28 = stringToBuiltinByteStringHex
  "000000000000000000000000000000000000000000000000"

dummyHex40 :: BuiltinByteString
dummyHex40 = stringToBuiltinByteStringHex
  "00000000000000000000000000000000000000000000000000000000"

--------------------------------------------------------------------------------
-- Auction Parameters
--------------------------------------------------------------------------------

auctionParams :: AuctionParams
auctionParams = AuctionParams
  { apSeller         = Crypto.PubKeyHash dummyHex28
  , apCurrencySymbol = Value.CurrencySymbol dummyHex40
  , apTokenName      = Value.tokenName "MY_TOKEN"
  , apMinBid         = 100  -- In Lovelace
  , apEndTime        = Time.fromMilliSeconds 1_725_227_091_000  -- Example UNIX timestamp
  }

--------------------------------------------------------------------------------
-- Contract Blueprint
--------------------------------------------------------------------------------

myContractBlueprint :: ContractBlueprint
myContractBlueprint = MkContractBlueprint
  { contractId          = Just "auction-validator"
  , contractPreamble    = myPreamble
  , contractValidators  = Set.singleton myValidator
  , contractDefinitions = deriveDefinitions @[AuctionParams, AuctionDatum, AuctionRedeemer]
  }

myPreamble :: Preamble
myPreamble = MkPreamble
  { preambleTitle         = "Auction Validator"
  , preambleDescription   = Just "Blueprint for a Plutus script validating auction transactions"
  , preambleVersion       = "1.0.0"
  , preamblePlutusVersion = PlutusV2
  , preambleLicense       = Just "MIT"
  }

--------------------------------------------------------------------------------
-- Validator Blueprint
--------------------------------------------------------------------------------

myValidator :: ValidatorBlueprint referencedTypes
myValidator = MkValidatorBlueprint
  { validatorTitle        = "Auction Validator"
  , validatorDescription  = Just "Plutus script validating auction transactions"
  , validatorParameters   =
      [ MkParameterBlueprint
          { parameterTitle       = Just "Parameters"
          , parameterDescription = Just "Compile-time validator parameters"
          , parameterPurpose     = Set.singleton Spend
          , parameterSchema      = definitionRef @AuctionParams
          }
      ]
  , validatorRedeemer     = MkArgumentBlueprint
      { argumentTitle       = Just "Redeemer"
      , argumentDescription = Just "Redeemer for the auction validator"
      , argumentPurpose     = Set.singleton Spend
      , argumentSchema      = definitionRef @()
      }
  , validatorDatum        = Nothing
  , validatorCompiled     = Just $ compileValidatorScript auctionParams
  }

compileValidatorScript :: AuctionParams -> CompiledValidator
compileValidatorScript params =
  let script = auctionValidatorScript params
      code   = Short.fromShort $ serialiseCompiledCode script
  in compiledValidator PlutusV2 code

--------------------------------------------------------------------------------
-- CLI Entrypoint
--------------------------------------------------------------------------------

-- | Writes the contract blueprint to a file.
writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

-- | Main function: expects 1 argument (output path).
main :: IO ()
main = getArgs >>= \case
  [outputPath] -> writeBlueprintToFile outputPath
  args         -> printUsageError args

printUsageError :: [String] -> IO ()
printUsageError args = putStrLn $ unlines
  [ "❌ Error: Expected 1 argument (output path), but got " ++ show (length args)
  , "Usage: auction-validator <output-path.json>"
  ]
