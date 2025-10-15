{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import AuctionMintingPolicy
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)
import PlutusLedgerApi.V1.Address (PubKeyHash) -- Considered a placeholder type import

-- | Define your contract blueprint
myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "auction-minting-policy"
    , contractPreamble = myPreamble
    , contractValidators = Set.singleton myValidator
    , contractDefinitions = deriveDefinitions @[AuctionMintingParams, ()]
    }

-- | Metadata for the contract
myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Auction Minting Policy"
    , preambleDescription = Just "A simple minting policy"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }

-- | Replace this with actual logic for fetching or generating a seller PubKeyHash
sellerPubKeyHash :: PubKeyHash
sellerPubKeyHash = error "TODO: Replace with actual seller public key hash"

-- | Blueprint for your validator
myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "Auction Minting Validator"
    , validatorDescription = Just "A simple minting validator"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Minting Validator Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Mint
            , parameterSchema = definitionRef @AuctionMintingParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "The minting policy does not use a redeemer, so this is unit ()"
          , argumentPurpose = Set.singleton Mint
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiled =
        let script = auctionMintingPolicyScript sellerPubKeyHash
            code = Short.fromShort (serialiseCompiledCode script)
         in Just (compiledValidator PlutusV2 code)
    }

-- | Write the blueprint to the specified file
writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

-- | Main entry point: expects exactly one argument (output path)
main :: IO ()
main = getArgs >>= \case
  [arg] -> writeBlueprintToFile arg
  args -> do
    putStrLn "Usage: auction-blueprint <output-file>"
    putStrLn $ "Expected 1 argument, but got " <> show (length args) <> ": " <> show args
    fail "Invalid number of arguments"
