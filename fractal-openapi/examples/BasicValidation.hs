{-# LANGUAGE OverloadedStrings #-}

-- | Basic validation example
--
-- Demonstrates simple schema validation with boolean schemas.
module Main where

import Fractal.JsonSchema
import Data.Aeson (Value(..), Bool(..))

main :: IO ()
main = do
  putStrLn "=== Fractal OpenAPI - Basic Validation Example ==="
  putStrLn ""
  
  -- Example 1: Boolean schema (allow all)
  let allowAllSchema = Schema
        { schemaVersion = Just Draft202012
        , schemaId = Nothing
        , schemaCore = BooleanSchema True
        , schemaVocabulary = Nothing
        , schemaExtensions = mempty
        }
  
  putStrLn "1. Schema: true (allows everything)"
  case validateValue defaultValidationConfig allowAllSchema Null of
    ValidationSuccess _ -> putStrLn "   ✓ null validates"
    ValidationFailure _ -> putStrLn "   ✗ null rejected"
  
  case validateValue defaultValidationConfig allowAllSchema (Bool True) of
    ValidationSuccess _ -> putStrLn "   ✓ true validates"
    ValidationFailure _ -> putStrLn "   ✗ true rejected"
  
  -- Example 2: Boolean schema (reject all)
  let rejectAllSchema = Schema
        { schemaVersion = Just Draft202012
        , schemaId = Nothing
        , schemaCore = BooleanSchema False
        , schemaVocabulary = Nothing
        , schemaExtensions = mempty
        }
  
  putStrLn ""
  putStrLn "2. Schema: false (rejects everything)"
  case validateValue defaultValidationConfig rejectAllSchema Null of
    ValidationSuccess _ -> putStrLn "   ✓ null validates"
    ValidationFailure errs -> putStrLn $ "   ✗ null rejected: " <> show (head $ unErrors errs)
  
  putStrLn ""
  putStrLn "More examples coming as implementation progresses!"

