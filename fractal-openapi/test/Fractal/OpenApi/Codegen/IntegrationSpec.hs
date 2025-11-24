{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end integration tests for code generation
-- This module actually generates real Haskell types using TH and tests them
module Fractal.OpenApi.Codegen.IntegrationSpec (spec) where

import Test.Hspec
import Fractal.OpenApi.Codegen.TH
import Fractal.OpenApi.Codegen.Core (schemaFor)
import Fractal.JsonSchema.Types (schemaCore, schemaAnnotations, annotationTitle, SchemaCore(..))
import Data.Aeson (Value(..), FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))

-- Generate a real Person type at compile time
$(deriveJSONSchema $ Aeson.object
  [ "type" Aeson..= String "object"
  , "title" Aeson..= String "Person"
  , "properties" Aeson..= Aeson.object
      [ "name" Aeson..= Aeson.object 
          [ "type" Aeson..= String "string" ]
      , "age" Aeson..= Aeson.object 
          [ "type" Aeson..= String "integer" ]
      , "email" Aeson..= Aeson.object 
          [ "type" Aeson..= String "string" ]
      ]
  , "required" Aeson..= Aeson.Array (Vector.fromList [String "name"])
  ])

-- Generate an enum type at compile time
$(deriveJSONSchema $ Aeson.object
  [ "type" Aeson..= String "string"
  , "title" Aeson..= String "Status"
  , "enum" Aeson..= Aeson.Array (Vector.fromList 
      [ String "active"
      , String "inactive"  
      , String "pending"
      ])
  ])

spec :: Spec
spec = describe "End-to-End Code Generation Integration" $ do
  
  describe "Generated Person type" $ do
    it "can parse valid JSON with all fields" $ do
      let json = Aeson.object
            [ "name" Aeson..= String "Alice"
            , "age" Aeson..= Number 30
            , "email" Aeson..= String "alice@example.com"
            ]
      
      case Aeson.fromJSON json of
        Aeson.Error err -> expectationFailure $ "Parse error: " <> err
        Aeson.Success (_person :: Person) -> do
          -- The type was generated and can parse!
          True `shouldBe` True
    
    it "can parse JSON with only required fields" $ do
      let json = Aeson.object
            [ "name" Aeson..= String "Bob"
            ]
      
      case Aeson.fromJSON json of
        Aeson.Error err -> expectationFailure $ "Parse error: " <> err
        Aeson.Success (_person :: Person) -> do
          -- Optional fields should be accepted as missing
          True `shouldBe` True
    
    it "roundtrips JSON encoding/decoding" $ do
      let json = Aeson.object
            [ "name" Aeson..= String "Charlie"
            , "age" Aeson..= Number 25
            ]
      
      case Aeson.fromJSON json :: Aeson.Result Person of
        Aeson.Error err -> expectationFailure $ "Initial parse error: " <> err
        Aeson.Success person -> do
          -- Encode and decode again
          let encoded = Aeson.encode person
          case Aeson.decode encoded :: Maybe Person of
            Nothing -> expectationFailure "Roundtrip decode failed"
            Just decoded -> do
              -- Roundtrip should preserve the value
              Aeson.toJSON decoded `shouldBe` Aeson.toJSON person
    
    it "has a working HasSchema instance" $ do
      -- The generated type should have a HasSchema instance
      let schema = schemaFor (Proxy :: Proxy Person)
      -- Schema should have the correct title
      case schemaCore schema of
        ObjectSchema obj -> do
          annotationTitle (schemaAnnotations obj) `shouldBe` Just "Person"
        _ -> expectationFailure "Expected ObjectSchema"
  
  describe "Generated Status enum" $ do
    it "can parse valid enum values with capitalized constructors" $ do
      -- Note: Generic Aeson instances use constructor names as tags
      -- So "active" in schema becomes "Active" constructor
      let json = String "Active"
      
      case Aeson.fromJSON json :: Aeson.Result Status of
        Aeson.Error err -> expectationFailure $ "Parse error: " <> err
        Aeson.Success _status -> do
          -- Successfully parsed enum value
          True `shouldBe` True
    
    it "roundtrips enum encoding/decoding" $ do
      let json = String "Pending"
      
      case Aeson.fromJSON json :: Aeson.Result Status of
        Aeson.Error err -> expectationFailure $ "Parse error: " <> err
        Aeson.Success status -> do
          -- Encode and decode
          let encoded = Aeson.encode status
          case Aeson.decode encoded :: Maybe Status of
            Nothing -> expectationFailure "Roundtrip failed"
            Just decoded -> do
              Aeson.toJSON decoded `shouldBe` Aeson.toJSON status

