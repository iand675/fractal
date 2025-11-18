{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Fractal.Schema.Compatibility.JsonSpec (spec) where

import Test.Hspec
import Data.Aeson (Value, decode)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as BL
import Fractal.Schema.Compatibility.Json
import Fractal.Schema.Types (CompatibilityLevel(..))

-- Helper function to parse JSON schema from string
parseJsonSchema :: String -> Value
parseJsonSchema s = fromJust $ decode $ BL.pack s

spec :: Spec
spec = describe "JSON Schema Compatibility" $ do
  describe "Basic Type Compatibility" $ do
    it "considers identical primitive types compatible" $ do
      let intSchema = parseJsonSchema "{\"type\": \"integer\"}"
      let stringSchema = parseJsonSchema "{\"type\": \"string\"}"
      let boolSchema = parseJsonSchema "{\"type\": \"boolean\"}"

      checkBackwardCompatibility intSchema intSchema `shouldBe` Compatible
      checkBackwardCompatibility stringSchema stringSchema `shouldBe` Compatible
      checkBackwardCompatibility boolSchema boolSchema `shouldBe` Compatible

    it "allows type widening (integer to number)" $ do
      let intSchema = parseJsonSchema "{\"type\": \"integer\"}"
      let numberSchema = parseJsonSchema "{\"type\": \"number\"}"

      checkBackwardCompatibility intSchema numberSchema `shouldBe` Compatible

    it "rejects type narrowing in backward compatibility" $ do
      let numberSchema = parseJsonSchema "{\"type\": \"number\"}"
      let intSchema = parseJsonSchema "{\"type\": \"integer\"}"

      checkBackwardCompatibility numberSchema intSchema `shouldNotBe` Compatible

    it "allows type narrowing in forward compatibility" $ do
      let numberSchema = parseJsonSchema "{\"type\": \"number\"}"
      let intSchema = parseJsonSchema "{\"type\": \"integer\"}"

      checkForwardCompatibility numberSchema intSchema `shouldBe` Compatible

    it "rejects incompatible primitive types" $ do
      let intSchema = parseJsonSchema "{\"type\": \"integer\"}"
      let stringSchema = parseJsonSchema "{\"type\": \"string\"}"
      let boolSchema = parseJsonSchema "{\"type\": \"boolean\"}"

      checkBackwardCompatibility intSchema stringSchema `shouldNotBe` Compatible
      checkBackwardCompatibility boolSchema intSchema `shouldNotBe` Compatible

  describe "Object Schema Compatibility" $ do
    it "considers identical objects compatible" $ do
      let schema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      checkBackwardCompatibility schema schema `shouldBe` Compatible

    it "allows adding optional properties" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"},\
          \\"name\": {\"type\": \"string\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects adding required properties" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"},\
          \\"name\": {\"type\": \"string\"}\
        \},\
        \\"required\": [\"id\", \"name\"]\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

    it "allows removing optional properties" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"},\
          \\"name\": {\"type\": \"string\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects removing required properties" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"},\
          \\"name\": {\"type\": \"string\"}\
        \},\
        \\"required\": [\"id\", \"name\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

  describe "Required Fields" $ do
    it "allows making fields optional (backward compatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"},\
          \\"name\": {\"type\": \"string\"}\
        \},\
        \\"required\": [\"id\", \"name\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"},\
          \\"name\": {\"type\": \"string\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects making fields required (backward incompatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"},\
          \\"name\": {\"type\": \"string\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"},\
          \\"name\": {\"type\": \"string\"}\
        \},\
        \\"required\": [\"id\", \"name\"]\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

    it "allows making fields required (forward compatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"},\
          \\"name\": {\"type\": \"string\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"},\
          \\"name\": {\"type\": \"string\"}\
        \},\
        \\"required\": [\"id\", \"name\"]\
      \}"
      checkForwardCompatibility oldSchema newSchema `shouldBe` Compatible

  describe "Enum Compatibility" $ do
    it "considers identical enums compatible" $ do
      let schema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"enum\": [\"A\", \"B\", \"C\"]\
      \}"
      checkBackwardCompatibility schema schema `shouldBe` Compatible

    it "allows adding enum values (backward compatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"enum\": [\"A\", \"B\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"enum\": [\"A\", \"B\", \"C\"]\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects removing enum values (backward incompatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"enum\": [\"A\", \"B\", \"C\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"enum\": [\"A\", \"B\"]\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

    it "allows removing enum values (forward compatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"enum\": [\"A\", \"B\", \"C\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"enum\": [\"A\", \"B\"]\
      \}"
      checkForwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects adding enum values (forward incompatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"enum\": [\"A\", \"B\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"enum\": [\"A\", \"B\", \"C\"]\
      \}"
      checkForwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

  describe "Numeric Constraints" $ do
    it "allows relaxing minimum constraint (backward compatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"integer\",\
        \\"minimum\": 10\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"integer\",\
        \\"minimum\": 5\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects tightening minimum constraint (backward incompatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"integer\",\
        \\"minimum\": 5\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"integer\",\
        \\"minimum\": 10\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

    it "allows relaxing maximum constraint (backward compatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"integer\",\
        \\"maximum\": 100\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"integer\",\
        \\"maximum\": 200\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects tightening maximum constraint (backward incompatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"integer\",\
        \\"maximum\": 200\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"integer\",\
        \\"maximum\": 100\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

  describe "String Constraints" $ do
    it "allows relaxing minLength constraint (backward compatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"minLength\": 10\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"minLength\": 5\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects tightening minLength constraint (backward incompatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"minLength\": 5\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"minLength\": 10\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

    it "allows relaxing maxLength constraint (backward compatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"maxLength\": 100\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"maxLength\": 200\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects tightening maxLength constraint (backward incompatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"maxLength\": 200\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"maxLength\": 100\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

  describe "Format Changes" $ do
    it "rejects format changes" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"format\": \"email\"\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"format\": \"uri\"\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

    it "allows adding format constraint (backward compatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"string\"\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"format\": \"email\"\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

    it "allows removing format constraint (backward compatible)" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"string\",\
        \\"format\": \"email\"\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"string\"\
      \}"
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

  describe "Compatibility Levels" $ do
    let schema1 = parseJsonSchema "{\"type\": \"string\"}"
    let schema2 = parseJsonSchema "{\"type\": \"integer\"}"
    let schema3 = parseJsonSchema "{\"type\": \"string\"}"
    let schemas = NE.fromList [schema3, schema2, schema1]

    it "NONE level always returns compatible" $ do
      checkCompatibilityLevel NONE schemas `shouldBe` Compatible

    it "BACKWARD checks newest against oldest" $ do
      checkCompatibilityLevel BACKWARD schemas `shouldBe` Compatible

    it "FORWARD checks newest against oldest" $ do
      checkCompatibilityLevel FORWARD schemas `shouldBe` Compatible

  describe "Full Compatibility" $ do
    it "accepts schemas that are both backward and forward compatible" $ do
      let schema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      checkFullCompatibility schema schema `shouldBe` Compatible

    it "rejects schemas that are only backward compatible" $ do
      let oldSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      let newSchema = parseJsonSchema "{\
        \\"type\": \"object\",\
        \\"properties\": {\
          \\"id\": {\"type\": \"integer\"},\
          \\"name\": {\"type\": \"string\"}\
        \},\
        \\"required\": [\"id\"]\
      \}"
      -- This is backward compatible but not forward compatible
      checkFullCompatibility oldSchema newSchema `shouldNotBe` Compatible

  describe "Error Formatting" $ do
    it "formats path components correctly" $ do
      let path = [PathProperty "user", PathType, PathRequired]
      formatPath path `shouldBe` ".user.type.required"

    it "formats error types correctly" $ do
      formatErrorType TypeMismatch `shouldBe` "Type mismatch"
      formatErrorType RequiredFieldAdded `shouldBe` "Required field added"
      formatErrorType EnumValueRemoved `shouldBe` "Enum value removed"
