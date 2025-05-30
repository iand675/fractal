{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Fractal.Schema.Compatibility.AvroSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Avro
import Data.Avro.Deriving
import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import Fractal.Schema.Compatibility.Avro
import Fractal.Schema.Types (CompatibilityLevel(..))

spec :: Spec
spec = describe "Avro Schema Compatibility" $ do
  describe "Basic Type Compatibility" $ do
    it "considers identical primitive types compatible" $ do
      let intSchema = $(makeSchemaFromByteString [r|{"type": "int"}|])
      let stringSchema = $(makeSchemaFromByteString [r|{"type": "string"}|])
      let booleanSchema = $(makeSchemaFromByteString [r|{"type": "boolean"}|])

      checkBackwardCompatibility intSchema intSchema `shouldBe` Compatible
      checkBackwardCompatibility stringSchema stringSchema `shouldBe` Compatible
      checkBackwardCompatibility booleanSchema booleanSchema `shouldBe` Compatible

    it "allows type promotion" $ do
      let intSchema = $(makeSchemaFromByteString [r|{"type": "int"}|])
      let longSchema = $(makeSchemaFromByteString [r|{"type": "long"}|])
      let floatSchema = $(makeSchemaFromByteString [r|{"type": "float"}|])
      let doubleSchema = $(makeSchemaFromByteString [r|{"type": "double"}|])

      checkBackwardCompatibility intSchema longSchema `shouldBe` Compatible
      checkBackwardCompatibility floatSchema doubleSchema `shouldBe` Compatible

    it "rejects incompatible primitive types" $ do
      let intSchema = $(makeSchemaFromByteString [r|{"type": "int"}|])
      let stringSchema = $(makeSchemaFromByteString [r|{"type": "string"}|])
      let booleanSchema = $(makeSchemaFromByteString [r|{"type": "boolean"}|])

      checkBackwardCompatibility intSchema stringSchema `shouldNotBe` Compatible
      checkBackwardCompatibility booleanSchema intSchema `shouldNotBe` Compatible

  describe "Record Compatibility" $ do
    it "considers identical records compatible" $ do
      let schema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {"name": "id", "type": "int"}
          ]
        }
      |])
      checkBackwardCompatibility schema schema `shouldBe` Compatible

    it "allows adding fields with defaults" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {"name": "id", "type": "int"}
          ]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {"name": "id", "type": "int"},
            {"name": "name", "type": "string", "default": "default"}
          ]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects removing required fields" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {"name": "id", "type": "int"},
            {"name": "name", "type": "string"}
          ]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {"name": "id", "type": "int"}
          ]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

    it "handles record aliases" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "name": "OldName",
          "type": "record",
          "aliases": ["NewName"],
          "fields": [
            {"name": "id", "type": "int"}
          ]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "name": "NewName",
          "type": "record",
          "aliases": ["OldName"],
          "fields": [
            {"name": "id", "type": "int"}
          ]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "handles record aliases with namespace" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "name": "OldName",
          "namespace": "com.example",
          "type": "record",
          "aliases": ["com.example.NewName"],
          "fields": [
            {"name": "id", "type": "int"}
          ]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "name": "NewName",
          "namespace": "com.example",
          "type": "record",
          "aliases": ["com.example.OldName"],
          "fields": [
            {"name": "id", "type": "int"}
          ]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

  describe "Field Aliases" $ do
    it "matches fields by aliases" $ do
      let readerField = Field
            { fldName = "new_name"
            , fldAliases = ["old_name", "legacy_name"]
            , fldDoc = Nothing
            , fldOrder = Nothing
            , fldType = Int Nothing
            , fldDefault = Nothing
            }
      let writerField = Field
            { fldName = "old_name"
            , fldAliases = ["new_name", "legacy_name"]
            , fldDoc = Nothing
            , fldOrder = Nothing
            , fldType = Int Nothing
            , fldDefault = Nothing
            }
      findMatchingField readerField [writerField] `shouldBe` Just writerField

    it "considers fields with matching aliases compatible" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {
              "name": "old_name",
              "type": "int",
              "aliases": ["new_name"]
            }
          ]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {
              "name": "new_name",
              "type": "int",
              "aliases": ["old_name"]
            }
          ]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

  describe "Schema References" $ do
    let addressSchema = $(makeSchemaFromByteString [r|
      {
        "name": "Address",
        "type": "record",
        "fields": [
          {"name": "street", "type": "string"},
          {"name": "city", "type": "string"}
        ]
      }
    |])
    let env = SchemaEnv
          { envReferences = HM.fromList [("com.example.Address", addressSchema)]
          , envNamespaces = HM.fromList [("com.example", "com.example")]
          }

    it "resolves schema references" $ do
      let ref = SchemaReference "Address" (Just "com.example")
      resolveReference env ref `shouldBe` Just addressSchema

    it "handles missing references" $ do
      let ref = SchemaReference "Missing" (Just "com.example")
      resolveReference env ref `shouldBe` Nothing

  describe "Union Types" $ do
    it "considers identical unions compatible" $ do
      let schema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {"name": "value", "type": ["int", "string"]}
          ]
        }
      |])
      checkBackwardCompatibility schema schema `shouldBe` Compatible

    it "allows adding union variants" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {"name": "value", "type": ["int"]}
          ]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {"name": "value", "type": ["int", "string"]}
          ]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects removing union variants" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {"name": "value", "type": ["int", "string"]}
          ]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "record",
          "fields": [
            {"name": "value", "type": ["int"]}
          ]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

  describe "Compatibility Levels" $ do
    let schemas = NE.fromList
          [ $(makeSchemaFromByteString [r|{"type": "string"}|])
          , $(makeSchemaFromByteString [r|{"type": "bytes"}|])
          , $(makeSchemaFromByteString [r|{"type": "string"}|])
          ]

    it "NONE level always returns compatible" $ do
      checkCompatibilityLevel NONE schemas `shouldBe` Compatible

    it "BACKWARD_TRANSITIVE checks all adjacent pairs" $ do
      checkCompatibilityLevel BACKWARD_TRANSITIVE schemas `shouldBe` Compatible

    it "FORWARD_TRANSITIVE checks all adjacent pairs" $ do
      checkCompatibilityLevel FORWARD_TRANSITIVE schemas `shouldBe` Compatible

    it "FULL_TRANSITIVE checks both directions" $ do
      checkCompatibilityLevel FULL_TRANSITIVE schemas `shouldBe` Compatible

  describe "Error Formatting" $ do
    it "formats path components correctly" $ do
      let path = [PathField "user", ArrayIndex, MapValue, UnionVariant 0]
      formatPath path `shouldBe` ".user[].value[0]"

    it "formats error types correctly" $ do
      formatErrorType TypeMismatch `shouldBe` "Type mismatch"
      formatErrorType MissingField `shouldBe` "Missing field"
      formatErrorType ReferenceError `shouldBe` "Schema reference error"

  describe "Enum Compatibility" $ do
    it "considers identical enums compatible" $ do
      let schema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "enum",
          "symbols": ["A", "B", "C"]
        }
      |])
      checkBackwardCompatibility schema schema `shouldBe` Compatible

    it "allows adding enum symbols" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "enum",
          "symbols": ["A", "B"]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "enum",
          "symbols": ["A", "B", "C"]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects removing enum symbols without default (top-level)" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "enum",
          "symbols": ["A", "B", "C"]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "enum",
          "symbols": ["A", "B"]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

    it "rejects removing enum symbols with default property in top-level enum (should be incompatible)" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "enum",
          "symbols": ["A", "B", "C"],
          "default": "A"
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "name": "Test",
          "type": "enum",
          "symbols": ["A", "B"]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

    it "allows removing enum symbols with default (field)" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "type": "record",
          "name": "Test",
          "fields": [
            { "name": "e", "type": { "type": "enum", "name": "E", "symbols": ["A", "B", "C"] }, "default": "A" }
          ]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "type": "record",
          "name": "Test",
          "fields": [
            { "name": "e", "type": { "type": "enum", "name": "E", "symbols": ["A", "B"] }, "default": "A" }
          ]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldBe` Compatible

    it "rejects removing the default enum symbol (field)" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "type": "record",
          "name": "Test",
          "fields": [
            { "name": "e", "type": { "type": "enum", "name": "E", "symbols": ["A", "B", "C"] }, "default": "A" }
          ]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "type": "record",
          "name": "Test",
          "fields": [
            { "name": "e", "type": { "type": "enum", "name": "E", "symbols": ["B", "C"] } }
          ]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible

    it "rejects removing enum symbols with no default (field)" $ do
      let oldSchema = $(makeSchemaFromByteString [r|
        {
          "type": "record",
          "name": "Test",
          "fields": [
            { "name": "e", "type": { "type": "enum", "name": "E", "symbols": ["A", "B", "C"] } }
          ]
        }
      |])
      let newSchema = $(makeSchemaFromByteString [r|
        {
          "type": "record",
          "name": "Test",
          "fields": [
            { "name": "e", "type": { "type": "enum", "name": "E", "symbols": ["A", "B"] } }
          ]
        }
      |])
      checkBackwardCompatibility oldSchema newSchema `shouldNotBe` Compatible
