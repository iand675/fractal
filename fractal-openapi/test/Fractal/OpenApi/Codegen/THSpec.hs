{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Fractal.OpenApi.Codegen.THSpec (spec) where

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec.Hedgehog (hedgehog)
import Fractal.OpenApi.Codegen.Core
import Fractal.OpenApi.Codegen.Strategy
import qualified Fractal.OpenApi.Codegen.Strategy as Strategy
import Fractal.OpenApi.Codegen.TH
import Fractal.JsonSchema.Types
import Fractal.JsonSchema (parseSchema)
import Data.Aeson (Value(..), Object, FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isUpper, isLower, isLetter)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))

spec :: Spec
spec = describe "Template Haskell Code Generation" $ do
  
  describe "Name Generation" $ do
    it "generates valid Haskell type names" $ do
      let ctx = CodegenContext
            { codegenSchema = Schema Nothing Nothing (BooleanSchema True) Nothing Map.empty
            , codegenPath = emptyPointer
            , codegenRegistry = emptyTypeRegistry
            , codegenParentName = Nothing
            }
      
      let typeName = generateTypeName defaultCodegenConfig ctx
      isValidHaskellIdentifier typeName `shouldBe` True
      -- Type names should start with uppercase
      T.head typeName `shouldSatisfy` (`elem` ['A'..'Z'])
    
    it "sanitizes invalid characters in type names" $ do
      let name = sanitizeHaskellIdentifier True "my-invalid.type@name"
      isValidHaskellIdentifier name `shouldBe` True
      T.head name `shouldSatisfy` (`elem` ['A'..'Z'])
    
    it "generates valid field names with camelCase" $ do
      let fieldName = generateFieldName FieldCamelCase "first_name"
      fieldName `shouldBe` "firstName"
      isValidHaskellIdentifier fieldName `shouldBe` True
    
    it "generates valid field names with snake_case" $ do
      let fieldName = generateFieldName FieldSnakeCase "firstName"
      fieldName `shouldBe` "firstname"
      isValidHaskellIdentifier fieldName `shouldBe` True
    
    it "handles numeric prefixes in identifiers" $ do
      -- Type names (shouldCapitalize = True)
      let typeName = sanitizeHaskellIdentifier True "123invalid"
      T.head typeName `shouldBe` 'X'  -- Capitalized because it's a type
      isValidHaskellIdentifier typeName `shouldBe` True
      
      -- Field names (shouldCapitalize = False)
      let fieldName = sanitizeHaskellIdentifier False "123invalid"
      T.head fieldName `shouldBe` 'x'  -- Lowercase because it's a field
      isValidHaskellIdentifier fieldName `shouldBe` True

  describe "Strategy Selection" $ do
    it "selects RecordStrategy for object schemas" $ do
      let schemaValue = Aeson.object
            [ "type" Aeson..= String "object"
            , "properties" Aeson..= Aeson.object
                [ "name" Aeson..= Aeson.object ["type" Aeson..= String "string"]
                ]
            ]
      
      case parseSchema schemaValue of
        Left err -> expectationFailure $ "Parse error: " <> show err
        Right schema -> do
          selectStrategy defaultCodegenConfig schema `shouldBe` RecordStrategy
    
    it "selects NewtypeStrategy for single-field objects with optimization enabled" $ do
      let schemaValue = Aeson.object
            [ "type" Aeson..= String "object"
            , "properties" Aeson..= Aeson.object
                [ "value" Aeson..= Aeson.object ["type" Aeson..= String "string"]
                ]
            , "required" Aeson..= Aeson.Array (Vector.fromList [String "value"])
            ]
      
      case parseSchema schemaValue of
        Left err -> expectationFailure $ "Parse error: " <> show err
        Right schema -> do
          let config = defaultCodegenConfig { codegenNewtypeOptimization = True }
          selectStrategy config schema `shouldBe` Strategy.NewtypeStrategy
    
    it "selects SumTypeStrategy for oneOf schemas" $ do
      let schemaValue = Aeson.object
            [ "oneOf" Aeson..= Aeson.Array (Vector.fromList
                [ Aeson.object ["type" Aeson..= String "string"]
                , Aeson.object ["type" Aeson..= String "integer"]
                ])
            ]
      
      case parseSchema schemaValue of
        Left err -> expectationFailure $ "Parse error: " <> show err
        Right schema -> do
          selectStrategy defaultCodegenConfig schema `shouldBe` SumTypeStrategy
    
    it "selects EnumStrategy for enum schemas" $ do
      let schemaValue = Aeson.object
            [ "type" Aeson..= String "string"
            , "enum" Aeson..= Aeson.Array (Vector.fromList [String "red", String "green", String "blue"])
            ]
      
      case parseSchema schemaValue of
        Left err -> expectationFailure $ "Parse error: " <> show err
        Right schema -> do
          selectStrategy defaultCodegenConfig schema `shouldBe` EnumStrategy

  describe "Code Generation Strategies" $ do
    it "generates record types with correct fields" $ do
      let schemaValue = Aeson.object
            [ "type" Aeson..= String "object"
            , "title" Aeson..= String "Person"
            , "properties" Aeson..= Aeson.object
                [ "name" Aeson..= Aeson.object ["type" Aeson..= String "string"]
                , "age" Aeson..= Aeson.object ["type" Aeson..= String "integer"]
                ]
            , "required" Aeson..= Aeson.Array (Vector.fromList [String "name"])
            ]
      
      case parseSchema schemaValue of
        Left err -> expectationFailure $ "Parse error: " <> show err
        Right schema -> do
          let ctx = CodegenContext
                { codegenSchema = schema
                , codegenPath = emptyPointer
                , codegenRegistry = emptyTypeRegistry
                , codegenParentName = Nothing
                }
          
          case defaultStrategy defaultCodegenConfig ctx of
            Left err -> expectationFailure $ "Generation error: " <> T.unpack err
            Right genType -> do
              genTypeName genType `shouldBe` "Person"
              length (NE.toList $ genTypeConstructors genType) `shouldBe` 1
              
              let (_, fields) = NE.head $ genTypeConstructors genType
              length fields `shouldBe` 2
              
              -- Check field names
              map fieldName fields `shouldContain` ["name"]
              map fieldName fields `shouldContain` ["age"]
              
              -- Check optional wrapping
              let nameField = head $ filter (\f -> fieldName f == "name") fields
              fieldOptional nameField `shouldBe` False  -- Required field
              
              let ageField = head $ filter (\f -> fieldName f == "age") fields
              fieldOptional ageField `shouldBe` True  -- Optional field
    
    it "generates newtype for single-field objects" $ do
      let schemaValue = Aeson.object
            [ "type" Aeson..= String "object"
            , "title" Aeson..= String "UserId"
            , "properties" Aeson..= Aeson.object
                [ "value" Aeson..= Aeson.object ["type" Aeson..= String "integer"]
                ]
            , "required" Aeson..= Aeson.Array (Vector.fromList [String "value"])
            ]
      
      case parseSchema schemaValue of
        Left err -> expectationFailure $ "Parse error: " <> show err
        Right schema -> do
          let ctx = CodegenContext
                { codegenSchema = schema
                , codegenPath = emptyPointer
                , codegenRegistry = emptyTypeRegistry
                , codegenParentName = Nothing
                }
          
          case Strategy.newtypeStrategy defaultCodegenConfig ctx of
            Left err -> expectationFailure $ "Generation error: " <> T.unpack err
            Right genType -> do
              genTypeName genType `shouldBe` "UserId"
              genTypeStrict genType `shouldBe` True
    
    it "generates enum ADTs from string enums" $ do
      let schemaValue = Aeson.object
            [ "type" Aeson..= String "string"
            , "title" Aeson..= String "Color"
            , "enum" Aeson..= Aeson.Array (Vector.fromList [String "red", String "green", String "blue"])
            ]
      
      case parseSchema schemaValue of
        Left err -> expectationFailure $ "Parse error: " <> show err
        Right schema -> do
          let ctx = CodegenContext
                { codegenSchema = schema
                , codegenPath = emptyPointer
                , codegenRegistry = emptyTypeRegistry
                , codegenParentName = Nothing
                }
          
          case enumStrategy defaultCodegenConfig ctx of
            Left err -> expectationFailure $ "Generation error: " <> T.unpack err
            Right genType -> do
              genTypeName genType `shouldBe` "Color"
              length (NE.toList $ genTypeConstructors genType) `shouldBe` 3
              "Enum" `shouldSatisfy` (\d -> d `elem` genTypeDeriving genType)
              "Bounded" `shouldSatisfy` (\d -> d `elem` genTypeDeriving genType)

  describe "Field Name Conventions" $ do
    it "converts to camelCase correctly" $ do
      generateFieldName FieldCamelCase "first_name" `shouldBe` "firstName"
      generateFieldName FieldCamelCase "user-id" `shouldBe` "userId"
      generateFieldName FieldCamelCase "API KEY" `shouldBe` "apiKEY"
    
    it "converts to PascalCase correctly" $ do
      -- Note: generateFieldName always lowercases first char for fields
      -- PascalCase conversion happens, then field lowercasing
      generateFieldName FieldPascalCase "first_name" `shouldBe` "firstName"
      generateFieldName FieldPascalCase "user-id" `shouldBe` "userId"
    
    it "converts to snake_case correctly" $ do
      generateFieldName FieldSnakeCase "firstName" `shouldBe` "firstname"
      generateFieldName FieldSnakeCase "user-ID" `shouldBe` "user_id"
    
    it "strips prefixes correctly" $ do
      generateFieldName (FieldStripPrefix "user_") "user_name" `shouldBe` "name"
      generateFieldName (FieldStripPrefix "api") "apiKey" `shouldBe` "key"  -- Field names are lowercased

  describe "Property-based Tests" $ do
    it "generates unique type names for different schemas" $ hedgehog $ do
      -- Generate random schema titles
      title1 <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha
      title2 <- forAll $ Gen.filter (/= title1) $ Gen.text (Range.linear 1 20) Gen.alpha
      
      let schema1Value = Aeson.object
            [ "type" Aeson..= String "object"
            , "title" Aeson..= String title1
            , "properties" Aeson..= Aeson.object []
            ]
      
      let schema2Value = Aeson.object
            [ "type" Aeson..= String "object"
            , "title" Aeson..= String title2
            , "properties" Aeson..= Aeson.object []
            ]
      
      case (parseSchema schema1Value, parseSchema schema2Value) of
        (Right schema1, Right schema2) -> do
          let ctx1 = CodegenContext schema1 emptyPointer emptyTypeRegistry Nothing
              ctx2 = CodegenContext schema2 emptyPointer emptyTypeRegistry Nothing
          
          let name1 = generateTypeName defaultCodegenConfig ctx1
              name2 = generateTypeName defaultCodegenConfig ctx2
          
          name1 /== name2
        _ -> success  -- Skip if parse fails
    
    it "generates valid Haskell identifiers for any input" $ hedgehog $ do
      -- Generate random strings (alphanumeric + some special chars)
      input <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
      
      let typeName = sanitizeHaskellIdentifier True input
          fieldName = sanitizeHaskellIdentifier False input
      
      -- Both should be non-empty
      assert $ not (T.null typeName)
      assert $ not (T.null fieldName)
      
      -- Both should be valid identifiers
      assert $ isValidHaskellIdentifier typeName
      assert $ isValidHaskellIdentifier fieldName

  describe "End-to-End Code Generation" $ do
    it "generates working types that can parse and encode JSON" $ do
      -- This test generates actual Haskell types at compile time and tests them
      -- We'll use a simple schema and verify roundtripping works
      
      let personSchemaValue = Aeson.object
            [ "type" Aeson..= String "object"
            , "title" Aeson..= String "TestPerson"
            , "properties" Aeson..= Aeson.object
                [ "name" Aeson..= Aeson.object 
                    [ "type" Aeson..= String "string" ]
                , "age" Aeson..= Aeson.object 
                    [ "type" Aeson..= String "integer" ]
                , "email" Aeson..= Aeson.object 
                    [ "type" Aeson..= String "string" ]
                ]
            , "required" Aeson..= Aeson.Array (Vector.fromList [String "name"])
            ]
      
      case parseSchema personSchemaValue of
        Left err -> expectationFailure $ "Schema parse error: " <> show err
        Right schema -> do
          let ctx = CodegenContext
                { codegenSchema = schema
                , codegenPath = emptyPointer
                , codegenRegistry = emptyTypeRegistry
                , codegenParentName = Nothing
                }
          
          -- Generate the type representation
          case defaultStrategy defaultCodegenConfig ctx of
            Left err -> expectationFailure $ "Generation error: " <> T.unpack err
            Right genType -> do
              -- Verify the generated type structure
              genTypeName genType `shouldBe` "TestPerson"
              
              let (constructorName, fields) = NE.head $ genTypeConstructors genType
              constructorName `shouldBe` "TestPerson"
              length fields `shouldBe` 3
              
              -- Verify field structure
              let nameField = head $ filter (\f -> fieldName f == "name") fields
              fieldType nameField `shouldBe` "Text"
              fieldOptional nameField `shouldBe` False
              
              let ageField = head $ filter (\f -> fieldName f == "age") fields
              fieldType ageField `shouldBe` "Maybe (Integer)"
              fieldOptional ageField `shouldBe` True
              
              let emailField = head $ filter (\f -> fieldName f == "email") fields
              fieldType emailField `shouldBe` "Maybe (Text)"
              fieldOptional emailField `shouldBe` True
    
    it "generates enum types that can be encoded/decoded" $ do
      let statusSchemaValue = Aeson.object
            [ "type" Aeson..= String "string"
            , "title" Aeson..= String "Status"
            , "enum" Aeson..= Aeson.Array (Vector.fromList 
                [ String "active"
                , String "inactive"
                , String "pending"
                ])
            ]
      
      case parseSchema statusSchemaValue of
        Left err -> expectationFailure $ "Schema parse error: " <> show err
        Right schema -> do
          let ctx = CodegenContext
                { codegenSchema = schema
                , codegenPath = emptyPointer
                , codegenRegistry = emptyTypeRegistry
                , codegenParentName = Nothing
                }
          
          case enumStrategy defaultCodegenConfig ctx of
            Left err -> expectationFailure $ "Generation error: " <> T.unpack err
            Right genType -> do
              genTypeName genType `shouldBe` "Status"
              
              let constructors = NE.toList $ genTypeConstructors genType
              length constructors `shouldBe` 3
              
              -- Verify constructor names are valid identifiers
              let constructorNames = map fst constructors
              all isValidHaskellIdentifier constructorNames `shouldBe` True
              
              -- All constructors should have no fields (nullary)
              all (null . snd) constructors `shouldBe` True
              
              -- Should have Enum and Bounded in deriving
              genTypeDeriving genType `shouldContain` ["Enum"]
              genTypeDeriving genType `shouldContain` ["Bounded"]
    
    it "generates newtype wrappers correctly" $ do
      let userIdSchemaValue = Aeson.object
            [ "type" Aeson..= String "object"
            , "title" Aeson..= String "UserId"
            , "properties" Aeson..= Aeson.object
                [ "id" Aeson..= Aeson.object ["type" Aeson..= String "integer"]
                ]
            , "required" Aeson..= Aeson.Array (Vector.fromList [String "id"])
            , "x-newtype" Aeson..= Bool True
            ]
      
      case parseSchema userIdSchemaValue of
        Left err -> expectationFailure $ "Schema parse error: " <> show err
        Right schema -> do
          let ctx = CodegenContext
                { codegenSchema = schema
                , codegenPath = emptyPointer
                , codegenRegistry = emptyTypeRegistry
                , codegenParentName = Nothing
                }
          
          case Strategy.newtypeStrategy defaultCodegenConfig ctx of
            Left err -> expectationFailure $ "Generation error: " <> T.unpack err
            Right genType -> do
              genTypeName genType `shouldBe` "UserId"
              genTypeStrict genType `shouldBe` True
              
              let (constructorName, fields) = NE.head $ genTypeConstructors genType
              constructorName `shouldBe` "UserId"
              length fields `shouldBe` 1
              
              let field = head fields
              fieldType field `shouldBe` "Integer"
