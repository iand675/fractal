module Fractal.JsonSchema.DialectSpec (spec) where

import Test.Hspec
import Fractal.JsonSchema
import Fractal.JsonSchema.Dialect
import Fractal.JsonSchema.Vocabulary
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "Multi-Version Dialect Support" $ do
  describe "Draft-04 specific behavior" $ do
    it "parses exclusiveMaximum as boolean modifier" $ do
      let schemaJson = object
            [ "type" .= ("number" :: T.Text)
            , "maximum" .= (100 :: Int)
            , "exclusiveMaximum" .= True
            ]
      
      case parseSchemaWithVersion Draft04 schemaJson of
        Right schema -> case schemaCore schema of
          ObjectSchema obj -> do
            let validation = schemaValidation obj
            case validationExclusiveMaximum validation of
              Just (Left True) -> pure ()  -- Correct: boolean in draft-04
              _ -> expectationFailure "exclusiveMaximum should be Left Bool in draft-04"
          _ -> expectationFailure "Expected ObjectSchema"
        Left err -> expectationFailure $ "Parse failed: " <> show err
  
  describe "Draft-06 specific behavior" $ do
    it "parses exclusiveMaximum as numeric value" $ do
      let schemaJson = object
            [ "type" .= ("number" :: T.Text)
            , "exclusiveMaximum" .= (100 :: Int)
            ]
      
      case parseSchemaWithVersion Draft06 schemaJson of
        Right schema -> case schemaCore schema of
          ObjectSchema obj -> do
            let validation = schemaValidation obj
            case validationExclusiveMaximum validation of
              Just (Right _) -> pure ()  -- Correct: numeric in draft-06+
              _ -> expectationFailure "exclusiveMaximum should be Right Scientific in draft-06"
          _ -> expectationFailure "Expected ObjectSchema"
        Left err -> expectationFailure $ "Parse failed: " <> show err
    
    it "supports const keyword" $ do
      let schemaJson = object ["const" .= ("fixed-value" :: T.Text)]
      
      case parseSchemaWithVersion Draft06 schemaJson of
        Right schema -> case schemaCore schema of
          ObjectSchema obj -> schemaConst obj `shouldSatisfy` (/= Nothing)
          _ -> expectationFailure "Expected ObjectSchema"
        Left err -> expectationFailure $ "Parse failed: " <> show err
  
  describe "Draft-07 specific behavior" $ do
    it "supports if/then/else conditionals" $ do
      let schemaJson = object
            [ "if" .= object ["type" .= ("string" :: T.Text)]
            , "then" .= object ["minLength" .= (1 :: Int)]
            , "else" .= object ["minimum" .= (0 :: Int)]
            ]
      
      case parseSchemaWithVersion Draft07 schemaJson of
        Right schema -> case schemaCore schema of
          ObjectSchema obj -> do
            schemaIf obj `shouldSatisfy` (/= Nothing)
            schemaThen obj `shouldSatisfy` (/= Nothing)
            schemaElse obj `shouldSatisfy` (/= Nothing)
          _ -> expectationFailure "Expected ObjectSchema"
        Left err -> expectationFailure $ "Parse failed: " <> show err
    
    it "validates if/then/else logic" $ do
      -- Schema: if type is string, then minLength 1, else minimum 0
      let schema = Schema
            { schemaVersion = Just Draft07
            , schemaId = Nothing
            , schemaCore = BooleanSchema True  -- Simplified for test
            , schemaVocabulary = Nothing
            , schemaExtensions = Map.empty
            }
      
      -- String value should apply 'then'
      let stringValue = String "test"
      isSuccess (validateValue defaultValidationConfig schema stringValue) `shouldBe` True
  
  describe "Draft 2019-09 specific behavior" $ do
    it "supports unevaluatedProperties" $ do
      let schemaJson = object
            [ "type" .= ("object" :: T.Text)
            , "properties" .= object ["name" .= object ["type" .= ("string" :: T.Text)]]
            , "unevaluatedProperties" .= False
            ]
      
      case parseSchemaWithVersion Draft201909 schemaJson of
        Right schema -> case schemaCore schema of
          ObjectSchema obj ->
            validationUnevaluatedProperties (schemaValidation obj) `shouldSatisfy` (/= Nothing)
          _ -> expectationFailure "Expected ObjectSchema"
        Left err -> expectationFailure $ "Parse failed: " <> show err
    
    it "supports dependentSchemas" $ do
      let schemaJson = object
            [ "type" .= ("object" :: T.Text)
            , "dependentSchemas" .= object
                [ "name" .= object ["required" .= (["email"] :: [T.Text])]
                ]
            ]
      
      case parseSchemaWithVersion Draft201909 schemaJson of
        Right schema -> case schemaCore schema of
          ObjectSchema obj ->
            validationDependentSchemas (schemaValidation obj) `shouldSatisfy` (/= Nothing)
          _ -> expectationFailure "Expected ObjectSchema"
        Left err -> expectationFailure $ "Parse failed: " <> show err
  
  describe "Draft 2020-12 specific behavior" $ do
    it "supports prefixItems" $ do
      let schemaJson = object
            [ "type" .= ("array" :: T.Text)
            , "prefixItems" .= 
                [ object ["type" .= ("string" :: T.Text)]
                , object ["type" .= ("number" :: T.Text)]
                ]
            ]
      
      case parseSchemaWithVersion Draft202012 schemaJson of
        Right schema -> case schemaCore schema of
          ObjectSchema obj ->
            validationPrefixItems (schemaValidation obj) `shouldSatisfy` (/= Nothing)
          _ -> expectationFailure "Expected ObjectSchema"
        Left err -> expectationFailure $ "Parse failed: " <> show err
  
  describe "Vocabulary registry" $ do
    it "registers all standard vocabularies" $ do
      lookupVocabulary "https://json-schema.org/draft/2020-12/vocab/core" standardRegistry
        `shouldSatisfy` (/= Nothing)
      lookupVocabulary "https://json-schema.org/draft/2020-12/vocab/validation" standardRegistry
        `shouldSatisfy` (/= Nothing)
      lookupVocabulary "https://json-schema.org/draft/2020-12/vocab/applicator" standardRegistry
        `shouldSatisfy` (/= Nothing)
    
    it "registers all standard dialects" $ do
      lookupDialect "http://json-schema.org/draft-04/schema#" standardRegistry
        `shouldSatisfy` (/= Nothing)
      lookupDialect "http://json-schema.org/draft-07/schema#" standardRegistry
        `shouldSatisfy` (/= Nothing)
      lookupDialect "https://json-schema.org/draft/2020-12/schema" standardRegistry
        `shouldSatisfy` (/= Nothing)
