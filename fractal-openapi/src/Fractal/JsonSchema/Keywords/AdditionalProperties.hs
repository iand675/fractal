{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'additionalProperties' keyword
--
-- The additionalProperties keyword validates object properties that are NOT
-- covered by the 'properties' or 'patternProperties' keywords. It applies
-- its schema to all "additional" properties.
module Fractal.JsonSchema.Keywords.AdditionalProperties
  ( additionalPropertiesKeyword
  , compileAdditionalProperties
  , AdditionalPropertiesData(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..), Regex(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationAdditionalProperties, validationProperties, validationPatternProperties
  , schemaValidation, ValidationAnnotations(..)
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..), KeywordScope(..)
  , CompilationContext(..), combineValidationResults
  )
import qualified Fractal.JsonSchema.Regex as RegexModule

-- | Compiled data for additionalProperties keyword
data AdditionalPropertiesData = AdditionalPropertiesData 
  { addlPropsSchema :: Schema
  , addlPropsDefinedProps :: Set Text  -- Properties covered by 'properties'
  , addlPropsPatterns :: [Regex]       -- Patterns from 'patternProperties'
  }
  deriving (Typeable)

-- | Compile the additionalProperties keyword
compileAdditionalProperties :: CompileFunc AdditionalPropertiesData
compileAdditionalProperties value schema ctx = do
  -- Parse the additionalProperties value as a schema
  addlSchema <- case contextParseSubschema ctx value of
    Left err -> Left $ "Invalid schema in additionalProperties: " <> err
    Right s -> Right s
  
  -- Read adjacent 'properties' and 'patternProperties' keywords from schema
  let validation = case schemaCore schema of
        ObjectSchema obj -> schemaValidation obj
        _ -> error "additionalProperties can only appear in object schemas"
      
      definedProps = maybe Set.empty Map.keysSet (validationProperties validation)
      patterns = maybe [] (map fst . Map.toList) (validationPatternProperties validation)
  
  Right $ AdditionalPropertiesData addlSchema definedProps patterns

-- | Check if a property name matches any pattern
matchesAnyPattern :: Text -> [Regex] -> Bool
matchesAnyPattern propName patterns = any matchesPattern patterns
  where
    matchesPattern (Regex patternText) = case RegexModule.compileRegex patternText of
      Right regex -> RegexModule.matchRegex regex propName
      Left _ -> False

-- | Validate additionalProperties using the pluggable keyword system
validateAdditionalPropertiesKeyword :: ValidateFunc AdditionalPropertiesData
validateAdditionalPropertiesKeyword recursiveValidator (AdditionalPropertiesData addlSchema definedProps patterns) _ctx (Object objMap) =
  -- Find properties not covered by 'properties' or 'patternProperties'
  let additionalProps = 
        [ (Key.toText k, v)
        | (k, v) <- KeyMap.toList objMap
        , let propName = Key.toText k
        , not (Set.member propName definedProps)
        , not (matchesAnyPattern propName patterns)
        ]
      
      -- Validate each additional property
      results = [recursiveValidator addlSchema propValue | (_, propValue) <- additionalProps]
  in pure $ combineValidationResults results

validateAdditionalPropertiesKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to objects

-- | Keyword definition for additionalProperties
additionalPropertiesKeyword :: KeywordDefinition
additionalPropertiesKeyword = KeywordDefinition
  { keywordName = "additionalProperties"
  , keywordScope = AnyScope
  , keywordCompile = compileAdditionalProperties
  , keywordValidate = validateAdditionalPropertiesKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> validationAdditionalProperties (schemaValidation obj)
      _ -> Nothing
  }

