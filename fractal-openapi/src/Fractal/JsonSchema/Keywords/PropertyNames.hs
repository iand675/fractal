{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'propertyNames' keyword
--
-- The propertyNames keyword validates that all property names in an object
-- match a given schema. Property names are validated as strings.
module Fractal.JsonSchema.Keywords.PropertyNames
  ( propertyNamesKeyword
  , compilePropertyNames
  , PropertyNamesData(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationPropertyNames, schemaValidation
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..), KeywordScope(..)
  )
import Fractal.JsonSchema.Parser (parseSchema)

-- | Compiled data for propertyNames keyword
newtype PropertyNamesData = PropertyNamesData Schema
  deriving (Typeable)

-- | Compile the propertyNames keyword
compilePropertyNames :: CompileFunc PropertyNamesData
compilePropertyNames value _schema _ctx = case parseSchema value of
  Left err -> Left $ "Invalid schema in propertyNames: " <> T.pack (show err)
  Right schema -> Right $ PropertyNamesData schema

-- | Validate propertyNames using the pluggable keyword system
validatePropertyNamesKeyword :: ValidateFunc PropertyNamesData
validatePropertyNamesKeyword recursiveValidator (PropertyNamesData nameSchema) _ctx (Object objMap) =
  -- Validate each property name (as a string) against the schema
  let propNames = [Key.toText k | k <- KeyMap.keys objMap]
      results = [recursiveValidator nameSchema (String propName) | propName <- propNames]
      failures = [errs | ValidationFailure errs <- results]
  in case failures of
    [] -> pure []  -- Success
    _ -> pure [T.pack $ show err | err <- failures]

validatePropertyNamesKeyword _ _ _ _ = pure []  -- Only applies to objects

-- | Keyword definition for propertyNames
propertyNamesKeyword :: KeywordDefinition
propertyNamesKeyword = KeywordDefinition
  { keywordName = "propertyNames"
  , keywordScope = AnyScope
  , keywordCompile = compilePropertyNames
  , keywordValidate = validatePropertyNamesKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> validationPropertyNames (schemaValidation obj)
      _ -> Nothing
  }

