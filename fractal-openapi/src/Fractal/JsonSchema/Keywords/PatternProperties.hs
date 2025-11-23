{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'patternProperties' keyword
--
-- The patternProperties keyword validates object properties whose names
-- match specific regex patterns. Each pattern maps to a schema that
-- validates values for properties matching that pattern.
module Fractal.JsonSchema.Keywords.PatternProperties
  ( patternPropertiesKeyword
  , compilePatternProperties
  , PatternPropertiesData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..), Regex(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationPatternProperties, schemaValidation
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..), KeywordScope(..)
  )
import Fractal.JsonSchema.Parser (parseSchema)
import qualified Fractal.JsonSchema.Regex as RegexModule

-- | Compiled data for patternProperties keyword
newtype PatternPropertiesData = PatternPropertiesData (Map Regex Schema)
  deriving (Typeable)

-- | Compile the patternProperties keyword
compilePatternProperties :: CompileFunc PatternPropertiesData
compilePatternProperties (Object obj) _schema _ctx = do
  -- Parse each property value as a schema, keys are regex patterns
  let entries = KeyMap.toList obj
  schemas <- mapM parseEntry entries
  Right $ PatternPropertiesData $ Map.fromList schemas
  where
    parseEntry (k, v) = case parseSchema v of
      Left err -> Left $ "Invalid schema for pattern '" <> Key.toText k <> "': " <> T.pack (show err)
      Right schema -> Right (Regex (Key.toText k), schema)

compilePatternProperties _ _ _ = Left "patternProperties must be an object"

-- | Validate patternProperties using the pluggable keyword system
validatePatternPropertiesKeyword :: ValidateFunc PatternPropertiesData
validatePatternPropertiesKeyword recursiveValidator (PatternPropertiesData patternSchemas) _ctx (Object objMap) =
  -- For each property in the object, check if it matches any patterns
  let results = 
        [ recursiveValidator patternSchema propValue
        | (k, propValue) <- KeyMap.toList objMap
        , let propName = Key.toText k
        , (Regex patternText, patternSchema) <- Map.toList patternSchemas
        , case RegexModule.compileRegex patternText of
            Right regex -> RegexModule.matchRegex regex propName
            Left _ -> False  -- Invalid regex, skip
        ]
      failures = [errs | ValidationFailure errs <- results]
  in case failures of
    [] -> pure []  -- Success
    _ -> pure [T.pack $ show err | err <- failures]

validatePatternPropertiesKeyword _ _ _ _ = pure []  -- Only applies to objects

-- | Keyword definition for patternProperties
patternPropertiesKeyword :: KeywordDefinition
patternPropertiesKeyword = KeywordDefinition
  { keywordName = "patternProperties"
  , keywordScope = AnyScope
  , keywordCompile = compilePatternProperties
  , keywordValidate = validatePatternPropertiesKeyword
  , keywordNavigation = SchemaMap $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        case validationPatternProperties (schemaValidation obj) of
          Just patterns ->
            -- Convert Regex keys to Text keys for navigation
            Just $ Map.fromList [(pat, schema') | (Regex pat, schema') <- Map.toList patterns]
          Nothing -> Nothing
      _ -> Nothing
  }

