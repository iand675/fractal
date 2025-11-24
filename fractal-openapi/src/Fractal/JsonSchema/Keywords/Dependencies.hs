{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the legacy 'dependencies' keyword (Draft-04 through Draft-07)
module Fractal.JsonSchema.Keywords.Dependencies
  ( dependenciesKeyword
  , compileDependencies
  , DependenciesData(..)
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
  ( Schema(..), SchemaCore(..), SchemaObject(..), Dependency(..)
  , ValidationResult, pattern ValidationSuccess, validationFailure
  , schemaValidation, validationDependencies
  )
import Fractal.JsonSchema.Keyword.Types
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  , combineValidationResults
  )

-- | Compiled data for dependencies keyword
newtype DependenciesData = DependenciesData (Map Text Dependency)
  deriving (Typeable)

-- | Compile the dependencies keyword using parsed schema metadata
compileDependencies :: CompileFunc DependenciesData
compileDependencies _ schema _ =
  case schemaCore schema of
    BooleanSchema _ -> Left "dependencies cannot appear in boolean schemas"
    ObjectSchema obj ->
      case validationDependencies (schemaValidation obj) of
        Nothing -> Left "dependencies keyword missing parsed data"
        Just deps -> Right $ DependenciesData deps

-- | Validate dependencies: property and schema dependencies (Draft-04/06/07)
validateDependenciesKeyword :: ValidateFunc DependenciesData
validateDependenciesKeyword recursiveValidator (DependenciesData deps) _ctx (Object objMap) =
  let presentProps = Set.fromList [Key.toText k | k <- KeyMap.keys objMap]
      results = map (validateDependency presentProps) (Map.toList deps)
  in pure $ combineValidationResults results
  where
    validateDependency present (propName, dep) =
      case KeyMap.lookup (Key.fromText propName) objMap of
        Nothing -> ValidationSuccess mempty
        Just _ ->
          case dep of
            DependencyProperties required ->
              let missing = Set.difference required present
              in if Set.null missing
                   then ValidationSuccess mempty
                   else validationFailure "dependencies" $
                        "Property '" <> propName <> "' requires these properties: " <>
                          T.intercalate ", " (Set.toList missing)
            DependencySchema depSchema ->
              recursiveValidator depSchema (Object objMap)
validateDependenciesKeyword _ _ _ _ = pure (ValidationSuccess mempty)

-- | Keyword definition for dependencies
dependenciesKeyword :: KeywordDefinition
dependenciesKeyword = KeywordDefinition
  { keywordName = "dependencies"
  , keywordCompile = compileDependencies
  , keywordValidate = validateDependenciesKeyword
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

