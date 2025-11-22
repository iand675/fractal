-- | Keyword registration and management API
--
-- This module provides the public API for registering custom keywords
-- and managing keyword registries. It enables users to extend JSON Schema
-- with domain-specific validation keywords.
--
-- Example usage:
--
-- @
-- -- Define a custom keyword for credit card validation
-- creditCardKeyword :: KeywordDefinition
-- creditCardKeyword = mkKeywordDefinition
--   "x-creditCard"
--   AnyScope
--   compileCreditCard
--   validateCreditCard
--
-- -- Register in a registry
-- registry <- registerKeyword creditCardKeyword emptyKeywordRegistry
-- @
module Fractal.JsonSchema.Keyword
  ( -- * Keyword Registry
    KeywordRegistry(..)
  , emptyKeywordRegistry
  , registerKeyword
  , lookupKeyword
  , getRegisteredKeywords
    -- * Keyword Definition Helpers
  , mkKeywordDefinition
  , mkSimpleKeyword
    -- * Re-exports from Types
  , KeywordDefinition(..)
  , KeywordScope(..)
  , CompileFunc
  , ValidateFunc
  , CompilationContext(..)
  , CompiledKeyword(..)
  , SomeCompiledData(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Aeson (Value)

import Fractal.JsonSchema.Keyword.Types
import Fractal.JsonSchema.Types (Schema)

-- | Empty keyword registry with no custom keywords
emptyKeywordRegistry :: KeywordRegistry
emptyKeywordRegistry = KeywordRegistry Map.empty

-- | Register a custom keyword in the registry
--
-- If a keyword with the same name already exists, it will be replaced
-- with the new definition (allowing for keyword shadowing).
registerKeyword :: KeywordDefinition -> KeywordRegistry -> KeywordRegistry
registerKeyword kw@(KeywordDefinition name _ _ _) (KeywordRegistry m) =
  KeywordRegistry (Map.insert name kw m)

-- | Look up a keyword by name in the registry
--
-- Returns Nothing if the keyword is not registered.
lookupKeyword :: Text -> KeywordRegistry -> Maybe KeywordDefinition
lookupKeyword name (KeywordRegistry m) = Map.lookup name m

-- | Get all registered keyword names
--
-- Useful for debugging and introspection.
getRegisteredKeywords :: KeywordRegistry -> [Text]
getRegisteredKeywords (KeywordRegistry m) = Map.keys m

-- | Helper to create a keyword definition
--
-- This is a convenience function that wraps the KeywordDefinition constructor
-- with a more ergonomic API.
mkKeywordDefinition
  :: Typeable a
  => Text                    -- ^ Keyword name
  -> KeywordScope            -- ^ Scope restriction
  -> CompileFunc a           -- ^ Compile function
  -> ValidateFunc a          -- ^ Validate function
  -> KeywordDefinition
mkKeywordDefinition = KeywordDefinition

-- | Create a simple keyword that doesn't need compilation
--
-- For keywords that just validate the instance directly without needing
-- a compilation phase, this helper creates a trivial compile function
-- that just passes through the keyword value.
mkSimpleKeyword
  :: Typeable a
  => Text                    -- ^ Keyword name
  -> KeywordScope            -- ^ Scope restriction
  -> (Value -> Either Text a)  -- ^ Parse keyword value
  -> (a -> Value -> [Text])    -- ^ Validate function
  -> KeywordDefinition
mkSimpleKeyword name scope parseValue validateValue =
  KeywordDefinition
    { keywordName = name
    , keywordScope = scope
    , keywordCompile = \val _schema _ctx -> parseValue val
    , keywordValidate = validateValue
    }
