{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Core types for the pluggable keyword system
--
-- This module defines the types that allow custom keywords to be registered
-- and used in JSON Schema validation, following the hyperjump architecture
-- pattern of compile-then-validate.
module Fractal.JsonSchema.Keyword.Types
  ( -- * Keyword Definition
    KeywordDefinition(..)
  , KeywordScope(..)
  , KeywordNavigation(..)
  , CompileFunc
  , ValidateFunc
    -- * Keyword Registry
  , KeywordRegistry(..)
    -- * Compilation Context
  , CompilationContext(..)
  , CompiledKeyword(..)
  , SomeCompiledData(..)
    -- * Validation Context
  , ValidationContext'(..)
    -- * Monadic Compilation
  , CompileM
  , CompilationState(..)
  , runCompileM
  , getCompilationState
  , modifyCompilationState
  , liftEither
  ) where

import Control.Monad.Trans.State.Strict (StateT, runStateT, get, modify')
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (Reader)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Types (Schema, ValidationResult, ValidationConfig)

-- | Scope restriction for keywords - defines which schema types they apply to
data KeywordScope
  = AnyScope              -- ^ Keyword applies to any schema type
  | ObjectOnly            -- ^ Keyword only applies to object schemas
  | ArrayOnly             -- ^ Keyword only applies to array schemas
  | StringOnly            -- ^ Keyword only applies to string schemas
  | NumericOnly           -- ^ Keyword only applies to numeric schemas
  | CustomScope (Value -> Bool)  -- ^ Custom scope predicate

instance Show KeywordScope where
  show AnyScope = "AnyScope"
  show ObjectOnly = "ObjectOnly"
  show ArrayOnly = "ArrayOnly"
  show StringOnly = "StringOnly"
  show NumericOnly = "NumericOnly"
  show (CustomScope _) = "CustomScope{<function>}"

-- | Compilation context provided to keyword compile functions
--
-- This follows hyperjump's pattern of giving compile functions access to:
-- - The schema registry for resolving references
-- - A function to resolve $ref URIs
-- - The current schema being compiled
-- - The parent schema path for error reporting
-- - The keyword registry for monadic compilation (optional)
data CompilationContext = CompilationContext
  { contextRegistry :: Map Text Schema
    -- ^ Access to the full schema registry (keyed by URI as Text)
  , contextResolveRef :: Text -> Either Text Schema
    -- ^ Function to resolve $ref URIs to schemas
  , contextCurrentSchema :: Schema
    -- ^ The schema currently being compiled
  , contextParentPath :: [Text]
    -- ^ Path from root to current schema (for error messages)
  , contextKeywordRegistry :: KeywordRegistry
    -- ^ Keyword registry for monadic compilation
    -- This allows compile functions to access adjacent keywords on-demand
  }

-- | Compile function signature: processes keyword value at schema parse time
--
-- The compile function receives:
-- 1. The keyword's value from the schema (e.g., for "maxLength": 10, this is Number 10)
-- 2. The full schema containing this keyword
-- 3. A compilation context with registry access and resolveRef
--
-- Returns compiled data that will be passed to the validate function,
-- or an error message if compilation fails.
type CompileFunc a = Value -> Schema -> CompilationContext -> Either Text a

-- | Validate function signature: validates instance data using compiled keyword
--
-- The validate function receives:
-- 1. A recursive validator for subschemas (Schema -> Value -> ValidationResult)
-- 2. The compiled data from the compile phase
-- 3. A validation context (instance and schema paths)
-- 4. The instance value being validated
--
-- Returns validation errors in a Reader monad with ValidationConfig as the environment.
-- This allows keywords to access configuration (e.g., format assertion vs annotation mode)
-- without explicit parameter passing.
--
-- For simple keywords that don't need recursive validation (e.g., type, enum),
-- the recursive validator parameter can be ignored.
type ValidateFunc a = 
  (Schema -> Value -> ValidationResult)  -- ^ Recursive validator for subschemas
  -> a                                    -- ^ Compiled data
  -> ValidationContext'                  -- ^ Validation context (paths)
  -> Value                                -- ^ Instance value
  -> Reader ValidationConfig [Text]      -- ^ Validation errors in Reader monad

-- | Validation context for keyword validators
--
-- Provides path information for error reporting during validation.
-- This is simpler than the main ValidationContext and used specifically
-- for keyword validation.
data ValidationContext' = ValidationContext'
  { kwContextInstancePath :: [Text]
    -- ^ Path to current location in instance (for error reporting)
  , kwContextSchemaPath :: [Text]
    -- ^ Path to current location in schema (for error reporting)
  }
  deriving (Show, Eq)

-- | Existentially quantified compiled keyword data
--
-- Allows storing compiled data of different types in the same collection,
-- while preserving type safety via Typeable.
data SomeCompiledData = forall a. Typeable a => SomeCompiledData a

-- | Result of compiling a keyword
data CompiledKeyword = CompiledKeyword
  { compiledKeywordName :: Text
    -- ^ Name of the keyword
  , compiledData :: SomeCompiledData
    -- ^ The compiled data (existentially quantified)
  , compiledValidate :: (Schema -> Value -> ValidationResult) -> ValidationContext' -> Value -> Reader ValidationConfig [Text]
    -- ^ Type-erased validate function (closed over compiled data)
    -- Takes: recursive validator, validation context, value
    -- Returns validation errors in Reader monad with ValidationConfig
    -- Returns: list of validation errors
  , compiledAdjacentData :: Map Text Value
    -- ^ Values from adjacent keywords accessed during compilation
  }

instance Show CompiledKeyword where
  show (CompiledKeyword name _ _ _) = "CompiledKeyword{" ++ show name ++ "}"

instance Eq CompiledKeyword where
  (CompiledKeyword name1 _ _ _) == (CompiledKeyword name2 _ _ _) = name1 == name2

-- ============================================================================
-- Monadic Compilation
-- ============================================================================

-- | Compilation state for monadic keyword compilation
--
-- Tracks compiled keywords and compilation progress to support:
-- - Lazy, on-demand compilation of adjacent keywords
-- - Memoization of compiled results
-- - Circular dependency detection
data CompilationState = CompilationState
  { stateCompiled :: Map Text CompiledKeyword
    -- ^ Keywords that have been compiled so far
  , stateCompiling :: Set Text
    -- ^ Keywords currently being compiled (for cycle detection)
  , stateSchema :: Schema
    -- ^ The schema currently being compiled
  , stateContext :: CompilationContext
    -- ^ Additional compilation context
  }

-- | Monadic compilation context
--
-- Provides stateful keyword compilation with:
-- - Access to already-compiled keywords
-- - Ability to request compilation of adjacent keywords
-- - Automatic memoization and cycle detection
newtype CompileM a = CompileM
  { unCompileM :: StateT CompilationState (Either Text) a
  }
  deriving newtype (Functor, Applicative, Monad)

-- | Run a monadic compilation
runCompileM :: CompileM a -> CompilationState -> Either Text (a, CompilationState)
runCompileM (CompileM action) state = runStateT action state

-- | Get the current compilation state
getCompilationState :: CompileM CompilationState
getCompilationState = CompileM get

-- | Modify the compilation state
modifyCompilationState :: (CompilationState -> CompilationState) -> CompileM ()
modifyCompilationState f = CompileM $ modify' f

-- | Lift an Either into the compilation monad
liftEither :: Either Text a -> CompileM a
liftEither = CompileM . lift

-- | Navigation modes for keywords that contain subschemas
--
-- Defines how JSON Pointer navigation should work for a keyword
data KeywordNavigation
  = NoNavigation
    -- ^ Keyword doesn't contain subschemas (e.g., type, enum, const)
  
  | SingleSchema (Schema -> Maybe Schema)
    -- ^ Single subschema (e.g., not, contains, propertyNames)
  
  | SchemaMap (Schema -> Maybe (Map Text Schema))
    -- ^ Map of subschemas keyed by Text (e.g., properties, dependentSchemas)
    -- Next pointer segment is used as map key
  
  | SchemaArray (Schema -> Maybe [Schema])
    -- ^ Array of subschemas (e.g., allOf, anyOf, oneOf)
    -- Next pointer segment must be numeric index
  
  | CustomNavigation (Schema -> Text -> [Text] -> Maybe (Schema, [Text]))
    -- ^ Custom navigation logic for complex cases (e.g., items with dual behavior)
    -- Takes: parent schema, current segment, remaining segments
    -- Returns: (resolved schema, remaining segments to process)

-- | Definition of a custom keyword
--
-- This is the main registration point for custom keywords. Users provide:
-- - A unique name for the keyword
-- - A scope restriction (what schema types it applies to)
-- - A compile function that processes the keyword value at schema parse time
-- - A validate function that uses the compiled data to validate instances
-- - Optional navigation support for subschema resolution
data KeywordDefinition = forall a. Typeable a => KeywordDefinition
  { keywordName :: Text
    -- ^ Unique identifier for this keyword (e.g., "maxLength", "x-creditCard")
  , keywordScope :: KeywordScope
    -- ^ Which schema types this keyword can be used with
  , keywordCompile :: CompileFunc a
    -- ^ Compile phase: process keyword value and schema structure
  , keywordValidate :: ValidateFunc a
    -- ^ Validate phase: check instance against compiled keyword
  , keywordNavigation :: KeywordNavigation
    -- ^ How to navigate into subschemas (if any)
  }

-- | Registry of custom keywords
--
-- Maps keyword names to their definitions. Used during schema parsing
-- and validation to look up custom keyword handlers.
newtype KeywordRegistry = KeywordRegistry
  { keywordMap :: Map Text KeywordDefinition
  }

instance Show KeywordRegistry where
  show (KeywordRegistry m) =
    "KeywordRegistry{keywords=" ++ show (Map.keys m) ++ "}"
