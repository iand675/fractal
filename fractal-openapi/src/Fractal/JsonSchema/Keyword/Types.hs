{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Core types for the pluggable keyword system
--
-- This module defines the types that allow custom keywords to be registered
-- and used in JSON Schema validation, following the hyperjump architecture
-- pattern of compile-then-validate.
module Fractal.JsonSchema.Keyword.Types
  ( -- * Keyword Definition
    KeywordDefinition(..)
  , KeywordScope(..)
  , CompileFunc
  , ValidateFunc
    -- * Keyword Registry
  , KeywordRegistry(..)
    -- * Compilation Context
  , CompilationContext(..)
  , CompiledKeyword(..)
  , SomeCompiledData(..)
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
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Types (Schema)

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
-- 1. The compiled data from the compile phase
-- 2. The instance value being validated
-- 3. A validation context (to be defined in Validator.Result module)
--
-- Returns validation errors (empty list = success), not a ValidationResult
-- to allow composition of multiple keyword validators.
type ValidateFunc a = a -> Value -> [Text]  -- TODO: Use proper ValidationError type

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
  , compiledValidate :: Value -> [Text]
    -- ^ Type-erased validate function (closed over compiled data)
  , compiledAdjacentData :: Map Text Value
    -- ^ Values from adjacent keywords accessed during compilation
  }

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
  deriving (Functor, Applicative, Monad)

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

-- | Definition of a custom keyword
--
-- This is the main registration point for custom keywords. Users provide:
-- - A unique name for the keyword
-- - A scope restriction (what schema types it applies to)
-- - A compile function that processes the keyword value at schema parse time
-- - A validate function that uses the compiled data to validate instances
data KeywordDefinition = forall a. Typeable a => KeywordDefinition
  { keywordName :: Text
    -- ^ Unique identifier for this keyword (e.g., "maxLength", "x-creditCard")
  , keywordScope :: KeywordScope
    -- ^ Which schema types this keyword can be used with
  , keywordCompile :: CompileFunc a
    -- ^ Compile phase: process keyword value and schema structure
  , keywordValidate :: ValidateFunc a
    -- ^ Validate phase: check instance against compiled keyword
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
