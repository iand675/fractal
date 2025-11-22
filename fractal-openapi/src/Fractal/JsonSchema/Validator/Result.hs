{-# LANGUAGE ExistentialQuantification #-}

-- | Validation result types with typed annotation support
--
-- This module provides extensible validation results that go beyond
-- simple Either/Boolean to support typed annotations that can be
-- inspected programmatically.
module Fractal.JsonSchema.Validator.Result
  ( -- * Validation Result
    ValidationResult(..)
  , ValidationError(..)
  , ValidationErrorTree(..)
    -- * Annotations
  , AnnotationCollection(..)
  , SomeAnnotation(..)
  , emptyAnnotationCollection
  , addAnnotation
  , getAnnotations
  , getAnnotationsOfType
    -- * Output Formats
  , OutputFormat(..)
  ) where

import Data.Aeson (Value)
import Data.Dynamic (Dynamic, Typeable, toDyn, fromDynamic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Typeable (TypeRep, typeOf)
import Data.Proxy (Proxy(..))

-- | JSON Pointer path type (e.g., "/properties/name")
type JSONPointer = Text

-- | Schema path type (e.g., "/properties/name/type")
type SchemaPath = Text

-- | A single validation error
data ValidationError = ValidationError
  { errorMessage :: Text
    -- ^ Human-readable error message
  , errorSchemaPath :: SchemaPath
    -- ^ Path in the schema where validation failed
  , errorInstancePath :: JSONPointer
    -- ^ Path in the instance where validation failed
  , errorKeyword :: Text
    -- ^ Keyword that produced this error
  }
  deriving (Show, Eq)

-- | Tree structure for validation errors
--
-- Represents the hierarchy of validation failures, useful for
-- structured error reporting with allOf/anyOf/oneOf.
data ValidationErrorTree
  = ErrorLeaf ValidationError
    -- ^ Single error
  | ErrorBranch Text [ValidationErrorTree]
    -- ^ Multiple errors under a context (e.g., "allOf[0]")
  deriving (Show, Eq)

-- | Existentially quantified annotation
--
-- Allows storing annotations of any type while preserving type information
-- via Typeable for later extraction.
data SomeAnnotation = forall a. Typeable a => SomeAnnotation
  { annotationValue :: a
    -- ^ The annotation value (existentially quantified)
  , annotationType :: TypeRep
    -- ^ Runtime type representation for extraction
  }

instance Show SomeAnnotation where
  show (SomeAnnotation _ t) = "SomeAnnotation{type=" ++ show t ++ "}"

-- | Collection of annotations produced during validation
--
-- Maps JSON Pointer paths to lists of annotations produced at that location.
-- Multiple annotations can exist at the same path (e.g., "title" and "default").
data AnnotationCollection = AnnotationCollection
  { annotationsByPath :: Map JSONPointer [SomeAnnotation]
    -- ^ Annotations indexed by their JSON Pointer path
  }
  deriving (Show)

-- | Empty annotation collection
emptyAnnotationCollection :: AnnotationCollection
emptyAnnotationCollection = AnnotationCollection
  { annotationsByPath = Map.empty
  }

-- | Add an annotation at a specific path
addAnnotation :: Typeable a => JSONPointer -> a -> AnnotationCollection -> AnnotationCollection
addAnnotation path value (AnnotationCollection m) =
  let annotation = SomeAnnotation value (typeOf value)
      existing = Map.findWithDefault [] path m
  in AnnotationCollection (Map.insert path (existing ++ [annotation]) m)

-- | Get all annotations at a specific path
getAnnotations :: JSONPointer -> AnnotationCollection -> [SomeAnnotation]
getAnnotations path (AnnotationCollection m) =
  Map.findWithDefault [] path m

-- | Get annotations of a specific type across all paths
--
-- Uses Typeable to extract annotations matching the requested type.
-- Returns (path, value) pairs for all matching annotations.
getAnnotationsOfType :: forall a. Typeable a => AnnotationCollection -> [(JSONPointer, a)]
getAnnotationsOfType (AnnotationCollection m) =
  let targetType = typeOf (undefined :: a)
  in [ (path, value)
     | (path, annotations) <- Map.toList m
     , SomeAnnotation val ty <- annotations
     , ty == targetType
     , Just value <- [fromDynamic (toDyn val)]
     ]

-- | Output format for validation results
--
-- Corresponds to JSON Schema specification output formats:
-- - FLAG: Boolean only (valid/invalid)
-- - BASIC: Boolean + flat error list
-- - DETAILED: Boolean + error tree + annotations
data OutputFormat
  = FlagFormat
    -- ^ Return only boolean (valid/invalid)
  | BasicFormat
    -- ^ Return boolean and flat list of errors
  | DetailedFormat
    -- ^ Return boolean, error tree, and annotations
  deriving (Show, Eq)

-- | Result of validation with errors and annotations
--
-- This is the main result type that replaces the simple Either approach.
-- It provides:
-- - Boolean validity status
-- - Structured error information
-- - Typed annotations for programmatic access
data ValidationResult = ValidationResult
  { resultValid :: Bool
    -- ^ Overall validation status
  , resultErrors :: ValidationErrorTree
    -- ^ Structured error tree (empty if valid)
  , resultAnnotations :: AnnotationCollection
    -- ^ Collected annotations (if enabled)
  }
  deriving (Show)
