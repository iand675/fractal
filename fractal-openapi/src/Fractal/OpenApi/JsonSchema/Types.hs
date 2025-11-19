{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Core JSON Schema types
--
-- This module defines the AST for JSON Schema representations supporting
-- all versions from draft-04 through 2020-12.
--
-- See SPEC.md for detailed design documentation.
module Fractal.OpenApi.JsonSchema.Types
  ( -- * Schema Versions
    JsonSchemaVersion(..)

  -- * Core Schema Types
  , Schema(..)
  , SchemaCore(..)
  , SchemaObject(..)
  , SchemaType(..)

  -- * Validation
  , SchemaValidation(..)
  , Format(..)

  -- * Annotations
  , SchemaAnnotations(..)
  , CodegenAnnotations(..)
  , NewtypeSpec(..)

  -- * References
  , Reference(..)
  , JSONPointer

  -- * Utilities
  , OneOrMany(..)
  ) where

import Data.Aeson (Value)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Scientific (Scientific)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | JSON Schema version
data JsonSchemaVersion
  = Draft04
  | Draft06
  | Draft07
  | Draft201909
  | Draft202012
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Main Schema type
--
-- This represents a complete JSON Schema with all metadata and validation rules.
data Schema = Schema
  { schemaVersion :: Maybe JsonSchemaVersion
  , schemaId :: Maybe Text -- URI
  , schemaCore :: SchemaCore
  , schemaValidation :: SchemaValidation
  , schemaAnnotations :: SchemaAnnotations
  , schemaExtensions :: Map Text Value
  } deriving (Eq, Show, Generic)

-- | Core schema structure
data SchemaCore
  = BooleanSchema Bool
  | ObjectSchema SchemaObject
  deriving (Eq, Show, Generic)

-- | Object schema with all keywords
data SchemaObject = SchemaObject
  { schemaType :: Maybe (OneOrMany SchemaType)
  , schemaEnum :: Maybe [Value]
  , schemaConst :: Maybe Value

  -- Composition
  , schemaAllOf :: Maybe [Schema]
  , schemaAnyOf :: Maybe [Schema]
  , schemaOneOf :: Maybe [Schema]
  , schemaNot :: Maybe Schema
  } deriving (Eq, Show, Generic)

-- | JSON Schema types
data SchemaType
  = NullType
  | BooleanType
  | ObjectType
  | ArrayType
  | NumberType
  | StringType
  | IntegerType
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Validation keywords
data SchemaValidation = SchemaValidation
  { -- Numeric
    validationMultipleOf :: Maybe Scientific
  , validationMaximum :: Maybe Scientific
  , validationMinimum :: Maybe Scientific

  -- String
  , validationMaxLength :: Maybe Natural
  , validationMinLength :: Maybe Natural
  , validationPattern :: Maybe Text
  , validationFormat :: Maybe Format

  -- Array
  , validationMaxItems :: Maybe Natural
  , validationMinItems :: Maybe Natural

  -- Object
  , validationProperties :: Maybe (Map Text Schema)
  , validationRequired :: Maybe (Set Text)
  } deriving (Eq, Show, Generic)

-- | Format annotations
data Format
  = DateTime | Date | Time
  | Email | IDNEmail
  | Hostname | IDNHostname
  | IPv4 | IPv6
  | URI | URIReference
  | UUID
  | CustomFormat Text
  deriving (Eq, Show, Generic)

-- | Schema annotations
data SchemaAnnotations = SchemaAnnotations
  { annotationTitle :: Maybe Text
  , annotationDescription :: Maybe Text
  , annotationDefault :: Maybe Value
  , annotationExamples :: [Value]
  , annotationCodegen :: Maybe CodegenAnnotations
  } deriving (Eq, Show, Generic)

-- | Code generation annotations
data CodegenAnnotations = CodegenAnnotations
  { codegenTypeName :: Maybe Text
  , codegenNewtype :: Maybe NewtypeSpec
  , codegenFieldMapping :: Maybe (Map Text Text)
  } deriving (Eq, Show, Generic)

-- | Newtype specification
data NewtypeSpec = NewtypeSpec
  { newtypeConstructor :: Text
  , newtypeModule :: Maybe Text
  , newtypeValidation :: Maybe Text
  , newtypeInstances :: [Text]
  } deriving (Eq, Show, Generic)

-- | Schema reference
data Reference = Reference
  { referenceURI :: Text
  , referencePointer :: Maybe JSONPointer
  } deriving (Eq, Show, Generic)

-- | JSON Pointer
type JSONPointer = Text

-- | One or many values
data OneOrMany a
  = One a
  | Many [a]
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
