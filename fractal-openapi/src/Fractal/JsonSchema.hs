-- | Fractal JSON Schema Library
--
-- A comprehensive, type-safe library for JSON Schema validation and code generation.
--
-- = Quick Start
--
-- > import Fractal.JsonSchema
-- > import Data.Aeson
-- >
-- > -- Create a simple schema
-- > let schema = Schema
-- >       { schemaCore = ObjectSchema ...
-- >       , ...
-- >       }
-- >
-- > -- Validate some JSON
-- > case validateValue defaultValidationConfig schema someValue of
-- >   ValidationSuccess _ -> putStrLn "Valid!"
-- >   ValidationFailure errs -> print errs
--
-- = Core Modules
--
-- * "Fractal.JsonSchema.Types" - Core type definitions
-- * "Fractal.JsonSchema.Parser" - Parse schemas from JSON/YAML
-- * "Fractal.JsonSchema.Validator" - Validate values against schemas
-- * "Fractal.JsonSchema.Renderer" - Render schemas to JSON/YAML
-- * "Fractal.JsonSchema.Vocabulary" - Extensible vocabulary system
module Fractal.JsonSchema
  ( -- * Core Types
    Schema(..)
  , SchemaCore(..)
  , SchemaObject(..)
  , JsonSchemaVersion(..)
  , SchemaType(..)
  
    -- * Validation
  , validateValue
  , ValidationResult(..)
  , ValidationError(..)
  , ValidationConfig(..)
  , defaultValidationConfig
  , strictValidationConfig
  
    -- * Parsing
  , parseSchema
  , parseSchemaWithVersion
  , ParseError(..)
  
    -- * Rendering
  , renderSchema
  
    -- * Re-exports from Types
  , module Fractal.JsonSchema.Types
  ) where

import Fractal.JsonSchema.Types
import Fractal.JsonSchema.Parser (parseSchema, parseSchemaWithVersion, ParseError(..))
import Fractal.JsonSchema.Validator (validateValue, ValidationConfig(..), defaultValidationConfig, strictValidationConfig)
import Fractal.JsonSchema.Renderer (renderSchema)

