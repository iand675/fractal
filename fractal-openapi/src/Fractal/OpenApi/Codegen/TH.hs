{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell code generation
module Fractal.OpenApi.Codegen.TH
  ( -- * Schema-based generation
    deriveJSONSchema
  , deriveJSONSchemaWith

  -- * OpenAPI-based generation
  , deriveOpenApiTypes
  ) where

import Language.Haskell.TH
import Fractal.OpenApi.JsonSchema.Types (Schema)
import Fractal.OpenApi.Types (OpenApiSpec)
import Fractal.OpenApi.Codegen.Core (CodegenConfig)

-- | Generate Haskell types from a JSON Schema
--
-- TODO: Implement Template Haskell codegen
deriveJSONSchema :: Schema -> Q [Dec]
deriveJSONSchema = error "deriveJSONSchema: not yet implemented"

-- | Generate with custom configuration
--
-- TODO: Implement configurable codegen
deriveJSONSchemaWith :: CodegenConfig -> Schema -> Q [Dec]
deriveJSONSchemaWith = error "deriveJSONSchemaWith: not yet implemented"

-- | Generate types from OpenAPI spec
--
-- TODO: Implement OpenAPI type generation
deriveOpenApiTypes :: OpenApiSpec -> Q [Dec]
deriveOpenApiTypes = error "deriveOpenApiTypes: not yet implemented"
