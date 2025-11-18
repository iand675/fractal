-- | JSON Schema rendering
module Fractal.OpenApi.JsonSchema.Renderer
  ( -- * Rendering
    renderSchema
  , renderSchemaMinimal
  ) where

import Data.Aeson (Value)
import Fractal.OpenApi.JsonSchema.Types

-- | Render a schema to JSON
--
-- TODO: Implement schema rendering
renderSchema :: Schema -> Value
renderSchema = error "renderSchema: not yet implemented"

-- | Render with minimal representation
--
-- TODO: Implement minimal rendering
renderSchemaMinimal :: Schema -> Value
renderSchemaMinimal = error "renderSchemaMinimal: not yet implemented"
