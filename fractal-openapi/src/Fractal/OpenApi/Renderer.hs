-- | OpenAPI rendering
module Fractal.OpenApi.Renderer
  ( renderOpenApiSpec
  ) where

import Data.Aeson (Value)
import Fractal.OpenApi.Types

-- | Render an OpenAPI spec to JSON
--
-- TODO: Implement OpenAPI rendering
renderOpenApiSpec :: OpenApiSpec -> Value
renderOpenApiSpec = error "renderOpenApiSpec: not yet implemented"
