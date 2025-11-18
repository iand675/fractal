-- | OpenAPI validation
module Fractal.OpenApi.Validator
  ( validateOpenApiSpec
  , ValidationError(..)
  ) where

import Data.Text (Text)
import Fractal.OpenApi.Types

-- | Validation error
data ValidationError = ValidationError
  { errorMessage :: Text
  } deriving (Eq, Show)

-- | Validate an OpenAPI specification
--
-- TODO: Implement OpenAPI validation
validateOpenApiSpec :: OpenApiSpec -> Either ValidationError ()
validateOpenApiSpec = error "validateOpenApiSpec: not yet implemented"
