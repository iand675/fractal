-- | OpenAPI parsing
module Fractal.OpenApi.Parser
  ( parseOpenApiSpec
  , ParseError(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Fractal.OpenApi.Types

-- | Parse error
data ParseError = ParseError
  { parseErrorMessage :: Text
  } deriving (Eq, Show)

-- | Parse an OpenAPI specification
--
-- TODO: Implement OpenAPI parsing
parseOpenApiSpec :: Value -> Either ParseError OpenApiSpec
parseOpenApiSpec = error "parseOpenApiSpec: not yet implemented"
