-- | JSON Schema parsing
module Fractal.OpenApi.JsonSchema.Parser
  ( -- * Parsing
    parseSchema
  , parseSchemaWithVersion
  , ParseError(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Fractal.OpenApi.JsonSchema.Types

-- | Parse error
data ParseError = ParseError
  { parseErrorMessage :: Text
  } deriving (Eq, Show)

-- | Parse a JSON Schema from a Value
--
-- TODO: Implement schema parsing
parseSchema :: Value -> Either ParseError Schema
parseSchema = error "parseSchema: not yet implemented"

-- | Parse with explicit version
--
-- TODO: Implement version-specific parsing
parseSchemaWithVersion :: JsonSchemaVersion -> Value -> Either ParseError Schema
parseSchemaWithVersion = error "parseSchemaWithVersion: not yet implemented"
