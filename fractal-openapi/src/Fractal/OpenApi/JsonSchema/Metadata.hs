-- | JSON Schema metadata extraction
module Fractal.OpenApi.JsonSchema.Metadata
  ( -- * Metadata
    extractMetadata
  , SchemaMetadata(..)
  ) where

import Data.Text (Text)
import Fractal.OpenApi.JsonSchema.Types

-- | Schema metadata
data SchemaMetadata = SchemaMetadata
  { metadataTitle :: Maybe Text
  , metadataDescription :: Maybe Text
  } deriving (Eq, Show)

-- | Extract metadata from a schema
--
-- TODO: Implement metadata extraction
extractMetadata :: Schema -> SchemaMetadata
extractMetadata = error "extractMetadata: not yet implemented"
