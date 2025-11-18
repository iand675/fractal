-- | OpenAPI types
module Fractal.OpenApi.Types
  ( -- * OpenAPI Spec
    OpenApiSpec(..)
  , PathItem(..)
  , Operation(..)
  ) where

import Data.Text (Text)
import Data.Map (Map)
import GHC.Generics (Generic)

-- | OpenAPI specification
--
-- TODO: Complete OpenAPI type definitions
data OpenApiSpec = OpenApiSpec
  { openApiVersion :: Text
  , openApiPaths :: Map Text PathItem
  } deriving (Eq, Show, Generic)

-- | Path item
data PathItem = PathItem
  { pathItemGet :: Maybe Operation
  , pathItemPost :: Maybe Operation
  } deriving (Eq, Show, Generic)

-- | Operation
data Operation = Operation
  { operationSummary :: Maybe Text
  , operationOperationId :: Maybe Text
  } deriving (Eq, Show, Generic)
