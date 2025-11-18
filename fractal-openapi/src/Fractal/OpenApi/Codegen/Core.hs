-- | Core codegen types and functions
module Fractal.OpenApi.Codegen.Core
  ( -- * Configuration
    CodegenConfig(..)
  , defaultCodegenConfig

  -- * Strategy
  , CodegenStrategy(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Code generation configuration
data CodegenConfig = CodegenConfig
  { codegenOmitNullable :: Bool
  , codegenGenerateLenses :: Bool
  , codegenFieldPrefixes :: Maybe Text
  } deriving (Eq, Show, Generic)

-- | Default configuration
defaultCodegenConfig :: CodegenConfig
defaultCodegenConfig = CodegenConfig
  { codegenOmitNullable = False
  , codegenGenerateLenses = False
  , codegenFieldPrefixes = Nothing
  }

-- | Code generation strategy
--
-- TODO: Implement strategy system
data CodegenStrategy = CodegenStrategy
  { strategyName :: Text
  } deriving (Eq, Show, Generic)
