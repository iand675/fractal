-- | Code generation
--
-- This module provides the main API for code generation from JSON Schema and OpenAPI.
module Fractal.OpenApi.Codegen
  ( -- * Re-exports
    module Fractal.OpenApi.Codegen.Core
  , module Fractal.OpenApi.Codegen.TH
  , module Fractal.OpenApi.Codegen.Strategy
  ) where

import Fractal.OpenApi.Codegen.Core
import Fractal.OpenApi.Codegen.TH
import Fractal.OpenApi.Codegen.Strategy
