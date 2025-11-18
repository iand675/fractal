-- | Code generation strategies
module Fractal.OpenApi.Codegen.Strategy
  ( -- * Strategies
    defaultStrategy
  , newtypeStrategy
  , sumTypeStrategy
  ) where

import Fractal.OpenApi.Codegen.Core (CodegenStrategy(..))
import Data.Text (Text)

-- | Default code generation strategy
defaultStrategy :: CodegenStrategy
defaultStrategy = CodegenStrategy "default"

-- | Newtype generation strategy
newtypeStrategy :: CodegenStrategy
newtypeStrategy = CodegenStrategy "newtype"

-- | Sum type generation strategy
sumTypeStrategy :: CodegenStrategy
sumTypeStrategy = CodegenStrategy "sum-type"
