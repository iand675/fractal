-- | Aeson instance generation
module Fractal.OpenApi.Codegen.Aeson
  ( -- * Instance generation
    generateToJSON
  , generateFromJSON
  ) where

import Language.Haskell.TH
import Fractal.OpenApi.JsonSchema.Types (Schema)

-- | Generate ToJSON instance
--
-- TODO: Implement ToJSON generation
generateToJSON :: Name -> Schema -> Q [Dec]
generateToJSON = error "generateToJSON: not yet implemented"

-- | Generate FromJSON instance
--
-- TODO: Implement FromJSON generation
generateFromJSON :: Name -> Schema -> Q [Dec]
generateFromJSON = error "generateFromJSON: not yet implemented"
