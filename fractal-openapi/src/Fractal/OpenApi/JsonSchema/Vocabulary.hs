-- | JSON Schema vocabulary system
module Fractal.OpenApi.JsonSchema.Vocabulary
  ( -- * Vocabulary
    Vocabulary(..)
  ) where

import Data.Text (Text)

-- | Vocabulary definition
--
-- TODO: Implement vocabulary system
data Vocabulary = Vocabulary
  { vocabularyURI :: Text
  } deriving (Eq, Show)
