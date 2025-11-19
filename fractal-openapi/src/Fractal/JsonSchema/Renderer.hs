-- | JSON Schema rendering
--
-- Render Schema AST back to JSON/YAML format.
module Fractal.JsonSchema.Renderer
  ( renderSchema
  , renderSchemaMinimal
  , renderSchemaCanonical
  ) where

import Fractal.JsonSchema.Types
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Render schema to JSON Value
renderSchema :: Schema -> Value
renderSchema schema = case schemaCore schema of
  BooleanSchema b -> Bool b
  ObjectSchema obj -> renderSchemaObject schema obj

-- | Render schema omitting default values
renderSchemaMinimal :: Schema -> Value
renderSchemaMinimal = renderSchema  -- TODO: Implement minimal rendering

-- | Render schema in canonical form (normalized)
renderSchemaCanonical :: Schema -> Value
renderSchemaCanonical = renderSchema  -- TODO: Implement canonical form

-- | Render schema object to JSON
renderSchemaObject :: Schema -> SchemaObject -> Value
renderSchemaObject schema obj = object $ concat
  [ maybe [] (\v -> ["$schema" .= v]) (schemaVersion schema)
  , maybe [] (\id' -> ["$id" .= id']) (schemaId schema)
  , maybe [] (\ref -> ["$ref" .= ref]) (schemaRef obj)
  , maybe [] (\title -> ["title" .= title]) (annotationTitle $ schemaAnnotations obj)
  , maybe [] (\desc -> ["description" .= desc]) (annotationDescription $ schemaAnnotations obj)
  -- TODO: Add all other keywords
  ]

