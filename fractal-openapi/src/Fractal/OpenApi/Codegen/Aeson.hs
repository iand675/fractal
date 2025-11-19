{-# LANGUAGE TemplateHaskell #-}

-- | Aeson instance generation
module Fractal.OpenApi.Codegen.Aeson
  ( -- * Aeson Instance Generation
    generateFromJSONInstance
  , generateToJSONInstance
  , generateAesonInstances
  
    -- * Options
  , AesonOptions(..)
  , defaultAesonOptions
  ) where

import Fractal.OpenApi.Codegen.Core
import Fractal.OpenApi.Codegen.Strategy
import Language.Haskell.TH
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

-- | Options for Aeson instance generation
data AesonOptions = AesonOptions
  { aesonFieldLabelModifier :: Text -> Text
    -- ^ Modify field labels for JSON encoding/decoding
  
  , aesonConstructorTagModifier :: Text -> Text
    -- ^ Modify constructor tags for sum types
  
  , aesonOmitNothingFields :: Bool
    -- ^ Omit Nothing fields from JSON
  
  , aesonRejectUnknownFields :: Bool
    -- ^ Reject unknown fields during parsing
  }

-- | Default Aeson options (pass-through field names)
defaultAesonOptions :: AesonOptions
defaultAesonOptions = AesonOptions
  { aesonFieldLabelModifier = id
  , aesonConstructorTagModifier = id
  , aesonOmitNothingFields = False
  , aesonRejectUnknownFields = False
  }

-- | Generate both FromJSON and ToJSON instances
generateAesonInstances :: GeneratedType -> Q [Dec]
generateAesonInstances genType = do
  fromJSON <- generateFromJSONInstance genType
  toJSON <- generateToJSONInstance genType
  pure $ fromJSON ++ toJSON

-- | Generate a FromJSON instance for a generated type
generateFromJSONInstance :: GeneratedType -> Q [Dec]
generateFromJSONInstance genType = do
  let typeName = mkName $ T.unpack $ genTypeName genType
      constructors = genTypeConstructors genType
  
  -- For now, use default generic instances
  -- Future: Add custom parsing with validation
  [d|
    instance FromJSON $(conT typeName)
    |]

-- | Generate a ToJSON instance for a generated type
generateToJSONInstance :: GeneratedType -> Q [Dec]
generateToJSONInstance genType = do
  let typeName = mkName $ T.unpack $ genTypeName genType
  
  -- For now, use default generic instances
  [d|
    instance ToJSON $(conT typeName)
    |]
