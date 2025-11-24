{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Pluggable keyword definition for the 'format' keyword
--
-- Wraps the existing format validation logic (in Fractal.JsonSchema.Keywords.Format)
-- into the pluggable keyword system.
module Fractal.JsonSchema.Keywords.FormatKeyword
  ( formatKeyword
  , compileFormat
  , FormatData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (ask)
import Data.Text (Text)
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Types 
  ( Format(..)
  , FormatBehavior(..)
  , ValidationConfig(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  )
import qualified Fractal.JsonSchema.Keywords.Format as FormatImpl

-- | Compiled data for format keyword
newtype FormatData = FormatData Format
  deriving (Show, Eq, Typeable)

-- | Compile the format keyword
compileFormat :: CompileFunc FormatData
compileFormat (String formatName) _schema _ctx =
  -- Parse format name into Format type
  Right $ FormatData $ parseFormatName formatName
  where
    parseFormatName "email" = Email
    parseFormatName "idn-email" = IDNEmail
    parseFormatName "uri" = URI
    parseFormatName "uri-reference" = URIRef
    parseFormatName "iri" = IRI
    parseFormatName "iri-reference" = IRIRef
    parseFormatName "ipv4" = IPv4
    parseFormatName "ipv6" = IPv6
    parseFormatName "uuid" = UUID
    parseFormatName "date-time" = DateTime
    parseFormatName "date" = Date
    parseFormatName "time" = Time
    parseFormatName "duration" = Duration
    parseFormatName "hostname" = Hostname
    parseFormatName "idn-hostname" = IDNHostname
    parseFormatName "json-pointer" = JSONPointerFormat
    parseFormatName "relative-json-pointer" = RelativeJSONPointerFormat
    parseFormatName "regex" = RegexFormat
    parseFormatName "uri-template" = URITemplate
    parseFormatName other = CustomFormat other

compileFormat _ _ _ = Left "format must be a string"

validateFormatKeyword :: ValidateFunc FormatData
validateFormatKeyword _recursiveValidator (FormatData format) _ctx (String txt) = do
  config <- ask
  let result = FormatImpl.validateFormatValue format txt
      assertionMode = case validationDialectFormatBehavior config of
        Just FormatAssertion -> True
        Just FormatAnnotation -> False
        Nothing -> validationFormatAssertion config
  pure $
    if assertionMode
      then result
      else case result of
             ValidationSuccess anns -> ValidationSuccess anns
             ValidationFailure _ -> ValidationSuccess mempty
validateFormatKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to strings

-- | Keyword definition for format
formatKeyword :: KeywordDefinition
formatKeyword = KeywordDefinition
  { keywordName = "format"
  , keywordCompile = compileFormat
  , keywordValidate = validateFormatKeyword
  , keywordNavigation = NoNavigation  -- No subschemas
  , keywordPostValidate = Nothing
  }

