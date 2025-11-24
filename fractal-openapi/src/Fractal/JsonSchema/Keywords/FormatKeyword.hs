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
import Data.Text (Text)
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Types 
  ( Format(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..), KeywordScope(..)
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

-- | Validate format using the pluggable keyword system
--
-- NOTE: This is a placeholder implementation. Full format validation requires
-- access to ValidationConfig (for format assertion vs annotation mode), which
-- is not available in the pluggable keyword system's ValidateFunc. For now,
-- format is treated as annotation-only (non-asserting).
--
-- TODO (fractal-b8g): When the hardcoded validation dispatch is replaced with
-- keyword registry dispatch, add ValidationConfig to ValidationContext' so that
-- format can properly support both assertion and annotation modes based on the
-- dialect configuration or global formatAssertion setting.
validateFormatKeyword :: ValidateFunc FormatData
validateFormatKeyword _recursiveValidator (FormatData format) _ctx (String txt) =
  -- For now, format is always annotation-only (spec-compliant for 2019-09+)
  -- Full assertion mode will be implemented in fractal-b8g
  let result = FormatImpl.validateFormatValue format txt
  in case result of
    ValidationSuccess anns -> pure (ValidationSuccess anns)  -- Format valid (annotations preserved)
    ValidationFailure _ -> pure (ValidationSuccess mempty)    -- Format invalid but annotation mode only

validateFormatKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to strings

-- | Keyword definition for format
formatKeyword :: KeywordDefinition
formatKeyword = KeywordDefinition
  { keywordName = "format"
  , keywordScope = AnyScope
  , keywordCompile = compileFormat
  , keywordValidate = validateFormatKeyword
  , keywordNavigation = NoNavigation  -- No subschemas
  }

