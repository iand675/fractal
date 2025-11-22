{-# LANGUAGE OverloadedStrings #-}
-- | String keyword validation
--
-- Validates string constraint keywords: minLength, maxLength, pattern,
-- contentEncoding, and contentMediaType.
module Fractal.JsonSchema.Keywords.String
  ( validateStringConstraints
  ) where

import Fractal.JsonSchema.Types
import qualified Fractal.JsonSchema.Regex as Regex
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64

-- | Validate string constraints
--
-- Validates minLength, maxLength, pattern, and content encoding/media type keywords.
validateStringConstraints :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateStringConstraints ctx obj (String txt) =
  let validation = schemaValidation obj
      textLength = T.length txt
  in combineResults
    [ maybe (ValidationSuccess mempty) (\max' ->
        if fromIntegral textLength <= max'
          then ValidationSuccess mempty
          else validationFailure "maxLength" "String length exceeds maxLength"
      ) (validationMaxLength validation)
    , maybe (ValidationSuccess mempty) (\min' ->
        if fromIntegral textLength >= min'
          then ValidationSuccess mempty
          else validationFailure "minLength" "String length below minLength"
      ) (validationMinLength validation)
    , validatePattern txt validation
    , validateContent ctx txt validation
    ]
  where
    combineResults :: [ValidationResult] -> ValidationResult
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
      in case failures of
        [] -> ValidationSuccess mempty
        (e:es) -> ValidationFailure $ foldl (<>) e es

    validatePattern :: Text -> SchemaValidation -> ValidationResult
    validatePattern text schemaValidation = case validationPattern schemaValidation of
      Nothing -> ValidationSuccess mempty
      Just (Regex pattern) ->
        -- Use ecma262-regex to match pattern
        case Regex.compileRegex pattern of
          Right regex ->
            if Regex.matchRegex regex text
              then ValidationSuccess mempty
              else validationFailure "pattern" $ "String does not match pattern: " <> pattern
          Left err -> validationFailure "pattern" $ "Invalid regex pattern: " <> err

    validateContent :: ValidationContext -> Text -> SchemaValidation -> ValidationResult
    validateContent context text schemaValidation =
      let encoding = validationContentEncoding schemaValidation
          mediaType = validationContentMediaType schemaValidation
          config = contextConfig context
          shouldAssert = validationContentAssertion config
      in if shouldAssert
         then case (encoding, mediaType) of
           (Nothing, Nothing) -> ValidationSuccess mempty
           (Just enc, Nothing) -> validateEncoding enc text
           (Nothing, Just mt) -> validateMediaType mt text
           (Just enc, Just mt) ->
             -- First validate encoding, then decode and validate media type
             case validateEncoding enc text of
               ValidationFailure err -> ValidationFailure err
               ValidationSuccess _ ->
                 case decodeContent enc text of
                   Left err -> validationFailure "contentEncoding" err
                   Right decoded -> validateMediaType mt decoded
         else ValidationSuccess mempty  -- Content as annotation only

    validateEncoding :: Text -> Text -> ValidationResult
    validateEncoding "base64" text =
      case Base64.decode (TE.encodeUtf8 text) of
        Left _ -> validationFailure "contentEncoding" "Invalid base64 encoding"
        Right _ -> ValidationSuccess mempty
    validateEncoding _ _ =
      -- Unknown encoding - just pass (encodings are optional to support)
      ValidationSuccess mempty

    decodeContent :: Text -> Text -> Either Text Text
    decodeContent "base64" text =
      case Base64.decode (TE.encodeUtf8 text) of
        Left _ -> Left "Invalid base64 encoding"
        Right bytes -> Right (TE.decodeUtf8 bytes)
    decodeContent _ text = Right text  -- Unknown encoding, pass through

    validateMediaType :: Text -> Text -> ValidationResult
    validateMediaType "application/json" content =
      case Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 content)) of
        Left _ -> validationFailure "contentMediaType" "Invalid JSON document"
        Right (_ :: Value) -> ValidationSuccess mempty
    validateMediaType _ _ =
      -- Unknown media type - just pass (media types are optional to support)
      ValidationSuccess mempty

validateStringConstraints _ _ _ = ValidationSuccess mempty
