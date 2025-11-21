{-# LANGUAGE TemplateHaskell #-}
-- | Embedded JSON Schema metaschemas
--
-- This module provides access to the standard JSON Schema metaschemas
-- embedded at compile time using file-embed. This ensures the library
-- is self-contained and doesn't require network access for standard schemas.
module Fractal.JsonSchema.EmbeddedMetaschemas
  ( standardMetaschemaLoader
  , embeddedMetaschemas
  , lookupEmbeddedMetaschema
  , embeddedMetaschemaURIs  -- Debug export
  , embeddedFiles  -- Debug export
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedDir)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (eitherDecode)
import System.FilePath ((</>), takeFileName, dropExtension)

import Fractal.JsonSchema.Types (Schema, ReferenceLoader)
import Fractal.JsonSchema.Parser (parseSchema)

-- | All embedded metaschemas as raw ByteStrings
-- Maps from file path to content
embeddedFiles :: [(FilePath, ByteString)]
embeddedFiles = $(embedDir "data/metaschemas")

-- | Parse and cache all embedded metaschemas
-- Maps from JSON Schema URI to parsed Schema
embeddedMetaschemas :: Map Text Schema
embeddedMetaschemas = Map.fromList $ concatMap parseMetaschema embeddedFiles
  where
    parseMetaschema :: (FilePath, ByteString) -> [(Text, Schema)]
    parseMetaschema (path, content) =
      case eitherDecode (BL.fromStrict content) of
        Left _err -> []  -- Skip invalid JSON
        Right val ->
          case parseSchema val of
            Left _err -> []  -- Skip unparseable schemas
            Right schema ->
              -- Extract URI from the schema and create mapping
              let uri = uriFromPath path
              in [(uri, schema)]

    -- Convert file path to JSON Schema URI
    -- Path format: 2020-12/meta-core.json or 2019-09/schema.json
    uriFromPath :: FilePath -> Text
    uriFromPath path =
      let parts = splitPath path
          fileName = takeFileName path
          baseSchemaName = dropExtension fileName
      in case parts of
           -- Main schema files (2019-09 and later use https)
           [version, file] | version == "2020-12" && file == "schema.json" ->
             "https://json-schema.org/draft/2020-12/schema"
           [version, file] | version == "2019-09" && file == "schema.json" ->
             "https://json-schema.org/draft/2019-09/schema"
           -- Older drafts use http (not https) and different path format
           [version, file] | version == "draft-07" && file == "schema.json" ->
             "http://json-schema.org/draft-07/schema"
           [version, file] | version == "draft-06" && file == "schema.json" ->
             "http://json-schema.org/draft-06/schema"
           [version, file] | version == "draft-04" && file == "schema.json" ->
             "http://json-schema.org/draft-04/schema"
           -- Vocabulary metaschemas with "meta-" prefix (2019-09+)
           [version, _] | version == "2020-12" && "meta-" `isPrefix` baseSchemaName ->
             "https://json-schema.org/draft/2020-12/meta/" <> T.pack (drop 5 baseSchemaName)
           [version, _] | version == "2019-09" && "meta-" `isPrefix` baseSchemaName ->
             "https://json-schema.org/draft/2019-09/meta/" <> T.pack (drop 5 baseSchemaName)
           _ -> T.pack path

    -- Split path by /
    splitPath :: FilePath -> [FilePath]
    splitPath = filter (not . null) . splitBy '/'

    -- Split by character
    splitBy :: Char -> String -> [String]
    splitBy _ [] = []
    splitBy c s =
      let (chunk, rest) = break (== c) s
      in chunk : case rest of
           [] -> []
           (_:xs) -> splitBy c xs

    -- Check if string starts with prefix
    isPrefix :: String -> String -> Bool
    isPrefix [] _ = True
    isPrefix _ [] = False
    isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

-- | Look up an embedded metaschema by URI
lookupEmbeddedMetaschema :: Text -> Maybe Schema
lookupEmbeddedMetaschema uri = Map.lookup normalizedURI embeddedMetaschemas
  where
    -- Normalize URI by removing fragment
    normalizedURI = T.takeWhile (/= '#') uri

-- | List all embedded metaschema URIs (for debugging)
embeddedMetaschemaURIs :: [Text]
embeddedMetaschemaURIs = Map.keys embeddedMetaschemas

-- | Standard metaschema loader that uses embedded schemas
-- This loader can be composed with other loaders using Alternative (<|>)
--
-- Example:
--   let loader = standardMetaschemaLoader <|> httpLoader <|> fileLoader
--
-- The loader will first try embedded schemas, then fall back to other loaders.
standardMetaschemaLoader :: ReferenceLoader
standardMetaschemaLoader uri =
  case lookupEmbeddedMetaschema uri of
    Just schema -> pure $ Right schema
    Nothing -> pure $ Left $ "Metaschema not found in embedded resources: " <> uri
