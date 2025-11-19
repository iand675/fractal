{-# LANGUAGE OverloadedStrings #-}

-- | Reference loaders for external schema resolution
--
-- This module provides pluggable loaders for fetching external schemas.
-- Users can provide custom loaders for different protocols or caching strategies.
module Fractal.JsonSchema.ReferenceLoader
  ( -- * Reference Loaders
    ReferenceLoader
  , noOpLoader
  , httpLoader
  , fileLoader
  , cachedLoader
  , compositeLoader
  
    -- * Helper Functions
  , resolveRelativeURI
  , isHttpURI
  , isFileURI
  ) where

import Fractal.JsonSchema.Types
import Fractal.JsonSchema.Parser (parseSchema, ParseError(parseErrorMessage))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody, HttpException)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.URI (URI, parseURI, parseURIReference, uriToString, relativeTo)
import qualified Network.URI as URI
import Control.Exception (try, Exception)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import System.IO.Unsafe (unsafePerformIO)

-- | No-op loader that always fails (for pure validation without external refs)
noOpLoader :: ReferenceLoader
noOpLoader uri = pure $ Left $ "External references not supported: " <> uri

-- | HTTP-based loader using http-client
-- Fetches schemas from http:// and https:// URIs
httpLoader :: Manager -> ReferenceLoader
httpLoader manager uri
  | isHttpURI uri = do
      result <- try $ do
        request <- parseRequest (T.unpack uri)
        response <- httpLbs request manager
        let body = responseBody response
        case Aeson.eitherDecode body of
          Left err -> pure $ Left $ "Failed to parse schema from " <> uri <> ": " <> T.pack err
          Right value -> case parseSchema value of
            Left parseErr -> pure $ Left $ "Failed to parse schema: " <> parseErrorMessage parseErr
            Right schema -> pure $ Right schema
      case result of
        Left (exc :: HttpException) -> pure $ Left $ "HTTP error loading " <> uri <> ": " <> T.pack (show exc)
        Right res -> pure res
  | otherwise = pure $ Left $ "Not an HTTP URI: " <> uri

-- | File system loader (for file:// URIs and relative paths)
fileLoader :: ReferenceLoader
fileLoader uri
  | isFileURI uri = do
      let path = T.unpack $ T.drop 7 uri  -- Remove "file://"
      result <- try $ Aeson.eitherDecodeFileStrict path
      case result of
        Left (exc :: IOError) -> pure $ Left $ "File error loading " <> uri <> ": " <> T.pack (show exc)
        Right (Left err) -> pure $ Left $ "JSON parse error: " <> T.pack err
        Right (Right value) -> case parseSchema value of
          Left parseErr -> pure $ Left $ "Schema parse error: " <> parseErrorMessage parseErr
          Right schema -> pure $ Right schema
  | T.isPrefixOf "/" uri || T.isPrefixOf "./" uri || T.isPrefixOf "../" uri = do
      -- Relative or absolute file path
      result <- try $ Aeson.eitherDecodeFileStrict (T.unpack uri)
      case result of
        Left (exc :: IOError) -> pure $ Left $ "File error: " <> T.pack (show exc)
        Right (Left err) -> pure $ Left $ "JSON parse error: " <> T.pack err
        Right (Right value) -> case parseSchema value of
          Left parseErr -> pure $ Left $ "Schema parse error: " <> parseErrorMessage parseErr
          Right schema -> pure $ Right schema
  | otherwise = pure $ Left $ "Not a file URI: " <> uri

-- | Caching loader that wraps another loader
-- Caches successful schema loads to avoid repeated fetches
cachedLoader :: ReferenceLoader -> IO ReferenceLoader
cachedLoader baseLoader = do
  cacheRef <- newIORef Map.empty
  pure $ \uri -> do
    cache <- readIORef cacheRef
    case Map.lookup uri cache of
      Just schema -> pure $ Right schema
      Nothing -> do
        result <- baseLoader uri
        case result of
          Right schema -> do
            modifyIORef' cacheRef (Map.insert uri schema)
            pure $ Right schema
          Left err -> pure $ Left err

-- | Composite loader that tries multiple loaders in order
compositeLoader :: [ReferenceLoader] -> ReferenceLoader
compositeLoader loaders uri = tryLoaders loaders
  where
    tryLoaders [] = pure $ Left $ "No loader could resolve: " <> uri
    tryLoaders (loader:rest) = do
      result <- loader uri
      case result of
        Right schema -> pure $ Right schema
        Left _ -> tryLoaders rest

-- | Resolve a relative URI against a base URI
resolveRelativeURI :: Text -> Text -> Either Text Text
resolveRelativeURI base relative =
  case (parseURI (T.unpack base), parseURIReference (T.unpack relative)) of
    (Just baseURI, Just relURI) ->
      let resolved = relURI `relativeTo` baseURI
      in Right $ T.pack $ uriToString id resolved ""
    _ -> Left $ "Invalid URI: base=" <> base <> ", relative=" <> relative

-- | Check if a URI is HTTP/HTTPS
isHttpURI :: Text -> Bool
isHttpURI uri = T.isPrefixOf "http://" uri || T.isPrefixOf "https://" uri

-- | Check if a URI is a file:// URI
isFileURI :: Text -> Bool
isFileURI uri = T.isPrefixOf "file://" uri

