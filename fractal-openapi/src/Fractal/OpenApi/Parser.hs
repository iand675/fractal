-- | OpenAPI spec parsing
module Fractal.OpenApi.Parser
  ( parseOpenApiSpec
  , detectOpenApiVersion
  , parseComponents
  ) where

import Fractal.OpenApi.Types
import Fractal.JsonSchema (parseSchema)
import Fractal.JsonSchema.Types (Schema)
import Data.Aeson (Value(..), Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- | Parse an OpenAPI/Swagger specification from JSON/YAML
-- Supports both Swagger 2.0 and OpenAPI 3.x
parseOpenApiSpec :: Value -> Either Text OpenApiSpec
parseOpenApiSpec (Object obj) = do
  -- Detect if this is Swagger 2.0 or OpenAPI 3.x
  let isSwagger = KeyMap.member "swagger" obj
      isOpenApi = KeyMap.member "openapi" obj
  
  case (isSwagger, isOpenApi) of
    (True, _) -> parseSwagger20Spec obj
    (_, True) -> parseOpenApi3Spec obj
    _ -> Left "Missing 'swagger' or 'openapi' field"
parseOpenApiSpec _ = Left "Spec must be a JSON object"

-- | Parse Swagger 2.0 specification
parseSwagger20Spec :: Object -> Either Text OpenApiSpec
parseSwagger20Spec obj = do
  -- Parse Swagger version
  swaggerVersion <- case KeyMap.lookup "swagger" obj of
    Just (String v)
      | v == "2.0" -> Right v
      | otherwise -> Left $ "Unsupported Swagger version: " <> v
    _ -> Left "Missing or invalid 'swagger' field"
  
  -- Parse info (required)
  info <- case KeyMap.lookup "info" obj of
    Just infoVal -> parseInfo infoVal
    Nothing -> Left "Missing required 'info' field"
  
  -- Parse paths (required)
  paths <- case KeyMap.lookup "paths" obj of
    Just pathsVal -> parsePaths pathsVal
    Nothing -> Left "Missing required 'paths' field"
  
  -- Parse Swagger 2.0 specific fields
  let host = getOptional "host" obj
      basePath = getOptional "basePath" obj
      schemes = getOptionalArray "schemes" obj
  
  -- Parse definitions (Swagger 2.0 schemas)
  definitions <- case KeyMap.lookup "definitions" obj of
    Just (Object defsObj) -> do
      let schemaPairs = [(Key.toText k, v) | (k, v) <- KeyMap.toList defsObj]
      parsed <- traverse (\(name, val) -> case parseSchema val of
        Left err -> Left $ "Failed to parse definition '" <> name <> "': " <> T.pack (show err)
        Right schema -> Right (name, schema)) schemaPairs
      pure $ Just $ Map.fromList parsed
    Just _ -> Left "definitions must be an object"
    Nothing -> Right Nothing
  
  -- Parse optional fields
  security <- traverse parseSecurity $ KeyMap.lookup "security" obj
  tags <- traverse parseTags $ KeyMap.lookup "tags" obj
  externalDocs <- traverse parseExternalDocs $ KeyMap.lookup "externalDocs" obj
  
  pure $ OpenApiSpec
    { specOpenApi = swaggerVersion
    , specInfo = info
    , specServers = Nothing  -- Swagger 2.0 doesn't have servers
    , specHost = host
    , specBasePath = basePath
    , specSchemes = schemes
    , specPaths = paths
    , specComponents = Nothing  -- Swagger 2.0 doesn't have components
    , specDefinitions = definitions
    , specSecurity = security
    , specTags = tags
    , specExternalDocs = externalDocs
    }

-- | Parse OpenAPI 3.x specification
parseOpenApi3Spec :: Object -> Either Text OpenApiSpec
parseOpenApi3Spec obj = do
  -- Parse OpenAPI version
  openApiVersion <- case KeyMap.lookup "openapi" obj of
    Just (String v) -> Right v
    _ -> Left "Missing or invalid 'openapi' field"
  
  -- Parse info (required)
  info <- case KeyMap.lookup "info" obj of
    Just infoVal -> parseInfo infoVal
    Nothing -> Left "Missing required 'info' field"
  
  -- Parse paths (required)
  paths <- case KeyMap.lookup "paths" obj of
    Just pathsVal -> parsePaths pathsVal
    Nothing -> Left "Missing required 'paths' field"
  
  -- Parse optional fields
  servers <- traverse parseServers $ KeyMap.lookup "servers" obj
  components <- traverse parseComponents $ KeyMap.lookup "components" obj
  security <- traverse parseSecurity $ KeyMap.lookup "security" obj
  tags <- traverse parseTags $ KeyMap.lookup "tags" obj
  externalDocs <- traverse parseExternalDocs $ KeyMap.lookup "externalDocs" obj
  
  pure $ OpenApiSpec
    { specOpenApi = openApiVersion
    , specInfo = info
    , specServers = servers
    , specHost = Nothing  -- OpenAPI 3.x doesn't have host
    , specBasePath = Nothing
    , specSchemes = Nothing
    , specPaths = paths
    , specComponents = components
    , specDefinitions = Nothing  -- OpenAPI 3.x doesn't have definitions
    , specSecurity = security
    , specTags = tags
    , specExternalDocs = externalDocs
    }

-- | Detect OpenAPI/Swagger version from spec
detectOpenApiVersion :: Value -> Either Text OpenApiVersion
detectOpenApiVersion (Object obj) = do
  -- Check for Swagger 2.0 first
  case KeyMap.lookup "swagger" obj of
    Just (String v)
      | v == "2.0" -> Right (Swagger20 v)
      | otherwise -> Left $ "Unsupported Swagger version: " <> v
    Just _ -> Left "Swagger version must be a string"
    Nothing ->
      -- Not Swagger, check for OpenAPI 3.x
      case KeyMap.lookup "openapi" obj of
        Just (String v)
          | T.isPrefixOf "3.0" v -> Right (OpenApi30 v)
          | T.isPrefixOf "3.1" v -> Right (OpenApi31 v)
          | otherwise -> Left $ "Unsupported OpenAPI version: " <> v
        Just _ -> Left "OpenAPI version must be a string"
        Nothing -> Left "Missing 'swagger' or 'openapi' field"
detectOpenApiVersion _ = Left "Spec must be an object"

-- | Parse Info object
parseInfo :: Value -> Either Text Info
parseInfo (Object obj) = do
  title <- getRequired "title" obj
  version <- getRequired "version" obj
  
  let description = getOptional "description" obj
      termsOfService = getOptional "termsOfService" obj
      contact = KeyMap.lookup "contact" obj >>= parseContactMaybe
      license = KeyMap.lookup "license" obj >>= parseLicenseMaybe
  
  pure $ Info
    { infoTitle = title
    , infoDescription = description
    , infoTermsOfService = termsOfService
    , infoContact = contact
    , infoLicense = license
    , infoVersion = version
    }
parseInfo _ = Left "Info must be an object"

parseContactMaybe :: Value -> Maybe Contact
parseContactMaybe (Object obj) = Just $ Contact
  { contactName = getOptional "name" obj
  , contactUrl = getOptional "url" obj
  , contactEmail = getOptional "email" obj
  }
parseContactMaybe _ = Nothing

parseLicenseMaybe :: Value -> Maybe License
parseLicenseMaybe (Object obj) = do
  name <- getOptional "name" obj
  let url = getOptional "url" obj
  Just $ License name url
parseLicenseMaybe _ = Nothing

-- | Parse servers array
parseServers :: Value -> Either Text [Server]
parseServers (Array arr) = traverse parseServer (toList arr)
parseServers _ = Left "Servers must be an array"

parseServer :: Value -> Either Text Server
parseServer (Object obj) = do
  url <- getRequired "url" obj
  let description = getOptional "description" obj
      variables = Nothing  -- Simplified for now
  pure $ Server url description variables
parseServer _ = Left "Server must be an object"

-- | Parse paths object
parsePaths :: Value -> Either Text Paths
parsePaths (Object obj) = do
  let pathPairs = [(Key.toText k, v) | (k, v) <- KeyMap.toList obj]
  pathItems <- traverse (\(path, val) -> (,) path <$> parsePathItem val) pathPairs
  pure $ Map.fromList pathItems
parsePaths _ = Left "Paths must be an object"

parsePathItem :: Value -> Either Text PathItem
parsePathItem (Object obj) = do
  let summary = getOptional "summary" obj
      description = getOptional "description" obj
      get = KeyMap.lookup "get" obj >>= parseOperationMaybe
      put = KeyMap.lookup "put" obj >>= parseOperationMaybe
      post = KeyMap.lookup "post" obj >>= parseOperationMaybe
      delete = KeyMap.lookup "delete" obj >>= parseOperationMaybe
      options = KeyMap.lookup "options" obj >>= parseOperationMaybe
      head' = KeyMap.lookup "head" obj >>= parseOperationMaybe
      patch = KeyMap.lookup "patch" obj >>= parseOperationMaybe
      trace = KeyMap.lookup "trace" obj >>= parseOperationMaybe
  
  pure $ PathItem
    { pathSummary = summary
    , pathDescription = description
    , pathGet = get
    , pathPut = put
    , pathPost = post
    , pathDelete = delete
    , pathOptions = options
    , pathHead = head'
    , pathPatch = patch
    , pathTrace = trace
    , pathServers = Nothing
    , pathParameters = Nothing
    }
parsePathItem _ = Left "PathItem must be an object"

parseOperationMaybe :: Value -> Maybe Operation
parseOperationMaybe (Object obj) = Just $ Operation
  { operationTags = getOptionalArray "tags" obj
  , operationSummary = getOptional "summary" obj
  , operationDescription = getOptional "description" obj
  , operationExternalDocs = Nothing
  , operationOperationId = getOptional "operationId" obj
  , operationParameters = Nothing  -- Simplified for now
  , operationRequestBody = Nothing
  , operationResponses = Map.empty  -- Will parse properly later
  , operationCallbacks = Nothing
  , operationDeprecated = getOptionalBool "deprecated" obj
  , operationSecurity = Nothing
  , operationServers = Nothing
  }
parseOperationMaybe _ = Nothing

-- | Parse components
parseComponents :: Value -> Either Text Components
parseComponents (Object obj) = do
  -- Parse schemas map
  schemas <- case KeyMap.lookup "schemas" obj of
    Just (Object schemasObj) -> do
      let schemaPairs = [(Key.toText k, v) | (k, v) <- KeyMap.toList schemasObj]
      parsed <- traverse (\(name, val) -> case parseSchema val of
        Left err -> Left $ "Failed to parse schema '" <> name <> "': " <> T.pack (show err)
        Right schema -> Right (name, schema)) schemaPairs
      pure $ Just $ Map.fromList parsed
    Just _ -> Left "components.schemas must be an object"
    Nothing -> Right Nothing
  
  pure $ Components
    { componentsSchemas = schemas
    , componentsResponses = Nothing  -- TODO
    , componentsParameters = Nothing
    , componentsExamples = Nothing
    , componentsRequestBodies = Nothing
    , componentsHeaders = Nothing
    , componentsSecuritySchemes = Nothing
    , componentsLinks = Nothing
    , componentsCallbacks = Nothing
    }
parseComponents _ = Left "Components must be an object"

parseSecurity :: Value -> Either Text [SecurityRequirement]
parseSecurity (Array arr) = pure []  -- Simplified for now
parseSecurity _ = Left "Security must be an array"

parseTags :: Value -> Either Text [Tag]
parseTags (Array arr) = pure []  -- Simplified for now
parseTags _ = Left "Tags must be an array"

parseExternalDocs :: Value -> Either Text ExternalDocs
parseExternalDocs (Object obj) = do
  url <- getRequired "url" obj
  let description = getOptional "description" obj
  pure $ ExternalDocs description url
parseExternalDocs _ = Left "ExternalDocs must be an object"

-- Helper functions
getRequired :: Text -> Object -> Either Text Text
getRequired key obj = case KeyMap.lookup (Key.fromText key) obj of
  Just (String val) -> Right val
  Just _ -> Left $ key <> " must be a string"
  Nothing -> Left $ "Missing required field: " <> key

getOptional :: Text -> Object -> Maybe Text
getOptional key obj = case KeyMap.lookup (Key.fromText key) obj of
  Just (String val) -> Just val
  _ -> Nothing

getOptionalBool :: Text -> Object -> Maybe Bool
getOptionalBool key obj = case KeyMap.lookup (Key.fromText key) obj of
  Just (Bool val) -> Just val
  _ -> Nothing

getOptionalArray :: Text -> Object -> Maybe [Text]
getOptionalArray key obj = case KeyMap.lookup (Key.fromText key) obj of
  Just (Array arr) -> Just [t | String t <- toList arr]
  _ -> Nothing

toList :: Foldable f => f a -> [a]
toList = foldr (:) []
