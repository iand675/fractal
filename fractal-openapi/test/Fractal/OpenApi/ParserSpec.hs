{-# LANGUAGE OverloadedStrings #-}

module Fractal.OpenApi.ParserSpec (spec) where

import Test.Hspec
import Fractal.OpenApi.Parser
import Fractal.OpenApi.Types
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "OpenAPI Parser" $ do
  
  describe "Version Detection" $ do
    it "detects Swagger 2.0" $ do
      let spec = object ["swagger" .= ("2.0" :: Text)]
      case detectOpenApiVersion spec of
        Right (Swagger20 v) -> v `shouldBe` "2.0"
        _ -> expectationFailure "Expected Swagger20"
    
    it "detects OpenAPI 3.0.x" $ do
      let spec = object ["openapi" .= ("3.0.3" :: Text)]
      case detectOpenApiVersion spec of
        Right (OpenApi30 v) -> v `shouldBe` "3.0.3"
        _ -> expectationFailure "Expected OpenApi30"
    
    it "detects OpenAPI 3.1.x" $ do
      let spec = object ["openapi" .= ("3.1.0" :: Text)]
      case detectOpenApiVersion spec of
        Right (OpenApi31 v) -> v `shouldBe` "3.1.0"
        _ -> expectationFailure "Expected OpenApi31"
    
    it "rejects unsupported versions" $ do
      let spec = object ["openapi" .= ("4.0" :: Text)]
      case detectOpenApiVersion spec of
        Left err -> T.unpack err `shouldContain` "Unsupported"
        Right _ -> expectationFailure "Should reject version 4.0"
  
  describe "Minimal Spec Parsing" $ do
    it "parses minimal valid OpenAPI 3.0 spec" $ do
      let specValue = object
            [ "openapi" .= ("3.0.0" :: Text)
            , "info" .= object
                [ "title" .= ("Test API" :: Text)
                , "version" .= ("1.0.0" :: Text)
                ]
            , "paths" .= object []
            ]
      
      case parseOpenApiSpec specValue of
        Left err -> expectationFailure $ "Parse error: " <> T.unpack err
        Right spec -> do
          specOpenApi spec `shouldBe` "3.0.0"
          infoTitle (specInfo spec) `shouldBe` "Test API"
          infoVersion (specInfo spec) `shouldBe` "1.0.0"
          Map.size (specPaths spec) `shouldBe` 0
    
    it "parses info with all optional fields" $ do
      let specValue = object
            [ "openapi" .= ("3.0.0" :: Text)
            , "info" .= object
                [ "title" .= ("Test API" :: Text)
                , "version" .= ("1.0.0" :: Text)
                , "description" .= ("A test API" :: Text)
                , "termsOfService" .= ("https://example.com/tos" :: Text)
                , "contact" .= object
                    [ "name" .= ("Support" :: Text)
                    , "email" .= ("support@example.com" :: Text)
                    ]
                , "license" .= object
                    [ "name" .= ("MIT" :: Text)
                    , "url" .= ("https://opensource.org/licenses/MIT" :: Text)
                    ]
                ]
            , "paths" .= object []
            ]
      
      case parseOpenApiSpec specValue of
        Left err -> expectationFailure $ "Parse error: " <> T.unpack err
        Right spec -> do
          infoDescription (specInfo spec) `shouldBe` Just "A test API"
          infoTermsOfService (specInfo spec) `shouldBe` Just "https://example.com/tos"
          case infoContact (specInfo spec) of
            Just contact -> contactEmail contact `shouldBe` Just "support@example.com"
            Nothing -> expectationFailure "Expected contact"
          case infoLicense (specInfo spec) of
            Just license -> licenseName license `shouldBe` "MIT"
            Nothing -> expectationFailure "Expected license"
  
  describe "Paths and Operations" $ do
    it "parses paths with operations" $ do
      let specValue = object
            [ "openapi" .= ("3.0.0" :: Text)
            , "info" .= object
                [ "title" .= ("Test API" :: Text)
                , "version" .= ("1.0.0" :: Text)
                ]
            , "paths" .= object
                [ "/users" .= object
                    [ "get" .= object
                        [ "summary" .= ("List users" :: Text)
                        , "operationId" .= ("listUsers" :: Text)
                        ]
                    , "post" .= object
                        [ "summary" .= ("Create user" :: Text)
                        , "operationId" .= ("createUser" :: Text)
                        ]
                    ]
                , "/users/{id}" .= object
                    [ "get" .= object
                        [ "summary" .= ("Get user" :: Text)
                        ]
                    ]
                ]
            ]
      
      case parseOpenApiSpec specValue of
        Left err -> expectationFailure $ "Parse error: " <> T.unpack err
        Right spec -> do
          Map.size (specPaths spec) `shouldBe` 2
          
          -- Check /users path
          case Map.lookup "/users" (specPaths spec) of
            Just pathItem -> do
              case pathGet pathItem of
                Just op -> operationSummary op `shouldBe` Just "List users"
                Nothing -> expectationFailure "Expected GET operation"
              case pathPost pathItem of
                Just op -> operationOperationId op `shouldBe` Just "createUser"
                Nothing -> expectationFailure "Expected POST operation"
            Nothing -> expectationFailure "Expected /users path"
          
          -- Check /users/{id} path
          case Map.lookup "/users/{id}" (specPaths spec) of
            Just pathItem -> do
              case pathGet pathItem of
                Just _ -> True `shouldBe` True
                Nothing -> expectationFailure "Expected GET operation"
            Nothing -> expectationFailure "Expected /users/{id} path"
  
  describe "Components Parsing" $ do
    it "parses component schemas" $ do
      let specValue = object
            [ "openapi" .= ("3.0.0" :: Text)
            , "info" .= object
                [ "title" .= ("Test API" :: Text)
                , "version" .= ("1.0.0" :: Text)
                ]
            , "paths" .= object []
            , "components" .= object
                [ "schemas" .= object
                    [ "User" .= object
                        [ "type" .= ("object" :: Text)
                        , "properties" .= object
                            [ "id" .= object ["type" .= ("integer" :: Text)]
                            , "name" .= object ["type" .= ("string" :: Text)]
                            ]
                        ]
                    , "Error" .= object
                        [ "type" .= ("object" :: Text)
                        , "properties" .= object
                            [ "message" .= object ["type" .= ("string" :: Text)]
                            ]
                        ]
                    ]
                ]
            ]
      
      case parseOpenApiSpec specValue of
        Left err -> expectationFailure $ "Parse error: " <> T.unpack err
        Right spec -> do
          case specComponents spec of
            Just components -> do
              case componentsSchemas components of
                Just schemas -> do
                  Map.size schemas `shouldBe` 2
                  Map.member "User" schemas `shouldBe` True
                  Map.member "Error" schemas `shouldBe` True
                Nothing -> expectationFailure "Expected schemas"
            Nothing -> expectationFailure "Expected components"
  
  describe "Servers Parsing" $ do
    it "parses server list" $ do
      let specValue = object
            [ "openapi" .= ("3.0.0" :: Text)
            , "info" .= object
                [ "title" .= ("Test API" :: Text)
                , "version" .= ("1.0.0" :: Text)
                ]
            , "paths" .= object []
            , "servers" .= Vector.fromList
                [ object
                    [ "url" .= ("https://api.example.com" :: Text)
                    , "description" .= ("Production server" :: Text)
                    ]
                , object
                    [ "url" .= ("https://staging.example.com" :: Text)
                    ]
                ]
            ]
      
      case parseOpenApiSpec specValue of
        Left err -> expectationFailure $ "Parse error: " <> T.unpack err
        Right spec -> do
          case specServers spec of
            Just servers -> do
              length servers `shouldBe` 2
              serverUrl (head servers) `shouldBe` "https://api.example.com"
              serverDescription (head servers) `shouldBe` Just "Production server"
            Nothing -> expectationFailure "Expected servers"
  
  describe "Swagger 2.0 Support" $ do
    it "parses minimal Swagger 2.0 spec" $ do
      let specValue = object
            [ "swagger" .= ("2.0" :: Text)
            , "info" .= object
                [ "title" .= ("Swagger API" :: Text)
                , "version" .= ("1.0.0" :: Text)
                ]
            , "paths" .= object []
            ]
      
      case parseOpenApiSpec specValue of
        Left err -> expectationFailure $ "Parse error: " <> T.unpack err
        Right spec -> do
          specOpenApi spec `shouldBe` "2.0"
          infoTitle (specInfo spec) `shouldBe` "Swagger API"
          Map.size (specPaths spec) `shouldBe` 0
    
    it "parses Swagger 2.0 with host and basePath" $ do
      let specValue = object
            [ "swagger" .= ("2.0" :: Text)
            , "info" .= object
                [ "title" .= ("API" :: Text)
                , "version" .= ("1.0" :: Text)
                ]
            , "host" .= ("api.example.com" :: Text)
            , "basePath" .= ("/v1" :: Text)
            , "schemes" .= Vector.fromList [String "https", String "http"]
            , "paths" .= object []
            ]
      
      case parseOpenApiSpec specValue of
        Left err -> expectationFailure $ "Parse error: " <> T.unpack err
        Right spec -> do
          specHost spec `shouldBe` Just "api.example.com"
          specBasePath spec `shouldBe` Just "/v1"
          specSchemes spec `shouldBe` Just ["https", "http"]
    
    it "parses Swagger 2.0 definitions" $ do
      let specValue = object
            [ "swagger" .= ("2.0" :: Text)
            , "info" .= object
                [ "title" .= ("API" :: Text)
                , "version" .= ("1.0" :: Text)
                ]
            , "paths" .= object []
            , "definitions" .= object
                [ "User" .= object
                    [ "type" .= ("object" :: Text)
                    , "properties" .= object
                        [ "id" .= object ["type" .= ("integer" :: Text)]
                        , "name" .= object ["type" .= ("string" :: Text)]
                        ]
                    ]
                ]
            ]
      
      case parseOpenApiSpec specValue of
        Left err -> expectationFailure $ "Parse error: " <> T.unpack err
        Right spec -> do
          case specDefinitions spec of
            Just defs -> do
              Map.size defs `shouldBe` 1
              Map.member "User" defs `shouldBe` True
            Nothing -> expectationFailure "Expected definitions"

