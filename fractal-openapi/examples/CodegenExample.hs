{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example: Using Template Haskell for code generation
--
-- This example shows how to generate Haskell types from JSON Schemas
-- at compile time using Template Haskell.
module CodegenExample where

import Fractal.OpenApi.Codegen.TH
import Fractal.OpenApi.Codegen.Core
import Fractal.JsonSchema
import Data.Aeson (Value(..), FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))

-- | Example 1: Generate a simple record type
--
-- This generates a Person type with fields:
-- - name :: Text (required)
-- - age :: Maybe Integer (optional)
-- - email :: Maybe Text (optional)
$(deriveJSONSchema $ Aeson.object
  [ "type" Aeson..= String "object"
  , "title" Aeson..= String "Person"
  , "description" Aeson..= String "A person entity"
  , "properties" Aeson..= Aeson.object
      [ "name" Aeson..= Aeson.object 
          [ "type" Aeson..= String "string"
          , "description" Aeson..= String "Full name"
          ]
      , "age" Aeson..= Aeson.object 
          [ "type" Aeson..= String "integer"
          , "minimum" Aeson..= Number 0
          , "description" Aeson..= String "Age in years"
          ]
      , "email" Aeson..= Aeson.object 
          [ "type" Aeson..= String "string"
          , "format" Aeson..= String "email"
          ]
      ]
  , "required" Aeson..= Aeson.Array (Vector.fromList [String "name"])
  ])

-- | Example 2: Generate an enum type
--
-- This generates a Status ADT with constructors: Active, Inactive, Pending
$(deriveJSONSchema $ Aeson.object
  [ "type" Aeson..= String "string"
  , "title" Aeson..= String "AccountStatus"
  , "enum" Aeson..= Aeson.Array (Vector.fromList 
      [ String "active"
      , String "inactive"  
      , String "pending"
      , String "suspended"
      ])
  ])

-- | Example 3: Using generated types
exampleUsage :: IO ()
exampleUsage = do
  putStrLn "=== Code Generation Examples ===\n"
  
  -- Example 1: Create and encode a Person
  let alice = Person
        { name = "Alice Johnson"
        , age = Just 30
        , email = Just "alice@example.com"
        }
  
  putStrLn "Person (all fields):"
  print $ Aeson.encode alice
  print alice
  
  -- Example 2: Create a Person with only required fields
  let bob = Person
        { name = "Bob Smith"
        , age = Nothing
        , email = Nothing
        }
  
  putStrLn "\nPerson (required fields only):"
  print $ Aeson.encode bob
  
  -- Example 3: Parse JSON into Person
  let jsonInput = Aeson.object
        [ "name" Aeson..= String "Charlie"
        , "age" Aeson..= Number 25
        ]
  
  case Aeson.fromJSON jsonInput :: Aeson.Result Person of
    Aeson.Error err -> putStrLn $ "Parse error: " <> err
    Aeson.Success person -> do
      putStrLn "\nParsed Person:"
      print person
  
  -- Example 4: Validate against schema using HasSchema
  let schema = schemaFor (Proxy :: Proxy Person)
  putStrLn "\nPerson schema title:"
  case schemaCore schema of
    ObjectSchema obj -> print $ annotationTitle (schemaAnnotations obj)
    _ -> putStrLn "Not an object schema"
  
  -- Example 5: Use enum types
  let status = Active
  putStrLn "\nAccount Status:"
  print $ Aeson.encode status
  
  -- Example 6: Roundtrip test
  let originalPerson = Person "Dana" (Just 28) Nothing
      encoded = Aeson.encode originalPerson
      decoded = Aeson.decode encoded :: Maybe Person
  
  case decoded of
    Just decodedPerson -> do
      putStrLn "\nRoundtrip successful!"
      putStrLn $ "Original: " <> show originalPerson
      putStrLn $ "Decoded:  " <> show decodedPerson
      putStrLn $ "Equal: " <> show (originalPerson == decodedPerson)
    Nothing -> putStrLn "Roundtrip failed!"

-- | Example 4: Generate from a file (commented out - requires actual file)
{-
-- If you have a schema file, you can generate from it:
$(deriveJSONSchemaFromFile "schemas/user.json")
-}

-- | Example 5: Custom configuration
--
-- Generate with custom naming conventions
$(deriveJSONSchemaWith 
    (defaultCodegenConfig 
      { codegenNaming = NamingConvention
          { conventionFieldNaming = FieldSnakeCase
          , conventionTypeNaming = TypeFromTitle
          , conventionStripPrefix = Just "api_"
          , conventionAddPrefix = Nothing
          }
      , codegenNewtypeOptimization = False  -- Always use records
      })
    (Aeson.object
      [ "type" Aeson..= String "object"
      , "title" Aeson..= String "ApiResponse"
      , "properties" Aeson..= Aeson.object
          [ "api_status_code" Aeson..= Aeson.object ["type" Aeson..= String "integer"]
          , "api_message" Aeson..= Aeson.object ["type" Aeson..= String "string"]
          ]
      , "required" Aeson..= Aeson.Array (Vector.fromList [String "api_status_code"])
      ]))

-- Now we have an ApiResponse type with fields:
-- - status_code :: Integer (prefix stripped, snake_case)
-- - message :: Maybe Text

