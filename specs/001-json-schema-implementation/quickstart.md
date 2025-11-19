# Quickstart Guide: fractal-openapi

Get started with fractal-openapi in 5 minutes.

## Installation

Add to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - fractal-openapi >= 0.1
```

Or with cabal:

```bash
cabal install fractal-openapi
```

## Quick Examples

### Example 1: Validate JSON Against a Schema

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Fractal.JsonSchema
import Data.Aeson
import Data.Text (Text)

-- Define a schema
personSchema :: Schema
personSchema = [schemaQ|
{
  "type": "object",
  "properties": {
    "name": { "type": "string", "minLength": 1 },
    "age": { "type": "integer", "minimum": 0, "maximum": 150 },
    "email": { "type": "string", "format": "email" }
  },
  "required": ["name", "age"]
}
|]

-- Validate some data
main :: IO ()
main = do
  let validPerson = object
        [ "name" .= ("Alice" :: Text)
        , "age" .= (30 :: Int)
        , "email" .= ("alice@example.com" :: Text)
        ]
  
  let invalidPerson = object
        [ "name" .= ("" :: Text)  -- Too short!
        , "age" .= (200 :: Int)   -- Too old!
        ]
  
  -- Validate
  case validateValue defaultConfig personSchema validPerson of
    ValidationSuccess _ -> putStrLn "✓ Valid person"
    ValidationFailure errs -> print errs
  
  case validateValue defaultConfig personSchema invalidPerson of
    ValidationSuccess _ -> putStrLn "✓ Valid person"
    ValidationFailure errs -> do
      putStrLn "✗ Invalid person:"
      mapM_ print (unErrors errs)
```

**Output**:
```
✓ Valid person
✗ Invalid person:
ValidationError {errorKeyword = "minLength", errorSchemaPath = /properties/name/minLength, errorInstancePath = /name, errorMessage = "String length 0 is less than minimum 1"}
ValidationError {errorKeyword = "maximum", errorSchemaPath = /properties/age/maximum, errorInstancePath = /age, errorMessage = "Value 200 exceeds maximum 150"}
```

---

### Example 2: Generate Types with Template Haskell

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

import Fractal.OpenApi.Codegen.TH
import GHC.Generics
import Data.Aeson

-- Generate types from schema file at compile time
$(deriveJSONSchemaFromFile "schemas/person.json")

-- Now you have:
-- data Person = Person
--   { personName :: Text
--   , personAge :: Int
--   , personEmail :: Maybe Text
--   }
-- instance FromJSON Person
-- instance ToJSON Person

main :: IO ()
main = do
  let json = "{\"name\":\"Alice\",\"age\":30,\"email\":\"alice@example.com\"}"
  
  case eitherDecode json of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right person -> do
      putStrLn $ "Name: " ++ show (personName person)
      putStrLn $ "Age: " ++ show (personAge person)
      
      -- Serialize back
      print $ encode person
```

---

### Example 3: Custom Vocabulary

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Fractal.JsonSchema.Vocabulary
import Data.Typeable
import GHC.Generics

-- Define a custom keyword value type
data CreditCardKeyword = CreditCardKeyword
  { ccProvider :: Maybe Text
  , ccLuhnCheck :: Bool
  } deriving (Eq, Show, Generic, Typeable)

-- Define the keyword
creditCardKeywordDef :: KeywordDefinition
creditCardKeywordDef = KeywordDefinition
  { keywordName = "x-credit-card"
  , keywordAppliesTo = StringSchemaOnly
  , keywordParser = parseCreditCardKeyword
  , keywordValidator = Just validateCreditCard
  , keywordAnnotator = Nothing
  , keywordPriority = 100
  }

-- Parse the keyword value from JSON
parseCreditCardKeyword :: Value -> Either ParseError KeywordValue
parseCreditCardKeyword = withObject "x-credit-card" $ \o -> do
  provider <- o .:? "provider"
  luhnCheck <- o .:? "luhn-check" .!= True
  pure $ KeywordValue $ CreditCardKeyword provider luhnCheck

-- Validate credit card numbers
validateCreditCard :: KeywordValidator
validateCreditCard (KeywordValue (cc :: CreditCardKeyword)) value _ctx =
  case value of
    String txt -> do
      when (ccLuhnCheck cc && not (luhnValidate txt)) $
        validationError "Credit card fails Luhn check"
      ValidationSuccess mempty
    _ -> ValidationFailure $ ValidationError "Must be string"

-- Create a custom vocabulary
businessVocabulary :: Vocabulary
businessVocabulary = createVocabulary
  "https://example.com/vocabs/business/v1"
  False  -- Optional
  [("x-credit-card", creditCardKeywordDef)]
  Nothing
  Nothing

-- Register and use
main :: IO ()
main = do
  let registry = registerVocabulary businessVocabulary standardRegistry
  
  -- Now schemas can use x-credit-card keyword
  let schema = parseSchemaWith registry [schemaQ|
    {
      "type": "string",
      "x-credit-card": {
        "provider": "visa",
        "luhn-check": true
      }
    }
  |]
  
  -- Validation will use custom validator
  case validateWith registry schema (String "4532015112830366") of
    ValidationSuccess _ -> putStrLn "✓ Valid credit card"
    ValidationFailure _ -> putStrLn "✗ Invalid credit card"
```

---

### Example 4: OpenAPI to Servant

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

import Servant
import Fractal.OpenApi.Codegen.TH

-- Generate Servant API from OpenAPI spec
$(deriveServantAPI =<< runIO (loadOpenApiSpec "api.yaml"))

-- Assuming api.yaml has /users endpoints, you now have:
-- type UserAPI
--   = "users" :> Get '[JSON] [User]
--   :<|> "users" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] User
--   :<|> "users" :> Capture "userId" UserId :> Get '[JSON] User

-- Implement handlers
server :: Server UserAPI
server = getUsers :<|> createUser :<|> getUser
  where
    getUsers = pure [User "Alice" 30, User "Bob" 25]
    createUser req = pure $ User (crName req) (crAge req)
    getUser userId = pure $ User "Alice" 30

main :: IO ()
main = run 8080 (serve (Proxy @UserAPI) server)
```

---

### Example 5: Multi-Version Schema Support

```haskell
import Fractal.JsonSchema
import Fractal.JsonSchema.Dialect

-- Load a draft-04 schema
draft04Schema <- parseSchemaFromFile "schemas/legacy-draft04.json"

-- Load a 2020-12 schema
modernSchema <- parseSchemaFromFile "schemas/modern-2020-12.json"

-- Both work! The parser auto-detects version
case validateValue defaultConfig draft04Schema someValue of
  ValidationSuccess _ -> putStrLn "Valid against draft-04"
  ValidationFailure _ -> putStrLn "Invalid"

case validateValue defaultConfig modernSchema someValue of
  ValidationSuccess _ -> putStrLn "Valid against 2020-12"
  ValidationFailure _ -> putStrLn "Invalid"

-- Explicitly use a specific dialect
let draft07Config = defaultConfig { validationDialect = draft07Dialect }
validateValue draft07Config someSchema someValue
```

---

### Example 6: Semantic Newtypes

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Fractal.OpenApi.Codegen.TH

-- Schema with x-newtype annotation
$(deriveJSONSchemaFromFile "schemas/email.json")

-- schemas/email.json:
-- {
--   "type": "string",
--   "format": "email",
--   "x-newtype": {
--     "constructor": "Email",
--     "validation": "validateEmail"
--   }
-- }

-- Generated:
-- newtype Email = Email Text
-- mkEmail :: Text -> Either ValidationError Email
-- validateEmail :: Text -> Either ValidationError Text
-- instance FromJSON Email
-- instance ToJSON Email

main :: IO ()
main = do
  -- Safe construction
  case mkEmail "alice@example.com" of
    Right email -> print email
    Left err -> print err
  
  -- Unsafe input rejected at parse time
  case eitherDecode "\"not-an-email\"" of
    Left err -> putStrLn $ "✗ Parse failed: " ++ err
    Right (email :: Email) -> print email
```

---

### Example 7: Schema Reflection with HasSchema

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Fractal.OpenApi.Codegen.TH
import Fractal.JsonSchema (HasSchema(..), validateDynamic)
import Data.Proxy

-- Generate type with HasSchema instance
$(deriveJSONSchemaFromFile "schemas/person.json")

-- Now Person has:
-- instance HasSchema Person where
--   schemaFor _ = <embedded schema>
--   schemaPath _ = Just "/schemas/person.json"

main :: IO ()
main = do
  let person = Person "Alice" 30 (Just "alice@example.com")
  
  -- Get schema at runtime
  let schema = schemaFor (Proxy @Person)
  putStrLn $ "Schema title: " ++ show (annotationTitle $ schemaAnnotations schema)
  
  -- Dynamic validation after modification
  let modified = person { personAge = 200 }  -- Exceeds constraint!
  case validateDynamic modified of
    ValidationSuccess _ -> putStrLn "Valid"
    ValidationFailure errs -> do
      putStrLn "Invalid person:"
      mapM_ (putStrLn . errorMessage) (unErrors errs)
  
  -- Query field constraints
  case fieldConstraints (Proxy @Person) "age" of
    Just ageSchema -> do
      putStrLn $ "Age minimum: " ++ show (validationMinimum $ schemaValidation ageSchema)
      putStrLn $ "Age maximum: " ++ show (validationMaximum $ schemaValidation ageSchema)
    Nothing -> putStrLn "No age field"
  
  -- Check if field is required
  putStrLn $ "Name required: " ++ show (isRequired (Proxy @Person) "name")
  putStrLn $ "Email required: " ++ show (isRequired (Proxy @Person) "email")
```

**Use Cases for HasSchema**:
- **Form Generation**: Build dynamic forms from schema constraints
- **API Documentation**: Generate OpenAPI docs from types
- **Runtime Validation**: Validate after modifications
- **Schema Migration**: Compare old/new versions
- **Property Testing**: Generate Arbitrary instances from schema
- **IDE Tooling**: Provide hints based on schema constraints

---

## Common Patterns

### Pattern 1: Schema Composition

```haskell
-- Combine schemas with allOf
let baseSchema = [schemaQ|
  {
    "type": "object",
    "properties": {
      "id": { "type": "string", "format": "uuid" },
      "createdAt": { "type": "string", "format": "date-time" }
    },
    "required": ["id", "createdAt"]
  }
|]

let userSchema = [schemaQ|
  {
    "allOf": [
      ${baseSchema},
      {
        "properties": {
          "name": { "type": "string" },
          "email": { "type": "string", "format": "email" }
        },
        "required": ["name", "email"]
      }
    ]
  }
|]
```

### Pattern 2: Reusable Schemas

```haskell
-- Define schemas in a module
module Schemas where

import Fractal.JsonSchema

-- Common schemas
uuidSchema :: Schema
uuidSchema = [schemaQ| { "type": "string", "format": "uuid" } |]

timestampSchema :: Schema
timestampSchema = [schemaQ| { "type": "string", "format": "date-time" } |]

-- Compose into complex schemas
userSchema :: Schema
userSchema = [schemaQ|
  {
    "type": "object",
    "properties": {
      "id": ${uuidSchema},
      "createdAt": ${timestampSchema},
      "name": { "type": "string" }
    }
  }
|]
```

### Pattern 3: Validation with Context

```haskell
-- Custom validator for cross-field validation
customValidator :: Schema -> Value -> Either ValidationError ()
customValidator schema value = do
  -- First validate against schema
  case validateValue defaultConfig schema value of
    ValidationFailure errs -> Left (head $ unErrors errs)
    ValidationSuccess _ -> pure ()
  
  -- Then custom logic
  case value of
    Object o -> do
      startDate <- o .: "startDate"
      endDate <- o .: "endDate"
      when (endDate < startDate) $
        Left $ ValidationError "endDate must be after startDate"
    _ -> pure ()
```

### Pattern 4: Batch Validation

```haskell
-- Validate multiple values efficiently
validateBatch :: Schema -> [Value] -> [(Value, ValidationResult)]
validateBatch schema values =
  -- Compile validator once
  let validator = compileValidator defaultConfig schema
  in case validator of
      Left err -> error $ "Failed to compile: " ++ show err
      Right v -> [(val, runValidator v val) | val <- values]
```

## Next Steps

- **Learn More**: Read the [full documentation](./README.md)
- **API Reference**: Browse [Haddock documentation](https://hackage.haskell.org/package/fractal-openapi)
- **Examples**: See [examples/](../fractal-openapi/examples/) directory
- **Custom Vocabularies**: Read [SPEC.md vocabulary section](../fractal-openapi/SPEC.md#vocabulary-system)
- **Code Generation**: See [code generation guide](../fractal-openapi/docs/codegen.md)
- **Performance**: Check [benchmarks](../fractal-openapi/benchmarks/)

## Troubleshooting

### Common Issues

**Issue**: "Unknown keyword: x-my-keyword"  
**Solution**: Register custom vocabulary or use `CollectUnknown` mode

**Issue**: "Circular reference detected"  
**Solution**: Ensure `$ref` cycles are intentional (recursive schemas are supported)

**Issue**: "Generated name conflicts with existing type"  
**Solution**: Use `x-codegen` annotations to customize type names

**Issue**: "Format validation failed but should be annotation"  
**Solution**: Set `validationFormatAssertion = False` in config

**Issue**: "Template Haskell compilation slow"  
**Solution**: Split large schemas into multiple modules, compile incrementally

## Tips

1. **Start Simple**: Begin with basic validation, add codegen later
2. **Use QuasiQuoters**: Embed schemas directly in code for type safety
3. **Leverage Existing Schemas**: Convert OpenAPI specs to get started quickly
4. **Test Properties**: Use Hedgehog to verify schema invariants
5. **Profile Performance**: Use criterion to benchmark validation hotspots
6. **Document Schemas**: Use `title` and `description` for generated documentation
7. **Version Carefully**: Use `$schema` keyword to lock version
8. **Compose Vocabularies**: Build domain-specific dialects for consistency

## Resources

- **JSON Schema Spec**: https://json-schema.org/specification.html
- **OpenAPI Spec**: https://spec.openapis.org/oas/latest.html
- **JSON Schema Test Suite**: https://github.com/json-schema-org/JSON-Schema-Test-Suite
- **Aeson Documentation**: https://hackage.haskell.org/package/aeson
- **Servant Documentation**: https://docs.servant.dev/
- **Template Haskell Guide**: https://wiki.haskell.org/Template_Haskell

Happy schema validation! 🎉

