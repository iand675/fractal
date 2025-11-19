# fractal-openapi

A comprehensive, production-ready Haskell library for JSON Schema validation and code generation.

## Features

- ✅ **Multi-Version JSON Schema Support**: Draft-04, Draft-06, Draft-07, 2019-09, and 2020-12
- ✅ **99.1% Test Suite Compliance**: 1029/1038 official JSON Schema tests passing
- ✅ **Complete $ref Resolution**: JSON Pointers, anchors, fragment IDs, with pluggable external loaders
- ✅ **Extensible Vocabulary System**: Define custom keywords for domain-specific validation
- ✅ **Template Haskell Code Generation**: Generate Haskell types with Aeson instances from schemas
- ✅ **HasSchema Typeclass**: Link generated types back to their schemas
- ✅ **Custom Validation**: Register domain-specific validators for custom keywords
- ✅ **Type-Safe**: Precise error reporting with JSON Pointer paths

## Quick Start

### Validation

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Fractal.JsonSchema
import Data.Aeson

main :: IO ()
main = do
  -- Parse a schema
  let schemaValue = object
        [ "type" .= ("object" :: Text)
        , "properties" .= object
            [ "name" .= object ["type" .= ("string" :: Text)]
            , "age" .= object 
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (0 :: Int)
                ]
            ]
        , "required" .= (["name"] :: [Text])
        ]
  
  case parseSchema schemaValue of
    Left err -> print err
    Right schema -> do
      -- Validate data
      let validData = object ["name" .= ("Alice" :: Text), "age" .= (30 :: Int)]
      let invalidData = object ["age" .= (-5 :: Int)]
      
      print $ validateValue defaultValidationConfig schema validData
      -- => ValidationSuccess
      
      print $ validateValue defaultValidationConfig schema invalidData  
      -- => ValidationFailure (missing "name", age below minimum)
```

### Code Generation

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Fractal.OpenApi.Codegen.TH
import Data.Aeson

-- Generate a Person type at compile time
$(deriveJSONSchema $ object
  [ "type" .= ("object" :: Text)
  , "title" .= ("Person" :: Text)
  , "properties" .= object
      [ "name" .= object ["type" .= ("string" :: Text)]
      , "age" .= object ["type" .= ("integer" :: Text)]
      ]
  , "required" .= (["name"] :: [Text])
  ])

-- Use the generated type
main :: IO ()
main = do
  let alice = Person { name = "Alice", age = Just 30 }
  print $ encode alice
  -- => {"name":"Alice","age":30}
```

## Installation

Add to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - fractal-openapi
```

## Building

Using Stack:

```bash
stack build fractal-openapi
stack test fractal-openapi
stack run basic-validation  # Run example
```

## Documentation

- [Quickstart Guide](./specs/001-json-schema-implementation/quickstart.md)
- [Technical Specification](./SPEC.md)
- [API Documentation](https://hackage.haskell.org/package/fractal-openapi)

## Status

**Version**: 0.1.0.0 (In Development)

This library is part of the [Fractal](https://github.com/iand675/fractal) project.

## License

BSD-3-Clause
