# Fractal OpenAPI - Technical Specification

## Project Overview

`fractal-openapi` is a comprehensive Haskell library for JSON Schema and OpenAPI 3.x support, featuring robust validation, metadata extraction, and a pluggable Template Haskell-based code generation system.

### Goals

1. **Complete JSON Schema Support**: All versions (draft-04, draft-06, draft-07, 2019-09, 2020-12)
2. **Vocabulary System**: Extensible vocabulary support for custom keywords
3. **Validation**: High-performance schema validation with detailed error reporting
4. **Metadata Extraction**: Rich introspection capabilities
5. **Pluggable Codegen**: Template Haskell-based code generation with customizable strategies
6. **Type Generation**: Generate Haskell datatypes with Aeson instances from schemas
7. **Semantic Newtypes**: Support schema annotations for generating semantic newtypes
8. **OpenAPI Support**: Full OpenAPI 3.x specification support
9. **Framework Integration**: Generate Servant and Yesod routing types

## Architecture Overview

```
fractal-openapi/
├── JsonSchema/           # JSON Schema core
│   ├── Types/           # Schema AST and types
│   ├── Parser/          # Schema parsing
│   ├── Renderer/        # Schema rendering
│   ├── Validator/       # Validation engine
│   ├── Vocabulary/      # Vocabulary system
│   └── Metadata/        # Metadata extraction
├── OpenApi/             # OpenAPI 3.x support
│   ├── Types/           # OpenAPI spec types
│   ├── Parser/          # Spec parsing
│   └── Renderer/        # Spec rendering
└── Codegen/             # Code generation
    ├── Core/            # Codegen infrastructure
    ├── TH/              # Template Haskell support
    ├── Strategy/        # Generation strategies
    ├── Aeson/           # Aeson instance generation
    ├── Servant/         # Servant route generation
    └── Yesod/           # Yesod route generation
```

## Part 1: JSON Schema Core

### 1.1 Schema Representation

#### Version Support

Support all major JSON Schema versions with a unified AST:

```haskell
data JsonSchemaVersion
  = Draft04
  | Draft06
  | Draft07
  | Draft201909
  | Draft202012
  deriving (Eq, Show, Enum, Bounded)

data Schema = Schema
  { schemaVersion :: Maybe JsonSchemaVersion
  , schemaId :: Maybe URI
  , schemaRef :: Maybe Reference
  , schemaVocabulary :: Maybe (Map Text Bool)
  , schemaCore :: SchemaCore
  , schemaAnnotations :: SchemaAnnotations
  , schemaValidation :: SchemaValidation
  , schemaMetadata :: SchemaMetadata
  , schemaExtensions :: Map Text Value -- For custom properties
  }
```

#### Core Schema Types

```haskell
data SchemaCore
  = BooleanSchema Bool
  | ObjectSchema SchemaObject
  deriving (Eq, Show)

data SchemaObject = SchemaObject
  { -- Type keywords
    schemaType :: Maybe (OneOrMany SchemaType)
  , schemaEnum :: Maybe (NonEmpty Value)
  , schemaConst :: Maybe Value

  -- Composition keywords
  , schemaAllOf :: Maybe (NonEmpty Schema)
  , schemaAnyOf :: Maybe (NonEmpty Schema)
  , schemaOneOf :: Maybe (NonEmpty Schema)
  , schemaNot :: Maybe Schema

  -- Conditional keywords (draft-07+)
  , schemaIf :: Maybe Schema
  , schemaThen :: Maybe Schema
  , schemaElse :: Maybe Schema

  -- Dynamic references (2019-09+)
  , schemaDynamicRef :: Maybe Reference
  , schemaDynamicAnchor :: Maybe Text
  }

data SchemaType
  = NullType
  | BooleanType
  | ObjectType
  | ArrayType
  | NumberType
  | StringType
  | IntegerType
  deriving (Eq, Show, Enum, Bounded)
```

#### Validation Keywords

```haskell
data SchemaValidation = SchemaValidation
  { -- Numeric validation
    validationMultipleOf :: Maybe Scientific
  , validationMaximum :: Maybe Scientific
  , validationExclusiveMaximum :: Maybe (Either Bool Scientific)
  , validationMinimum :: Maybe Scientific
  , validationExclusiveMinimum :: Maybe (Either Bool Scientific)

  -- String validation
  , validationMaxLength :: Maybe Natural
  , validationMinLength :: Maybe Natural
  , validationPattern :: Maybe Regex
  , validationFormat :: Maybe Format

  -- Array validation
  , validationItems :: Maybe (Either Schema (NonEmpty Schema))
  , validationPrefixItems :: Maybe (NonEmpty Schema) -- 2020-12
  , validationAdditionalItems :: Maybe Schema
  , validationContains :: Maybe Schema
  , validationMaxItems :: Maybe Natural
  , validationMinItems :: Maybe Natural
  , validationUniqueItems :: Maybe Bool
  , validationMaxContains :: Maybe Natural -- 2019-09+
  , validationMinContains :: Maybe Natural -- 2019-09+

  -- Object validation
  , validationProperties :: Maybe (Map Text Schema)
  , validationPatternProperties :: Maybe (Map Regex Schema)
  , validationAdditionalProperties :: Maybe Schema
  , validationPropertyNames :: Maybe Schema
  , validationMaxProperties :: Maybe Natural
  , validationMinProperties :: Maybe Natural
  , validationRequired :: Maybe (Set Text)
  , validationDependentRequired :: Maybe (Map Text (Set Text)) -- 2019-09+
  , validationDependentSchemas :: Maybe (Map Text Schema) -- 2019-09+
  }

data Format
  = DateTime | Date | Time
  | Email | IDNEmail
  | Hostname | IDNHostname
  | IPv4 | IPv6
  | URI | URIReference | IRI | IRIReference
  | URITemplate
  | JSONPointer | RelativeJSONPointer
  | Regex
  | UUID
  | CustomFormat Text
  deriving (Eq, Show)
```

#### Annotations and Metadata

```haskell
data SchemaAnnotations = SchemaAnnotations
  { annotationTitle :: Maybe Text
  , annotationDescription :: Maybe Text
  , annotationDefault :: Maybe Value
  , annotationExamples :: [Value]
  , annotationDeprecated :: Maybe Bool -- 2019-09+
  , annotationReadOnly :: Maybe Bool
  , annotationWriteOnly :: Maybe Bool

  -- Custom annotations for codegen
  , annotationCodegen :: Maybe CodegenAnnotations
  }

data CodegenAnnotations = CodegenAnnotations
  { codegenTypeName :: Maybe Text
  , codegenNewtype :: Maybe NewtypeSpec
  , codegenFieldMapping :: Maybe (Map Text Text)
  , codegenOmitEmpty :: Maybe Bool
  , codegenStrictFields :: Maybe Bool
  , codegenCustomStrategy :: Maybe Text
  , codegenImports :: [Text]
  }

data NewtypeSpec = NewtypeSpec
  { newtypeConstructor :: Text
  , newtypeModule :: Maybe Text
  , newtypeValidation :: Maybe Text -- Function name for validation
  , newtypeInstances :: [Text] -- Additional typeclasses
  }
```

### 1.2 Vocabulary System

Support the JSON Schema 2019-09+ vocabulary system for extensibility:

```haskell
data Vocabulary = Vocabulary
  { vocabularyURI :: URI
  , vocabularyKeywords :: Map Text KeywordDefinition
  , vocabularyValidator :: Maybe VocabularyValidator
  }

data KeywordDefinition = KeywordDefinition
  { keywordName :: Text
  , keywordAppliesTo :: KeywordScope
  , keywordParser :: Value -> Either ParseError KeywordValue
  , keywordValidator :: Maybe KeywordValidator
  , keywordAnnotator :: Maybe KeywordAnnotator
  }

data KeywordScope
  = AnySchema
  | ObjectSchemaOnly
  | ArraySchemaOnly
  | StringSchemaOnly
  | NumericSchemaOnly
  deriving (Eq, Show)

type VocabularyValidator = Schema -> ValidationContext -> Either ValidationError ()
type KeywordValidator = KeywordValue -> Value -> ValidationContext -> ValidationResult
type KeywordAnnotator = KeywordValue -> Value -> Map Text Value
```

Standard vocabularies to implement:

1. **Core Vocabulary** (`https://json-schema.org/draft/2020-12/vocab/core`)
2. **Applicator Vocabulary** (`https://json-schema.org/draft/2020-12/vocab/applicator`)
3. **Validation Vocabulary** (`https://json-schema.org/draft/2020-12/vocab/validation`)
4. **Meta-Data Vocabulary** (`https://json-schema.org/draft/2020-12/vocab/meta-data`)
5. **Format Annotation Vocabulary** (`https://json-schema.org/draft/2020-12/vocab/format-annotation`)
6. **Format Assertion Vocabulary** (`https://json-schema.org/draft/2020-12/vocab/format-assertion`)
7. **Content Vocabulary** (`https://json-schema.org/draft/2020-12/vocab/content`)
8. **Unevaluated Vocabulary** (`https://json-schema.org/draft/2020-12/vocab/unevaluated`)

### 1.3 Parser

Parse JSON Schema from JSON/YAML with version detection:

```haskell
parseSchema :: Value -> Either ParseError Schema
parseSchemaWithVersion :: JsonSchemaVersion -> Value -> Either ParseError Schema
parseSchemaStrict :: Value -> Either ParseError Schema -- Fail on unknown keywords

data ParseError = ParseError
  { parseErrorPath :: JSONPointer
  , parseErrorMessage :: Text
  , parseErrorContext :: Maybe Value
  }
```

Features:
- Automatic version detection from `$schema`
- Reference resolution (`$ref`, `$dynamicRef`)
- Schema bundling (resolve all external references)
- Strict vs. lenient parsing modes
- Custom vocabulary loading

### 1.4 Renderer

Render schemas back to JSON/YAML:

```haskell
renderSchema :: Schema -> Value
renderSchemaMinimal :: Schema -> Value -- Omit defaults
renderSchemaCanonical :: Schema -> Value -- Normalized form

-- Pretty-printing
renderSchemaYAML :: Schema -> ByteString
renderSchemaJSON :: Schema -> ByteString
```

### 1.5 Validator

High-performance validation engine with detailed error reporting:

```haskell
data ValidationConfig = ValidationConfig
  { validationVersion :: JsonSchemaVersion
  , validationVocabularies :: [Vocabulary]
  , validationFormatAssertion :: Bool
  , validationShortCircuit :: Bool -- Stop on first error
  , validationCollectAnnotations :: Bool
  , validationCustomValidators :: Map Text CustomValidator
  }

data ValidationResult
  = ValidationSuccess ValidationAnnotations
  | ValidationFailure ValidationErrors

data ValidationError = ValidationError
  { errorKeyword :: Text
  , errorSchemaPath :: JSONPointer
  , errorInstancePath :: JSONPointer
  , errorMessage :: Text
  , errorDetails :: Maybe Value
  }

newtype ValidationErrors = ValidationErrors (NonEmpty ValidationError)
newtype ValidationAnnotations = ValidationAnnotations (Map JSONPointer (Map Text Value))

-- Main validation functions
validateValue :: ValidationConfig -> Schema -> Value -> ValidationResult
validateValueWithContext :: ValidationContext -> Schema -> Value -> ValidationResult

-- Batch validation
validateValues :: ValidationConfig -> Schema -> [Value] -> [(Value, ValidationResult)]

-- Incremental validation (for streaming)
newtype Validator = Validator { runValidator :: Value -> ValidationResult }
compileValidator :: ValidationConfig -> Schema -> Either CompileError Validator
```

#### Validation Context

```haskell
data ValidationContext = ValidationContext
  { contextConfig :: ValidationConfig
  , contextSchemaRegistry :: SchemaRegistry
  , contextBaseURI :: Maybe URI
  , contextDynamicScope :: [Schema]
  , contextEvaluatedProperties :: Set Text
  , contextEvaluatedItems :: Set Int
  }

data SchemaRegistry = SchemaRegistry
  { registrySchemas :: Map URI Schema
  , registryAnchors :: Map (URI, Text) Schema
  }
```

### 1.6 Metadata Extraction

Extract rich metadata from schemas for introspection:

```haskell
data SchemaMetadata = SchemaMetadata
  { metadataTitle :: Maybe Text
  , metadataDescription :: Maybe Text
  , metadataType :: SchemaTypeInfo
  , metadataProperties :: Map Text PropertyMetadata
  , metadataConstraints :: [Constraint]
  , metadataExamples :: [Value]
  , metadataDeprecated :: Bool
  , metadataCustom :: Map Text Value
  }

data SchemaTypeInfo
  = TypeSimple SchemaType
  | TypeUnion (NonEmpty SchemaType)
  | TypeComposite CompositeType
  | TypeAny
  | TypeNever

data CompositeType
  = AllOfType (NonEmpty Schema)
  | AnyOfType (NonEmpty Schema)
  | OneOfType (NonEmpty Schema)

data PropertyMetadata = PropertyMetadata
  { propertyType :: SchemaTypeInfo
  , propertyRequired :: Bool
  , propertyReadOnly :: Bool
  , propertyWriteOnly :: Bool
  , propertyDeprecated :: Bool
  , propertyDescription :: Maybe Text
  , propertyConstraints :: [Constraint]
  }

data Constraint
  = RangeConstraint Scientific Scientific
  | LengthConstraint Natural Natural
  | PatternConstraint Regex
  | FormatConstraint Format
  | EnumConstraint (NonEmpty Value)
  | CustomConstraint Text Value

-- Extraction functions
extractMetadata :: Schema -> SchemaMetadata
extractPropertyMetadata :: Text -> Schema -> Maybe PropertyMetadata
analyzeSchemaType :: Schema -> SchemaTypeInfo
extractConstraints :: Schema -> [Constraint]
```

## Part 2: OpenAPI 3.x Support

### 2.1 OpenAPI Types

```haskell
data OpenApiSpec = OpenApiSpec
  { openApiVersion :: Text -- "3.0.0", "3.1.0"
  , openApiInfo :: Info
  , openApiServers :: [Server]
  , openApiPaths :: Map Text PathItem
  , openApiComponents :: Maybe Components
  , openApiSecurity :: [SecurityRequirement]
  , openApiTags :: [Tag]
  , openApiExternalDocs :: Maybe ExternalDocs
  , openApiExtensions :: Map Text Value
  }

data PathItem = PathItem
  { pathItemSummary :: Maybe Text
  , pathItemDescription :: Maybe Text
  , pathItemGet :: Maybe Operation
  , pathItemPut :: Maybe Operation
  , pathItemPost :: Maybe Operation
  , pathItemDelete :: Maybe Operation
  , pathItemOptions :: Maybe Operation
  , pathItemHead :: Maybe Operation
  , pathItemPatch :: Maybe Operation
  , pathItemTrace :: Maybe Operation
  , pathItemServers :: [Server]
  , pathItemParameters :: [ReferenceOr Parameter]
  }

data Operation = Operation
  { operationTags :: [Text]
  , operationSummary :: Maybe Text
  , operationDescription :: Maybe Text
  , operationOperationId :: Maybe Text
  , operationParameters :: [ReferenceOr Parameter]
  , operationRequestBody :: Maybe (ReferenceOr RequestBody)
  , operationResponses :: Responses
  , operationCallbacks :: Map Text (ReferenceOr Callback)
  , operationDeprecated :: Bool
  , operationSecurity :: [SecurityRequirement]
  , operationServers :: [Server]
  }

data Components = Components
  { componentsSchemas :: Map Text (ReferenceOr Schema)
  , componentsResponses :: Map Text (ReferenceOr Response)
  , componentsParameters :: Map Text (ReferenceOr Parameter)
  , componentsExamples :: Map Text (ReferenceOr Example)
  , componentsRequestBodies :: Map Text (ReferenceOr RequestBody)
  , componentsHeaders :: Map Text (ReferenceOr Header)
  , componentsSecuritySchemes :: Map Text (ReferenceOr SecurityScheme)
  , componentsLinks :: Map Text (ReferenceOr Link)
  , componentsCallbacks :: Map Text (ReferenceOr Callback)
  }

data ReferenceOr a
  = Reference Reference
  | Inline a
```

### 2.2 OpenAPI Parsing and Rendering

```haskell
parseOpenApiSpec :: Value -> Either ParseError OpenApiSpec
parseOpenApiSpecFromFile :: FilePath -> IO (Either ParseError OpenApiSpec)

renderOpenApiSpec :: OpenApiSpec -> Value
renderOpenApiSpecYAML :: OpenApiSpec -> ByteString
renderOpenApiSpecJSON :: OpenApiSpec -> ByteString

-- Reference resolution
resolveOpenApiReferences :: OpenApiSpec -> Either ResolveError OpenApiSpec
bundleOpenApiSpec :: OpenApiSpec -> Either BundleError OpenApiSpec
```

### 2.3 OpenAPI Validation

Validate OpenAPI specs and validate data against OpenAPI operations:

```haskell
validateOpenApiSpec :: OpenApiSpec -> Either ValidationError ()

validateRequest
  :: OpenApiSpec
  -> Text -- Path
  -> Text -- Method
  -> Value -- Request body
  -> Either ValidationError ()

validateResponse
  :: OpenApiSpec
  -> Text -- Path
  -> Text -- Method
  -> Int -- Status code
  -> Value -- Response body
  -> Either ValidationError ()
```

## Part 3: Code Generation System

### 3.1 Codegen Architecture

The code generation system is designed to be highly pluggable and extensible:

```haskell
-- Core codegen types
data CodegenContext = CodegenContext
  { codegenSchema :: Schema
  , codegenConfig :: CodegenConfig
  , codegenStrategy :: CodegenStrategy
  , codegenNamingConvention :: NamingConvention
  , codegenModuleName :: Maybe ModuleName
  , codegenImports :: Set Import
  , codegenTypeRegistry :: TypeRegistry
  }

data CodegenConfig = CodegenConfig
  { codegenTargetVersion :: JsonSchemaVersion
  , codegenStrictValidation :: Bool
  , codegenOmitNullable :: Bool
  , codegenGenerateToJSON :: Bool
  , codegenGenerateFromJSON :: Bool
  , codegenGenerateLenses :: Bool
  , codegenSmartConstructors :: Bool
  , codegenNewtypeAnnotations :: Bool
  , codegenFieldPrefixes :: Maybe Text
  , codegenCustomStrategies :: Map Text CodegenStrategy
  }

-- Pluggable strategy system
data CodegenStrategy = CodegenStrategy
  { strategyName :: Text
  , strategyTypeGen :: TypeGenerator
  , strategyFieldGen :: FieldGenerator
  , strategyInstanceGen :: InstanceGenerator
  , strategyConstraintGen :: ConstraintGenerator
  }

type TypeGenerator = CodegenContext -> Schema -> Q [Dec]
type FieldGenerator = CodegenContext -> Text -> Schema -> Q (Name, Type)
type InstanceGenerator = CodegenContext -> Name -> Schema -> Q [Dec]
type ConstraintGenerator = CodegenContext -> Schema -> Q [Exp]
```

### 3.2 Type Generation Strategies

#### Default Strategy

Generates standard Haskell datatypes:

```haskell
-- Schema:
-- {
--   "type": "object",
--   "properties": {
--     "name": {"type": "string"},
--     "age": {"type": "integer", "minimum": 0}
--   },
--   "required": ["name"]
-- }

-- Generated:
data Person = Person
  { personName :: Text
  , personAge :: Maybe Int
  } deriving (Eq, Show, Generic)

instance FromJSON Person where
  parseJSON = withObject "Person" $ \o -> do
    personName <- o .: "name"
    personAge <- o .:? "age"
    -- Validation: age >= 0
    forM_ personAge $ \age ->
      when (age < 0) $ fail "age must be >= 0"
    pure Person{..}

instance ToJSON Person where
  toJSON Person{..} = object
    [ "name" .= personName
    , "age" .= personAge
    ]
```

#### Newtype Strategy

Generates semantic newtypes with validation:

```haskell
-- Schema with annotation:
-- {
--   "type": "string",
--   "format": "email",
--   "x-newtype": {
--     "constructor": "Email",
--     "validation": "validateEmail"
--   }
-- }

-- Generated:
newtype Email = Email Text
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (Hashable)

mkEmail :: Text -> Either ValidationError Email
mkEmail txt =
  if validateEmail txt
    then Right (Email txt)
    else Left (ValidationError "Invalid email format")

instance FromJSON Email where
  parseJSON = withText "Email" $ \txt ->
    either (fail . show) pure (mkEmail txt)

instance ToJSON Email where
  toJSON (Email txt) = String txt
```

#### Sum Type Strategy

For `oneOf` and `anyOf`:

```haskell
-- Schema:
-- {
--   "oneOf": [
--     {"type": "string"},
--     {"type": "integer"}
--   ]
-- }

-- Generated:
data StringOrInt
  = SIString Text
  | SIInteger Int
  deriving (Eq, Show, Generic)

instance FromJSON StringOrInt where
  parseJSON v =
    (SIString <$> parseJSON v) <|>
    (SIInteger <$> parseJSON v)

instance ToJSON StringOrInt where
  toJSON (SIString s) = toJSON s
  toJSON (SIInteger i) = toJSON i
```

### 3.3 Template Haskell Interface

```haskell
-- Generate types from a schema
deriveJSONSchema :: Schema -> Q [Dec]
deriveJSONSchemaWith :: CodegenConfig -> Schema -> Q [Dec]

-- Generate from file
deriveJSONSchemaFromFile :: FilePath -> Q [Dec]
deriveJSONSchemaFromURL :: String -> Q [Dec]

-- Generate with custom strategy
deriveJSONSchemaCustom :: CodegenStrategy -> Schema -> Q [Dec]

-- Generate specific components
deriveType :: Schema -> Q [Dec]
deriveAesonInstances :: Name -> Schema -> Q [Dec]
deriveValidator :: Name -> Schema -> Q [Dec]
deriveLenses :: Name -> Q [Dec]

-- OpenAPI-specific
deriveOpenApiTypes :: OpenApiSpec -> Q [Dec]
deriveServantAPI :: OpenApiSpec -> Q [Dec]
deriveYesodRoutes :: OpenApiSpec -> Q [Dec]
```

Example usage:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Fractal.OpenApi.Codegen.TH

-- From embedded schema
$(deriveJSONSchemaFromFile "schemas/person.json")

-- From OpenAPI spec
$(deriveOpenApiTypes =<< runIO (loadOpenApiSpec "api.yaml"))

-- Custom strategy
$(deriveJSONSchemaCustom myCustomStrategy mySchema)
```

### 3.4 Semantic Newtype Annotations

Extension to JSON Schema for rich type generation:

```json
{
  "type": "object",
  "properties": {
    "email": {
      "type": "string",
      "format": "email",
      "x-newtype": {
        "constructor": "Email",
        "module": "Types.Email",
        "validation": "validateEmail",
        "instances": ["Hashable", "ToText"]
      }
    },
    "userId": {
      "type": "string",
      "format": "uuid",
      "x-newtype": {
        "constructor": "UserId",
        "instances": ["Hashable", "Ord"]
      }
    },
    "age": {
      "type": "integer",
      "minimum": 0,
      "maximum": 150,
      "x-newtype": {
        "constructor": "Age",
        "validation": "validateAge"
      }
    }
  },
  "x-codegen": {
    "type-name": "User",
    "strict-fields": true,
    "generate-lenses": true,
    "omit-empty": false
  }
}
```

Generated code:

```haskell
-- Types.Email module (if specified)
module Types.Email (Email, mkEmail, validateEmail) where

newtype Email = Email Text
  deriving (Eq, Show, Generic)
  deriving newtype (ToText, Hashable)

mkEmail :: Text -> Either ValidationError Email
mkEmail = validateEmail >=> pure . Email

validateEmail :: Text -> Either ValidationError Text
validateEmail txt = -- RFC 5322 validation

instance FromJSON Email
instance ToJSON Email

-- Main User type
data User = User
  { userEmail :: !Email
  , userUserId :: !UserId
  , userAge :: !Age
  } deriving (Eq, Show, Generic)

makeLenses ''User

instance FromJSON User
instance ToJSON User
```

### 3.5 Servant Integration

Generate Servant API types from OpenAPI specs:

```haskell
-- OpenAPI spec with operations
$(deriveServantAPI =<< runIO (loadOpenApiSpec "api.yaml"))

-- Generated:
type UserAPI
  = "users" :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] User
  :<|> "users" :> Capture "userId" UserId :> Get '[JSON] User
  :<|> "users" :> Capture "userId" UserId :> ReqBody '[JSON] UpdateUserRequest :> Put '[JSON] User
  :<|> "users" :> Capture "userId" UserId :> Delete '[JSON] NoContent

-- Also generate handler type expectations
type UserHandler m
  = m [User]
  :<|> (CreateUserRequest -> m User)
  :<|> (UserId -> m User)
  :<|> (UserId -> UpdateUserRequest -> m User)
  :<|> (UserId -> m NoContent)

userAPI :: Proxy UserAPI
userAPI = Proxy

-- Generate client
userClient :: UserHandler ClientM
userClient = client userAPI
```

### 3.6 Yesod Integration

Generate Yesod routes from OpenAPI specs:

```haskell
$(deriveYesodRoutes =<< runIO (loadOpenApiSpec "api.yaml"))

-- Generated routes file:
-- /users UsersR GET POST
-- /users/#UserId UserR GET PUT DELETE

-- Handler type expectations
getUsersR :: Handler [User]
postUsersR :: Handler User
getUserR :: UserId -> Handler User
putUserR :: UserId -> Handler User
deleteUserR :: UserId -> Handler ()
```

## Part 4: Implementation Roadmap

### Phase 1: Core JSON Schema (Weeks 1-3)

**Week 1: Types and Parser**
- [ ] Define core Schema types (draft-07 support first)
- [ ] Implement JSON parser with FromJSON instances
- [ ] Reference resolution ($ref support)
- [ ] Basic test suite

**Week 2: Validator**
- [ ] Validation context and registry
- [ ] Core validation keywords (type, properties, items, etc.)
- [ ] Numeric validation keywords
- [ ] String validation keywords
- [ ] Array validation keywords
- [ ] Object validation keywords

**Week 3: Advanced Features**
- [ ] Composition keywords (allOf, anyOf, oneOf, not)
- [ ] Conditional keywords (if/then/else)
- [ ] Format validation
- [ ] Detailed error reporting
- [ ] Validation test suite

### Phase 2: Multi-Version Support (Weeks 4-5)

**Week 4: Version Compatibility**
- [ ] Draft-04 support
- [ ] Draft-06 support
- [ ] Version detection and migration
- [ ] Backward compatibility tests

**Week 5: Modern Versions**
- [ ] Draft 2019-09 support (unevaluated*, dependent*)
- [ ] Draft 2020-12 support (prefixItems, $dynamicRef)
- [ ] Version-specific test suites

### Phase 3: Vocabulary System (Week 6)

- [ ] Vocabulary type definitions
- [ ] Standard vocabulary implementations
- [ ] Custom vocabulary API
- [ ] Vocabulary validation
- [ ] Extension mechanism

### Phase 4: Metadata and Rendering (Week 7)

- [ ] Metadata extraction API
- [ ] Schema introspection
- [ ] Schema rendering (JSON/YAML)
- [ ] Schema normalization and canonicalization

### Phase 5: Code Generation Foundation (Weeks 8-10)

**Week 8: Codegen Core**
- [ ] CodegenContext and configuration
- [ ] Strategy system architecture
- [ ] Type registry
- [ ] Naming conventions
- [ ] Module management

**Week 9: Template Haskell**
- [ ] TH quoters and splices
- [ ] Type generation from Schema
- [ ] Name generation and hygiene
- [ ] Import management

**Week 10: Default Strategy**
- [ ] Simple type generation (object, array, primitives)
- [ ] Aeson instance generation
- [ ] Validation embedding
- [ ] Optional/required field handling

### Phase 6: Advanced Codegen (Weeks 11-13)

**Week 11: Newtype Strategy**
- [ ] Newtype annotation parsing
- [ ] Smart constructor generation
- [ ] Validation function generation
- [ ] Derived instances

**Week 12: Sum Type Strategy**
- [ ] oneOf/anyOf handling
- [ ] Discriminated unions
- [ ] Tagged/untagged variants
- [ ] Aeson instance for sum types

**Week 13: Additional Features**
- [ ] Lens generation
- [ ] Constraint embedding
- [ ] Documentation generation
- [ ] Custom strategy examples

### Phase 7: OpenAPI Support (Weeks 14-16)

**Week 14: OpenAPI Types**
- [ ] OpenAPI 3.0 type definitions
- [ ] OpenAPI 3.1 type definitions
- [ ] Parser and renderer
- [ ] Reference resolution

**Week 15: OpenAPI Validation**
- [ ] Spec validation
- [ ] Request validation
- [ ] Response validation
- [ ] Security validation

**Week 16: OpenAPI Codegen**
- [ ] Component extraction
- [ ] Type generation from components
- [ ] Operation analysis
- [ ] Parameter handling

### Phase 8: Framework Integration (Weeks 17-19)

**Week 17: Servant Integration**
- [ ] API type generation
- [ ] Route analysis
- [ ] Handler type generation
- [ ] Client generation
- [ ] Examples and documentation

**Week 18: Yesod Integration**
- [ ] Route file generation
- [ ] Handler type generation
- [ ] Form generation
- [ ] Widget generation
- [ ] Examples and documentation

**Week 19: Integration Testing**
- [ ] End-to-end Servant examples
- [ ] End-to-end Yesod examples
- [ ] Performance benchmarks
- [ ] Real-world API examples

### Phase 9: Polish and Documentation (Week 20)

- [ ] Comprehensive documentation
- [ ] Tutorial and guides
- [ ] API reference
- [ ] Migration guides
- [ ] Performance optimization
- [ ] Final testing and bug fixes

## Part 5: Testing Strategy

### Unit Tests

- JSON Schema parser tests
- Validator tests for each keyword
- Vocabulary system tests
- Codegen strategy tests
- TH generation tests

### Integration Tests

- Multi-schema validation
- Complex composition scenarios
- OpenAPI end-to-end tests
- Framework integration tests

### Property Tests

- Parser/renderer roundtrip
- Validation consistency
- Generated code validity
- Schema normalization

### Compliance Tests

- JSON Schema Test Suite (official tests)
- OpenAPI validation tests
- Format validation tests

### Performance Tests

- Large schema parsing
- Validation performance
- Compilation time benchmarks
- Generated code performance

## Part 6: Dependencies

### Core Dependencies

```yaml
base: ">= 4.17.0.0 && < 5"
aeson: ">= 2.0 && < 3"
text: ">= 2.0 && < 3"
bytestring: ">= 0.11 && < 1"
containers: ">= 0.6 && < 1"
unordered-containers: ">= 0.2 && < 1"
vector: ">= 0.13 && < 1"
scientific: ">= 0.3 && < 1"
hashable: ">= 1.4 && < 2"
```

### Additional Dependencies

```yaml
# Parsing
yaml: ">= 0.11 && < 1"
uri-bytestring: ">= 0.3 && < 1"

# Regex
regex-tdfa: ">= 1.3 && < 2"

# Template Haskell
template-haskell: ">= 2.19 && < 3"
th-lift: ">= 0.8 && < 1"

# Validation
validation-selective: ">= 0.1 && < 1"

# Optional framework integration
servant: ">= 0.19 && < 1"  # Optional
servant-server: ">= 0.19 && < 1"  # Optional
yesod-core: ">= 1.6 && < 2"  # Optional

# Lenses (optional)
lens: ">= 5.0 && < 6"
```

## Part 7: Design Principles

1. **Correctness First**: Strict adherence to JSON Schema specifications
2. **Performance**: Efficient validation and code generation
3. **Extensibility**: Pluggable vocabularies and codegen strategies
4. **Type Safety**: Leverage Haskell's type system for safety
5. **Ergonomics**: Simple API for common cases, powerful API for advanced use
6. **Documentation**: Comprehensive docs and examples
7. **Testing**: High test coverage with compliance tests

## Part 8: Future Enhancements

- [ ] GraphQL schema generation
- [ ] Protocol Buffers integration
- [ ] AsyncAPI support
- [ ] JSON-LD support
- [ ] Schema migration tools
- [ ] Web UI for schema exploration
- [ ] LSP server for schema editing
- [ ] CLI tools for validation and codegen
- [ ] Integration with fractal-schema registry

## Part 9: Non-Goals

- Not a JSON parser (use aeson)
- Not a YAML parser (use yaml)
- Not a web framework (integrate with existing ones)
- Not a database library
- Not a general-purpose validation library
- Not a replacement for aeson or servant

## References

- [JSON Schema Specification](https://json-schema.org/specification.html)
- [OpenAPI Specification](https://spec.openapis.org/oas/latest.html)
- [JSON Schema Test Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite)
- [Understanding JSON Schema](https://json-schema.org/understanding-json-schema/)
- [Servant Documentation](https://docs.servant.dev/)
- [Yesod Book](https://www.yesodweb.com/book)
