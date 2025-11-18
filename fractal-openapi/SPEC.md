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

### 1.2 Vocabulary and Dialect System

The vocabulary system is the cornerstone of extensibility in JSON Schema 2019-09+. It allows custom keywords to be added, providing domain-specific validation, annotations, and semantics.

#### 1.2.1 Vocabularies

A **vocabulary** is a collection of keywords with defined semantics. Each vocabulary has a unique URI identifier and specifies how its keywords should be interpreted.

```haskell
data Vocabulary = Vocabulary
  { vocabularyURI :: URI
  , vocabularyRequired :: Bool  -- Whether this vocabulary must be understood
  , vocabularyKeywords :: Map Text KeywordDefinition
  , vocabularyValidator :: Maybe VocabularyValidator
  , vocabularyMetaSchema :: Maybe Schema  -- Schema that describes this vocabulary
  }

data KeywordDefinition = KeywordDefinition
  { keywordName :: Text
  , keywordAppliesTo :: KeywordScope
  , keywordParser :: Value -> Either ParseError KeywordValue
  , keywordValidator :: Maybe KeywordValidator
  , keywordAnnotator :: Maybe KeywordAnnotator
  , keywordPriority :: Int  -- Evaluation order
  }

data KeywordScope
  = AnySchema
  | ObjectSchemaOnly
  | ArraySchemaOnly
  | StringSchemaOnly
  | NumericSchemaOnly
  | BooleanSchemaOnly
  deriving (Eq, Show, Ord)

-- Keyword value - opaque type that keywords can define
data KeywordValue where
  KeywordValue :: (Eq a, Show a, Typeable a) => a -> KeywordValue

type VocabularyValidator = Schema -> ValidationContext -> Either ValidationError ()
type KeywordValidator = KeywordValue -> Value -> ValidationContext -> ValidationResult
type KeywordAnnotator = KeywordValue -> Value -> Map Text Value
```

#### 1.2.2 Dialects

A **dialect** is a complete configuration of vocabularies that defines a JSON Schema variant. Dialects specify which vocabularies are enabled, their requirement status, and any additional configuration.

```haskell
data Dialect = Dialect
  { dialectURI :: URI
  , dialectName :: Text
  , dialectVersion :: Text
  , dialectVocabularies :: Map URI Bool  -- URI -> required?
  , dialectMetaSchema :: Schema
  , dialectDefaultFormat :: FormatBehavior
  , dialectUnknownKeywords :: UnknownKeywordMode
  }

-- How to handle unknown keywords
data UnknownKeywordMode
  = IgnoreUnknown        -- Silently ignore
  | WarnUnknown          -- Emit warnings but continue
  | ErrorOnUnknown       -- Fail parsing
  | CollectUnknown       -- Collect in extensions map
  deriving (Eq, Show)

-- Format keyword behavior
data FormatBehavior
  = FormatAssertion      -- Formats are validation
  | FormatAnnotation     -- Formats are annotations only
  deriving (Eq, Show)
```

Standard dialects to implement:

1. **Draft-04 Dialect** (`http://json-schema.org/draft-04/schema#`)
2. **Draft-06 Dialect** (`http://json-schema.org/draft-06/schema#`)
3. **Draft-07 Dialect** (`http://json-schema.org/draft-07/schema#`)
4. **2019-09 Dialect** (`https://json-schema.org/draft/2019-09/schema`)
5. **2020-12 Dialect** (`https://json-schema.org/draft/2020-12/schema`)

#### 1.2.3 Vocabulary Registry

The vocabulary registry manages all known vocabularies and provides lookup and composition functions.

```haskell
data VocabularyRegistry = VocabularyRegistry
  { registeredVocabularies :: Map URI Vocabulary
  , registeredDialects :: Map URI Dialect
  , defaultDialect :: Dialect
  }

-- Registry operations
emptyRegistry :: VocabularyRegistry
registerVocabulary :: Vocabulary -> VocabularyRegistry -> VocabularyRegistry
registerDialect :: Dialect -> VocabularyRegistry -> VocabularyRegistry
lookupVocabulary :: URI -> VocabularyRegistry -> Maybe Vocabulary
lookupDialect :: URI -> VocabularyRegistry -> Maybe Dialect

-- Compose vocabularies into a dialect
composeDialect
  :: Text                    -- Dialect name
  -> URI                     -- Dialect URI
  -> [(URI, Bool)]           -- Vocabularies (URI, required?)
  -> VocabularyRegistry
  -> Either ComposeError Dialect

-- Check vocabulary compatibility
checkVocabularyCompatibility
  :: Vocabulary
  -> Vocabulary
  -> Either CompatibilityError ()
```

#### 1.2.4 Creating Custom Vocabularies

Example: Creating a business domain vocabulary

```haskell
-- Define custom keywords for business validation
data CreditCardKeyword = CreditCardKeyword
  { ccProvider :: Maybe Text  -- "visa", "mastercard", etc.
  , ccLuhnCheck :: Bool
  } deriving (Eq, Show, Typeable)

data TaxIDKeyword = TaxIDKeyword
  { taxIDCountry :: Text
  , taxIDFormat :: Text
  } deriving (Eq, Show, Typeable)

-- Create the vocabulary
businessVocabulary :: Vocabulary
businessVocabulary = Vocabulary
  { vocabularyURI = "https://example.com/vocabs/business/v1"
  , vocabularyRequired = False
  , vocabularyKeywords = Map.fromList
      [ ("x-credit-card", creditCardKeywordDef)
      , ("x-tax-id", taxIDKeywordDef)
      , ("x-currency", currencyKeywordDef)
      ]
  , vocabularyValidator = Nothing
  , vocabularyMetaSchema = Nothing
  }

-- Define credit card keyword
creditCardKeywordDef :: KeywordDefinition
creditCardKeywordDef = KeywordDefinition
  { keywordName = "x-credit-card"
  , keywordAppliesTo = StringSchemaOnly
  , keywordParser = parseCreditCardKeyword
  , keywordValidator = Just validateCreditCard
  , keywordAnnotator = Just annotateCreditCard
  , keywordPriority = 100
  }

parseCreditCardKeyword :: Value -> Either ParseError KeywordValue
parseCreditCardKeyword = \case
  Object o -> do
    provider <- o .:? "provider"
    luhnCheck <- o .:? "luhn-check" .!= True
    pure $ KeywordValue $ CreditCardKeyword provider luhnCheck
  Bool b -> pure $ KeywordValue $ CreditCardKeyword Nothing b
  _ -> Left $ ParseError "x-credit-card must be object or boolean"

validateCreditCard :: KeywordValidator
validateCreditCard (KeywordValue (cc :: CreditCardKeyword)) value ctx =
  case value of
    String txt -> do
      -- Validate credit card number
      when (ccLuhnCheck cc && not (luhnValidate txt)) $
        validationError "Credit card number fails Luhn check"

      -- Validate provider if specified
      forM_ (ccProvider cc) $ \provider -> do
        let prefix = detectCardProvider txt
        when (prefix /= provider) $
          validationError $ "Credit card provider mismatch: expected "
                         <> provider <> ", got " <> prefix

      ValidationSuccess mempty
    _ -> ValidationFailure $ ValidationError "Value must be a string"

annotateCreditCard :: KeywordAnnotator
annotateCreditCard (KeywordValue (cc :: CreditCardKeyword)) value =
  Map.fromList
    [ ("creditCardProvider", toJSON $ ccProvider cc)
    , ("creditCardLuhnCheck", toJSON $ ccLuhnCheck cc)
    ]
```

#### 1.2.5 Custom Dialect Example

Creating a company-specific dialect:

```haskell
-- Company dialect combining standard + custom vocabularies
acmeCorpDialect :: Dialect
acmeCorpDialect = Dialect
  { dialectURI = "https://acme.com/json-schema/2024"
  , dialectName = "ACME Corp JSON Schema"
  , dialectVersion = "2024.1"
  , dialectVocabularies = Map.fromList
      -- Standard vocabularies (all required)
      [ ("https://json-schema.org/draft/2020-12/vocab/core", True)
      , ("https://json-schema.org/draft/2020-12/vocab/applicator", True)
      , ("https://json-schema.org/draft/2020-12/vocab/validation", True)
      , ("https://json-schema.org/draft/2020-12/vocab/meta-data", True)
      , ("https://json-schema.org/draft/2020-12/vocab/format-annotation", False)

      -- Custom vocabularies
      , ("https://acme.com/vocabs/business/v1", True)      -- Required
      , ("https://acme.com/vocabs/security/v1", True)      -- Required
      , ("https://acme.com/vocabs/codegen/v1", False)      -- Optional
      ]
  , dialectMetaSchema = acmeMetaSchema
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = WarnUnknown
  }

-- Security vocabulary with custom keywords
securityVocabulary :: Vocabulary
securityVocabulary = Vocabulary
  { vocabularyURI = "https://acme.com/vocabs/security/v1"
  , vocabularyRequired = True
  , vocabularyKeywords = Map.fromList
      [ ("x-sensitive", sensitiveKeywordDef)
      , ("x-pii", piiKeywordDef)
      , ("x-encryption-required", encryptionKeywordDef)
      , ("x-access-level", accessLevelKeywordDef)
      ]
  , vocabularyValidator = Just validateSecurityConstraints
  , vocabularyMetaSchema = Just securityMetaSchema
  }

-- Codegen vocabulary for type generation hints
codegenVocabulary :: Vocabulary
codegenVocabulary = Vocabulary
  { vocabularyURI = "https://acme.com/vocabs/codegen/v1"
  , vocabularyRequired = False  -- Optional - won't fail if unknown
  , vocabularyKeywords = Map.fromList
      [ ("x-haskell-type", haskellTypeKeywordDef)
      , ("x-haskell-module", haskellModuleKeywordDef)
      , ("x-haskell-deriving", haskellDerivingKeywordDef)
      , ("x-haskell-newtype", haskellNewtypeKeywordDef)
      ]
  , vocabularyValidator = Nothing  -- Codegen keywords don't validate data
  , vocabularyMetaSchema = Just codegenMetaSchema
  }
```

#### 1.2.6 Vocabulary Composition and Inheritance

Support vocabulary inheritance and composition:

```haskell
-- Extend a vocabulary with additional keywords
extendVocabulary
  :: Vocabulary                    -- Base vocabulary
  -> [(Text, KeywordDefinition)]  -- Additional keywords
  -> Vocabulary

-- Compose multiple vocabularies (later ones override earlier)
composeVocabularies
  :: NonEmpty Vocabulary
  -> Either ComposeError Vocabulary

-- Create a vocabulary variant
variantVocabulary
  :: Vocabulary                    -- Base
  -> (Vocabulary -> Vocabulary)    -- Modifications
  -> Vocabulary
```

Example usage:

```haskell
-- Create a stricter version of the validation vocabulary
strictValidationVocab :: Vocabulary
strictValidationVocab = variantVocabulary standardValidationVocab $ \vocab ->
  vocab { vocabularyRequired = True
        , vocabularyKeywords = Map.adjust makeStrict "format" (vocabularyKeywords vocab)
        }
  where
    makeStrict keywordDef = keywordDef
      { keywordValidator = Just strictFormatValidator }
```

#### 1.2.7 Runtime Vocabulary Loading

Support loading vocabularies at runtime:

```haskell
-- Load vocabulary from JSON/YAML definition
loadVocabularyFromFile :: FilePath -> IO (Either LoadError Vocabulary)
loadVocabularyFromURL :: String -> IO (Either LoadError Vocabulary)

-- Vocabulary definition format (JSON/YAML)
data VocabularyDefinition = VocabularyDefinition
  { vocabDefURI :: Text
  , vocabDefName :: Text
  , vocabDefKeywords :: [KeywordDefinitionJSON]
  , vocabDefMetaSchemaURI :: Maybe Text
  }

-- Register vocabulary plugin (for keyword implementations)
type VocabularyPlugin = VocabularyDefinition -> Either PluginError Vocabulary

registerVocabularyPlugin
  :: URI                    -- Vocabulary URI
  -> VocabularyPlugin
  -> VocabularyRegistry
  -> VocabularyRegistry
```

#### 1.2.8 Standard Vocabularies

Implement all standard JSON Schema vocabularies:

##### Core Vocabulary
- `$schema`, `$id`, `$ref`, `$anchor`, `$dynamicRef`, `$dynamicAnchor`, `$vocabulary`, `$comment`, `$defs`

##### Applicator Vocabulary
- `prefixItems`, `items`, `contains`, `additionalProperties`, `properties`, `patternProperties`, `dependentSchemas`, `propertyNames`, `if`, `then`, `else`, `allOf`, `anyOf`, `oneOf`, `not`

##### Unevaluated Vocabulary
- `unevaluatedItems`, `unevaluatedProperties`

##### Validation Vocabulary
- Numeric: `multipleOf`, `maximum`, `exclusiveMaximum`, `minimum`, `exclusiveMinimum`
- String: `maxLength`, `minLength`, `pattern`
- Array: `maxItems`, `minItems`, `uniqueItems`, `maxContains`, `minContains`
- Object: `maxProperties`, `minProperties`, `required`, `dependentRequired`
- Generic: `type`, `enum`, `const`

##### Format Vocabularies
- Format Annotation: Treat `format` as annotation
- Format Assertion: Treat `format` as validation

##### Meta-Data Vocabulary
- `title`, `description`, `default`, `deprecated`, `readOnly`, `writeOnly`, `examples`

##### Content Vocabulary
- `contentEncoding`, `contentMediaType`, `contentSchema`

#### 1.2.9 Vocabulary Validation

Validate vocabulary definitions and check for conflicts:

```haskell
validateVocabulary :: Vocabulary -> Either ValidationError ()
validateVocabularyAgainstMetaSchema :: Vocabulary -> Either ValidationError ()

-- Check for keyword conflicts between vocabularies
detectKeywordConflicts
  :: [Vocabulary]
  -> Either ConflictError ()

-- Conflict resolution strategies
data ConflictResolution
  = FirstWins      -- First vocabulary takes precedence
  | LastWins       -- Last vocabulary takes precedence
  | ErrorOnConflict -- Fail if conflicts detected
  | MergeKeywords   -- Attempt to merge keyword definitions
  deriving (Eq, Show)

resolveVocabularyConflicts
  :: ConflictResolution
  -> [Vocabulary]
  -> Either ConflictError Vocabulary
```

#### 1.2.10 Practical Example: Full Custom Schema

Complete example using custom dialect:

```json
{
  "$schema": "https://acme.com/json-schema/2024",
  "type": "object",
  "properties": {
    "creditCard": {
      "type": "string",
      "x-credit-card": {
        "provider": "visa",
        "luhn-check": true
      },
      "x-sensitive": true,
      "x-encryption-required": "AES-256",
      "x-haskell-newtype": {
        "constructor": "CreditCardNumber",
        "module": "Acme.Payment.Types"
      }
    },
    "ssn": {
      "type": "string",
      "x-tax-id": {
        "country": "US",
        "format": "XXX-XX-XXXX"
      },
      "x-pii": {
        "category": "government-id",
        "retention-days": 2555
      },
      "x-access-level": "confidential"
    }
  }
}
```

This schema uses:
- Standard 2020-12 core vocabularies
- Custom business vocabulary (`x-credit-card`, `x-tax-id`)
- Custom security vocabulary (`x-sensitive`, `x-pii`, `x-encryption-required`, `x-access-level`)
- Custom codegen vocabulary (`x-haskell-newtype`)

All vocabularies are validated, composed, and used for both validation and code generation.
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
