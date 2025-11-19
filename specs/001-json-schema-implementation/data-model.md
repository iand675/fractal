# Phase 1: Data Model & Domain Types

**Feature**: Comprehensive JSON Schema and OpenAPI Library  
**Date**: 2025-11-18  
**Status**: Complete

## Overview

This document defines the core domain types for fractal-openapi. All types follow the constitution's type-driven development principle: invalid states are unrepresentable, and domain invariants are encoded in types.

---

## 1. JSON Schema Core Types

### 1.1 Schema (Top-Level)

The root schema type that represents any JSON Schema document.

```haskell
-- | A JSON Schema document with version, metadata, and validation rules
data Schema = Schema
  { schemaVersion :: Maybe JsonSchemaVersion
    -- ^ Schema version from $schema keyword. Nothing means latest version.
  
  , schemaId :: Maybe URI
    -- ^ Unique identifier for this schema ($id keyword)
    -- Invariant: Must be absolute URI if present
  
  , schemaCore :: SchemaCore
    -- ^ Core schema structure (boolean or object schema)
  
  , schemaVocabulary :: Maybe (Map URI Bool)
    -- ^ Vocabularies and whether they're required ($vocabulary keyword)
    -- Only valid for 2019-09+
    -- Invariant: All required vocabularies must be understood
  
  , schemaExtensions :: Map Text Value
    -- ^ Unknown keywords collected during parsing
    -- Allows forward compatibility
  }
  deriving (Eq, Show, Generic)

-- | JSON Schema specification versions
data JsonSchemaVersion
  = Draft04        -- ^ http://json-schema.org/draft-04/schema#
  | Draft06        -- ^ http://json-schema.org/draft-06/schema#
  | Draft07        -- ^ http://json-schema.org/draft-07/schema#
  | Draft201909    -- ^ https://json-schema.org/draft/2019-09/schema
  | Draft202012    -- ^ https://json-schema.org/draft/2020-12/schema
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- | Core schema structure: boolean shorthand or full object
data SchemaCore
  = BooleanSchema Bool
    -- ^ true = allow all, false = allow none
  | ObjectSchema SchemaObject
    -- ^ Full schema object with keywords
  deriving (Eq, Show, Generic)
```

**Invariants**:
- `schemaId` must be absolute URI (no relative paths)
- `schemaVocabulary` only present for 2019-09 and 2020-12
- Boolean schemas have no other keywords

---

### 1.2 SchemaObject (Main Schema Structure)

The full schema object containing all keyword groups.

```haskell
data SchemaObject = SchemaObject
  { -- === Type Keywords ===
    schemaType :: Maybe (OneOrMany SchemaType)
    -- ^ Type constraint (single type or union)
  
  , schemaEnum :: Maybe (NonEmpty Value)
    -- ^ Enumeration of allowed values
    -- Invariant: At least one value
  
  , schemaConst :: Maybe Value
    -- ^ Single allowed value (draft-06+)

    -- === Reference Keywords ===
  , schemaRef :: Maybe Reference
    -- ^ Reference to another schema ($ref)
    -- Invariant: Mutually exclusive with most other keywords in draft-04/06/07
  
  , schemaDynamicRef :: Maybe Reference
    -- ^ Dynamic reference (2020-12+)
  
  , schemaAnchor :: Maybe Text
    -- ^ Named anchor for references ($anchor, 2019-09+)
  
  , schemaDynamicAnchor :: Maybe Text
    -- ^ Dynamic anchor (2020-12+)

    -- === Composition Keywords ===
  , schemaAllOf :: Maybe (NonEmpty Schema)
    -- ^ Must satisfy all subschemas
    -- Invariant: At least one subschema
  
  , schemaAnyOf :: Maybe (NonEmpty Schema)
    -- ^ Must satisfy at least one subschema
  
  , schemaOneOf :: Maybe (NonEmpty Schema)
    -- ^ Must satisfy exactly one subschema
  
  , schemaNot :: Maybe Schema
    -- ^ Must NOT satisfy this subschema

    -- === Conditional Keywords (draft-07+) ===
  , schemaIf :: Maybe Schema
    -- ^ Condition schema
  
  , schemaThen :: Maybe Schema
    -- ^ Applied if 'if' validates successfully
  
  , schemaElse :: Maybe Schema
    -- ^ Applied if 'if' validates unsuccessfully

    -- === Validation Substructure ===
  , schemaValidation :: SchemaValidation
    -- ^ All validation keywords (numeric, string, array, object)

    -- === Annotation/Metadata ===
  , schemaAnnotations :: SchemaAnnotations
    -- ^ Annotations (title, description, examples, etc.)

    -- === Definition Storage ===
  , schemaDefs :: Map Text Schema
    -- ^ Local schema definitions ($defs or definitions)
  }
  deriving (Eq, Show, Generic)

-- | JSON Schema primitive types
data SchemaType
  = NullType
  | BooleanType
  | ObjectType
  | ArrayType
  | NumberType
  | StringType
  | IntegerType
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- | One or many values (for type unions)
data OneOrMany a
  = One a
  | Many (NonEmpty a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
```

**Invariants**:
- `allOf`, `anyOf`, `oneOf` must have at least one subschema (NonEmpty)
- `schemaEnum` must have at least one value
- `schemaRef` in draft-04/06/07 should be the only keyword (except for annotations)
- Conditional keywords (`if/then/else`) only valid in draft-07+

---

### 1.3 SchemaValidation (Validation Keywords)

All validation constraints grouped by type.

```haskell
data SchemaValidation = SchemaValidation
  { -- === Numeric Validation ===
    validationMultipleOf :: Maybe Scientific
    -- ^ Number must be multiple of this value
    -- Invariant: Must be > 0
  
  , validationMaximum :: Maybe Scientific
    -- ^ Maximum value (inclusive unless exclusiveMaximum set)
  
  , validationExclusiveMaximum :: Maybe (Either Bool Scientific)
    -- ^ Left Bool: draft-04 (modifies maximum)
    -- ^ Right Scientific: draft-06+ (standalone constraint)
  
  , validationMinimum :: Maybe Scientific
    -- ^ Minimum value (inclusive unless exclusiveMinimum set)
  
  , validationExclusiveMinimum :: Maybe (Either Bool Scientific)
    -- ^ Left Bool: draft-04, Right Scientific: draft-06+

    -- === String Validation ===
  , validationMaxLength :: Maybe Natural
    -- ^ Maximum string length (in characters, not bytes)
  
  , validationMinLength :: Maybe Natural
    -- ^ Minimum string length
    -- Invariant: minLength <= maxLength if both present
  
  , validationPattern :: Maybe Regex
    -- ^ Regular expression pattern (ECMA-262)
  
  , validationFormat :: Maybe Format
    -- ^ Semantic format (email, uri, date-time, etc.)

    -- === Array Validation ===
  , validationItems :: Maybe ArrayItemsValidation
    -- ^ Items validation (version-dependent structure)
  
  , validationPrefixItems :: Maybe (NonEmpty Schema)
    -- ^ Tuple validation (2020-12+)
  
  , validationContains :: Maybe Schema
    -- ^ At least one item must match
  
  , validationMaxItems :: Maybe Natural
    -- ^ Maximum array length
  
  , validationMinItems :: Maybe Natural
    -- ^ Minimum array length
    -- Invariant: minItems <= maxItems if both present
  
  , validationUniqueItems :: Maybe Bool
    -- ^ All items must be unique
  
  , validationMaxContains :: Maybe Natural
    -- ^ Maximum items matching 'contains' (2019-09+)
  
  , validationMinContains :: Maybe Natural
    -- ^ Minimum items matching 'contains' (2019-09+)
    -- Invariant: minContains <= maxContains if both present

    -- === Object Validation ===
  , validationProperties :: Maybe (Map Text Schema)
    -- ^ Property-specific schemas
  
  , validationPatternProperties :: Maybe (Map Regex Schema)
    -- ^ Regex pattern matching for property names
  
  , validationAdditionalProperties :: Maybe Schema
    -- ^ Schema for properties not in 'properties' or matched by 'patternProperties'
  
  , validationUnevaluatedProperties :: Maybe Schema
    -- ^ Schema for properties not evaluated by any keyword (2019-09+)
  
  , validationPropertyNames :: Maybe Schema
    -- ^ Schema that property names must satisfy
  
  , validationMaxProperties :: Maybe Natural
    -- ^ Maximum number of properties
  
  , validationMinProperties :: Maybe Natural
    -- ^ Minimum number of properties
    -- Invariant: minProperties <= maxProperties if both present
  
  , validationRequired :: Maybe (Set Text)
    -- ^ Set of required property names
  
  , validationDependentRequired :: Maybe (Map Text (Set Text))
    -- ^ If key present, value properties must be present (2019-09+)
  
  , validationDependentSchemas :: Maybe (Map Text Schema)
    -- ^ If key present, value schema must validate (2019-09+)
  
  , validationDependencies :: Maybe (Map Text Dependency)
    -- ^ Property dependencies (draft-04/06/07, deprecated in 2019-09+)
  }
  deriving (Eq, Show, Generic)

-- | Array items validation (version-dependent)
data ArrayItemsValidation
  = ItemsSchema Schema
    -- ^ All items must match this schema
  | ItemsTuple (NonEmpty Schema) (Maybe Schema)
    -- ^ Tuple: positional schemas + optional additional items schema
  deriving (Eq, Show, Generic)

-- | Dependency specification (draft-04 through draft-07)
data Dependency
  = DependencyProperties (Set Text)
    -- ^ If property exists, these properties must exist
  | DependencySchema Schema
    -- ^ If property exists, this schema must validate
  deriving (Eq, Show, Generic)

-- | Regular expression wrapper
newtype Regex = Regex Text
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)

-- | Semantic format specifiers
data Format
  = DateTime | Date | Time | Duration
  | Email | IDNEmail
  | Hostname | IDNHostname
  | IPv4 | IPv6
  | URI | URIReference | IRI | IRIReference
  | URITemplate
  | JSONPointer | RelativeJSONPointer
  | RegexFormat  -- "regex" format
  | UUID
  | CustomFormat Text
  deriving (Eq, Show, Ord, Generic)
```

**Invariants**:
- `multipleOf` > 0
- `minLength` ≤ `maxLength`
- `minItems` ≤ `maxItems`
- `minProperties` ≤ `maxProperties`
- `minContains` ≤ `maxContains`
- `prefixItems` only valid in 2020-12
- `unevaluatedProperties` only valid in 2019-09+

---

### 1.4 SchemaAnnotations (Metadata)

```haskell
data SchemaAnnotations = SchemaAnnotations
  { annotationTitle :: Maybe Text
    -- ^ Short description
  
  , annotationDescription :: Maybe Text
    -- ^ Detailed description (may be Markdown)
  
  , annotationDefault :: Maybe Value
    -- ^ Default value for this schema
  
  , annotationExamples :: [Value]
    -- ^ Example values (changed from 'example' in OpenAPI)
  
  , annotationDeprecated :: Maybe Bool
    -- ^ Whether this schema is deprecated (2019-09+)
  
  , annotationReadOnly :: Maybe Bool
    -- ^ Property is read-only (OpenAPI influence)
  
  , annotationWriteOnly :: Maybe Bool
    -- ^ Property is write-only (OpenAPI influence)
    -- Invariant: readOnly and writeOnly cannot both be true
  
  , annotationComment :: Maybe Text
    -- ^ Comments for schema authors ($comment, draft-07+)

    -- === Custom Codegen Annotations ===
  , annotationCodegen :: Maybe CodegenAnnotations
    -- ^ Code generation hints (custom extension)
  }
  deriving (Eq, Show, Generic)

-- | Code generation annotations (custom vocabulary)
data CodegenAnnotations = CodegenAnnotations
  { codegenTypeName :: Maybe Text
    -- ^ Override generated type name
  
  , codegenNewtype :: Maybe NewtypeSpec
    -- ^ Generate newtype instead of type alias
  
  , codegenFieldMapping :: Maybe (Map Text Text)
    -- ^ Map JSON field names to Haskell field names
  
  , codegenOmitEmpty :: Maybe Bool
    -- ^ Omit null/empty values in ToJSON
  
  , codegenStrictFields :: Maybe Bool
    -- ^ Use strict fields (BangPatterns)
  
  , codegenCustomStrategy :: Maybe Text
    -- ^ Name of custom generation strategy
  
  , codegenImports :: [Text]
    -- ^ Additional imports needed for generated code
  }
  deriving (Eq, Show, Generic)

-- | Newtype generation specification
data NewtypeSpec = NewtypeSpec
  { newtypeConstructor :: Text
    -- ^ Constructor name (must be valid Haskell identifier)
  
  , newtypeModule :: Maybe Text
    -- ^ Module to generate newtype in
  
  , newtypeValidation :: Maybe Text
    -- ^ Validation function name
  
  , newtypeInstances :: [Text]
    -- ^ Additional typeclass instances to derive
  }
  deriving (Eq, Show, Generic)
```

**Invariants**:
- `readOnly` and `writeOnly` cannot both be True
- `newtypeConstructor` must be valid Haskell identifier
- `codegenTypeName` must be valid Haskell type name

---

### 1.5 Reference Types

```haskell
-- | JSON reference (URI or JSON Pointer)
newtype Reference = Reference URI
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)

-- | JSON Pointer (RFC 6901)
newtype JSONPointer = JSONPointer [Text]
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (Semigroup, Monoid)

-- Helper functions (not in data model, but important)
-- emptyPointer :: JSONPointer
-- (/) :: JSONPointer -> Text -> JSONPointer
-- renderPointer :: JSONPointer -> Text
-- parsePointer :: Text -> Either ParseError JSONPointer
```

---

## 2. Vocabulary System Types

### 2.1 Vocabulary Definition

```haskell
-- | A vocabulary defines a collection of keywords with semantics
data Vocabulary = Vocabulary
  { vocabularyURI :: URI
    -- ^ Unique identifier for this vocabulary
    -- Invariant: Must be absolute URI
  
  , vocabularyRequired :: Bool
    -- ^ Whether understanding this vocabulary is required
  
  , vocabularyKeywords :: Map Text KeywordDefinition
    -- ^ Keyword name -> definition
    -- Invariant: No duplicate keywords
  
  , vocabularyValidator :: Maybe VocabularyValidator
    -- ^ Optional whole-vocabulary validator
  
  , vocabularyMetaSchema :: Maybe Schema
    -- ^ Schema describing this vocabulary
  }
  deriving (Generic)

-- Can't derive Eq/Show due to function fields
instance Show Vocabulary where
  show v = "Vocabulary { uri = " ++ show (vocabularyURI v)
        ++ ", keywords = " ++ show (Map.keys $ vocabularyKeywords v) ++ " }"

-- | Keyword definition with parser, validator, and annotator
data KeywordDefinition = KeywordDefinition
  { keywordName :: Text
    -- ^ Keyword name (e.g., "properties", "x-credit-card")
    -- Invariant: Must be valid JSON object key
  
  , keywordAppliesTo :: KeywordScope
    -- ^ Which schema types this keyword applies to
  
  , keywordParser :: Value -> Either ParseError KeywordValue
    -- ^ Parse keyword value from JSON
  
  , keywordValidator :: Maybe KeywordValidator
    -- ^ Validate instance against keyword constraints
  
  , keywordAnnotator :: Maybe KeywordAnnotator
    -- ^ Produce annotations from keyword
  
  , keywordPriority :: Int
    -- ^ Evaluation priority (higher = earlier)
    -- Invariant: Forms total ordering for deterministic validation
  }
  deriving (Generic)

-- | Which schema types a keyword can appear in
data KeywordScope
  = AnySchema
  | ObjectSchemaOnly
  | ArraySchemaOnly
  | StringSchemaOnly
  | NumericSchemaOnly
  | BooleanSchemaOnly
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- | Opaque keyword value with type safety via existential
data KeywordValue where
  KeywordValue :: (Eq a, Show a, Typeable a) => a -> KeywordValue

instance Eq KeywordValue where
  (KeywordValue (a :: a)) == (KeywordValue (b :: b)) =
    case eqT @a @b of
      Just Refl -> a == b
      Nothing -> False

instance Show KeywordValue where
  show (KeywordValue a) = show a

-- | Function types for keyword operations
type VocabularyValidator = Schema -> ValidationContext -> Either ValidationError ()
type KeywordValidator = KeywordValue -> Value -> ValidationContext -> ValidationResult
type KeywordAnnotator = KeywordValue -> Value -> Map Text Value
```

**Invariants**:
- `vocabularyURI` must be absolute
- `keywordName` must be unique within vocabulary
- `keywordPriority` values should form total ordering
- `KeywordValue` uses existential types for type-safe heterogeneous storage

---

### 2.2 Dialect Definition

```haskell
-- | A dialect is a complete configuration of vocabularies
data Dialect = Dialect
  { dialectURI :: URI
    -- ^ Unique identifier for this dialect
    -- Invariant: Must be absolute URI
  
  , dialectName :: Text
    -- ^ Human-readable name (e.g., "JSON Schema Draft-07")
  
  , dialectVersion :: Text
    -- ^ Version string (e.g., "draft-07", "2020-12")
  
  , dialectVocabularies :: Map URI Bool
    -- ^ Vocabularies and whether they're required
    -- Key: vocabulary URI, Value: required?
  
  , dialectMetaSchema :: Schema
    -- ^ The meta-schema for this dialect
  
  , dialectDefaultFormat :: FormatBehavior
    -- ^ How to treat format keyword
  
  , dialectUnknownKeywords :: UnknownKeywordMode
    -- ^ How to handle unknown keywords
  }
  deriving (Eq, Show, Generic)

-- | Format keyword behavior
data FormatBehavior
  = FormatAssertion      -- ^ Format is validation constraint
  | FormatAnnotation     -- ^ Format is annotation only
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- | Unknown keyword handling strategy
data UnknownKeywordMode
  = IgnoreUnknown        -- ^ Silently ignore
  | WarnUnknown          -- ^ Emit warnings but continue
  | ErrorOnUnknown       -- ^ Fail parsing
  | CollectUnknown       -- ^ Collect in extensions map
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)
```

**Invariants**:
- `dialectURI` must be absolute
- All vocabularies in `dialectVocabularies` must be registered if required

---

### 2.3 Vocabulary Registry

```haskell
-- | Central registry for vocabularies and dialects
data VocabularyRegistry = VocabularyRegistry
  { registeredVocabularies :: Map URI Vocabulary
    -- ^ All registered vocabularies by URI
  
  , registeredDialects :: Map URI Dialect
    -- ^ All registered dialects by URI
  
  , defaultDialect :: Dialect
    -- ^ Dialect to use when $schema not specified
  }
  deriving (Generic)

instance Show VocabularyRegistry where
  show vr = "VocabularyRegistry { vocabularies = "
         ++ show (Map.keys $ registeredVocabularies vr)
         ++ ", dialects = "
         ++ show (Map.keys $ registeredDialects vr) ++ " }"

-- Registry operations (type signatures only)
-- emptyRegistry :: VocabularyRegistry
-- registerVocabulary :: Vocabulary -> VocabularyRegistry -> VocabularyRegistry
-- registerDialect :: Dialect -> VocabularyRegistry -> VocabularyRegistry
-- lookupVocabulary :: URI -> VocabularyRegistry -> Maybe Vocabulary
-- lookupDialect :: URI -> VocabularyRegistry -> Maybe Dialect
-- composeDialect :: Text -> URI -> [(URI, Bool)] -> VocabularyRegistry -> Either ComposeError Dialect
```

---

## 3. Validation Types

### 3.1 ValidationResult and Errors

```haskell
-- | Result of validation: success with annotations or failure with errors
data ValidationResult
  = ValidationSuccess ValidationAnnotations
    -- ^ Validation succeeded, collected annotations
  | ValidationFailure ValidationErrors
    -- ^ Validation failed with one or more errors
  deriving (Eq, Show, Generic)

-- Alternative instance for combining validations
instance Alternative ValidationResult where
  empty = ValidationFailure (ValidationErrors $ pure emptyError)
  ValidationSuccess a <|> _ = ValidationSuccess a
  ValidationFailure _ <|> b = b

-- | Collected annotations from validation
newtype ValidationAnnotations = ValidationAnnotations 
  { unAnnotations :: Map JSONPointer (Map Text Value) }
  deriving (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

-- | Non-empty list of validation errors
newtype ValidationErrors = ValidationErrors 
  { unErrors :: NonEmpty ValidationError }
  deriving (Eq, Show, Generic)
  deriving newtype (Semigroup)

-- | Single validation error with context
data ValidationError = ValidationError
  { errorKeyword :: Text
    -- ^ Keyword that failed (e.g., "minimum", "pattern")
  
  , errorSchemaPath :: JSONPointer
    -- ^ Path to the failing keyword in the schema
  
  , errorInstancePath :: JSONPointer
    -- ^ Path to the failing value in the instance
  
  , errorMessage :: Text
    -- ^ Human-readable error message
  
  , errorDetails :: Maybe Value
    -- ^ Additional context (actual value, expected constraint, etc.)
  }
  deriving (Eq, Show, Generic)
```

**Invariants**:
- `ValidationErrors` contains at least one error (NonEmpty)
- `errorSchemaPath` and `errorInstancePath` are valid JSON Pointers
- Cannot be both success and failure

---

### 3.2 ValidationContext

```haskell
-- | Stateful context threaded through validation
data ValidationContext = ValidationContext
  { contextConfig :: ValidationConfig
    -- ^ Validation configuration
  
  , contextSchemaRegistry :: SchemaRegistry
    -- ^ Registry of schemas for reference resolution
  
  , contextBaseURI :: Maybe URI
    -- ^ Current base URI for relative reference resolution
  
  , contextDynamicScope :: [Schema]
    -- ^ Stack for $dynamicRef resolution (2020-12+)
  
  , contextEvaluatedProperties :: Set Text
    -- ^ Properties evaluated so far (for unevaluatedProperties)
  
  , contextEvaluatedItems :: Set Int
    -- ^ Array indices evaluated so far (for unevaluatedItems)
  
  , contextVisitedSchemas :: Set SchemaFingerprint
    -- ^ Schemas visited (for cycle detection)
  }
  deriving (Eq, Show, Generic)

-- | Configuration for validation behavior
data ValidationConfig = ValidationConfig
  { validationVersion :: JsonSchemaVersion
    -- ^ Schema version to validate against
  
  , validationVocabularyRegistry :: VocabularyRegistry
    -- ^ Available vocabularies
  
  , validationFormatAssertion :: Bool
    -- ^ Treat format as assertion (True) or annotation (False)
  
  , validationShortCircuit :: Bool
    -- ^ Stop on first error (True) or collect all (False)
  
  , validationCollectAnnotations :: Bool
    -- ^ Collect annotations during validation
  
  , validationCustomValidators :: Map Text CustomValidator
    -- ^ Custom validators for format or other extensions
  }
  deriving (Generic)

-- | Custom validator function type
type CustomValidator = Value -> Either ValidationError ()

-- | Schema fingerprint for cycle detection
newtype SchemaFingerprint = SchemaFingerprint ByteString
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

-- computeFingerprint :: Schema -> SchemaFingerprint
```

**Invariants**:
- `contextDynamicScope` stack is properly maintained (pushed/popped)
- `contextVisitedSchemas` prevents infinite recursion
- `contextEvaluatedProperties` ⊆ actual properties in instance
- `contextEvaluatedItems` ⊆ valid indices in array

---

### 3.3 SchemaRegistry

```haskell
-- | Registry of schemas for reference resolution
data SchemaRegistry = SchemaRegistry
  { registrySchemas :: Map URI Schema
    -- ^ URI -> Schema mapping
  
  , registryAnchors :: Map (URI, Text) Schema
    -- ^ (Base URI, anchor name) -> Schema mapping
  
  , registryDynamicAnchors :: Map (URI, Text) Schema
    -- ^ Dynamic anchors (2020-12+)
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically SchemaRegistry)

-- Registry operations
-- emptyRegistry :: SchemaRegistry
-- registerSchema :: Schema -> SchemaRegistry -> SchemaRegistry
-- resolveReference :: Reference -> SchemaRegistry -> Maybe Schema
-- resolveAnchor :: URI -> Text -> SchemaRegistry -> Maybe Schema
```

**Invariants**:
- All URIs in registry are absolute
- Anchor names are unique within schema
- Referenced schemas exist in registry (or error)

---

## 4. Code Generation Types

### 4.1 CodegenContext

```haskell
-- | Context for code generation
data CodegenContext = CodegenContext
  { codegenSchema :: Schema
    -- ^ Schema being generated from
  
  , codegenConfig :: CodegenConfig
    -- ^ Configuration for generation
  
  , codegenStrategy :: CodegenStrategy
    -- ^ Strategy for this schema
  
  , codegenNamingConvention :: NamingConvention
    -- ^ How to generate names
  
  , codegenModuleName :: Maybe ModuleName
    -- ^ Target module name
  
  , codegenImports :: Set Import
    -- ^ Required imports accumulated
  
  , codegenTypeRegistry :: TypeRegistry
    -- ^ Types already generated (avoid duplicates)
  }
  deriving (Generic)

-- | Configuration for code generation
data CodegenConfig = CodegenConfig
  { codegenTargetVersion :: JsonSchemaVersion
    -- ^ Schema version to assume
  
  , codegenStrictValidation :: Bool
    -- ^ Embed validation in instances
  
  , codegenOmitNullable :: Bool
    -- ^ Omit Maybe wrapper for nullable types
  
  , codegenGenerateToJSON :: Bool
    -- ^ Generate ToJSON instances
  
  , codegenGenerateFromJSON :: Bool
    -- ^ Generate FromJSON instances
  
  , codegenGenerateLenses :: Bool
    -- ^ Generate lens definitions
  
  , codegenSmartConstructors :: Bool
    -- ^ Generate smart constructors for newtypes
  
  , codegenNewtypeAnnotations :: Bool
    -- ^ Respect x-newtype annotations
  
  , codegenFieldPrefixes :: Maybe Text
    -- ^ Prefix for record fields
  
  , codegenCustomStrategies :: Map Text CodegenStrategy
    -- ^ Named custom strategies
  }
  deriving (Generic)
```

---

### 4.2 CodegenStrategy

```haskell
-- | Pluggable code generation strategy
data CodegenStrategy = CodegenStrategy
  { strategyName :: Text
    -- ^ Strategy name for identification
  
  , strategyTypeGen :: TypeGenerator
    -- ^ Generate type definition
  
  , strategyFieldGen :: FieldGenerator
    -- ^ Generate record field
  
  , strategyInstanceGen :: InstanceGenerator
    -- ^ Generate typeclass instances
  
  , strategyConstraintGen :: ConstraintGenerator
    -- ^ Generate validation constraints
  }

-- | Function types for generation
type TypeGenerator = CodegenContext -> Schema -> Q [Dec]
type FieldGenerator = CodegenContext -> Text -> Schema -> Q (Name, Type)
type InstanceGenerator = CodegenContext -> Name -> Schema -> Q [Dec]
type ConstraintGenerator = CodegenContext -> Schema -> Q [Exp]
```

---

### 4.3 Supporting Types

```haskell
-- | Typeclass linking generated types back to their originating schema
class HasSchema a where
  -- | Retrieve the schema that this type was generated from
  schemaFor :: Proxy a -> Schema
  
  -- | Optionally provide the JSON Pointer to the schema location
  -- Useful when type was generated from a subschema (e.g., property)
  schemaPath :: Proxy a -> Maybe JSONPointer
  schemaPath _ = Nothing

-- Example generated instance:
-- instance HasSchema Person where
--   schemaFor _ = personSchema  -- Embedded at compile time
--   schemaPath _ = Just "/components/schemas/Person"

-- Utility functions (not in data model, but important)
-- validateDynamic :: HasSchema a => a -> ValidationResult
-- toSchema :: HasSchema a => Proxy a -> Schema
-- describeType :: HasSchema a => Proxy a -> Text

-- | Type registry to avoid duplicate generation
data TypeRegistry = TypeRegistry
  { registryTypes :: Map SchemaFingerprint Name
    -- ^ Schema fingerprint -> generated type name
  
  , registryNames :: Set Text
    -- ^ All generated names (for uniqueness)
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically TypeRegistry)

-- | Naming convention for generated code
data NamingConvention
  = CamelCase        -- ^ camelCase
  | PascalCase       -- ^ PascalCase
  | SnakeCase        -- ^ snake_case
  | KebabCase        -- ^ kebab-case
  | CustomNaming (Text -> Text)  -- ^ Custom function

-- | Import specification
data Import = Import
  { importModule :: ModuleName
  , importQualified :: Bool
  , importAs :: Maybe ModuleName
  , importItems :: Maybe (NonEmpty ImportItem)
  }
  deriving (Eq, Show, Ord, Generic)

data ImportItem
  = ImportType Text
  | ImportValue Text
  | ImportPattern Text
  deriving (Eq, Show, Ord, Generic)

newtype ModuleName = ModuleName Text
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (IsString, ToJSON, FromJSON)
```

---

## 5. OpenAPI Types

### 5.1 OpenApiSpec

```haskell
-- | Complete OpenAPI specification
data OpenApiSpec = OpenApiSpec
  { openApiVersion :: OpenApiVersion
    -- ^ OpenAPI version (3.0.x or 3.1.x)
  
  , openApiInfo :: Info
    -- ^ API metadata
  
  , openApiServers :: [Server]
    -- ^ Server information
  
  , openApiPaths :: Map Text PathItem
    -- ^ Path -> operations mapping
    -- Invariant: Path must start with /
  
  , openApiComponents :: Maybe Components
    -- ^ Reusable components (schemas, responses, etc.)
  
  , openApiSecurity :: [SecurityRequirement]
    -- ^ Global security requirements
  
  , openApiTags :: [Tag]
    -- ^ Tag definitions
  
  , openApiExternalDocs :: Maybe ExternalDocs
    -- ^ External documentation
  
  , openApiExtensions :: Map Text Value
    -- ^ OpenAPI extensions (x-*)
  }
  deriving (Eq, Show, Generic)

data OpenApiVersion
  = OpenApi30 Text  -- ^ 3.0.x
  | OpenApi31 Text  -- ^ 3.1.x
  deriving (Eq, Show, Ord, Generic)

-- | API metadata
data Info = Info
  { infoTitle :: Text
  , infoDescription :: Maybe Text
  , infoTermsOfService :: Maybe URI
  , infoContact :: Maybe Contact
  , infoLicense :: Maybe License
  , infoVersion :: Text
  }
  deriving (Eq, Show, Generic)
```

---

### 5.2 PathItem and Operation

```haskell
-- | Path item with operations
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
  deriving (Eq, Show, Generic)

-- | HTTP operation
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
  deriving (Eq, Show, Generic)
```

---

### 5.3 ReferenceOr Pattern

```haskell
-- | Either a reference or an inline value
data ReferenceOr a
  = Reference Reference
    -- ^ $ref pointer
  | Inline a
    -- ^ Inline definition
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

-- Ensures references and inline values cannot be mixed
```

**Invariants**:
- Cannot be both reference and inline
- Reference must resolve in Components

---

## 6. Type-Level Invariants Summary

### Invariants Encoded in Types

1. **NonEmpty Lists**: `allOf`, `anyOf`, `oneOf`, `enum` use `NonEmpty` to prevent empty collections
2. **Set vs List**: `required` uses `Set` to encode uniqueness
3. **Either for Version Differences**: `exclusiveMaximum` uses `Either Bool Scientific` to handle draft-04 vs draft-06+ difference
4. **Maybe for Optional**: All optional keywords use `Maybe`
5. **ReferenceOr**: Prevents mixing references with inline definitions
6. **Newtype Wrappers**: `JSONPointer`, `Regex`, `Reference` prevent string confusion
7. **Sum Types**: `SchemaCore` ensures boolean vs object distinction
8. **ValidationResult**: Either-like structure prevents simultaneous success/failure

### Invariants Requiring Runtime Validation

1. URI absoluteness checks
2. Numeric constraint relationships (min ≤ max)
3. readOnly/writeOnly mutual exclusion
4. Version-specific keyword presence
5. Vocabulary URI registration
6. Reference resolution
7. Haskell identifier validity
8. Circular schema detection

### Total Functions

All domain functions should be total:
- Pattern matching is exhaustive
- Partial functions like `head` avoided
- Use `NonEmpty` where at least one element needed
- Errors represented in return types (`Either`, `Maybe`, `ValidationResult`)

---

## Summary

This data model provides a complete, type-safe foundation for the fractal-openapi library:

- **Schema types** encode JSON Schema structure with version-specific features
- **Vocabulary system** allows extensibility while maintaining type safety
- **Validation types** track context and provide detailed error reporting
- **Codegen types** enable pluggable code generation strategies
- **OpenAPI types** extend schema foundation with API-specific concepts

All types follow the constitution's principles:
- Invalid states unrepresentable (NonEmpty, ReferenceOr, etc.)
- Explicit effect tracking (Q monad for TH, IO for file loading)
- Composable abstractions (Semigroup, Monoid, Alternative)
- Property-testable (Eq instances enable property verification)

Next: Define internal contracts (validation pipeline, vocabulary operations, code generation flow).

