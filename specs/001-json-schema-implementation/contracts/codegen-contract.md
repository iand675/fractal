# Code Generation Contract

**Module**: `Fractal.OpenApi.Codegen.TH`  
**Purpose**: Generate Haskell types and instances from JSON Schemas via Template Haskell

## Public API

### Primary Generation Functions

```haskell
-- | Generate types and instances from a schema
deriveJSONSchema :: Schema -> Q [Dec]

-- | Generate with custom configuration
deriveJSONSchemaWith :: CodegenConfig -> Schema -> Q [Dec]

-- | Generate from file (load at compile time)
deriveJSONSchemaFromFile :: FilePath -> Q [Dec]

-- | Generate from URL (fetch at compile time)
deriveJSONSchemaFromURL :: String -> Q [Dec]

-- | Generate with custom strategy
deriveJSONSchemaCustom :: CodegenStrategy -> Schema -> Q [Dec]
```

### Component Generation

```haskell
-- | Generate only the type definition
deriveType :: Schema -> Q [Dec]

-- | Generate Aeson instances for existing type
deriveAesonInstances :: Name -> Schema -> Q [Dec]

-- | Generate validator function
deriveValidator :: Name -> Schema -> Q [Dec]

-- | Generate lenses (if configured)
deriveLenses :: Name -> Q [Dec]
```

### OpenAPI-Specific

```haskell
-- | Generate all types from OpenAPI components
deriveOpenApiTypes :: OpenApiSpec -> Q [Dec]

-- | Generate Servant API type
deriveServantAPI :: OpenApiSpec -> Q [Dec]

-- | Generate Yesod routes
deriveYesodRoutes :: OpenApiSpec -> Q [Dec]
```

## Contracts

### Type Generation

**Pre-conditions**:
- Schema is well-formed and parsed
- Schema is compile-time constant (known at TH expansion)
- Generated names don't conflict with existing code

**Post-conditions**:
- Generated code compiles without errors
- Generated types match schema structure
- Type names are valid Haskell identifiers

**Invariants**:
- Object schemas → record types
- Enum schemas → sum types with no fields
- OneOf/anyOf → sum types with payload
- Primitive schemas with constraints → newtypes (if annotated)
- Required fields → direct fields
- Optional fields → Maybe fields

### Instance Generation

**FromJSON Instance**:
- Parses JSON values matching schema structure
- Embeds validation constraints in parser
- Fails with descriptive error messages
- Respects schema defaults

**ToJSON Instance**:
- Serializes to JSON matching schema
- Omits null fields if configured
- Respects field name mappings
- Produces valid JSON for schema

**HasSchema Instance**:
- Links generated type back to originating schema
- Schema embedded at compile time (via Template Haskell)
- Enables runtime introspection and dynamic validation
- Provides schema path within larger documents

**Invariants**:
- `decode . encode ≡ Right` for valid values
- Generated instances match Aeson conventions
- Validation errors surface during parsing, not construction
- `schemaFor` returns the exact schema used for generation

### Naming Conventions

**Type Names**:
- CamelCase with first letter uppercase
- Schema `title` used if present and valid
- Property name used if at root
- Generated names for anonymous schemas (e.g., `Property1`)

**Field Names**:
- camelCase with first letter lowercase
- Type name prefix (configurable): `personName`, `personAge`
- Avoid Haskell keywords (append `'` if needed)
- Respect `x-codegen` field mappings

**Invariants**:
- All generated names are valid Haskell identifiers
- No name conflicts within module
- Keywords escaped or avoided

### Validation Embedding

**Constraints Embedded in FromJSON**:
- Type constraints (e.g., integer vs number)
- Range constraints (minimum/maximum)
- Length constraints (minLength/maxLength)
- Pattern constraints (regex validation)
- Format constraints (if configured)
- Required fields (parse error if missing)

**Example**:
```haskell
-- Schema: { "type": "integer", "minimum": 0, "maximum": 150 }
instance FromJSON Age where
  parseJSON = withScientific "Age" $ \n ->
    case toBoundedInteger n of
      Nothing -> fail "Age must be an integer"
      Just i -> do
        when (i < 0) $ fail "Age must be >= 0"
        when (i > 150) $ fail "Age must be <= 150"
        pure (Age i)
```

**Invariants**:
- Validation occurs during parsing
- Invalid values cannot be constructed via FromJSON
- Validation errors include constraint that failed

### Newtype Generation

**Triggered by `x-newtype` annotation**:

```json
{
  "type": "string",
  "format": "email",
  "x-newtype": {
    "constructor": "Email",
    "validation": "validateEmail"
  }
}
```

**Generated code**:
```haskell
newtype Email = Email Text
  deriving (Eq, Show, Ord)
  deriving newtype (Hashable)

mkEmail :: Text -> Either ValidationError Email
mkEmail txt = validateEmail txt >>= pure . Email

validateEmail :: Text -> Either ValidationError Text
validateEmail txt = ... -- Validation logic

instance FromJSON Email where
  parseJSON = withText "Email" $ \txt ->
    either (fail . show) pure (mkEmail txt)

instance ToJSON Email where
  toJSON (Email txt) = String txt
```

**Invariants**:
- Smart constructor performs validation
- FromJSON uses smart constructor
- Unsafe constructor not exported (if configured)
- Validation function total (handles all inputs)

### Sum Type Generation

**For oneOf/anyOf schemas**:

```json
{
  "oneOf": [
    {"type": "string"},
    {"type": "integer"}
  ]
}
```

**Generated code**:
```haskell
data StringOrInt
  = StringOrIntString Text
  | StringOrIntInteger Int
  deriving (Eq, Show)

instance FromJSON StringOrInt where
  parseJSON v =
    (StringOrIntString <$> parseJSON v) <|>
    (StringOrIntInteger <$> parseJSON v)

instance ToJSON StringOrInt where
  toJSON (StringOrIntString s) = toJSON s
  toJSON (StringOrIntInteger i) = toJSON i
```

**With discriminator (OpenAPI)**:
```json
{
  "oneOf": [...],
  "discriminator": {"propertyName": "type"}
}
```

**Generated code uses tagged union**:
```haskell
data Animal
  = AnimalDog Dog
  | AnimalCat Cat
  deriving (Eq, Show)

instance FromJSON Animal where
  parseJSON = withObject "Animal" $ \o -> do
    t <- o .: "type"
    case t of
      "dog" -> AnimalDog <$> parseJSON (Object o)
      "cat" -> AnimalCat <$> parseJSON (Object o)
      _ -> fail $ "Unknown type: " ++ t
```

**Invariants**:
- Constructors named unambiguously
- Alternative parsing for untagged unions
- Tagged unions use discriminator field
- ToJSON matches FromJSON structure

## Properties

### Roundtrip Property

```haskell
-- Property: Generated code roundtrips JSON correctly
property_codegenRoundtrip :: Schema -> Value -> Property
property_codegenRoundtrip schema value =
  -- If value validates against schema
  isSuccess (validateValue defaultConfig schema value) ==>
    -- Then generated type roundtrips
    let generatedCode = deriveJSONSchema schema
        -- (Assume we can somehow evaluate TH and test)
    in decode (encode parsedValue) === Just parsedValue
```

### Validation Preservation

```haskell
-- Property: Generated parser rejects invalid values
property_validationPreserved :: Schema -> Value -> Property
property_validationPreserved schema value =
  case validateValue defaultConfig schema value of
    ValidationFailure _ ->
      -- Generated parser should also fail
      isNothing (decode @GeneratedType (encode value))
    ValidationSuccess _ ->
      -- Generated parser should succeed
      isJust (decode @GeneratedType (encode value))
```

### Name Uniqueness

```haskell
-- Property: Generated names are unique
property_uniqueNames :: Schema -> Property
property_uniqueNames schema =
  let generated = deriveJSONSchema schema
      names = extractNames generated
  in length names === length (nub names)
```

## Error Conditions

| Condition | Error |
|-----------|-------|
| Schema too complex | `CodegenError "Schema nesting too deep"` |
| Invalid Haskell identifier | `CodegenError "Cannot generate valid name from '<name>'"` |
| Circular type reference | `CodegenError "Circular type dependency detected"` |
| Unsupported schema feature | `CodegenError "Feature not supported: <feature>"` |
| Name conflict | `CodegenError "Name '<name>' conflicts with existing type"` |
| Invalid file path | `CodegenError "Cannot read schema file: <path>"` |

## HasSchema Typeclass

### Purpose

The `HasSchema` typeclass provides a compile-time link between generated types and their originating schemas, enabling:

1. **Runtime Introspection**: Query schema properties at runtime
2. **Dynamic Validation**: Re-validate values after construction/modification
3. **Documentation Generation**: Extract schema metadata for API docs
4. **Form Generation**: Build UI forms from schema constraints
5. **Migration Support**: Compare schemas across versions
6. **Tooling**: IDE support, linters, analyzers

### Definition

```haskell
class HasSchema a where
  -- | Get the schema this type was generated from
  schemaFor :: Proxy a -> Schema
  
  -- | Location within a larger schema document
  schemaPath :: Proxy a -> Maybe JSONPointer
  schemaPath _ = Nothing
```

### Generated Instances

```haskell
-- From simple object schema
data Person = Person
  { personName :: Text
  , personAge :: Int
  } deriving (Eq, Show, Generic)

instance HasSchema Person where
  schemaFor _ = Schema
    { schemaCore = ObjectSchema SchemaObject
        { schemaType = Just (One ObjectType)
        , schemaValidation = SchemaValidation
            { validationProperties = Just $ Map.fromList
                [ ("name", stringSchema)
                , ("age", integerSchema { validationMinimum = Just 0 })
                ]
            , validationRequired = Just $ Set.fromList ["name", "age"]
            , ...
            }
        , ...
        }
    , ...
    }
  schemaPath _ = Just "/components/schemas/Person"

instance FromJSON Person
instance ToJSON Person
```

### Utility Functions

Library provides helper functions leveraging `HasSchema`:

```haskell
-- | Validate a value against its schema
validateDynamic :: (HasSchema a, ToJSON a) => a -> ValidationResult
validateDynamic val = validateValue defaultConfig (schemaFor proxy) (toJSON val)
  where proxy = Proxy @a

-- | Get schema title/description
describeType :: HasSchema a => Proxy a -> Maybe Text
describeType p = case schemaFor p of
  Schema { schemaAnnotations = annots } -> annotationDescription annots

-- | Get all property names from an object schema
propertyNames :: HasSchema a => Proxy a -> [Text]
propertyNames p = case schemaFor p of
  Schema { schemaCore = ObjectSchema obj } ->
    maybe [] Map.keys (validationProperties $ schemaValidation obj)
  _ -> []

-- | Check if a field is required
isRequired :: HasSchema a => Proxy a -> Text -> Bool
isRequired p fieldName = case schemaFor p of
  Schema { schemaCore = ObjectSchema obj } ->
    maybe False (Set.member fieldName) (validationRequired $ schemaValidation obj)
  _ -> False

-- | Get field constraints
fieldConstraints :: HasSchema a => Proxy a -> Text -> Maybe Schema
fieldConstraints p fieldName = case schemaFor p of
  Schema { schemaCore = ObjectSchema obj } ->
    validationProperties (schemaValidation obj) >>= Map.lookup fieldName
  _ -> Nothing
```

### Usage Examples

**Example 1: Runtime Validation**

```haskell
-- Validate after modification
validateAfterUpdate :: HasSchema a => a -> Either ValidationError a
validateAfterUpdate val =
  case validateDynamic val of
    ValidationSuccess _ -> Right val
    ValidationFailure errs -> Left (head $ unErrors errs)

-- Usage
main = do
  let person = Person "Alice" 30
  let modified = person { personAge = 200 }  -- Exceeds constraints!
  
  case validateAfterUpdate modified of
    Left err -> putStrLn $ "Validation failed: " ++ errorMessage err
    Right p -> putStrLn "Valid person"
```

**Example 2: Form Generation**

```haskell
-- Generate HTML form from schema
generateForm :: HasSchema a => Proxy a -> Html
generateForm proxy =
  let schema = schemaFor proxy
      title = maybe "Form" id (annotationTitle $ schemaAnnotations schema)
      fields = extractFields schema
  in form_ [action_ "/submit"] $ do
       h2_ (toHtml title)
       forM_ fields $ \(name, fieldSchema) ->
         div_ [class_ "field"] $ do
           label_ (toHtml name)
           input_ [type_ (fieldType fieldSchema), name_ name]
       button_ [type_ "submit"] "Submit"

-- Automatic form for Person type
personForm :: Html
personForm = generateForm (Proxy @Person)
```

**Example 3: API Documentation**

```haskell
-- Generate OpenAPI schema from HasSchema instance
toOpenApiSchema :: HasSchema a => Proxy a -> OpenApiSchema
toOpenApiSchema proxy =
  let schema = schemaFor proxy
      path = schemaPath proxy
  in OpenApiSchema
       { schemaTitle = annotationTitle (schemaAnnotations schema)
       , schemaDescription = annotationDescription (schemaAnnotations schema)
       , schemaType = extractType schema
       , schemaProperties = extractProperties schema
       , schemaRequired = extractRequired schema
       , schemaExample = headMay (annotationExamples $ schemaAnnotations schema)
       }

-- Use with Servant for automatic documentation
type UserAPI = "users" :> Get '[JSON] [User]

instance ToSchema User where
  declareNamedSchema _ = pure $ NamedSchema (Just "User") (toOpenApiSchema (Proxy @User))
```

**Example 4: Schema Comparison**

```haskell
-- Compare schemas across versions for migration
compareSchemas :: (HasSchema a, HasSchema b) => Proxy a -> Proxy b -> SchemaComparison
compareSchemas oldProxy newProxy =
  let oldSchema = schemaFor oldProxy
      newSchema = schemaFor newProxy
  in SchemaComparison
       { addedFields = findAddedProperties oldSchema newSchema
       , removedFields = findRemovedProperties oldSchema newSchema
       , changedTypes = findTypeChanges oldSchema newSchema
       , breaking = hasBreakingChanges oldSchema newSchema
       }
```

**Example 5: Property-Based Testing**

```haskell
-- Generate arbitrary instances based on schema
instance HasSchema a => Arbitrary a where
  arbitrary = do
    let schema = schemaFor (Proxy @a)
    value <- generateFromSchema schema
    case decode (encode value) of
      Just a -> pure a
      Nothing -> error "Generated invalid value"

-- Property: All generated values validate against schema
prop_generatedValuesValid :: forall a. (HasSchema a, ToJSON a, Arbitrary a) => Property
prop_generatedValuesValid = property $ do
  val <- forAll arbitrary
  case validateDynamic (val :: a) of
    ValidationSuccess _ -> success
    ValidationFailure _ -> failure
```

### Configuration

Control `HasSchema` generation via config:

```haskell
data CodegenConfig = CodegenConfig
  { ...
  , codegenGenerateHasSchema :: Bool
    -- ^ Generate HasSchema instances (default: True)
  
  , codegenEmbedSchema :: SchemaEmbedMode
    -- ^ How to embed schema in generated code
  }

data SchemaEmbedMode
  = EmbedFull        -- ^ Full schema embedded (larger binary)
  | EmbedMinimal     -- ^ Only essential metadata
  | EmbedNone        -- ^ No embedding (HasSchema not generated)
```

### Performance Considerations

- Schema embedded at compile time (no runtime overhead to construct)
- Schema size impacts binary size (~1-10KB per schema typically)
- Use `EmbedMinimal` for large schemas where only metadata needed
- `schemaFor` call is O(1) (returns pre-constructed value)

---

## Code Quality Guarantees

### Generated Code Standards

**Compilation**:
- Compiles with `-Wall` without warnings
- No orphan instances
- No incomplete pattern matches
- Explicit export lists in generated modules

**Documentation**:
- Haddock comments from schema `title` and `description`
- Field documentation from property descriptions
- Examples from schema `examples` field
- Type signatures always explicit

**Performance**:
- No unnecessary boxing/unboxing
- Strict fields (if configured)
- Efficient instance implementations
- No space leaks

**Safety**:
- Total functions (no `undefined`, `error`)
- Exhaustive pattern matching
- Smart constructors for constrained types
- Validation in FromJSON, not after construction

## Strategies

### Default Strategy (Records)

**For**: Object schemas  
**Generates**: Haskell records  
**Features**: Required/optional fields, nested objects, field validation

### Newtype Strategy

**For**: Primitive types with constraints  
**Generates**: Newtypes with smart constructors  
**Features**: Validation functions, safe construction, derived instances

### Sum Type Strategy

**For**: oneOf, anyOf schemas  
**Generates**: Sum types with constructors  
**Features**: Alternative parsing, tagged/untagged unions, discriminators

### Enum Strategy

**For**: enum keyword with string values  
**Generates**: ADT with no fields  
**Features**: Bounded, Enum instances, exhaustive

### Array Strategy

**For**: Array schemas  
**Generates**: `[a]` or `Vector a`  
**Features**: Item type, length constraints, uniqueness

### Map Strategy

**For**: Objects with patternProperties or additionalProperties  
**Generates**: `Map Text a` or `HashMap Text a`  
**Features**: Dynamic keys, value type

## Framework Integration

### Servant API Generation

**From**: OpenAPI paths and operations  
**Generates**: Servant API type

```haskell
type UserAPI
  = "users" :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] User
  :<|> "users" :> Capture "userId" UserId :> Get '[JSON] User
```

**Features**: Path parameters, query parameters, request/response types

### Yesod Routes Generation

**From**: OpenAPI paths and operations  
**Generates**: Yesod route definitions

```
/users UsersR GET POST
/users/#UserId UserR GET PUT DELETE
```

**Features**: Handler type signatures, resource names, HTTP methods

## Performance Guarantees

- TH compilation overhead < 10% vs hand-written code
- Generated code performance equivalent to hand-written
- No runtime overhead from code generation
- Compile-time schema validation (fail fast)

