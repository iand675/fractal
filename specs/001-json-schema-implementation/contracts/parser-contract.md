# Parser Contract

**Module**: `Fractal.JsonSchema.Parser`  
**Purpose**: Parse JSON Schema from JSON/YAML with version detection and reference resolution

## Public API

### Core Parsing Functions

```haskell
-- | Parse a JSON Schema from a Value with automatic version detection
parseSchema :: Value -> Either ParseError Schema

-- | Parse with explicit version (skip detection)
parseSchemaWithVersion :: JsonSchemaVersion -> Value -> Either ParseError Schema

-- | Strict parsing (fail on unknown keywords)
parseSchemaStrict :: Value -> Either ParseError Schema

-- | Parse from file (handles JSON and YAML)
parseSchemaFromFile :: FilePath -> IO (Either ParseError Schema)

-- | Parse from URI (fetch and parse)
parseSchemaFromURI :: URI -> IO (Either ParseError Schema)
```

### Error Type

```haskell
data ParseError = ParseError
  { parseErrorPath :: JSONPointer      -- Where in the schema
  , parseErrorMessage :: Text           -- What went wrong
  , parseErrorContext :: Maybe Value    -- Problematic value
  } deriving (Eq, Show)
```

## Contracts

### Version Detection

**Pre-conditions**:
- Input is valid JSON/YAML

**Post-conditions**:
- If `$schema` present and recognized → uses that version
- If `$schema` present but unrecognized → error
- If `$schema` absent → uses default dialect (2020-12)
- Version stored in `schemaVersion` field

**Invariants**:
- Detected version matches schema features used

### Reference Handling

**Pre-conditions**:
- Schema contains `$ref` or `$dynamicRef` keywords

**Post-conditions**:
- `$ref` parsed into `schemaRef` field
- Relative references preserved (not resolved during parsing)
- External references noted for later resolution

**Invariants**:
- `$ref` in draft-04/06/07 is the only keyword (except annotations)
- In 2019-09+, `$ref` can coexist with other keywords

### Unknown Keyword Handling

**Behavior depends on dialect's `unknownKeywordMode`**:

- **IgnoreUnknown**: Silently skip
- **WarnUnknown**: Add to warnings, continue parsing
- **ErrorOnUnknown**: Fail with `UnknownKeywordError`
- **CollectUnknown**: Store in `schemaExtensions` map

**Invariants**:
- Standard keywords never in `schemaExtensions`
- Custom vocabulary keywords recognized if vocabulary registered

### Format Validation

**Pre-conditions**:
- Schema contains `format` keyword

**Post-conditions**:
- Format string parsed into `Format` enum or `CustomFormat`
- Invalid format strings → `CustomFormat` wrapper

**Invariants**:
- Standard formats recognized correctly
- Format behavior (assertion/annotation) determined by dialect

## Properties

### Roundtrip Property

```haskell
-- Property: Parse then render produces equivalent schema
property_parseRenderRoundtrip :: Schema -> Property
property_parseRenderRoundtrip schema =
  parseSchema (renderSchema schema) === Right schema
```

### Version Consistency

```haskell
-- Property: Version-specific keywords only present when version supports them
property_versionConsistency :: Schema -> Property
property_versionConsistency schema =
  let ver = fromMaybe Draft202012 (schemaVersion schema)
  in case (ver, schemaCore schema) of
      (Draft04, _) -> not (hasUnevaluatedKeywords schema)
      (Draft06, _) -> not (hasIfThenElse schema)
      (Draft07, _) -> not (hasUnevaluatedKeywords schema)
      _ -> True
```

## Error Conditions

| Condition | Error |
|-----------|-------|
| Invalid JSON/YAML | `ParseError "Invalid JSON/YAML"` |
| Unknown `$schema` URI | `ParseError "Unknown schema version"` |
| Unknown keyword (strict mode) | `ParseError "Unknown keyword: <name>"` |
| Invalid keyword value | `ParseError "<keyword> must be <expected>"` |
| Circular `$id` references | `ParseError "Circular $id reference"` |

## Performance Guarantees

- Parse 1000 schemas/second (schemas < 10KB)
- Memory usage O(n) where n = schema size
- No I/O blocking for in-memory schemas

