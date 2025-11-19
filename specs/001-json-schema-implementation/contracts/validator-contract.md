# Validator Contract

**Module**: `Fractal.JsonSchema.Validator`  
**Purpose**: Validate JSON values against schemas with detailed error reporting

## Public API

### Core Validation Functions

```haskell
-- | Validate a value against a schema
validateValue 
  :: ValidationConfig 
  -> Schema 
  -> Value 
  -> ValidationResult

-- | Validate with explicit context
validateValueWithContext 
  :: ValidationContext 
  -> Schema 
  -> Value 
  -> ValidationResult

-- | Compile a validator for repeated use
compileValidator 
  :: ValidationConfig 
  -> Schema 
  -> Either CompileError Validator

newtype Validator = Validator { runValidator :: Value -> ValidationResult }

-- | Batch validation
validateValues 
  :: ValidationConfig 
  -> Schema 
  -> [Value] 
  -> [(Value, ValidationResult)]
```

### Default Configuration

```haskell
-- | Default validation configuration (latest version, format annotation)
defaultValidationConfig :: ValidationConfig

-- | Strict validation (format assertion, all errors collected)
strictValidationConfig :: ValidationConfig
```

## Contracts

### Validation Semantics

**Pre-conditions**:
- Schema is well-formed (passed parsing)
- Value is valid JSON

**Post-conditions**:
- Returns `ValidationSuccess` with annotations OR
- Returns `ValidationFailure` with non-empty error list
- Never both success and failure
- Error paths are valid JSON Pointers

**Invariants**:
- Validation is deterministic (same inputs → same outputs)
- Validation is pure (no side effects)
- Annotations only collected if config requests them

### Type Validation

**For each JSON type**:

| Schema Type | Valid JSON Types |
|-------------|------------------|
| `null` | Null |
| `boolean` | Boolean |
| `integer` | Integer (whole number, may be `Number` in JSON) |
| `number` | Number (integer or float) |
| `string` | String |
| `array` | Array |
| `object` | Object |

**Invariants**:
- Type union (`["string", "number"]`) passes if value matches ANY type
- Missing `type` keyword → all types valid

### Numeric Validation

**Keywords**: `minimum`, `maximum`, `exclusiveMinimum`, `exclusiveMaximum`, `multipleOf`

**Contracts**:
- `minimum`: value ≥ minimum
- `maximum`: value ≤ maximum
- `exclusiveMinimum` (draft-06+): value > exclusiveMinimum
- `exclusiveMinimum: true` (draft-04): value > maximum
- `multipleOf`: value % multipleOf == 0 (within floating point precision)

**Invariants**:
- Draft-04 `exclusiveMaximum/Minimum` are booleans modifying maximum/minimum
- Draft-06+ they are standalone numeric values
- `multipleOf` must be > 0

### String Validation

**Keywords**: `minLength`, `maxLength`, `pattern`, `format`

**Contracts**:
- `minLength/maxLength`: count **Unicode characters**, not bytes
- `pattern`: ECMA-262 regular expression match
- `format`: semantic validation (configurable assertion/annotation)

**Invariants**:
- Length measured in Unicode code points
- Pattern matching uses regex-tdfa (POSIX compliance)
- Format assertion only if config enables it

### Array Validation

**Keywords**: `items`, `prefixItems`, `additionalItems`, `contains`, `minItems`, `maxItems`, `uniqueItems`

**Contracts**:
- `items` (schema): all array items validate against schema
- `items` (array of schemas, draft-04/06/07): tuple validation + additional items
- `prefixItems` (2020-12): tuple validation (first N items)
- `contains`: at least one item validates against schema
- `uniqueItems`: no duplicate items (deep equality)

**Invariants**:
- Item evaluation tracked for `unevaluatedItems`
- `contains` fails if array empty (unless `minContains: 0`)
- Unique items uses deep equality (structural comparison)

### Object Validation

**Keywords**: `properties`, `patternProperties`, `additionalProperties`, `propertyNames`, `required`, `minProperties`, `maxProperties`, `dependentRequired`, `dependentSchemas`

**Contracts**:
- `properties`: named properties validate against their schemas
- `patternProperties`: properties matching regex validate against schema
- `additionalProperties`: properties not in properties/patternProperties validate
- `required`: listed properties must be present
- `propertyNames`: property names validate against schema

**Invariants**:
- Property evaluation tracked for `unevaluatedProperties`
- `required` is Set (no duplicates)
- Pattern matching uses compiled regex
- Property name schema typically validates strings

### Composition

**Keywords**: `allOf`, `anyOf`, `oneOf`, `not`

**Contracts**:
- `allOf`: validates against ALL subschemas
- `anyOf`: validates against AT LEAST ONE subschema
- `oneOf`: validates against EXACTLY ONE subschema
- `not`: does NOT validate against subschema

**Invariants**:
- `allOf` is conjunction (AND)
- `anyOf` is disjunction (OR)
- `oneOf` is exclusive disjunction (XOR)
- `not` is negation (NOT)
- Annotations from multiple branches in `anyOf`/`oneOf` are merged

### Conditional Validation (draft-07+)

**Keywords**: `if`, `then`, `else`

**Contracts**:
- If `if` validates successfully → apply `then`
- If `if` fails validation → apply `else`
- Missing `then`/`else` means no additional validation

**Invariants**:
- `if` validation result doesn't affect overall result
- Only `then` or `else` applied, never both

### Unevaluated (2019-09+)

**Keywords**: `unevaluatedProperties`, `unevaluatedItems`

**Contracts**:
- Track which properties/items evaluated by any keyword
- Apply schema to unevaluated ones

**Invariants**:
- Evaluation tracking includes: `properties`, `patternProperties`, `additionalProperties`, `allOf`, `anyOf`, `oneOf`, `if/then/else`
- Sibling keywords evaluated first
- `unevaluated*` evaluated last

### Reference Resolution

**Keywords**: `$ref`, `$dynamicRef`

**Contracts**:
- `$ref` resolves to referenced schema and validates
- `$dynamicRef` resolves using dynamic scope
- Circular references detected (no infinite loops)

**Invariants**:
- References resolved from SchemaRegistry
- Base URI tracked for relative resolution
- Visited schemas tracked to prevent cycles

## Properties

### Monotonicity Property

```haskell
-- Property: More restrictive schema → subset of valid values
property_validationMonotonicity :: Schema -> Schema -> Value -> Property
property_validationMonotonicity schema1 schema2 value =
  -- If schema2 is more restrictive (allOf [schema1, additional constraints])
  -- Then: validates(value, schema2) => validates(value, schema1)
  let schema2IsMoreRestrictive = isMoreRestrictive schema1 schema2
      validates1 = isSuccess $ validateValue defaultConfig schema1 value
      validates2 = isSuccess $ validateValue defaultConfig schema2 value
  in schema2IsMoreRestrictive && validates2 ==> validates1
```

### Error Path Correctness

```haskell
-- Property: Error paths point to actual problem locations
property_errorPathsValid :: Schema -> Value -> Property
property_errorPathsValid schema value =
  case validateValue defaultConfig schema value of
    ValidationSuccess _ -> property True
    ValidationFailure errs -> all (pathExistsInValue value) (errorInstancePath <$> unErrors errs)
```

### Annotation Collection

```haskell
-- Property: Annotations collected when enabled
property_annotationsCollected :: Schema -> Value -> Property
property_annotationsCollected schema value =
  let config = defaultConfig { validationCollectAnnotations = True }
      result = validateValue config schema value
  in case result of
      ValidationSuccess annots -> not (Map.null $ unAnnotations annots)
      ValidationFailure _ -> property True
```

## Error Conditions

| Condition | Error Example |
|-----------|---------------|
| Type mismatch | `Expected string, got number at /path` |
| Constraint violation | `Value 200 exceeds maximum 150 at /age` |
| Missing required | `Required property "name" missing at /` |
| Pattern mismatch | `Value does not match pattern "^[a-z]+$" at /username` |
| Circular reference | `Circular reference detected at $ref` |
| Unknown format | `Unknown format "my-custom-format" at /field` |
| No matching anyOf | `Value matches none of the schemas in anyOf` |

## Performance Guarantees

- Validate 1000+ values/second against compiled validator
- Short-circuit mode: stop at first error (faster)
- Full error mode: collect all errors (more informative)
- Memory usage O(n) where n = max(schema size, value size)
- No I/O during validation (pure function)

## Compatibility

- draft-04: Basic validation, no conditionals, no unevaluated
- draft-06: Adds `const`, `propertyNames`
- draft-07: Adds `if/then/else`, `readOnly/writeOnly`
- 2019-09: Adds `unevaluatedProperties/Items`, `dependentRequired/Schemas`
- 2020-12: Adds `prefixItems`, `$dynamicRef`

