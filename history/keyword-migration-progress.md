# JSON Schema Keyword Migration Progress

## Overview
This document tracks the migration of standard JSON Schema keywords from the old validation system to the new pluggable keyword architecture.

## Completed Migrations (17 keywords)

### Basic Validation (3)
- ✅ `const` - exact value equality
- ✅ `enum` - value in allowed set
- ✅ `type` - JSON type validation (null, boolean, string, number, integer, object, array, unions)

### String Validation (3)
- ✅ `minLength` - minimum string length
- ✅ `maxLength` - maximum string length
- ✅ `pattern` - ECMAScript regex pattern matching (compiles regex once during compilation)

### Numeric Validation (5)
- ✅ `minimum` - number >= minimum
- ✅ `maximum` - number <= maximum
- ✅ `multipleOf` - number is multiple of divisor
- ✅ `exclusiveMinimum` - number > minimum (Draft-06+ standalone numeric style)
- ✅ `exclusiveMaximum` - number < maximum (Draft-06+ standalone numeric style)

### Array Validation (3)
- ✅ `minItems` - minimum array length
- ✅ `maxItems` - maximum array length
- ✅ `uniqueItems` - ensure all array elements are unique

### Object Validation (3)
- ✅ `required` - validates that specified properties are present
- ✅ `minProperties` - validates minimum number of object properties
- ✅ `maxProperties` - validates maximum number of object properties

## Blocked - Requires Recursive Validation Support

The following keywords cannot be migrated with the current pluggable architecture because they require recursive schema validation:

### Array Validation (Blocked)
- ⛔ `items` / `prefixItems` - schema validation for array items
- ⛔ `contains` / `minContains` / `maxContains` - at least N items match schema

### Object Validation (Partially Blocked)
- ⛔ `properties` - schema validation for object properties
- ⛔ `patternProperties` - schema validation for properties matching patterns
- ⛔ `additionalProperties` - schema for unmatched properties
- ⛔ `propertyNames` - schema for property names themselves
- ⛔ `dependentSchemas` - schemas to apply when property is present
- 🟡 `dependentRequired` - properties required when another is present (might be doable without recursion)

### Logical Combinators (All Blocked)
- ⛔ `allOf` - value must validate against all schemas
- ⛔ `anyOf` - value must validate against at least one schema
- ⛔ `oneOf` - value must validate against exactly one schema
- ⛔ `not` - value must NOT validate against schema

### Conditional Logic (Blocked)
- ⛔ `if` / `then` / `else` - conditional schema application

### References (Blocked)
- ⛔ `$ref` - reference to another schema
- ⛔ `$dynamicRef` - dynamic reference resolution

## Potentially Migrateable (No Recursive Validation Needed)

### Object Validation (Simple)
- 🟡 `dependentRequired` - checks property presence dependencies (similar to `required`)

### Format Validation
- 🟡 `format` - validates string format (uri, email, date-time, hostname, etc.)
  - This would be a meta-keyword that dispatches to specific format validators

### Annotations (Metadata Only)
- 🟡 `title`, `description`, `default`, `examples`, `deprecated`, `readOnly`, `writeOnly`
  - These are metadata, not really validation
  - Could be "validated" (always pass) just to store/track them in the pluggable system

## Architecture Limitation Discovered

**Issue**: The current pluggable keyword architecture has a fundamental limitation for keywords that need recursive validation:

### Current `ValidateFunc` Signature
```haskell
type ValidateFunc a = a -> Value -> [Text]
```

This signature only allows:
- Access to compiled keyword data (`a`)
- Access to the instance value being validated (`Value`)
- Returning error messages (`[Text]`)

### What's Missing for Recursive Validation
1. **No validation context** - Can't access the schema registry or validation configuration
2. **No recursive validator function** - Can't validate sub-values against sub-schemas
3. **No annotation collection** - Can't collect/propagate annotations for unevaluated* keywords
4. **No proper error reporting** - Just flat strings, not structured errors with paths

### Proposed Solution

To support recursive validation, we need to enhance the `ValidateFunc` type to include:

```haskell
type ValidateFunc a = ValidateContext -> a -> Value -> ValidationResult

data ValidateContext = ValidateContext
  { validateContextRecurse :: Schema -> Value -> ValidationResult
    -- ^ Function to recursively validate a value against a schema
  , validateContextConfig :: ValidationConfig
    -- ^ Validation configuration
  , validateContextInstancePath :: [Text]
    -- ^ Current path in instance (for error reporting)
  , validateContextSchemaPath :: [Text]
    -- ^ Current path in schema (for error reporting)
  }
```

This would allow keywords like `items`, `properties`, `allOf`, etc. to:
1. Compile sub-schemas during the compile phase
2. Recursively validate against those sub-schemas during the validate phase
3. Properly collect and propagate annotations
4. Generate structured error messages with paths

## Test Status

All 7701 existing tests continue to pass with the 17 migrated keywords.

## Commits

1. `f50336c` - feat(json-schema): Migrate minItems, maxItems, uniqueItems to pluggable keywords
2. `6c74537` - feat(json-schema): Migrate exclusiveMinimum, exclusiveMaximum to pluggable keywords
3. `8f62506` - feat(json-schema): Migrate required, minProperties, maxProperties to pluggable keywords

## Next Steps

1. **Short term**: Migrate the remaining simple keywords that don't need recursion:
   - `dependentRequired` (similar to `required`)
   - `format` (as a meta-keyword with format-specific validators)

2. **Long term**: Enhance the pluggable architecture to support recursive validation:
   - Design the enhanced `ValidateFunc` signature
   - Update the validation infrastructure to support recursive calls
   - Migrate complex keywords (items, properties, allOf, etc.)
   - Update all simple keywords to use new signature (backward compatible change)

## bd Issues

### Completed
- `fractal-5oz` ✅ - Migrate minItems keyword to pluggable architecture (closed)
- `fractal-8ju` ✅ - Migrate maxItems keyword to pluggable architecture (closed)
- `fractal-dob` ✅ - Migrate uniqueItems keyword to pluggable architecture (closed)
- `fractal-ka4` ✅ - Migrate exclusiveMinimum keyword to pluggable architecture (closed)
- `fractal-fh4` ✅ - Migrate exclusiveMaximum keyword to pluggable architecture (closed)
- `fractal-cwf` ✅ - Migrate required keyword to pluggable architecture (closed)
- `fractal-0yt` ✅ - Migrate minProperties keyword to pluggable architecture (closed)
- `fractal-awp` ✅ - Migrate maxProperties keyword to pluggable architecture (closed)

### Blocked
- `fractal-14g` ⛔ - Migrate items/prefixItems keywords to pluggable architecture (blocked)
- `fractal-qzm` ⛔ - Migrate contains/minContains/maxContains keywords to pluggable architecture (blocked)

### Critical Blocker
- `fractal-tos` 🔴 - Enhance pluggable keyword ValidateFunc to support recursive validation (open, priority 0 - critical blocker)
