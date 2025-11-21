# Registry Pre-loading Fix Specification

## Problem Statement

The `buildRegistryWithExternalRefs` function fails to properly resolve relative $ref URIs when schemas contain nested $id values that change the effective base URI. This causes external schema files to not be loaded into the registry, resulting in "Unable to resolve reference" errors during validation.

## Root Cause

In `Types.hs:983-1010`, the `collectFromObject` function recursively collects external references from nested schemas (definitions, properties, items, etc.). However, it passes the SAME `base` URI parameter to all nested schemas, regardless of whether those schemas have their own `$id` that would change the effective base.

### Example Failure Case

```json
{
  "$id": "http://localhost:1234/scope_change_defs2.json",
  "definitions": {
    "baz": {
      "$id": "baseUriChangeFolderInSubschema/",
      "definitions": {
        "bar": {
          "$ref": "folderInteger.json"
        }
      }
    }
  }
}
```

**Current behavior:**
1. Process main schema with base `http://localhost:1234/scope_change_defs2.json`
2. Recurse into `definitions/baz` with base `http://localhost:1234/scope_change_defs2.json`
3. Find `$ref: "folderInteger.json"`
4. Resolve against base → `http://localhost:1234/folderInteger.json` ❌ WRONG!

**Expected behavior:**
1. Process main schema with base `http://localhost:1234/scope_change_defs2.json`
2. Recurse into `definitions/baz`, compute its effective base:
   - It has `$id: "baseUriChangeFolderInSubschema/"`
   - Resolve against parent base → `http://localhost:1234/baseUriChangeFolderInSubschema/`
3. Find `$ref: "folderInteger.json"`
4. Resolve against baz's effective base → `http://localhost:1234/baseUriChangeFolderInSubschema/folderInteger.json` ✓ CORRECT!

## Solution

### Phase 1: Fix `collectExternalReferenceDocs` recursion

Modify `collectExternalReferenceDocs` to:
1. Compute the effective base URI for each schema using `schemaEffectiveBase`
2. Pass this effective base when recursing into nested schemas

**Changes required in `Types.hs`:**

```haskell
-- BEFORE (line 959-968):
collectExternalReferenceDocs :: Maybe Text -> Schema -> [Text]
collectExternalReferenceDocs parentBase schema =
  case schemaCore schema of
    BooleanSchema _ -> []
    ObjectSchema obj ->
      let info = schemaRegistrationInfo parentBase schema
          baseUri = sriBaseURI info  -- Just uses schema's $id
          directRefs = maybe [] (resolveRef baseUri) (schemaRef obj)
          dynamicRefs = maybe [] (resolveRef baseUri) (schemaDynamicRef obj)
          subRefs = collectFromObject baseUri obj  -- BUG: passes same base to all children!
      in uniqueTexts (directRefs <> dynamicRefs <> subRefs)

-- AFTER:
collectExternalReferenceDocs :: Maybe Text -> Schema -> [Text]
collectExternalReferenceDocs parentBase schema =
  case schemaCore schema of
    BooleanSchema _ -> []
    ObjectSchema obj ->
      let effectiveBase = schemaEffectiveBase parentBase schema  -- Compute effective base
          directRefs = maybe [] (resolveRef effectiveBase) (schemaRef obj)
          dynamicRefs = maybe [] (resolveRef effectiveBase) (schemaDynamicRef obj)
          subRefs = collectFromObject effectiveBase obj  -- Pass effective base
      in uniqueTexts (directRefs <> dynamicRefs <> subRefs)
```

### Phase 2: Ensure nested schema base URIs are computed correctly

The `collectFromObject` function at line 983 needs to ensure that when it recurses into nested schemas (definitions, properties, etc.), each schema's references are resolved against THAT schema's effective base, not the parent's base.

**Current code:**
```haskell
collectFromObject :: Maybe Text -> SchemaObject -> [Text]
collectFromObject base obj =
  let defRefs = concatMap (collectExternalReferenceDocs base) (Map.elems $ schemaDefs obj)
      -- ^^^ BUG: passes parent 'base' to all definitions
```

The schemas in `schemaDefs obj` might have their own `$id` values that change the effective base. We need to pass the parent base and let each schema compute its own effective base.

**This is actually already handled correctly** because `collectExternalReferenceDocs` calls `schemaEffectiveBase` at the start! So the fix in Phase 1 is sufficient.

## Testing

After implementing the fix, these test cases should pass:

1. **base URI change - change folder in subschema** (5 tests)
   - Failing: `folderInteger.json` not found
   - Should load: `http://localhost:1234/baseUriChangeFolderInSubschema/folderInteger.json`

2. **root ref in remote ref** (4 tests)
   - Failing: `name-defs.json#/$defs/orNull` not found
   - Should load: `http://localhost:1234/draft*/name-defs.json`

3. **Other base URI change scenarios** (various)
   - Complex nested $id with relative paths

## Implementation Notes

1. The fix is minimal - only change how we compute `baseUri` in `collectExternalReferenceDocs`
2. Use existing `schemaEffectiveBase` function which already handles:
   - Relative $id resolution
   - Fragment-only $id (anchors)
   - No $id (inherits parent base)
3. No changes needed to `registerSchemaInRegistry` - it already handles base URI correctly
4. No changes needed to validation logic - only affects registry building

## Expected Impact

- **Tests fixed**: ~14 of the remaining 19 $ref failures
- **Risk**: Low - only affects how we collect URIs for pre-loading, not validation logic
- **Performance**: No impact - same number of operations, just with correct URIs

## Alternative Approaches Considered

### 1. Change `collectFromObject` signature
Pass both parent base and effective base separately. **Rejected** - unnecessary complexity when `schemaEffectiveBase` already handles this.

### 2. Post-process collected refs
Collect all refs first, then resolve against proper bases. **Rejected** - requires maintaining schema tree structure, more complex.

### 3. Two-pass registry building
First pass: register all schemas; Second pass: load external refs. **Rejected** - doesn't solve the base URI computation problem.

## Related Issues

- This fix addresses the core registry pre-loading issue
- Remaining $ref failures after this fix will be:
  - Complex cross-version references (draft4/name.json, etc.) - may need version-specific loader logic
  - Edge cases with URI resolution (bar#/$defs/item) - may need fragment handling in collector

## References

- JSON Schema specification: Base URI and reference resolution
- RFC 3986: URI Generic Syntax (relative URI resolution)
- Current implementation: `fractal-openapi/src/Fractal/JsonSchema/Types.hs:918-1011`
