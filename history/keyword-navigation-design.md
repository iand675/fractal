# Design: Pluggable Keyword Schema Navigation

## Problem

The `resolvePointerInSchemaWithBase` function has hardcoded navigation logic for 13+ keywords. This prevents custom keywords from properly declaring how to navigate their subschemas.

## Solution Overview

Add a navigation capability to `KeywordDefinition` that declares:
1. Whether the keyword contains subschemas
2. How to extract/navigate into those subschemas via JSON Pointer

## Navigation Types

Keywords can contain subschemas in several ways:

```haskell
data KeywordNavigation
  = NoNavigation
    -- ^ Keyword doesn't contain subschemas (e.g., type, enum, const)
  
  | SingleSchema (Schema -> Maybe Schema)
    -- ^ Single subschema (e.g., not, contains, propertyNames, additionalProperties)
    -- Function extracts the subschema from a parent schema
  
  | SchemaMap (Schema -> Maybe (Map Text Schema))
    -- ^ Map of subschemas keyed by Text (e.g., properties, patternProperties, dependentSchemas)
    -- Next pointer segment is used as map key
  
  | SchemaArray (Schema -> Maybe [Schema])
    -- ^ Array of subschemas (e.g., allOf, anyOf, oneOf, prefixItems)
    -- Next pointer segment must be numeric index
  
  | CustomNavigation (Schema -> Text -> [Text] -> Maybe (Schema, [Text]))
    -- ^ Custom navigation logic for complex cases (e.g., items with dual behavior)
    -- Takes: parent schema, current segment, remaining segments
    -- Returns: (resolved schema, remaining segments to process)
```

## KeywordDefinition Update

```haskell
data KeywordDefinition = KeywordDefinition
  { keywordName :: Text
  , keywordScope :: KeywordScope
  , keywordCompile :: CompileFunc a
  , keywordValidate :: ValidateFunc a
  , keywordNavigation :: KeywordNavigation  -- NEW
  }
```

## Helper Function

```haskell
-- | Create keyword definition without navigation (most keywords)
mkSimpleKeyword :: Typeable a 
                => Text 
                -> KeywordScope 
                -> CompileFunc a 
                -> ValidateFunc a 
                -> KeywordDefinition
mkSimpleKeyword name scope compile validate = KeywordDefinition
  { keywordName = name
  , keywordScope = scope
  , keywordCompile = compile
  , keywordValidate = validate
  , keywordNavigation = NoNavigation
  }

-- | Create keyword definition with navigation
mkNavigableKeyword :: Typeable a
                   => Text
                   -> KeywordScope
                   -> CompileFunc a
                   -> ValidateFunc a
                   -> KeywordNavigation
                   -> KeywordDefinition
```

## Updated resolvePointerInSchemaWithBase

```haskell
resolvePointerInSchemaWithBase :: KeywordRegistry -> Text -> Schema -> Maybe Text -> Maybe (Schema, Maybe Text)
resolvePointerInSchemaWithBase registry pointer schema currentBase =
  case parsePointer pointer of
    Left _ -> Nothing
    Right (JSONPointer segments) -> followPointer segments schema currentBase
  where
    followPointer :: [Text] -> Schema -> Maybe Text -> Maybe (Schema, Maybe Text)
    followPointer [] s base = Just (s, base)
    followPointer (seg:rest) s base = case schemaCore s of
      BooleanSchema _ -> Nothing
      ObjectSchema obj -> 
        -- Try pluggable keywords first
        case tryPluggableNavigation seg rest s obj base of
          Just result -> Just result
          Nothing -> tryFallbackNavigation seg rest s obj base
    
    -- Try navigation via registered keywords
    tryPluggableNavigation seg rest parentSchema obj base =
      case lookupKeyword seg registry of
        Just keywordDef ->
          case keywordNavigation keywordDef of
            NoNavigation -> Nothing
            
            SingleSchema extract ->
              case extract parentSchema of
                Just subSchema -> followPointer rest subSchema base
                Nothing -> Nothing
            
            SchemaMap extractMap ->
              case rest of
                (key:remaining) ->
                  case extractMap parentSchema >>= Map.lookup key of
                    Just subSchema -> followPointer remaining subSchema base
                    Nothing -> Nothing
                [] -> Nothing
            
            SchemaArray extractArray ->
              case rest of
                (idx:remaining) ->
                  case reads (T.unpack idx) :: [(Int, String)] of
                    [(n, "")] ->
                      case extractArray parentSchema of
                        Just schemas | n >= 0 && n < length schemas ->
                          followPointer remaining (schemas !! n) base
                        _ -> Nothing
                    _ -> Nothing
                [] -> Nothing
            
            CustomNavigation nav ->
              nav parentSchema seg rest >>= \(resolved, remaining) ->
                followPointer remaining resolved base
        
        Nothing -> Nothing
    
    -- Fallback for built-in structural keywords ($defs, etc.)
    tryFallbackNavigation seg rest parentSchema obj base
      | seg == "$defs" || seg == "definitions" = ... -- Keep minimal fallbacks
      | otherwise = 
          -- Try schemaExtensions as last resort
          case Map.lookup seg (schemaExtensions parentSchema) of ...
```

## Example Implementations

### Simple Single Schema Keywords

```haskell
-- not keyword
notKeyword :: KeywordDefinition
notKeyword = mkNavigableKeyword "not" AnyScope compileNot validateNot $
  SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> schemaNot obj
    _ -> Nothing

-- contains keyword
containsKeyword :: KeywordDefinition
containsKeyword = mkNavigableKeyword "contains" ArrayScope compileContains validateContains $
  SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationContains (schemaValidation obj)
    _ -> Nothing
```

### Map-Based Keywords

```haskell
-- properties keyword
propertiesKeyword :: KeywordDefinition
propertiesKeyword = mkNavigableKeyword "properties" ObjectScope compileProperties validateProperties $
  SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationProperties (schemaValidation obj)
    _ -> Nothing

-- dependentSchemas keyword
dependentSchemasKeyword :: KeywordDefinition
dependentSchemasKeyword = mkNavigableKeyword "dependentSchemas" ObjectScope compileDependentSchemas validateDependentSchemas $
  SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationDependentSchemas (schemaValidation obj)
    _ -> Nothing
```

### Array-Based Keywords

```haskell
-- allOf keyword
allOfKeyword :: KeywordDefinition
allOfKeyword = mkNavigableKeyword "allOf" AnyScope compileAllOf validateAllOf $
  SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> fmap NE.toList (schemaAllOf obj)
    _ -> Nothing

-- prefixItems keyword
prefixItemsKeyword :: KeywordDefinition
prefixItemsKeyword = mkNavigableKeyword "prefixItems" ArrayScope compilePrefixItems validatePrefixItems $
  SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> fmap NE.toList (validationPrefixItems (schemaValidation obj))
    _ -> Nothing
```

### Custom Navigation (Complex Cases)

```haskell
-- items keyword (dual behavior in Draft-04)
itemsKeyword :: KeywordDefinition
itemsKeyword = mkNavigableKeyword "items" ArrayScope compileItems validateItems $
  CustomNavigation $ \schema seg rest -> case schemaCore schema of
    ObjectSchema obj ->
      case validationItems (schemaValidation obj) of
        Just (ItemsSchema itemSchema) ->
          -- Single schema for all items
          Just (itemSchema, rest)
        
        Just (ItemsTuple schemas _) ->
          -- Tuple validation - need numeric index
          case reads (T.unpack seg) :: [(Int, String)] of
            [(n, "")] | n >= 0 && n < length schemas ->
              Just (NE.toList schemas !! n, rest)
            _ -> Nothing
        
        Nothing -> Nothing
    _ -> Nothing
```

## Migration Strategy

1. **Phase 1**: Add `keywordNavigation` field to `KeywordDefinition` (default to `NoNavigation`)
2. **Phase 2**: Update helper functions (`mkSimpleKeyword`, `mkNavigableKeyword`)
3. **Phase 3**: Add navigation to existing keyword implementations
4. **Phase 4**: Update `resolvePointerInSchemaWithBase` to try pluggable navigation first
5. **Phase 5**: Remove hardcoded navigation for keywords that are now pluggable
6. **Phase 6**: Keep minimal fallback for `$defs`/`definitions` as structural keywords

## Benefits

- Custom keywords can declare navigation behavior
- Consistent navigation mechanism across all keywords
- Reduces hardcoded logic in validator
- Enables proper `$ref` resolution through custom keywords
- Supports recursive validation in pluggable system

## Backward Compatibility

- All existing keywords continue to work (NoNavigation is safe default)
- Fallback path preserves old behavior during migration
- Can migrate keywords incrementally

