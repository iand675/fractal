# Monadic Compilation Context Design

## Problem

Keywords need to access compiled data from adjacent keywords during compilation. Examples:

1. **Draft-04 `minimum`**: Needs to check if `exclusiveMinimum` (boolean) is present
2. **`unevaluatedProperties`**: Needs to know what properties were evaluated by `properties`, `patternProperties`, etc.
3. **Composite validators**: Any keyword that modifies or depends on another

Current architecture compiles keywords independently with no access to each other's compiled state.

## Solution: Monadic Compilation with StateT

Use `StateT` to carry mutable compilation state, allowing keywords to lazily request compilation of adjacent keywords.

### Core Types

```haskell
-- Compilation monad with mutable state
type CompileM = StateT CompilationState (Either Text)

-- State tracked during compilation
data CompilationState = CompilationState
  { stateCompiled :: CompiledKeywords
    -- ^ Keywords compiled so far
  , stateRegistry :: KeywordRegistry
    -- ^ Available keyword definitions
  , stateSchema :: Schema
    -- ^ Current schema being compiled
  , stateContext :: CompilationContext
    -- ^ Additional context (refs, parent path, etc.)
  , stateCompiling :: Set Text
    -- ^ Currently being compiled (for cycle detection)
  }

-- Updated compile function signature
type CompileFunc a = Value -> CompileM a

-- Updated keyword definition
data KeywordDefinition = KeywordDefinition
  { keywordName :: Text
  , keywordScope :: KeywordScope
  , keywordCompile :: forall a. Typeable a => Value -> CompileM a
  , keywordValidate :: forall a. Typeable a => a -> Value -> [Text]
  }
```

### Key Operations

```haskell
-- Request compilation of an adjacent keyword (lazy, memoized)
compileAdjacent :: Text -> CompileM (Maybe CompiledKeyword)
compileAdjacent name = do
  state <- get

  -- Check if already compiled
  case lookupCompiledKeyword name (stateCompiled state) of
    Just ck -> pure (Just ck)
    Nothing -> do
      -- Check for circular dependency
      when (Set.member name (stateCompiling state)) $
        lift $ Left $ "Circular keyword dependency detected: " <> name

      -- Try to find the keyword value in schema
      case getKeywordValue name (stateSchema state) of
        Nothing -> pure Nothing  -- Keyword not present
        Just value -> do
          -- Mark as compiling
          modify' $ \s -> s { stateCompiling = Set.insert name (stateCompiling s) }

          -- Find keyword definition
          case lookupKeyword name (stateRegistry state) of
            Nothing -> lift $ Left $ "Unknown keyword: " <> name
            Just keywordDef -> do
              -- Compile it
              compiled <- compileKeywordWithDef keywordDef name value

              -- Store in state
              modify' $ \s -> s
                { stateCompiled = addCompiledKeyword name compiled (stateCompiled s)
                , stateCompiling = Set.delete name (stateCompiling s)
                }

              pure (Just compiled)

-- Get a typed adjacent keyword value
getAdjacentData :: forall a. Typeable a => Text -> CompileM (Maybe a)
getAdjacentData name = do
  mCompiled <- compileAdjacent name
  pure $ mCompiled >>= extractTypedData
  where
    extractTypedData (CompiledKeyword _ (SomeCompiledData dat) _ _) = cast dat
```

### Example: Draft-04 Minimum

```haskell
-- Draft-04 exclusive minimum is a boolean
newtype ExclusiveMinimumData = ExclusiveMinimumData Bool
  deriving (Show, Eq, Typeable)

compileExclusiveMinimum :: Value -> CompileM ExclusiveMinimumData
compileExclusiveMinimum value = case value of
  Bool b -> pure $ ExclusiveMinimumData b
  _ -> lift $ Left "exclusiveMinimum must be a boolean (Draft-04)"

-- Draft-04 minimum checks for boolean exclusive
data MinimumData = MinimumData
  { minimumValue :: Scientific
  , minimumExclusive :: Bool
  } deriving (Show, Eq, Typeable)

compileMinimum :: Value -> CompileM MinimumData
compileMinimum value = case value of
  Number n -> do
    -- Lazily compile exclusiveMinimum if present
    mExclusive <- getAdjacentData @ExclusiveMinimumData "exclusiveMinimum"

    let exclusive = case mExclusive of
          Just (ExclusiveMinimumData b) -> b
          Nothing -> False

    pure $ MinimumData n exclusive
  _ -> lift $ Left "minimum must be a number"
```

### Example: unevaluatedProperties

```haskell
-- Tracks which properties were evaluated
data PropertiesData = PropertiesData
  { propertiesEvaluated :: Set Text
  , propertiesSchemas :: Map Text Schema
  } deriving (Show, Eq, Typeable)

compileUnevaluatedProperties :: Value -> CompileM UnevaluatedPropertiesData
compileUnevaluatedProperties schemaValue = do
  schema <- parseSchema schemaValue

  -- Compile adjacent keywords that evaluate properties
  mProperties <- getAdjacentData @PropertiesData "properties"
  mPatternProps <- getAdjacentData @PatternPropertiesData "patternProperties"
  mAdditionalProps <- getAdjacentData @AdditionalPropertiesData "additionalProperties"

  -- Collect all evaluated properties
  let evaluated = Set.unions
        [ maybe Set.empty propertiesEvaluated mProperties
        , maybe Set.empty patternPropertiesEvaluated mPatternProps
        , maybe Set.empty (const Set.empty) mAdditionalProps  -- additionalProperties evaluates all remaining
        ]

  pure $ UnevaluatedPropertiesData schema evaluated
```

## Migration Path

### Phase 1: Add Monadic Infrastructure
1. Define `CompileM` monad and `CompilationState`
2. Add `compileAdjacent` and `getAdjacentData` helpers
3. Update keyword compiler to use `CompileM`
4. Maintain backward compatibility by wrapping existing `CompileFunc`

### Phase 2: Migrate Simple Keywords
1. Update all 17 existing keywords to use `CompileM`
2. Most won't use adjacent access, just `pure` their result
3. No behavior changes, just type signature updates

### Phase 3: Implement Draft-04 Keywords
1. Implement Draft-04 `minimum`/`maximum` with boolean exclusive
2. Implement Draft-04 `exclusiveMinimum`/`exclusiveMaximum` as booleans
3. Test with Draft-04 test suite

### Phase 4: Enable Complex Keywords
1. Once recursive validation is added, use `compileAdjacent` for:
   - `unevaluatedProperties`/`unevaluatedItems`
   - Keywords that need to know about siblings
   - Composite validation patterns

## Benefits

1. **On-demand compilation**: Keywords compile only when needed
2. **Automatic memoization**: State monad caches compiled keywords
3. **Cycle detection**: Tracks compilation stack to detect circular deps
4. **Type-safe access**: `getAdjacentData` provides typed access to adjacent data
5. **Clean API**: No manual dependency ordering required
6. **Extensible**: Easy to add new keywords that depend on others

## Testing Strategy

1. **Unit tests**: Test `compileAdjacent` memoization and cycle detection
2. **Integration tests**: Draft-04 schemas with exclusive keywords
3. **Property tests**: Ensure compilation order doesn't matter (commutative)
4. **Performance**: Verify lazy compilation doesn't cause slowdowns

## Open Questions

1. **Schema access**: How to get keyword value from `Schema`?
   - Need to add accessor function or expose schema structure
   - May need different handling for different schema types (Object vs Boolean)

2. **Error handling**: How to report errors with context?
   - Include keyword path in errors
   - Show which keyword triggered compilation of which other

3. **Validation changes**: Should `ValidateFunc` also be monadic?
   - Probably yes, for recursive validation
   - Could share similar state monad pattern

## Related Issues

- `fractal-7da` - Implement Draft-04 minimum/maximum (unblocked by this)
- `fractal-tos` - Recursive validation support (similar monadic pattern needed)
- `fractal-4o1` - This issue (implementing monadic compilation)
