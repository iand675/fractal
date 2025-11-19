# Vocabulary System Contract

**Module**: `Fractal.JsonSchema.Vocabulary`  
**Purpose**: Extensible vocabulary system for custom keywords

## Public API

### Vocabulary Definition

```haskell
-- | Create a new vocabulary
createVocabulary
  :: URI                              -- Vocabulary URI
  -> Bool                             -- Required?
  -> [(Text, KeywordDefinition)]     -- Keywords
  -> Maybe (Schema -> ValidationContext -> Either ValidationError ())
  -> Maybe Schema                     -- Meta-schema
  -> Vocabulary

-- | Define a keyword
defineKeyword
  :: Text                             -- Keyword name
  -> KeywordScope                     -- Scope
  -> (Value -> Either ParseError KeywordValue)  -- Parser
  -> Maybe KeywordValidator           -- Validator
  -> Maybe KeywordAnnotator           -- Annotator
  -> Int                               -- Priority
  -> KeywordDefinition
```

### Registry Operations

```haskell
-- | Empty registry with standard dialects
emptyRegistry :: VocabularyRegistry

-- | Standard registry (all standard vocabularies + dialects registered)
standardRegistry :: VocabularyRegistry

-- | Register a custom vocabulary
registerVocabulary :: Vocabulary -> VocabularyRegistry -> VocabularyRegistry

-- | Register a custom dialect
registerDialect :: Dialect -> VocabularyRegistry -> VocabularyRegistry

-- | Lookup vocabulary by URI
lookupVocabulary :: URI -> VocabularyRegistry -> Maybe Vocabulary

-- | Lookup dialect by URI
lookupDialect :: URI -> VocabularyRegistry -> Maybe Dialect

-- | Compose vocabularies into a new dialect
composeDialect
  :: Text                             -- Dialect name
  -> URI                              -- Dialect URI
  -> [(URI, Bool)]                    -- (Vocabulary URI, required?)
  -> VocabularyRegistry
  -> Either ComposeError Dialect
```

### Standard Vocabularies

```haskell
-- Standard JSON Schema vocabularies (pre-registered)
coreVocabulary          :: Vocabulary  -- $schema, $id, $ref, etc.
applicatorVocabulary    :: Vocabulary  -- properties, items, etc.
validationVocabulary    :: Vocabulary  -- type, minimum, etc.
unevaluatedVocabulary   :: Vocabulary  -- unevaluatedProperties/Items
formatAnnotationVocab   :: Vocabulary  -- format (annotation)
formatAssertionVocab    :: Vocabulary  -- format (assertion)
metadataVocabulary      :: Vocabulary  -- title, description, etc.
contentVocabulary       :: Vocabulary  -- contentEncoding, etc.
```

### Standard Dialects

```haskell
draft04Dialect   :: Dialect
draft06Dialect   :: Dialect
draft07Dialect   :: Dialect
draft201909Dialect :: Dialect
draft202012Dialect :: Dialect
```

## Contracts

### Vocabulary Registration

**Pre-conditions**:
- Vocabulary URI is absolute and unique
- All keyword names are valid JSON object keys
- No duplicate keyword names within vocabulary

**Post-conditions**:
- Vocabulary accessible via `lookupVocabulary`
- Keywords available for parsing and validation
- Vocabulary can be used in dialect composition

**Invariants**:
- Vocabulary URIs are unique within registry
- Keyword names unique within vocabulary
- Required vocabularies must be understood (registered)

### Keyword Definition

**Pre-conditions**:
- Keyword name is valid JSON object key
- Parser function is pure and total (handles all Value types)
- Validator (if present) is pure
- Annotator (if present) is pure

**Post-conditions**:
- Keyword recognized during schema parsing
- Parser extracts keyword value from JSON
- Validator applies constraints to instance values
- Annotator produces annotations

**Invariants**:
- Parser must return `Left ParseError` for invalid values
- Validator must be pure (no I/O, no exceptions)
- Priority determines evaluation order (higher = earlier)
- Keyword scope enforced (e.g., ObjectSchemaOnly)

### Dialect Composition

**Pre-conditions**:
- All referenced vocabularies exist in registry OR are optional
- Dialect URI is unique
- No keyword conflicts between vocabularies (or conflict resolution strategy)

**Post-conditions**:
- Dialect registered and accessible
- Meta-schema available for validation
- Vocabularies ordered by priority

**Invariants**:
- Required vocabularies must be registered before use
- Optional vocabularies gracefully ignored if missing
- Keyword conflicts detected and reported
- Dialect URI unique within registry

### Keyword Evaluation Order

**Contracts**:
- Keywords evaluated by priority (highest first)
- Within same priority, evaluation order is deterministic but unspecified
- Standard keyword priorities:
  - Core keywords (100): `$ref`, `$dynamicRef`
  - Composition keywords (90): `allOf`, `anyOf`, `oneOf`, `not`, `if/then/else`
  - Type keywords (80): `type`, `enum`, `const`
  - Validation keywords (70): type-specific constraints
  - Applicator keywords (60): `properties`, `items`, etc.
  - Unevaluated keywords (50): `unevaluatedProperties/Items`
  - Annotation keywords (40): `title`, `description`, etc.

**Invariants**:
- Ref keywords evaluated before other validation
- Composition keywords evaluated before type-specific
- Unevaluated keywords evaluated last
- Priorities form total ordering

### KeywordValue Type Safety

**Contracts**:
- `KeywordValue` wraps typed values with Typeable constraint
- Type-safe extraction via pattern matching
- Eq instance allows comparing keyword values
- Show instance for debugging

**Example**:
```haskell
-- Define a custom keyword value type
data CreditCardSpec = CreditCard { provider :: Text, luhnCheck :: Bool }
  deriving (Eq, Show, Typeable)

-- Parser produces KeywordValue
parseCreditCard :: Value -> Either ParseError KeywordValue
parseCreditCard v = ... pure (KeywordValue creditCard)

-- Validator extracts typed value
validateCreditCard :: KeywordValidator
validateCreditCard (KeywordValue (cc :: CreditCardSpec)) value ctx = ...
```

**Invariants**:
- KeywordValue preserves type information via Typeable
- Eq instance requires matching types
- Extraction fails gracefully for type mismatches

## Properties

### Vocabulary Composition

```haskell
-- Property: Composing non-conflicting vocabularies succeeds
property_compositionSuccess :: [Vocabulary] -> Property
property_compositionSuccess vocabs =
  noKeywordConflicts vocabs ==>
    isRight (composeDialect "test" testURI (map vocabToEntry vocabs) stdRegistry)

-- Property: Required vocabulary missing → error
property_requiredVocabMissing :: URI -> Property
property_requiredVocabMissing uri =
  not (uri `elem` registeredURIs stdRegistry) ==>
    isLeft (composeDialect "test" testURI [(uri, True)] stdRegistry)
```

### Keyword Priority

```haskell
-- Property: Higher priority keywords evaluated first
property_priorityOrdering :: Vocabulary -> Property
property_priorityOrdering vocab =
  let keywords = Map.elems (vocabularyKeywords vocab)
      priorities = map keywordPriority keywords
  in sort priorities == reverse (sort priorities)  -- High to low
```

### Type Safety

```haskell
-- Property: KeywordValue round-trips types correctly
property_keywordValueTypeSafe :: (Eq a, Show a, Typeable a) => a -> Property
property_keywordValueTypeSafe val =
  let kv = KeywordValue val
  in case kv of
      KeywordValue (extracted :: a) -> extracted === val
```

## Error Conditions

| Condition | Error |
|-----------|-------|
| Unknown vocabulary (required) | `UnknownVocabularyError "URI not registered"` |
| Keyword conflict | `KeywordConflictError "keyword in multiple vocabs"` |
| Invalid keyword scope | `InvalidScopeError "keyword not applicable"` |
| Parse error in keyword value | `KeywordParseError "invalid value"` |
| Duplicate vocabulary URI | `DuplicateVocabularyError "URI already registered"` |
| Circular vocabulary dependencies | `CircularDependencyError "cycle detected"` |

## Standard Vocabulary Coverage

### Core Vocabulary (2020-12)
- `$schema`, `$id`, `$ref`, `$anchor`, `$dynamicRef`, `$dynamicAnchor`
- `$vocabulary`, `$comment`, `$defs`

### Applicator Vocabulary (2020-12)
- `prefixItems`, `items`, `contains`, `additionalProperties`
- `properties`, `patternProperties`, `dependentSchemas`
- `propertyNames`, `if`, `then`, `else`
- `allOf`, `anyOf`, `oneOf`, `not`

### Validation Vocabulary (2020-12)
- Numeric: `type`, `enum`, `const`, `multipleOf`, `maximum`, `exclusiveMaximum`, `minimum`, `exclusiveMinimum`
- String: `maxLength`, `minLength`, `pattern`
- Array: `maxItems`, `minItems`, `uniqueItems`, `maxContains`, `minContains`
- Object: `maxProperties`, `minProperties`, `required`, `dependentRequired`

### Unevaluated Vocabulary (2020-12)
- `unevaluatedItems`, `unevaluatedProperties`

### Format Vocabularies (2020-12)
- Format Annotation: `format` (annotation only)
- Format Assertion: `format` (validation)

### Meta-Data Vocabulary (2020-12)
- `title`, `description`, `default`, `deprecated`, `readOnly`, `writeOnly`, `examples`

### Content Vocabulary (2020-12)
- `contentEncoding`, `contentMediaType`, `contentSchema`

## Extension Example

```haskell
-- Custom business vocabulary
businessVocabulary :: Vocabulary
businessVocabulary = createVocabulary
  "https://example.com/vocabs/business/v1"
  False  -- Optional
  [ ("x-credit-card", creditCardKeywordDef)
  , ("x-tax-id", taxIDKeywordDef)
  , ("x-currency", currencyKeywordDef)
  ]
  Nothing  -- No vocabulary-level validator
  Nothing  -- No meta-schema

-- Use in custom dialect
myDialect :: Dialect
myDialect = composeDialect
  "My Custom Dialect"
  "https://example.com/schemas/2024"
  [ ("https://json-schema.org/draft/2020-12/vocab/core", True)
  , ("https://json-schema.org/draft/2020-12/vocab/applicator", True)
  , ("https://json-schema.org/draft/2020-12/vocab/validation", True)
  , ("https://example.com/vocabs/business/v1", True)  -- Custom
  ]
  standardRegistry
```

## Performance Guarantees

- Vocabulary lookup O(1) via URI HashMap
- Keyword lookup O(1) within vocabulary
- Priority-based evaluation deterministic
- No dynamic dispatch overhead (compiled validators)

