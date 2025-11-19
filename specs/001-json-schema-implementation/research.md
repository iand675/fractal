# Phase 0: Research & Technical Decisions

**Feature**: Comprehensive JSON Schema and OpenAPI Library  
**Date**: 2025-11-18  
**Status**: Complete

## Overview

This document captures research findings and technical decisions for implementing fractal-openapi, a comprehensive JSON Schema and OpenAPI 3.x library for Haskell.

## Research Areas

### 1. JSON Schema Specification Compliance

**Question**: Which JSON Schema versions should be supported and what are the key differences?

**Decision**: Support draft-04, draft-06, draft-07, 2019-09, and 2020-12

**Rationale**:
- **draft-04**: Still widely used in legacy systems, simpler keyword set
- **draft-06**: Added `const`, `contains`, property name validation
- **draft-07**: Added `if/then/else`, `readOnly/writeOnly`, comments
- **2019-09**: Major redesign with vocabulary system, `unevaluatedProperties/Items`, `dependentSchemas`
- **2020-12**: Latest stable, adds `prefixItems`, `$dynamicRef`, refinements

**Key Differences Table**:

| Feature | draft-04 | draft-06 | draft-07 | 2019-09 | 2020-12 |
|---------|----------|----------|----------|---------|---------|
| `$schema` required | No | No | No | Yes | Yes |
| `const` | No | Yes | Yes | Yes | Yes |
| `if/then/else` | No | No | Yes | Yes | Yes |
| `unevaluated*` | No | No | No | Yes | Yes |
| `$vocabulary` | No | No | No | Yes | Yes |
| `prefixItems` | No | No | No | No | Yes |
| `$dynamicRef` | No | No | No | No | Yes |
| `exclusiveMaximum` type | Boolean | Numeric | Numeric | Numeric | Numeric |
| `dependencies` | Object | Object | Object | Deprecated | Deprecated |
| `dependentSchemas` | No | No | No | Yes | Yes |

**Implementation Strategy**: 
- Unified AST with version-specific features encoded in Maybe/Either types
- Dialect system maps version URIs to feature sets
- Parser detects version from `$schema` keyword
- Validator switches behavior based on schema version

**Alternatives Considered**:
- Only support latest version → Rejected: breaks compatibility with existing schemas
- Separate AST per version → Rejected: massive duplication, harder to maintain
- Support draft-03 and earlier → Rejected: minimal adoption, significantly different semantics

---

### 2. Validation Engine Architecture

**Question**: What architecture provides best performance while maintaining correctness?

**Decision**: Compiled validation with validation context threading

**Rationale**:
- **Compiled approach**: Pre-process schema into optimized validator function
- **Context threading**: Pass ValidationContext through validation tree for reference resolution, evaluated tracking
- **Short-circuit option**: Configurable early exit on first error vs. collect all errors
- **Annotation collection**: Track annotations during validation for meta-information

**Performance Considerations**:
- Cache compiled validators for repeated use
- Use strict evaluation for validation results to avoid space leaks
- Lazy error collection allows short-circuiting without losing detail
- Reference resolution happens once during compilation

**Alternatives Considered**:
- Interpret schema on every validation → Rejected: 10-100x slower
- Generate Haskell code for each schema → Rejected: compilation overhead too high
- Pure recursive descent → Rejected: difficult to handle unevaluated tracking

---

### 3. Vocabulary System Design

**Question**: How should custom vocabularies integrate with the standard system?

**Decision**: Registry-based with keyword definition API

**Rationale**:
- **VocabularyRegistry**: Central registry maps URI → Vocabulary
- **KeywordDefinition**: Parser + validator + annotator for each keyword
- **Dialect composition**: Combine vocabularies with conflict detection
- **Plugin architecture**: Load vocabularies at runtime or compile-time

**Key Design Elements**:
- Vocabularies identified by unique URI
- Keywords have scope (any schema vs. specific types)
- Priority ordering ensures deterministic evaluation
- Existential types (`KeywordValue`) allow type-safe heterogeneous storage

**Implementation Pattern**:
```haskell
-- User defines keyword value type
data MyCreditCardKeyword = CreditCard { provider :: Text, luhnCheck :: Bool }
  deriving (Eq, Show, Typeable)

-- User provides parser, validator, annotator
creditCardKeywordDef :: KeywordDefinition
creditCardKeywordDef = KeywordDefinition
  { keywordName = "x-credit-card"
  , keywordParser = parseCreditCard  -- Value -> Either ParseError KeywordValue
  , keywordValidator = Just validateCreditCard  -- Type-safe validation
  , keywordAnnotator = Just annotateCreditCard
  , keywordPriority = 100
  }
```

**Alternatives Considered**:
- Hardcode all keywords → Rejected: not extensible
- Dynamic typing with Value → Rejected: loses type safety
- Type classes for keywords → Rejected: requires recompilation for new vocabularies

---

### 4. Template Haskell Code Generation Strategy

**Question**: How should code generation handle diverse schema patterns?

**Decision**: Pluggable strategy system with built-in strategies for common cases

**Rationale**:
- **Strategy pattern**: Different schema patterns need different generation approaches
- **Composability**: Strategies can delegate to sub-strategies for nested schemas
- **Configurability**: Users can customize via annotations or custom strategies
- **Type registry**: Track generated types to avoid duplicates and enable sharing

**Built-in Strategies**:

1. **Default Strategy** (records for objects):
   - Object schema → Haskell record
   - Required fields → direct fields
   - Optional fields → Maybe fields
   - Validation embedded in FromJSON

2. **Newtype Strategy**:
   - Primitive types with constraints → newtype with smart constructor
   - Triggered by `x-newtype` annotation
   - Generates validation function and safe constructor

3. **Sum Type Strategy**:
   - `oneOf`/`anyOf` → Haskell sum type
   - Tagged unions when discriminator present
   - Untagged unions with Alternative parsing

4. **Enum Strategy**:
   - `enum` with string values → Haskell ADT with no fields
   - Derives Eq, Ord, Show, Bounded, Enum

**Code Quality**:
- Generated code includes Haddock comments from schema description
- Uses language extensions only when necessary
- Respects GHC warnings (no orphans, complete patterns)

**Schema Reflection**:
- Generate `HasSchema` typeclass instance linking type back to originating schema
- Enables runtime introspection, dynamic validation, and tooling
- Schema embedded at compile time, accessible via type proxy

**Alternatives Considered**:
- Single monolithic generator → Rejected: unmaintainable for complex schemas
- External code generator tool → Rejected: loses type safety of TH
- Generic programming (GHC.Generics) → Rejected: runtime overhead, less precise types

---

### 5. Reference Resolution Mechanism

**Question**: How should `$ref`, `$dynamicRef`, and external references be handled?

**Decision**: SchemaRegistry with URI-based resolution and bundling support

**Rationale**:
- **SchemaRegistry**: Maps URIs to resolved schemas
- **Base URI tracking**: Resolve relative references correctly
- **Anchor support**: Named anchors (`$anchor`) for internal references
- **Dynamic scope**: Stack for `$dynamicRef` resolution
- **Bundling**: Resolve all external refs into single schema document

**Resolution Algorithm**:
1. Parse schema and extract `$id` to establish base URI
2. Register schema and anchors in registry
3. Resolve `$ref` by combining base URI + ref URI
4. Lookup in registry, load external if needed
5. For `$dynamicRef`, walk dynamic scope stack

**Circular Reference Handling**:
- Detect cycles during validation (track visited schemas)
- Allow recursive schemas but prevent infinite validation loops
- Use tying-the-knot for circular type generation

**Alternatives Considered**:
- Inline all references during parsing → Rejected: loses structure, explodes size
- Lazy resolution → Rejected: errors surface late, harder to debug
- Disallow external references → Rejected: breaks real-world use cases

---

### 6. Error Reporting Strategy

**Question**: What error representation provides best debugging experience?

**Decision**: Structured errors with JSON Pointer paths and context

**Rationale**:
- **JSON Pointer**: RFC 6901 standard for path representation
- **Dual paths**: Schema path (which rule failed) + instance path (where in data)
- **Structured errors**: Machine-readable for tooling, human-readable for developers
- **Error context**: Include actual value, expected constraints, keyword that failed

**Error Format**:
```haskell
data ValidationError = ValidationError
  { errorKeyword :: Text              -- e.g., "minimum"
  , errorSchemaPath :: JSONPointer    -- e.g., "/properties/age/minimum"
  , errorInstancePath :: JSONPointer  -- e.g., "/age"
  , errorMessage :: Text              -- Human-readable explanation
  , errorDetails :: Maybe Value       -- Additional context (actual value, etc.)
  }
```

**Error Aggregation**:
- ValidationErrors is NonEmpty list (at least one error)
- Collect all errors in branch for anyOf/oneOf
- Short-circuit option stops at first error (faster for validation-only)

**Alternatives Considered**:
- String error messages → Rejected: not structured, hard to parse
- Exception-based → Rejected: not functional, loses composability
- Line/column numbers → Rejected: JSON has no canonical format

---

### 7. Performance Optimization Techniques

**Question**: What optimizations ensure high-performance validation?

**Decision**: Multiple complementary optimization strategies

**Rationale & Techniques**:

1. **Validator Compilation**:
   - Pre-process schema into optimized validator function
   - Inline simple checks, avoid repeated parsing
   - Cache compiled validators with schema fingerprint

2. **Strict Evaluation**:
   - Use `BangPatterns` for validation results
   - Avoid thunk accumulation in error lists
   - Strict fields in ValidationContext

3. **Short-Circuit Semantics**:
   - `allOf`: stop on first failure
   - `anyOf`: stop on first success
   - Configurable full error collection

4. **Efficient Data Structures**:
   - `HashMap` for property lookup (O(1))
   - `Set` for required fields checking
   - `IntMap` for evaluated items tracking

5. **Regex Compilation**:
   - Compile regex patterns once during schema parsing
   - Cache in Schema AST
   - Use regex-tdfa for performance

6. **Ref Resolution Caching**:
   - Resolve `$ref` once, store in registry
   - Reuse resolved schemas
   - No repeated URI parsing

**Benchmarking Strategy**:
- Criterion benchmarks for common schema patterns
- Compare against other implementations (Aeson, JSON Schema validators)
- Profile with GHC profiler for hotspots

**Alternatives Considered**:
- Lazy evaluation throughout → Rejected: space leaks
- Parallel validation → Rejected: overhead exceeds benefits for typical schemas
- Native code generation → Rejected: compilation time too high

---

### 8. Testing Strategy

**Question**: How to ensure comprehensive correctness and compliance?

**Decision**: Multi-layered testing with official test suite integration

**Rationale & Approach**:

1. **JSON Schema Test Suite** (MANDATORY):
   - Official test suite from json-schema-org
   - 1000+ test cases covering all versions
   - 100% pass rate required for mandatory tests
   - Integration via HSpec

2. **Property-Based Tests** (Hedgehog):
   - Parse/render roundtrip: `parseSchema . renderSchema ≡ id`
   - Validation monotonicity: more restrictive → fewer valid values
   - Generated code roundtrip: `decode . encode ≡ Right`
   - Vocabulary composition associativity

3. **Contract Tests**:
   - Public API surface remains stable
   - Error types and messages follow spec
   - Generated code matches expected patterns

4. **Integration Tests**:
   - Real-world schemas (OpenAPI specs from popular APIs)
   - Framework integration (Servant, Yesod)
   - Cross-version compatibility

5. **Example-Based Unit Tests**:
   - Edge cases and regression tests
   - Specific version features
   - Error message clarity

**Test Organization**:
```
test/
├── Spec.hs                          # hspec-discover entry
├── Fractal/JsonSchema/
│   ├── ParserSpec.hs               # Parser unit tests
│   ├── ValidatorSpec.hs            # Validator tests
│   ├── ComplianceSpec.hs           # JSON Schema Test Suite integration
│   └── Properties/
│       ├── RoundtripSpec.hs       # Property tests
│       └── MonotonicitySpec.hs
```

**Alternatives Considered**:
- Example tests only → Rejected: insufficient coverage
- Manual test cases → Rejected: reinvents official test suite
- Skip property tests → Rejected: misses emergent bugs

---

### 9. OpenAPI Integration Points

**Question**: How should OpenAPI support layer on top of JSON Schema?

**Decision**: Separate module hierarchy with shared Schema types

**Rationale**:
- OpenAPI 3.1 uses JSON Schema 2020-12 (mostly compatible)
- OpenAPI 3.0 uses JSON Schema draft-04/draft-05 hybrid
- Share Schema AST, differ in validation and extensions

**Architecture**:
```
Fractal.OpenApi
├── Types           -- OpenAPI-specific types (PathItem, Operation, Components)
├── Parser          -- Parse OpenAPI YAML/JSON
├── Validator       -- Validate OpenAPI specs
└── Codegen         -- Generate API types (Servant, Yesod)
    ├── Servant     -- Servant API generation
    └── Yesod       -- Yesod route generation
```

**Key Differences**:
- OpenAPI has `discriminator` for polymorphism
- OpenAPI allows `example` vs. JSON Schema `examples`
- OpenAPI 3.0 doesn't support all JSON Schema features
- OpenAPI has request/response validation context

**Alternatives Considered**:
- Completely separate Schema implementation → Rejected: massive duplication
- Ignore OpenAPI differences → Rejected: breaks compatibility
- Only support OpenAPI 3.1 → Rejected: 3.0 still widely used

---

### 10. Dependency Management

**Question**: What dependencies provide best balance of functionality and maintainability?

**Decision**: Minimal, well-established dependencies

**Rationale**:

**Core Dependencies** (required):
- `aeson >= 2.0`: JSON parsing/rendering, well-maintained, ubiquitous
- `text >= 2.0`: UTF-8 text, standard in Haskell ecosystem
- `bytestring >= 0.11`: Binary data, efficient I/O
- `containers >= 0.6`: Standard collections (Map, Set, Seq)
- `unordered-containers >= 0.2`: HashMap, HashSet for O(1) lookup
- `vector >= 0.13`: Efficient arrays
- `scientific >= 0.3`: Arbitrary-precision numbers (JSON numbers)
- `hashable >= 1.4`: Hashing support

**Parsing Dependencies**:
- `yaml >= 0.11`: YAML support (OpenAPI specs commonly in YAML)
- `uri-bytestring >= 0.3`: URI parsing and manipulation
- `regex-tdfa >= 1.3`: POSIX regex (pattern validation)

**Code Generation Dependencies**:
- `template-haskell >= 2.21`: TH support (ships with GHC 9.8)
- `th-lift >= 0.8`: Lift instances for common types

**Optional Dependencies** (framework integration):
- `servant >= 0.20`: Servant API type generation
- `servant-server >= 0.20`: Server-side Servant support
- `yesod-core >= 1.6`: Yesod route generation

**Testing Dependencies**:
- `hspec >= 2.11`: Testing framework
- `hspec-discover >= 2.11`: Automatic test discovery
- `hedgehog >= 1.4`: Property-based testing
- `QuickCheck >= 2.14`: Alternative property testing (if Hedgehog insufficient)

**Avoided Dependencies**:
- `lens`: Too heavy, use record updates or simple helpers
- `megaparsec`/`attoparsec`: aeson already provides JSON parsing
- Custom effect systems: Use mtl-style or plain IO

**Alternatives Considered**:
- lens for nested updates → Rejected: heavy dependency, can use manual traversals
- Custom JSON parser → Rejected: aeson is battle-tested
- streaming libraries → Rejected: not needed for typical schema sizes

---

## Summary of Technical Decisions

| Area | Decision | Key Rationale |
|------|----------|---------------|
| **Schema Versions** | Support draft-04 through 2020-12 | Real-world compatibility + latest features |
| **Validation** | Compiled validators with context threading | Performance + correctness |
| **Vocabularies** | Registry with keyword definition API | Extensibility + type safety |
| **Code Generation** | Pluggable strategy system with TH | Flexibility + type-safe generation |
| **References** | SchemaRegistry with URI resolution | Standard compliance + bundling support |
| **Errors** | Structured errors with JSON Pointer paths | Developer experience + tooling support |
| **Performance** | Multiple optimizations (compilation, strict eval, caching) | 1000+ schemas/sec target |
| **Testing** | JSON Schema Test Suite + property tests | Compliance + correctness guarantees |
| **OpenAPI** | Shared Schema AST, separate types | Code reuse + proper abstraction |
| **Dependencies** | Minimal, established libraries | Stability + maintainability |

---

## Risks and Mitigations

### Risk: JSON Schema Test Suite failures
- **Mitigation**: Integrate test suite early, track pass rate as development progresses
- **Fallback**: Document known deviations, provide workarounds

### Risk: Performance targets not met
- **Mitigation**: Benchmark continuously, profile early, optimize hotspots
- **Fallback**: Document performance characteristics, provide optimization guide

### Risk: Template Haskell complexity
- **Mitigation**: Start simple, add complexity incrementally, comprehensive testing
- **Fallback**: Provide runtime code generation API as alternative

### Risk: Version compatibility issues
- **Mitigation**: Clear version detection, explicit error messages for unsupported features
- **Fallback**: Support subset of features across all versions

---

## Next Steps

Phase 0 complete. Proceed to Phase 1:
1. Design data model (types for Schema, Vocabulary, Validation, etc.)
2. Define internal contracts (validation pipeline, vocabulary system interfaces)
3. Create quickstart guide for library users

All technical questions resolved. Implementation can proceed with confidence.

