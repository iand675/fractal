# Implementation Plan: Comprehensive JSON Schema and OpenAPI Library

**Branch**: `001-json-schema-implementation` | **Date**: 2025-11-18 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-json-schema-implementation/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Implement `fractal-openapi`, a comprehensive Haskell library for JSON Schema (draft-04 through 2020-12) and OpenAPI 3.x support. The library provides type-safe schema parsing, high-performance validation with detailed error reporting, an extensible vocabulary system for custom keywords, and Template Haskell-based code generation with pluggable strategies. Integration with Servant and Yesod enables schema-first API development. The design emphasizes type-driven development with domain types preventing invalid states, functional purity with effects tracked in types, and composability through standard abstractions (Functor, Applicative, Monad).

## Technical Context

**Language/Version**: Haskell with GHC >= 9.8 (base >= 4.19.0.0)  
**Primary Dependencies**: 
- Core: aeson (>= 2.0), text (>= 2.0), bytestring (>= 0.11), containers, unordered-containers, vector, scientific, hashable
- Parsing: yaml (>= 0.11), uri-bytestring (>= 0.3)
- Validation: regex-tdfa (>= 1.3)
- Code generation: template-haskell (>= 2.19), th-lift (>= 0.8)
- Optional framework integration: servant (>= 0.19), yesod-core (>= 1.6)  
**Storage**: N/A (pure library, schemas loaded from files/network)  
**Testing**: HSpec with hspec-discover for test discovery, Hedgehog for property-based testing, JSON Schema Test Suite for compliance  
**Target Platform**: Cross-platform (Linux, macOS, Windows) - library targets GHC native compilation  
**Project Type**: Single library package within Fractal monorepo  
**Performance Goals**: 
- Validate 1000+ schemas/second on single core
- Parse/validate 10MB OpenAPI specs within 5 seconds
- Generated code compilation overhead < 10% vs hand-written code  
**Constraints**: 
- Memory usage < 100MB for schemas up to 1MB
- Zero runtime dependencies beyond Haskell base libraries
- Exception-safe resource management for I/O operations  
**Scale/Scope**: 
- Support 5 JSON Schema versions (draft-04, 06, 07, 2019-09, 2020-12)
- ~10K LOC for core implementation
- ~8 major modules (JsonSchema, Validator, Vocabulary, Codegen, OpenApi, etc.)
- Comprehensive test suite with 100% JSON Schema Test Suite compliance

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Verify compliance with `.specify/memory/constitution.md`:

- [x] **Type-Driven Development**: Domain types defined first? Invalid states unrepresentable?
  - ✓ Schema AST types encode version-specific features in types
  - ✓ ValidationResult is Either-like (success with annotations OR errors, never both)
  - ✓ KeywordValue uses existential types with Typeable constraint for type-safe keyword values
  - ✓ ReferenceOr pattern prevents mixing references with inline definitions
  - ✓ Extensive use of NonEmpty, Set, Map to encode non-empty collections and uniqueness invariants

- [x] **Library-First Architecture**: Feature scoped as standalone library? Clear purpose? Self-contained?
  - ✓ fractal-openapi is standalone package in Fractal monorepo
  - ✓ Clear purpose: JSON Schema validation + OpenAPI support + code generation
  - ✓ Self-contained with explicit dependencies (no fractal-* dependencies required)
  - ✓ Optional framework integrations (Servant, Yesod) via separate modules

- [x] **Functional Purity**: Side effects tracked in types? Pure functions default?
  - ✓ Parsing, validation, and rendering are pure functions
  - ✓ I/O effects (loading schemas from files/URLs) explicitly in IO type
  - ✓ Template Haskell code generation in Q monad (effect tracked)
  - ✓ Vocabulary validators are pure: Schema -> Value -> ValidationContext -> ValidationResult

- [x] **Property-Based Testing**: Test strategy includes properties for domain invariants?
  - ✓ Property: parseSchema . renderSchema ≡ id (roundtrip)
  - ✓ Property: valid schema validates against its meta-schema
  - ✓ Property: validation monotonicity (more restrictive schema → subset of valid values)
  - ✓ Property: generated code compiles and roundtrips JSON correctly
  - ✓ JSON Schema Test Suite provides comprehensive property-like coverage

- [x] **Composability**: Design uses appropriate abstractions? Provides combinators?
  - ✓ Vocabulary composition via Semigroup/Monoid
  - ✓ ValidationResult as Alternative for combining validation outcomes
  - ✓ Codegen strategies composable via strategy combinators
  - ✓ Schema combinators for allOf/anyOf/oneOf composition
  - ✓ Functor/Applicative/Monad instances where semantically appropriate

**Deviations**: None. All principles satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/[###-feature]/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
fractal-openapi/
├── fractal-openapi.cabal         # Package definition with dependencies
├── README.md                      # User-facing documentation
├── SPEC.md                        # Technical specification (design doc)
├── src/
│   └── Fractal/
│       ├── OpenApi.hs            # Top-level module re-exports
│       ├── OpenApi/
│       │   ├── Types.hs          # OpenAPI 3.x types
│       │   ├── Parser.hs         # OpenAPI spec parsing
│       │   ├── Renderer.hs       # OpenAPI spec rendering
│       │   ├── Validator.hs      # OpenAPI spec validation
│       │   └── Codegen.hs        # OpenAPI code generation entry
│       └── JsonSchema/
│           ├── Types.hs          # Core Schema AST (Schema, SchemaObject, etc.)
│           ├── Parser.hs         # JSON Schema parsing with version detection
│           ├── Renderer.hs       # Schema rendering (JSON/YAML)
│           ├── Validator.hs      # Validation engine with detailed errors
│           ├── Metadata.hs       # Metadata extraction and introspection
│           ├── Dialect.hs        # Dialect definitions (draft-04 through 2020-12)
│           └── Vocabulary.hs     # Vocabulary system (registry, composition)
│           └── Codegen/
│               ├── Core.hs       # Codegen infrastructure (Context, Config, Registry)
│               ├── Strategy.hs   # Strategy system (pluggable type generation)
│               ├── TH.hs         # Template Haskell API (deriveJSONSchema, etc.)
│               └── Aeson.hs      # Aeson instance generation
├── test/
│   ├── Spec.hs                   # hspec-discover entry point
│   └── Fractal/
│       ├── OpenApi/
│       │   ├── ParserSpec.hs
│       │   ├── ValidatorSpec.hs
│       │   └── Codegen/
│       │       └── THSpec.hs
│       └── JsonSchema/
│           ├── ParserSpec.hs
│           ├── ValidatorSpec.hs      # Core validation tests
│           ├── VocabularySpec.hs     # Custom vocabulary tests
│           ├── DialectSpec.hs        # Multi-version support tests
│           └── Codegen/
│               ├── StrategySpec.hs
│               └── AesonSpec.hs
├── test-suite/                    # JSON Schema Test Suite integration
│   └── json-schema-test-suite/   # Git submodule or downloaded tests
├── examples/
│   ├── BasicValidation.hs        # Simple validation examples
│   ├── CustomVocabulary.hs       # Custom vocabulary example
│   ├── CodegenExample.hs         # Template Haskell code generation
│   └── ServantIntegration.hs     # Servant API generation example
└── benchmarks/
    ├── ValidationBench.hs        # Validation performance benchmarks
    └── ParsingBench.hs           # Parsing performance benchmarks
```

**Structure Decision**: Single library package (`fractal-openapi`) within the Fractal monorepo. The library is organized by concern with clear module boundaries:

- **JsonSchema/**: Core JSON Schema functionality (types, parsing, validation, vocabularies)
- **JsonSchema/Codegen/**: Code generation infrastructure and strategies
- **OpenApi/**: OpenAPI 3.x specific functionality built on top of JsonSchema
- **test/**: Mirrors src/ structure with Spec suffix
- **test-suite/**: Official JSON Schema Test Suite for compliance validation
- **examples/**: Runnable examples demonstrating key features
- **benchmarks/**: Performance benchmarks using criterion

This structure follows the Fractal pattern of independent packages with `fractal-` prefix, allowing users to depend only on what they need.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| [e.g., 4th project] | [current need] | [why 3 projects insufficient] |
| [e.g., Repository pattern] | [specific problem] | [why direct DB access insufficient] |
