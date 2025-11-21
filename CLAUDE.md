# Fractal Project - AI Assistant Guide

**Note**: This project uses [bd (beads)](https://github.com/steveyegge/beads) for issue tracking. Use `bd` commands instead of markdown TODOs. See AGENTS.md for workflow details.

This document provides context for AI assistants working on the Fractal project.

## Project Overview

Fractal is a multi-package Haskell project focused on composable abstractions for resource management, dependency injection, event streaming, and schema management. The project is organized as a monorepo with three independent but related packages.

**Governance**: This project follows the principles defined in `.specify/memory/constitution.md`. All development decisions must align with:
- Type-Driven Development (domain types first, invalid states unrepresentable)
- Library-First Architecture (standalone, composable packages)
- Functional Purity & Immutability (side effects tracked in types)
- Property-Based Testing (Hedgehog for domain invariants)
- Composability Through Abstraction (Arrow, Monad, Category, etc.)

Refer to the constitution for detailed requirements and rationale.

## Package Structure

The project consists of three subpackages:

### 1. **fractal-layer** (`fractal-layer/`)
**Purpose**: Composable resource management and dependency injection

**Key modules**:
- `Fractal.Layer` - Core Layer abstraction with Arrow, Monad, and Alternative instances
- `Fractal.Layer.Diagnostics` - Debugging and introspection tools
- `Fractal.Layer.Interceptor` - Interceptor pattern for layer operations

**Key concepts**:
- Layers provide composable resource management with automatic cleanup
- Support for vertical composition (`>>>`) and horizontal composition (`&&&`)
- Service caching for expensive initializations
- Type-safe dependency injection via `MonadReader`
- Exception-safe resource handling

**Test coverage**: Comprehensive test suite including:
- Basic construction and composition tests
- Typeclass law verification (Functor, Applicative, Monad, Arrow, Category)
- Resource management and cleanup order tests
- Concurrent composition tests
- Service initialization and caching tests

### 2. **fractal-schema** (`fractal-schema/`)
**Purpose**: Schema registry with Avro support and compatibility checking

**Key modules**:
- `Fractal.Schema.Registry` - HTTP API for schema registry (Servant-based)
- `Fractal.Schema.Types` - Core schema types and API definitions
- `Fractal.Schema.Backend.Class` - Storage backend interface
- `Fractal.Schema.Backend.PostgreSQL` - PostgreSQL storage implementation
- `Fractal.Schema.Compatibility.Avro` - Avro schema compatibility checking
- `Fractal.Schema.Client` - HTTP client for schema registry

**Key concepts**:
- Schema versioning and evolution
- Compatibility level checking (backward, forward, full, none)
- Subject-based schema organization
- PostgreSQL backend for persistence

**Dependencies**: Independent package, can be used without fractal-stream

### 3. **fractal-stream** (`fractal-stream/`)
**Purpose**: Event streaming with Redis/Kinesis backends

**Key modules**:
- `Fractal.Stream` - Main streaming interface
- `Fractal.Stream.Types` - Core types (EventEnvelope, StreamConfig, etc.)
- `Fractal.Stream.Class` - MonadStream typeclass
- `Fractal.Stream.Backend.Redis` - Redis backend implementation
- `Fractal.Stream.Outbox` - Outbox pattern for exactly-once delivery

**Key concepts**:
- Event sourcing patterns
- Multiple backend support (Redis for dev, Kinesis for production)
- Exactly-once delivery via outbox pattern
- Consumer groups and offset management
- Event replay capabilities

**Dependencies**: Depends on both `fractal-layer` and `fractal-schema`

**Test status**: Placeholder test suite - needs Stream-specific tests

### 4. **fractal-openapi** (`fractal-openapi/`)
**Purpose**: JSON Schema and OpenAPI 3.x validation and code generation

**Key modules**:
- `Fractal.JsonSchema` - Core JSON Schema validation engine (draft-04 through 2020-12)
- `Fractal.JsonSchema.Validator` - Schema validation with annotations
- `Fractal.JsonSchema.Parser` - JSON Schema parsing
- `Fractal.JsonSchema.Codegen` - Code generation from schemas
- `Fractal.OpenApi` - OpenAPI 3.x support

**Key concepts**:
- Full JSON Schema specification compliance
- Support for multiple schema drafts
- Reference resolution ($ref, $dynamicRef)
- Format validators (URI, email, date-time, hostname, IDN hostname, etc.)
- Annotation collection for unevaluated properties/items

**Dependencies**: Depends on `ecma262-regex` and `idn` for format validation

### 5. **ecma262-regex** (`ecma262-regex/`)
**Purpose**: ECMAScript 262 (JavaScript) regular expressions for Haskell

**Key modules**:
- `Text.Regex.ECMA262` - High-level API for ECMA-262 regex
- `Text.Regex.ECMA262.Internal` - Low-level FFI bindings to libregexp

**Key concepts**:
- Full ECMAScript 2023 regex specification compliance
- Unicode mode support with proper UTF-16 handling
- Named capture groups, lookahead/lookbehind
- Uses QuickJS's libregexp (bundled C sources)

**Dependencies**: No external dependencies (C code bundled in cbits/)

### 6. **fractal-idna2008** (`idn/`)
**Purpose**: Pure Haskell IDNA2008 (Internationalized Domain Names)

**Key concepts**:
- ToASCII and ToUnicode conversion (Punycode encoding/decoding)
- IDNA2008 validation (RFC 5890-5894)
- Unicode normalization (NFC)
- Contextual rules (CONTEXTJ, CONTEXTO)
- BiDi validation for RTL domains

## System Dependencies

Some packages in this project require external C libraries to be installed:

### ecma262-regex
**Required**: None (bundled)

This package includes QuickJS's libregexp as bundled C sources in `cbits/`, so no external dependencies are needed.

## Build System

### Stack Multi-Package Project

The project uses Stack with multiple packages. The `stack.yaml` file references all packages:

```yaml
packages:
  - fractal-layer
  - fractal-schema
  - fractal-stream
  - fractal-openapi
  - ecma262-regex
  - idn
```

### Building

```bash
# Build all packages
stack build

# Build specific package
stack build fractal-layer
stack build fractal-openapi
stack build idn

# Run tests
stack test
stack test fractal-openapi
```

Practially speaking, you can typically go straight to running stack test since it will show you build failures too.

### Common Configuration

All packages share common settings:
- **GHC warnings**: Comprehensive warning flags enabled
- **Default extensions**: OverloadedStrings, RecordWildCards, DeriveGeneric, etc.
- **Cabal version**: 3.0+
- **Base GHC version**: >= 4.17.0.0 (GHC 9.4+)

## Directory Structure

```
fractal/
├── cabal.project                 # Multi-package configuration
├── stack.yaml                    # Stack configuration
├── devenv.nix                    # devenv configuration with system dependencies
├── README.md                     # User-facing documentation
├── CLAUDE.md                     # This file (AI assistant guide)
│
├── fractal-layer/                # Layer abstraction package
│   ├── fractal-layer.cabal
│   ├── src/Fractal/
│   │   └── Layer/
│   ├── test/
│   │   ├── Spec.hs              # hspec-discover entry point
│   │   └── Fractal/Layer/
│   │       ├── LayerSpec.hs     # Core Layer tests
│   │       └── DiagnosticsSpec.hs
│   └── examples/
│
├── fractal-schema/               # Schema registry package
│   ├── fractal-schema.cabal
│   ├── src/Fractal/Schema/
│   │   ├── Backend/
│   │   └── Compatibility/
│   ├── test/
│   │   ├── Spec.hs
│   │   └── Fractal/Schema/
│   └── examples/
│
├── fractal-stream/               # Event streaming package
│   ├── fractal-stream.cabal
│   ├── src/Fractal/
│   │   ├── Stream/
│   │   └── Yesod/               # Yesod integration utilities
│   ├── test/
│   │   └── Spec.hs              # Placeholder - needs real tests
│   └── examples/
│       └── redis/
│
├── fractal-openapi/              # JSON Schema & OpenAPI package
│   ├── fractal-openapi.cabal
│   ├── src/Fractal/
│   │   ├── JsonSchema/
│   │   └── OpenApi/
│   ├── test/
│   │   └── Fractal/
│   └── test-suite/              # Vendored JSON Schema test suite
│
├── ecma262-regex/                # ECMAScript regex package
│   ├── ecma262-regex.cabal
│   ├── cbits/                   # Bundled QuickJS libregexp sources
│   ├── src/Text/Regex/ECMA262/
│   ├── test/
│   └── examples/
│
└── fractal-idna2008/             # IDNA2008 bindings package
    ├── fractal-idna2008.cabal
    ├── shell.nix                # Standalone nix shell with libidn2
    ├── src/Text/IDNA2008/
    └── test/
```

## Testing Conventions

### Test Framework

All packages use **HSpec** with **hspec-discover** for automatic test discovery.

**Test file naming**: `<ModuleName>Spec.hs` in `test/` directory matching module structure

**Example**:
- Module: `Fractal.Layer.Diagnostics`
- Test: `test/Fractal/Layer/DiagnosticsSpec.hs`

### Test Organization

Each test file should:
1. Export a `spec :: Spec` function
2. Use `describe` blocks to organize related tests
3. **Emphasize property-based tests** (Hedgehog) for domain invariants and critical functions
4. Include contract tests for public APIs and integration tests for cross-module interactions
5. Test resource cleanup and exception safety for resource-related code
6. **Avoid testing library implementation details** (e.g., simple field access)
7. **NEVER use `threadDelay` in tests** - use proper synchronization primitives (MVars, STM, etc.)

### Running Tests

```bash
# Run all tests
cabal test all

# Run specific package tests
cabal test fractal-layer

# Run with test output
cabal test --test-show-details=streaming
```

## Development Workflow

### Making Changes

1. **Single package changes**: Work in the specific package directory
2. **Cross-package changes**: Be aware of dependency order:
   - `fractal-layer` is independent (no fractal dependencies)
   - `fractal-schema` is independent (no fractal dependencies)
   - `fractal-stream` depends on both `fractal-layer` and `fractal-schema`

### Adding New Modules

1. Create the module file in `<package>/src/`
2. Add to `exposed-modules` in the package's `.cabal` file
3. Create corresponding test file in `<package>/test/`
4. Add to `other-modules` in test-suite section if needed (hspec-discover may auto-discover)

### Dependency Management

**Adding dependencies**:
1. Add to `build-depends` in the appropriate `.cabal` file
2. Use version bounds (e.g., `>= 2.0 && < 3`)
3. Keep dependencies aligned across packages where possible

**Common dependencies**:
- `base >= 4.17.0.0 && < 5`
- `aeson >= 2.0 && < 3`
- `text >= 2.0 && < 3`
- `bytestring >= 0.11 && < 1`
- `unliftio >= 0.2 && < 1`

## Code Style and Conventions

### Language Extensions

Default extensions are defined in the `common extensions` stanza:
- `OverloadedStrings`
- `OverloadedRecordDot`
- `RecordWildCards`
- `DeriveGeneric`
- `DerivingStrategies`
- `LambdaCase`
- `GeneralizedNewtypeDeriving`
- `RankNTypes`
- `ScopedTypeVariables`
- `BlockArguments`

Additional extensions can be enabled per-module with `{-# LANGUAGE #-}` pragmas.

### Module Organization

**Standard module structure**:
```haskell
{-# LANGUAGE <AdditionalExtensions> #-}

module Fractal.Package.Module
  ( -- * Primary Types
    MainType(..)
  , HelperType
    -- * Core Functions
  , mainFunction
  , helperFunction
  ) where

import Control.Monad (...)
import Data.Text (Text)
import qualified Data.Text as T
-- ...

-- Module implementation
```

### Error Handling

- Use `UnliftIO` for exception-safe resource management
- Provide cleanup guarantees via `bracket`, `finally`, or resource-tracking abstractions
- Document exception behavior in function documentation

## Key Architectural Decisions

### 1. Multi-Package Structure
**Decision**: Split into three independent packages instead of a single monolithic package

**Rationale**:
- Clear separation of concerns
- Independent versioning
- Reduced dependency footprint for users who only need specific functionality
- Schema registry can be used without the streaming library

### 2. Layer Abstraction
**Decision**: Use Arrow + Monad + Alternative typeclasses for Layer

**Rationale**:
- Arrow provides composition operators (`>>>`, `&&&`, `***`)
- Monad enables sequential resource management
- Alternative provides fallback behavior
- Composable and type-safe dependency injection

### 3. Test Organization
**Decision**: Tests live in each package's `test/` directory, not in fractal-stream

**Rationale**:
- Each package owns its tests
- Faster iteration on individual packages
- Clear test coverage per package
- Prevents test misplacement (e.g., Layer tests in Stream package)

### 4. Servant for Schema Registry API
**Decision**: Use Servant for schema registry HTTP API

**Rationale**:
- Type-safe API definitions
- Automatic client generation
- Well-documented ecosystem
- JSON serialization via Aeson

## Common Tasks

### Adding a New Backend

**For Stream backends**:
1. Create `fractal-stream/src/Fractal/Stream/Backend/<Name>.hs`
2. Implement `MonadStream` typeclass
3. Add backend-specific dependencies to `fractal-stream.cabal`
4. Add tests in `fractal-stream/test/Fractal/Stream/Backend/<Name>Spec.hs`

**For Schema backends**:
1. Create `fractal-schema/src/Fractal/Schema/Backend/<Name>.hs`
2. Implement `Fractal.Schema.Backend.Class` interface
3. Add backend-specific dependencies to `fractal-schema.cabal`
4. Add tests

### Adding Examples

Examples go in `<package>/examples/` directory:
- Create subdirectory for the example (e.g., `examples/redis/`)
- Add executable stanza to `.cabal` file
- Keep examples focused and well-documented

### CI/CD Considerations

**GitHub Actions workflow** (`.github/workflows/`):
- Build all packages
- Run all test suites
- Check for warnings
- Build on multiple GHC versions if needed

## Troubleshooting

### "Module not found" errors
- Check `exposed-modules` in `.cabal` file
- Verify module path matches directory structure
- Run `stack clean` and rebuild

### Test discovery issues
- Ensure test file ends with `Spec.hs`
- Verify `spec :: Spec` is exported
- Check `hspec-discover` is in `build-tool-depends`

### Dependency conflicts
- Check version bounds across all packages
- Use `stack freeze` to generate exact dependency versions
- Review `stack.yaml` for package-specific constraints

## References

- **Project Repository**: https://github.com/iand675/fractal
- **Cabal User Guide**: https://cabal.readthedocs.io/
- **HSpec Documentation**: https://hspec.github.io/
- **Servant Documentation**: https://docs.servant.dev/

## Version History

- **Current**: Multi-package structure with fractal-layer, fractal-schema, and fractal-stream
- **Recent changes**:
  - Split schema registry into independent package
  - Moved Layer tests from fractal-stream to fractal-layer
  - Reorganized as multi-package cabal project

---

**Note for AI Assistants**: This project uses a multi-package structure. Always verify which package you're working in and respect the dependency graph. Tests should be in the same package as the code they test.
