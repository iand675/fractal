# Feature Specification: Comprehensive JSON Schema and OpenAPI Library

**Feature Branch**: `001-json-schema-implementation`  
**Created**: 2025-11-18  
**Status**: Draft  
**Input**: Implement a comprehensive, type-safe JSON Schema and OpenAPI 3.x library for Haskell with validation, vocabulary extensibility, and pluggable code generation

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Core JSON Schema Validation (Priority: P1)

Validate JSON data against JSON Schema specifications (draft-04 through 2020-12) with detailed error reporting and high performance.

**Why this priority**: Foundation for all other features. Without robust validation, the library cannot deliver value. This is the core use case that every user will need.

**Independent Test**: Can be fully tested by parsing a schema, validating JSON values against it, and verifying error messages. Delivers immediate value for validation use cases.

**Acceptance Scenarios**:

1. **Given** a JSON Schema defining an object with required string "name" and optional integer "age", **When** validating `{"name": "Alice", "age": 30}`, **Then** validation succeeds
2. **Given** the same schema, **When** validating `{"age": 30}` (missing "name"), **Then** validation fails with error pointing to missing required property
3. **Given** a schema with numeric constraints (minimum: 0, maximum: 150), **When** validating age = 200, **Then** validation fails with constraint violation error
4. **Given** a schema with format validation (email), **When** validating an invalid email, **Then** validation fails with format error

---

### User Story 2 - Multi-Version Schema Support (Priority: P1)

Support all major JSON Schema versions (draft-04, draft-06, draft-07, 2019-09, 2020-12) with automatic version detection and unified API.

**Why this priority**: Real-world schemas use different versions. Without multi-version support, the library cannot handle existing schemas, severely limiting adoption.

**Independent Test**: Can be tested by loading schemas with different `$schema` declarations and verifying version-specific keyword behavior (e.g., `exclusiveMaximum` boolean in draft-04 vs numeric in draft-06+).

**Acceptance Scenarios**:

1. **Given** a draft-04 schema with `exclusiveMaximum: true`, **When** parsing, **Then** correctly interprets as boolean modifier
2. **Given** a draft-07 schema with `if/then/else`, **When** validating, **Then** applies conditional logic correctly
3. **Given** a 2019-09 schema with `unevaluatedProperties`, **When** validating, **Then** tracks evaluated properties correctly
4. **Given** a schema without `$schema`, **When** parsing, **Then** defaults to latest supported version

---

### User Story 3 - Custom Vocabulary System (Priority: P2)

Define and register custom vocabularies with domain-specific keywords for validation, annotation, and code generation hints.

**Why this priority**: Differentiator that enables domain-specific schema extensions. Allows users to embed business logic, security constraints, and codegen hints directly in schemas.

**Independent Test**: Can be tested by creating a custom vocabulary (e.g., credit card validation), registering it, and using it in a schema with custom keywords.

**Acceptance Scenarios**:

1. **Given** a custom "business" vocabulary with `x-credit-card` keyword, **When** registered and used in schema, **Then** validates credit card numbers with Luhn check
2. **Given** a custom vocabulary with `x-sensitive` keyword, **When** parsed, **Then** metadata extraction includes sensitivity annotation
3. **Given** conflicting vocabularies with same keyword, **When** composing into dialect, **Then** detects conflict and reports error
4. **Given** a schema using unknown vocabulary marked as required, **When** parsing, **Then** fails with vocabulary not found error

---

### User Story 4 - Template Haskell Code Generation (Priority: P2)

Generate Haskell datatypes with Aeson instances from JSON Schemas using Template Haskell, with support for semantic newtypes and custom strategies.

**Why this priority**: Primary use case for schema-driven development. Eliminates boilerplate and ensures type safety. Code generation is the killer feature for adoption.

**Independent Test**: Can be tested by generating types from various schema patterns and verifying the generated code compiles and correctly serializes/deserializes JSON.

**Acceptance Scenarios**:

1. **Given** an object schema with required and optional properties, **When** generating types, **Then** produces record with proper Maybe fields
2. **Given** a schema with `x-newtype` annotation, **When** generating types, **Then** produces newtype with smart constructor
3. **Given** a schema with validation constraints, **When** generating FromJSON instance, **Then** embeds validation in parser
4. **Given** a oneOf schema, **When** generating types, **Then** produces sum type with alternative constructors

---

### User Story 5 - OpenAPI 3.x Support with Framework Integration (Priority: P3)

Parse OpenAPI 3.x specifications and generate Servant API types or Yesod routes with request/response validation.

**Why this priority**: Extends the library's utility to API design and documentation. Enables schema-first API development with type-safe routing.

**Independent Test**: Can be tested by loading an OpenAPI spec, generating Servant types, and verifying they match expected API structure.

**Acceptance Scenarios**:

1. **Given** an OpenAPI spec with multiple endpoints, **When** generating Servant types, **Then** produces correct API type with captures, query params, and request bodies
2. **Given** an OpenAPI operation with request body schema, **When** validating request, **Then** checks conformance to schema
3. **Given** an OpenAPI operation with multiple response schemas, **When** generating types, **Then** produces response types for each status code
4. **Given** an OpenAPI spec with component schemas, **When** generating Yesod routes, **Then** produces route file and handler type signatures

---

### Edge Cases

- What happens when circular `$ref` references create infinite loops?
- How does the system handle schemas larger than available memory?
- What happens when a schema uses a vocabulary with a keyword that conflicts with standard keywords?
- How are URI references resolved when base URI changes mid-schema?
- What happens when format validation fails but format is annotation-only?
- How does unevaluated* tracking work with deeply nested allOf/anyOf compositions?
- What happens when generating code for a schema with invalid Haskell identifiers (keywords, special characters)?
- How are polymorphic JSON values (oneOf/anyOf) represented when they cannot be disambiguated?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST parse JSON Schema from JSON/YAML with automatic version detection
- **FR-002**: System MUST validate JSON values against schemas with detailed error paths (JSON Pointer)
- **FR-003**: System MUST support all JSON Schema versions: draft-04, draft-06, draft-07, 2019-09, 2020-12
- **FR-004**: System MUST resolve `$ref` references including external URIs and relative paths
- **FR-005**: System MUST implement all standard vocabularies (core, applicator, validation, unevaluated, format, metadata, content)
- **FR-006**: System MUST provide custom vocabulary API with keyword definition, validation, and annotation
- **FR-007**: System MUST support dialect composition from multiple vocabularies
- **FR-008**: System MUST generate Haskell datatypes from schemas via Template Haskell
- **FR-009**: System MUST generate Aeson ToJSON/FromJSON instances with embedded validation
- **FR-010**: System MUST support semantic newtypes via schema annotations
- **FR-011**: System MUST parse OpenAPI 3.0 and 3.1 specifications
- **FR-012**: System MUST validate HTTP requests/responses against OpenAPI operations
- **FR-013**: System MUST generate Servant API types from OpenAPI specifications
- **FR-014**: System MUST generate Yesod routes from OpenAPI specifications
- **FR-015**: System MUST extract metadata from schemas for introspection
- **FR-016**: System MUST render schemas back to JSON/YAML in canonical form
- **FR-017**: System MUST pass official JSON Schema Test Suite for all supported versions (100% mandatory tests)
- **FR-018**: System MUST provide pluggable code generation strategies
- **FR-019**: System MUST track evaluated properties/items for unevaluated* keywords
- **FR-020**: System MUST support format validation with configurable assertion/annotation modes

### Key Entities

- **Schema**: Central AST representing a JSON Schema with version, vocabularies, core structure, validation rules, and metadata
  - **Invariants**: Valid URI for `$id` if present; vocabulary URIs must be registered if marked required; `$ref` must resolve
- **Vocabulary**: Collection of custom keywords with parsers, validators, and annotators
  - **Invariants**: Unique vocabulary URI; no duplicate keywords within vocabulary; keyword names must be valid JSON object keys
- **Dialect**: Configuration of vocabularies that defines a schema variant
  - **Invariants**: Unique dialect URI; all referenced vocabularies must exist; no conflicting keywords across vocabularies
- **ValidationResult**: Outcome of validation with success annotations or failure errors
  - **Invariants**: Cannot be both success and failure; error paths must be valid JSON Pointers
- **ValidationContext**: Stateful context during validation with schema registry, dynamic scope, and evaluated tracking
  - **Invariants**: Dynamic scope must form valid stack; evaluated properties/items must be subset of actual properties/items
- **CodegenContext**: Context for code generation with schema, config, strategy, naming, and type registry
  - **Invariants**: Generated names must be valid Haskell identifiers; no duplicate type names in registry
- **OpenApiSpec**: Complete OpenAPI specification with paths, operations, components, and security
  - **Invariants**: Valid OpenAPI version (3.0.x or 3.1.x); operation IDs unique if present; component references must resolve

**Domain Invariants**:
- Schema URI references must be resolvable or produce clear error
- Validation errors must include both schema path and instance path
- Generated Haskell code must be syntactically valid and pass GHC compilation
- Schema meta-schemas must validate successfully against themselves
- Vocabulary keyword priorities must form total ordering for deterministic evaluation
- Format validators must be pure functions with no side effects
- Codegen strategies must preserve schema semantics in generated types

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 100% pass rate on JSON Schema Test Suite for all supported versions (draft-04 through 2020-12)
- **SC-002**: Validate 1000+ schemas per second on commodity hardware (single core)
- **SC-003**: Parse and validate OpenAPI 3.x specs up to 10MB in size within 5 seconds
- **SC-004**: Generate compilable Haskell code from 95%+ of real-world schemas (measured against public schema corpus)
- **SC-005**: Validation error messages include precise JSON Pointer paths in 100% of cases
- **SC-006**: Support at least 5 custom vocabularies registered simultaneously without performance degradation
- **SC-007**: Generated code compiles without warnings on GHC 9.4+ with -Wall enabled
- **SC-008**: API documentation coverage at 100% for all public modules and types
- **SC-009**: Property-based tests achieve 1000+ successful cases per property without failures
- **SC-010**: Memory usage remains under 100MB for schemas up to 1MB in size

## Additional Requirements

### Non-Functional Requirements

- **NFR-001**: Type-safe API that makes invalid schemas unrepresentable at compile time
- **NFR-002**: Comprehensive Haddock documentation for all public interfaces
- **NFR-003**: Support for incremental validation (compile schema once, validate many values)
- **NFR-004**: Exception-safe resource handling for file/network schema loading
- **NFR-005**: Detailed error messages with context for both users and developers

### Dependencies

- Core: aeson, text, bytestring, containers, unordered-containers, vector, scientific, hashable
- Parsing: yaml, uri-bytestring
- Validation: regex-tdfa
- Code generation: template-haskell, th-lift
- Optional: servant, servant-server, yesod-core, lens

### Compatibility

- GHC >= 9.4 (base >= 4.17)
- Compatible with existing Fractal libraries (fractal-layer for resource management)
- No breaking changes to JSON Schema or OpenAPI specifications

