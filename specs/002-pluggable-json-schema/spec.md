# Feature Specification: Pluggable JSON Schema Architecture

**Feature Branch**: `2-pluggable-json-schema`
**Created**: 2025-11-21
**Status**: Draft
**Input**: Refactor fractal-openapi JSON Schema implementation to use pluggable architecture inspired by hyperjump library, supporting custom keywords, vocabularies, dialects, formats, and extensible validation results

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Register and Use Custom Keywords (Priority: P1)

Library users need to define custom validation keywords specific to their domain (e.g., credit card validation, business rules) that integrate seamlessly with standard JSON Schema validation.

**Why this priority**: Core architectural change that enables all other extensibility features. Without pluggable keywords, users are locked into standard vocabulary only, limiting real-world applicability.

**Independent Test**: Can be fully tested by defining a custom keyword (with compile and validate functions), registering it in a vocabulary, using it in a schema, and verifying validation behavior. No dependencies on other features.

**Acceptance Scenarios**:

1. **Given** a custom keyword "x-creditCard" with Luhn algorithm validator, **When** registered in a vocabulary and used in schema, **Then** validates credit card numbers correctly
2. **Given** a custom keyword "x-positive" for numeric validation, **When** schema uses both standard "type": "number" and custom "x-positive": true, **Then** both validations are applied
3. **Given** a custom keyword with compilation phase, **When** schema is parsed, **Then** compilation executes once and validation uses compiled result
4. **Given** a custom keyword that produces annotations, **When** validation succeeds, **Then** annotations are collected and returned in validation result

---

### User Story 2 - Define and Register Custom Vocabularies (Priority: P1)

Users need to organize related custom keywords into vocabularies that can be versioned, composed, and registered for use across multiple schemas.

**Why this priority**: Fundamental to extensibility architecture. Vocabularies provide the organizational unit for custom keywords, enabling modular extension of the schema system.

**Independent Test**: Can be tested by creating a vocabulary with multiple custom keywords, registering it, and using it in schemas with `$vocabulary` declarations.

**Acceptance Scenarios**:

1. **Given** a vocabulary "https://example.com/vocab/business" with keywords ["x-taxId", "x-creditCard"], **When** registered and referenced in schema `$vocabulary`, **Then** keywords are available for validation
2. **Given** a required vocabulary not registered, **When** parsing schema with `$vocabulary` declaration, **Then** fails with clear error about missing vocabulary
3. **Given** two vocabularies with same keyword name, **When** both registered in registry, **Then** detects conflict and reports error with vocabulary URIs
4. **Given** an optional vocabulary not registered, **When** parsing schema, **Then** proceeds without error, ignoring unknown keywords

---

### User Story 3 - Compose Custom Dialects (Priority: P1)

Users need to create custom JSON Schema dialects that combine standard and custom vocabularies with specific validation behaviors (format assertion/annotation, unknown keyword handling).

**Why this priority**: Dialects define the validation contract. Custom dialects enable users to create domain-specific schema languages that extend JSON Schema for their specific use cases.

**Independent Test**: Can be tested by composing a dialect from vocabularies, registering it, and validating schemas that declare the dialect via `$schema`.

**Acceptance Scenarios**:

1. **Given** a dialect combining core, validation, and custom business vocabularies, **When** schema declares dialect in `$schema`, **Then** all vocabularies' keywords are available
2. **Given** a dialect with format assertion enabled, **When** validating against schema with "format": "email", **Then** format validation failures cause overall validation failure
3. **Given** a dialect with unknown keyword mode set to "error", **When** schema contains unrecognized keyword, **Then** parsing fails with unknown keyword error
4. **Given** a dialect registered with URI "https://example.com/schema/2024", **When** schema uses `$schema`: "https://example.com/schema/2024", **Then** dialect configuration is applied

---

### User Story 4 - Register Custom Formats (Priority: P2)

Users need to define custom format validators (beyond standard formats like email, URI) for domain-specific string validation (e.g., ISBN, IBAN, custom identifiers).

**Why this priority**: Common extension point for string validation. While standard formats cover many cases, domain-specific formats are essential for business applications.

**Independent Test**: Can be tested by registering a custom format validator, using it in a schema with `"format": "custom-name"`, and validating strings.

**Acceptance Scenarios**:

1. **Given** a custom format "isbn" with ISBN validator, **When** registered and used in schema, **Then** validates ISBN strings correctly
2. **Given** format assertion mode enabled, **When** custom format validation fails, **Then** produces validation error
3. **Given** format annotation mode, **When** custom format validation fails, **Then** produces annotation but validation succeeds
4. **Given** a format validator that throws exception, **When** validation runs, **Then** catches exception and produces validation error with context

---

### User Story 5 - Register Custom Meta-Schemas (Priority: P2)

Users need to register custom meta-schemas that define the structure and constraints for their custom vocabularies and dialects, enabling "schemas about schemas" validation.

**Why this priority**: Ensures schema correctness at authoring time. Meta-schemas validate that custom schemas conform to their dialect's structure before they're used for validation.

**Independent Test**: Can be tested by registering a meta-schema with `$dynamicAnchor`, creating a schema that should conform, and validating the schema itself.

**Acceptance Scenarios**:

1. **Given** a meta-schema for custom dialect with required "x-version" property, **When** validating a schema missing "x-version", **Then** meta-validation fails
2. **Given** a meta-schema with recursive `$dynamicRef`, **When** validating nested schema structure, **Then** recursively validates all levels
3. **Given** a standard schema with `$schema` pointing to custom meta-schema URI, **When** meta-validating, **Then** uses registered custom meta-schema
4. **Given** a meta-schema not registered, **When** attempting meta-validation, **Then** fails with clear error about missing meta-schema

---

### User Story 6 - Extensible Validation Results with Typed Annotations (Priority: P1)

Users need validation results that go beyond boolean true/false or simple errors, including typed annotations that can be inspected and used programmatically.

**Why this priority**: Critical architectural change. Current Either-based results are limiting. Typed annotations enable extracting metadata, documentation, and computed values from schemas.

**Independent Test**: Can be tested by validating with annotation collection enabled, then using Typeable to extract specific annotation types from the result.

**Acceptance Scenarios**:

1. **Given** validation with `validationCollectAnnotations = True`, **When** schema has "title" and "default" keywords, **Then** result includes typed annotations that can be extracted
2. **Given** a custom keyword that produces typed annotation (e.g., `x-metadata :: BusinessMetadata`), **When** validation succeeds, **Then** annotation can be extracted with correct type
3. **Given** validation result with multiple annotations of same type, **When** querying for that type, **Then** returns all annotations with JSON Pointer paths
4. **Given** validation failure, **When** inspecting result, **Then** provides structured error tree with precise paths and annotation context

---

### User Story 7 - Manual Schema Registration (Priority: P2)

Users need to programmatically register schemas in the schema registry for use in `$ref` resolution, without requiring file system or network access.

**Why this priority**: Essential for testing, embedded schemas, and offline validation. Enables complete control over the schema resolution environment.

**Independent Test**: Can be tested by manually registering schemas with specific URIs, then validating schemas that reference those URIs via `$ref`.

**Acceptance Scenarios**:

1. **Given** a schema registered at URI "https://example.com/schema/user", **When** another schema references "$ref": "https://example.com/schema/user", **Then** resolution succeeds without network access
2. **Given** multiple schemas registered with same base URI but different fragments, **When** resolving `$ref` with fragment, **Then** resolves to correct subschema
3. **Given** a schema registry with embedded meta-schemas, **When** validating schema structure, **Then** uses registered meta-schemas without external loading
4. **Given** an attempt to register schema with duplicate URI, **When** registration mode is "error", **Then** fails with clear conflict error

---

### User Story 8 - Custom Keywords with Code Generation Metadata (Priority: P1)

Users need custom keywords to provide metadata that guides code generation (Template Haskell), enabling custom Aeson instances, validation embedding, and type hints.

**Why this priority**: Critical for making the pluggable architecture useful for codegen. Without this, custom keywords are invisible to code generation, severely limiting their utility.

**Independent Test**: Can be tested by defining a custom keyword that provides codegen metadata, using it in a schema, generating code, and verifying the metadata influenced the generated types.

**Acceptance Scenarios**:

1. **Given** a custom keyword "x-newtype": true that provides CodegenHint annotation, **When** generating code from schema, **Then** produces newtype instead of record
2. **Given** a custom keyword "x-validator": "creditCard" that provides ValidatorHint annotation, **When** generating FromJSON instance, **Then** embeds custom validator in parser
3. **Given** a custom keyword "x-field-naming": "snake_case" that provides FieldNamingHint annotation, **When** generating record fields, **Then** applies snake_case naming convention
4. **Given** multiple custom keywords providing codegen metadata, **When** generating code, **Then** composes hints in priority order without conflicts

---

### Edge Cases

**Validation & Compilation**:
- What happens when a custom keyword's compile function throws an exception during schema parsing?
- How does the system handle circular dependencies between vocabularies in a dialect?
- What happens when a custom format validator is registered with the same name as a standard format?
- How are annotations from nested schemas (allOf, anyOf, oneOf) merged in the final result?
- What happens when validation result annotation extraction requests a type that doesn't exist?
- How does the system handle custom keywords that modify validation context (e.g., changing base URI)?
- What happens when a dialect references a vocabulary that references another vocabulary?
- How are evaluation order and dependencies between custom keywords determined?
- What happens when manual schema registration conflicts with embedded meta-schemas?
- How does annotation collection affect validation performance for deeply nested schemas?

**Code Generation**:
- What happens when multiple custom keywords provide conflicting codegen hints (e.g., both "newtype" and "record" strategies)?
- How does codegen handle custom keywords that reference external schemas via $ref during hint generation?
- What happens when a custom keyword's codegen hint produces invalid Haskell identifiers?
- How are codegen hints from nested schemas (allOf compositions) merged into a single generated type?
- What happens when generating code from a schema with both standard and custom validators?
- How does the HasSchema instance reference custom keywords from the original schema?
- What happens when a custom keyword provides a codegen hint but validation metadata is inconsistent?

## Requirements *(mandatory)*

### Functional Requirements

#### Architecture & Extensibility

- **FR-001**: System MUST support registration of custom keywords with compile and validate phases
- **FR-002**: System MUST support registration of custom vocabularies containing related keywords
- **FR-003**: System MUST support registration of custom dialects composed from vocabularies
- **FR-004**: System MUST support registration of custom format validators
- **FR-005**: System MUST support registration of custom meta-schemas for dialect validation
- **FR-006**: System MUST detect and report keyword conflicts between vocabularies in a dialect
- **FR-007**: System MUST support pluggable schema loaders for different URI schemes (file://, http://, embedded)

#### Validation Results

- **FR-008**: Validation results MUST be extensible beyond Either/Boolean to support typed annotations
- **FR-009**: System MUST collect annotations during validation when configured
- **FR-010**: System MUST provide typed annotation extraction via Typeable mechanism
- **FR-011**: System MUST associate annotations with JSON Pointer paths indicating where they originated
- **FR-012**: Validation errors MUST include structured error trees with schema path and instance path
- **FR-013**: System MUST support multiple output formats (FLAG, BASIC, DETAILED) as in JSON Schema spec

#### Schema Management

- **FR-014**: System MUST support manual schema registration by URI without file/network access
- **FR-015**: System MUST support schema registration conflict detection (error, warn, replace modes)
- **FR-016**: System MUST maintain separation between standard embedded meta-schemas and user-registered schemas
- **FR-017**: System MUST support schema preloading into registry before validation
- **FR-018**: Registry MUST support querying for registered schemas by URI and by URI pattern

#### Backward Compatibility & Migration

- **FR-019**: System MUST maintain 100% test suite pass rate during refactoring
- **FR-020**: System MUST provide migration path from current baked-in validation to pluggable architecture
- **FR-021**: System MUST not break existing API for users not using custom extensions
- **FR-022**: Standard validators MUST be implemented as registered keywords using the new pluggable system

#### Keyword Compilation & Validation

- **FR-023**: Custom keywords MUST support two-phase compile-then-validate pattern
- **FR-024**: Keyword compile functions MUST receive schema context with registry access and resolveRef capability
- **FR-025**: Keyword validators MUST be able to produce both errors and annotations
- **FR-026**: System MUST evaluate keywords in structural order (parse order for compilation, schema structure for validation)
- **FR-027**: System MUST allow keywords to access adjacent keyword values during compilation
- **FR-028**: System MUST support keyword scope restrictions (object-only, array-only, etc.)

#### Code Generation Integration

- **FR-029**: Custom keywords MUST be able to register codegen handlers separately from validation handlers
- **FR-030**: Codegen handlers MUST be registered per target (Aeson, Servant, Proto3, etc.) allowing target-specific behavior
- **FR-031**: Code generation MUST query registered codegen handlers (NOT validation/annotation system) before generating types
- **FR-032**: System MUST provide conflict resolution for multiple keywords providing same codegen hint type
- **FR-033**: Generated HasSchema instances MUST preserve custom keyword metadata for runtime access
- **FR-034**: Template Haskell codegen MUST support pluggable strategy selection based on custom keyword hints
- **FR-035**: Codegen handlers MUST be able to reference validator functions by name/module for embedding in generated code
- **FR-036**: Codegen MUST validate that custom keyword hints are consistent with schema structure before generating code
- **FR-037**: System MUST support multiple codegen targets with independent handler registries per target

### Key Entities

#### Keyword System

- **KeywordDefinition**: Specification of a custom keyword
  - **Fields**: name, scope (which schema types it applies to), compileFunc, validateFunc
  - **Compile Function Signature**: `(keywordValue, schema, context) -> CompiledData` where context includes registry access and resolveRef
  - **Validate Function Signature**: `(compiledData, instance, validationContext) -> ValidationResult`
  - **Invariants**: Name must be valid JSON object key; compile and validate functions must not throw unhandled exceptions; compilation must be deterministic

- **CompiledKeyword**: Result of keyword compilation phase
  - **Fields**: keywordName, compiledData (existentially quantified), adjacentKeywordData (values from adjacent keywords accessed during compilation)
  - **Invariants**: Compiled data type must be serializable for caching; compilation must be deterministic; adjacent keyword access must be read-only

- **CompilationContext**: Context provided to keyword compile functions
  - **Fields**: schemaRegistry, resolveRef (URI -> Schema), currentSchema, parentSchemaPath
  - **Invariants**: resolveRef must handle cycles; registry must be immutable during compilation

#### Vocabulary System

- **Vocabulary**: Collection of related keywords
  - **Fields**: vocabularyURI (unique identifier), required (boolean), keywords (Map Text KeywordDefinition)
  - **Invariants**: URI must be absolute; keywords must have unique names within vocabulary; URI must not conflict with standard vocabularies unless explicitly overriding

- **VocabularyRegistry**: Global registry of available vocabularies
  - **Fields**: vocabularies (Map URI Vocabulary), dialects (Map URI Dialect)
  - **Invariants**: No duplicate vocabulary URIs; vocabulary dependencies must be resolvable

#### Dialect System

- **Dialect**: Configuration defining a schema variant
  - **Fields**: dialectURI, schemaVersion, vocabularies [(URI, required)], formatBehavior, unknownKeywordMode
  - **Invariants**: Dialect URI must be absolute; all vocabulary URIs must resolve; no keyword conflicts between vocabularies

#### Validation Result System

- **ValidationResult**: Outcome of validation with errors and annotations
  - **Fields**: valid (Boolean), errors (ValidationErrorTree), annotations (AnnotationCollection)
  - **Invariants**: If valid=False, errors must be non-empty; annotation paths must be valid JSON Pointers

- **AnnotationCollection**: Typed annotation storage
  - **Fields**: annotations (Map JsonPointer [SomeAnnotation])
  - **Invariants**: Annotations must be Typeable; paths must correspond to schema locations

- **SomeAnnotation**: Existentially quantified annotation
  - **Fields**: annotationType (TypeRep), annotationValue (Dynamic)
  - **Invariants**: Value must match type representation

#### Format System

- **FormatValidator**: Custom format validation function
  - **Fields**: formatName, validatorFunc (Text -> Bool), errorMessage (Text -> Text)
  - **Invariants**: Validator must be pure; must not throw exceptions; must handle all Unicode text

- **FormatRegistry**: Registry of format validators
  - **Fields**: formats (Map Text FormatValidator)
  - **Invariants**: No duplicate format names; standard formats must be registered by default

#### Schema Registry

- **SchemaRegistry**: Storage for registered schemas
  - **Fields**: schemas (Map URI Schema), metaSchemas (Map URI Schema), conflictMode
  - **Invariants**: All URIs must be absolute; schemas must be parseable; meta-schemas must validate themselves

#### Code Generation System

- **CodegenTarget**: Enumeration of code generation targets
  - **Values**: AesonTarget (ToJSON/FromJSON instances), ServantTarget (API types), YesodTarget (routes), Proto3Target (protobuf), CustomTarget (user-defined)
  - **Invariants**: Each target has independent handler registry; targets are extensible

- **CodegenHint**: Hint type for guiding code generation
  - **Subtypes**: StrategyHint (newtype/record/sum), FieldNamingHint (camelCase/snake_case), ValidatorHint (validator references), DeriveHint (additional deriving clauses), TargetSpecificHint (opaque hint for custom targets)
  - **Invariants**: Hints must be composable; conflicting hints must be resolvable via priority; target-specific hints are opaque to core system

- **CodegenHandler**: Handler function registered for a keyword and target
  - **Signature**: `(keywordValue :: Value, schema :: Schema) -> [CodegenHint]`
  - **Fields**: keywordName, target, handlerFunc
  - **Invariants**: Handlers must be deterministic; must not perform IO; can inspect schema structure

- **CodegenHandlerRegistry**: Registry mapping (keyword, target) pairs to handlers
  - **Fields**: handlers (Map (Text, CodegenTarget) CodegenHandler)
  - **Invariants**: At most one handler per (keyword, target) pair; lookups by target must be efficient

- **ValidatorReference**: Reference to a validator function for embedding in generated code
  - **Fields**: validatorName (Text), validatorModule (Maybe Text), validatorType (Type representation)
  - **Invariants**: Validator must be in scope at codegen site; type signature must be compatible with validation context

**Domain Invariants**:
- Custom keywords registered in vocabularies must not conflict unless explicitly shadowing standard keywords
- Validation with annotation collection must not change validation outcome (valid/invalid)
- Meta-schema validation must complete before schema is used for instance validation
- Schema compilation must be idempotent (same schema compiles to same validators)
- Annotation types must be registered before validation if typed extraction is needed
- Format validators must complete in bounded time (no infinite loops)
- Dialect composition must detect cycles in vocabulary dependencies
- Codegen hints from custom keywords must be serializable to Template Haskell AST
- Custom validators referenced in codegen must be pure or explicitly IO-based
- Generated HasSchema instances must roundtrip schema metadata without loss

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 100% of existing test suite passes throughout entire refactoring process (no regressions)
- **SC-002**: Users can register and use custom keywords with less than 20 lines of boilerplate code
- **SC-003**: Custom vocabulary registration takes less than 100ms for vocabularies with up to 50 keywords
- **SC-004**: Validation performance degradation is less than 10% compared to current baked-in implementation
- **SC-005**: Annotation collection overhead is less than 20% when enabled, 0% when disabled
- **SC-006**: API documentation includes complete examples for all extension points (keywords, vocabularies, formats)
- **SC-007**: Migration guide demonstrates refactoring path with zero breaking changes to existing API
- **SC-008**: Custom format validators can be registered and used without modifying library source code
- **SC-009**: Manual schema registration supports at least 1000 schemas without performance degradation
- **SC-010**: Typed annotation extraction succeeds for 100% of registered annotation types
- **SC-011**: Code generated from schemas with custom keywords compiles without errors or warnings
- **SC-012**: Custom codegen hints influence 100% of relevant generated code elements (types, fields, instances)
- **SC-013**: Generated Aeson instances successfully serialize and deserialize JSON that validates against the source schema
- **SC-014**: Custom validators embedded in generated FromJSON instances execute and report errors correctly
- **SC-015**: HasSchema instances for generated types provide access to custom keyword metadata at runtime
- **SC-016**: Multiple codegen targets (Aeson, Servant, etc.) can coexist with independent handler registries
- **SC-017**: Codegen handlers can be registered for custom targets without modifying core library

### Qualitative Outcomes

- Library users can extend JSON Schema vocabulary without forking the codebase
- Domain-specific schema dialects can be defined and maintained independently
- Validation results provide programmatic access to schema metadata beyond errors
- Schema registry can be fully controlled for testing and embedded use cases
- Refactoring maintains backward compatibility for users not using extensions
- Custom keywords seamlessly integrate with code generation pipeline
- Generated types preserve all schema metadata including custom keywords
- Users can create domain-specific codegen strategies via custom keyword hints

## Additional Requirements

### Non-Functional Requirements

- **NFR-001**: All extension points (keywords, vocabularies, formats, meta-schemas) must be documented with examples
- **NFR-002**: Gradual refactoring approach must maintain test suite pass rate after each step
- **NFR-003**: Keyword registration must be thread-safe for concurrent vocabulary initialization
- **NFR-004**: Custom keyword compile/validate functions must be exception-safe (caught and converted to errors)
- **NFR-005**: Validation result annotation storage must use efficient data structures (no O(n²) operations)
- **NFR-006**: Registry operations (lookup, register) must be O(log n) or better

### Assumptions

- Custom keywords will primarily be used for domain-specific validation, not reimplementing standard keywords
- Most users will use standard dialects; custom dialects will be a power-user feature
- Annotation collection will be disabled by default due to performance overhead
- Format validators will be pure functions without IO (network/file access via separate loader mechanism)
- Custom vocabularies will be registered at application startup, not dynamically during validation
- Backward compatibility means existing ValidationConfig, validateValue, and Schema types remain usable

### Dependencies

**Existing Fractal Packages**:
- fractal-layer (no changes needed)
- fractal-schema (no changes needed)

**External Libraries**:
- aeson (JSON parsing and Value types) - no changes
- text, bytestring (text handling) - no changes
- containers (Map, Set for registries) - no changes
- Typeable, Dynamic (typed annotations) - new usage for annotation system

### Constraints

- **Gradual Refactoring**: Changes must be incremental with test suite passing after each step
- **Zero Breaking Changes**: Existing public API must remain compatible
- **Performance**: Custom extension overhead must be minimal when extensions are not used
- **Type Safety**: Pluggable architecture must maintain type safety via Typeable/existentials

### Scope

**In Scope**:
- Pluggable keyword registration and validation
- Custom vocabulary and dialect composition
- Extensible validation results with typed annotations
- Custom format validators
- Manual schema registration
- Custom meta-schema registration
- Migration of standard validators to pluggable architecture
- Annotation API for extracting typed schema metadata

**Out of Scope**:
- Changes to OpenAPI 3.x support (that's separate feature)
- Code generation changes (Template Haskell codegen uses schema API as-is)
- Network/file loading changes (use existing ReferenceLoader mechanism)
- CLI tool changes
- Breaking changes to public API
- Performance optimization beyond maintaining current performance

## Design Decisions

### Compilation Scope (Resolved)
**Decision**: Custom keyword compile functions WILL have access to the full schema registry during compilation.

**Rationale**: Following hyperjump's architecture, the compile function receives parameters that allow resolving references to other schemas. This enables custom keywords to perform cross-schema validation logic preprocessing. The registry access allows using `getSchema(uri)` to resolve `$ref` during compilation, enabling powerful keyword composition.

**Implementation**: Compile functions receive `(keywordValue, schema, context)` where `context` includes registry access and a `resolveRef` function.

### Annotation Collection (Resolved)
**Decision**: Annotation collection will use full in-memory storage.

**Rationale**: JSON schemas are metadata documents, typically small (< 1MB). Even with extensive annotation collection, memory usage is unlikely to be problematic. In-memory storage provides simpler API, easier querying, and better performance than streaming alternatives.

**Implementation**: `AnnotationCollection` stores `Map JsonPointer [SomeAnnotation]` in memory during validation.

### Keyword Evaluation Order (Resolved)
**Decision**: Keyword evaluation order is STRUCTURAL and IMPLICIT, not priority-based.

**Rationale**: Following hyperjump's design, keywords don't have explicit priorities. Instead, evaluation order emerges from the schema structure and the two-phase compile/validate pattern. Keywords that need results from other keywords access them via the validation context or by compiling sub-schemas during their compilation phase. This is more composable than explicit priorities and matches JSON Schema's applicator model.

**Implementation**: Keywords are compiled in parse order. During validation, applicator keywords (allOf, anyOf, etc.) control evaluation flow. Custom keywords can access adjacent keyword values during compilation via the schema parameter, and can delegate to sub-schema validation via context.

### Codegen Hint Mechanism (Resolved)
**Decision**: Separate registration mechanism (Option B), independent of validation, with support for multiple codegen targets.

**Rationale**:
1. **Separation of concerns**: Codegen and validation are different concerns with different lifecycles. Codegen happens at TH compile time, validation at runtime.
2. **Performance**: TH code generation shouldn't need to run validation machinery.
3. **Multiple targets**: Codegen should work for multiple targets (Aeson, Servant, Yesod, Proto3, etc.), not just Aeson instances.
4. **Validation access when needed**: Codegen handlers can reference validator functions by name/module, allowing generated code to call them without tight coupling.

**Implementation**:
- Keywords register codegen handlers separately from validation: `registerCodegenHandler :: Text -> CodegenTarget -> (Value -> Schema -> [CodegenHint]) -> Registry`
- Codegen handlers receive the schema for inspection but don't trigger validation
- Generated code can reference validators: `instance FromJSON Foo where parseJSON = validate fooValidator <=< fromJSON`
- Support for multiple codegen targets via `CodegenTarget` parameter (Aeson, Servant, Proto3, etc.)

**Concrete Example - OpenAPI to Yesod Routes**:
```haskell
-- OpenAPI spec with custom keyword for Yesod route config
{
  "paths": {
    "/users/{id}": {
      "get": {
        "operationId": "getUser",
        "x-yesod-route": {
          "name": "UserR",
          "methods": ["GET"],
          "auth": "requireAuth"
        }
      }
    }
  }
}

-- Register codegen handler for Yesod target
registerCodegenHandler "x-yesod-route" YesodTarget $ \routeConfig schema ->
  [ YesodRouteHint
      { routeName = routeConfig.name
      , routeMethods = routeConfig.methods
      , routeAuth = Just (ValidatorReference "requireAuth" (Just "Foundation") ...)
      }
  ]

-- Same keyword could have Aeson handler for request/response types
registerCodegenHandler "x-yesod-route" AesonTarget $ \routeConfig schema ->
  [ StrategyHint Record  -- Generate request/response DTOs
  , DeriveHint ["ToJSON", "FromJSON"]
  ]

-- Generated Yesod routes file:
-- /users/#UserId UserR GET
--
-- Generated handler signature:
-- getUserR :: UserId -> Handler TypedContent
-- getUserR uid = do
--   requireAuth  -- Validator reference embedded
--   user <- runDB $ get uid
--   returnJson user
```

## Open Questions

None - all design decisions have been resolved.
