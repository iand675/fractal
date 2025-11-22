# Specification Quality Checklist: Pluggable JSON Schema Architecture

**Purpose**: Validate specification completeness and quality before proceeding to planning
**Created**: 2025-11-21
**Feature**: [spec.md](../spec.md)

## Content Quality

- [x] No implementation details (languages, frameworks, APIs)
- [x] Focused on user value and business needs
- [x] Written for non-technical stakeholders
- [x] All mandatory sections completed

## Requirement Completeness

- [x] No [NEEDS CLARIFICATION] markers remain
- [x] Requirements are testable and unambiguous
- [x] Success criteria are measurable
- [x] Success criteria are technology-agnostic (no implementation details)
- [x] All acceptance scenarios are defined
- [x] Edge cases are identified
- [x] Scope is clearly bounded
- [x] Dependencies and assumptions identified

## Feature Readiness

- [x] All functional requirements have clear acceptance criteria (37 functional requirements)
- [x] User scenarios cover primary flows (8 user stories including codegen)
- [x] Feature meets measurable outcomes defined in Success Criteria (17 success criteria)
- [x] No implementation details leak into specification
- [x] Four design decisions resolved (compilation scope, annotations, evaluation order, codegen mechanism)

## Resolved Design Decisions

### Question 1: Custom Keyword Compilation Scope ✓

**Decision**: Option A - Full registry access during compilation

**Rationale**: Following hyperjump's architecture, compile functions receive context with registry access and resolveRef capability. This enables powerful keyword composition and cross-schema validation preprocessing.

**Implementation**: Compile function signature is `(keywordValue, schema, context)` where `context` includes `schemaRegistry` and `resolveRef :: URI -> Schema`.

---

### Question 2: Annotation Collection Memory Model ✓

**Decision**: Option A - Full in-memory collection

**Rationale**: User confirmed JSON schemas are typically small metadata documents (< 1MB). In-memory storage provides simpler API, easier querying, and better performance.

**Implementation**: `AnnotationCollection` stores `Map JsonPointer [SomeAnnotation]` in memory during validation.

---

### Question 3: Keyword Evaluation Order ✓

**Decision**: Structural/implicit evaluation (inspired by hyperjump design)

**Rationale**: Based on hyperjump research, keywords don't use explicit priorities. Evaluation order emerges from schema structure and the two-phase compile/validate pattern. Keywords can access adjacent keyword values during compilation and delegate to sub-schema validation.

**Implementation**: Keywords compiled in parse order. During validation, evaluation follows schema structure (applicator keywords control flow). Custom keywords access adjacent values via schema parameter during compilation.

---

### Question 4: Codegen Hint Mechanism ✓

**Decision**: Option B - Separate registration mechanism, independent of validation, with multiple target support

**Rationale**: User confirmed that codegen should not be tied to validation directly. Key requirements:
1. Codegen and validation are separate concerns with different lifecycles
2. Generated code should be able to reference validators when needed (by name/module)
3. Codegen should support multiple targets beyond Aeson (Servant, Yesod, Proto3, custom targets)
4. Performance: TH codegen should be fast and not require running validation machinery

**Implementation**:
- Codegen handlers registered separately: `registerCodegenHandler :: Text -> CodegenTarget -> (Value -> Schema -> [CodegenHint]) -> Registry`
- Support for multiple independent target registries (AesonTarget, ServantTarget, Proto3Target, CustomTarget)
- Handlers can reference validators: `ValidatorReference` with name/module/type info
- Target-specific hints via `TargetSpecificHint` for custom targets

## Notes

- Specification is comprehensive and well-structured
- All functional requirements are testable (37 total functional requirements)
- Success criteria are measurable and appropriate (17 success criteria)
- Edge cases are thoroughly considered (both validation and codegen - 17 edge cases)
- Scope is well-defined with clear in-scope/out-of-scope boundaries
- Added User Story 8 for code generation integration with custom keywords
- Added 9 functional requirements (FR-029 through FR-037) for codegen integration
- Added codegen-specific entities: CodegenTarget, CodegenHint, CodegenHandler, CodegenHandlerRegistry, ValidatorReference
- Added CompilationContext entity for keyword compile functions
- Added 7 success criteria (SC-011 through SC-017) for codegen verification
- All four design decisions resolved:
  1. Compilation scope: Full registry access (following hyperjump)
  2. Annotation collection: In-memory (user confirmed schemas are small)
  3. Keyword evaluation order: Structural/implicit (following hyperjump)
  4. Codegen mechanism: Separate registration with multiple target support (user confirmed)

**Status**: ✅ READY FOR `/speckit.plan` - All clarifications resolved!
