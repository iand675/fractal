-- | JSON Schema validation engine
--
-- High-performance validation with detailed error reporting using JSON Pointer paths.
-- Supports all JSON Schema versions from draft-04 through 2020-12.
module Fractal.JsonSchema.Validator
  ( -- * Main Validation Functions
    validateValue
  , validateValueWithRegistry
  , validateValueWithContext
  , compileValidator

    -- * Configuration
  , ValidationConfig(..)
  , defaultValidationConfig
  , strictValidationConfig

    -- * Validator Type
  , Validator(..)

    -- * Errors
  , CompileError(..)
  ) where

import Fractal.JsonSchema.Types
import qualified Fractal.JsonSchema.Parser as Parser
import qualified Fractal.JsonSchema.Keywords.Type as KW
import qualified Fractal.JsonSchema.Keywords.Enum as KW
import qualified Fractal.JsonSchema.Keywords.Const as KW
import qualified Fractal.JsonSchema.Keywords.Numeric as KW
import qualified Fractal.JsonSchema.Keywords.String as KW
import qualified Fractal.JsonSchema.Keywords.Format as KW
-- Pluggable keyword system
import Fractal.JsonSchema.Keywords.Standard (standardKeywordRegistry)
import Fractal.JsonSchema.Keywords.Registry (draft04Registry, draft06Registry, draft07Registry, draft201909Registry, draft202012Registry)
import Fractal.JsonSchema.Keyword (keywordMap, lookupKeyword)
import qualified Fractal.JsonSchema.Keyword as Keyword
import Fractal.JsonSchema.Keyword.Types (KeywordNavigation(..))
import Fractal.JsonSchema.Keyword.Compile (compileKeywords, buildCompilationContext, CompiledKeywords(..))
import qualified Fractal.JsonSchema.Keyword.Validate as KeywordValidate
-- Applicator keywords (composition and conditional)
import qualified Fractal.JsonSchema.Keywords.AllOf as AllOf
import qualified Fractal.JsonSchema.Keywords.AnyOf as AnyOf
import qualified Fractal.JsonSchema.Keywords.OneOf as OneOf
import qualified Fractal.JsonSchema.Keywords.Not as Not
import qualified Fractal.JsonSchema.Keywords.Conditional as Cond
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Scientific as Sci
import Data.Foldable (toList)
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe, maybeToList)
import Control.Monad (mplus)
import Data.Vector (Vector, (!), fromList)
import qualified Fractal.JsonSchema.Regex as Regex
import qualified Data.UUID as UUID
import qualified Data.Time.Format.ISO8601 as Time
import qualified Data.Time.Clock as Time
import qualified Data.Time.Calendar as Time
import qualified Text.Read as Read
import qualified Data.Time.RFC3339 as RFC3339
import qualified Text.Email.Validate as Email
import qualified Network.URI as URI
import qualified Data.IP as IP
import qualified Numeric
import qualified Network.URI.Template.Parser as URITemplate

-- | Error during validator compilation
data CompileError = CompileError Text
  deriving (Eq, Show)

-- | Compiled validator for repeated use
newtype Validator = Validator { runValidator :: Value -> ValidationResult }

-- | Default validation configuration (latest version, format annotation)
defaultValidationConfig :: ValidationConfig
defaultValidationConfig = ValidationConfig
  { validationVersion = Draft202012
  , validationFormatAssertion = False  -- Format as annotation
  , validationContentAssertion = False  -- Content as annotation
  , validationShortCircuit = False     -- Collect all errors
  , validationCollectAnnotations = False
  , validationCustomValidators = Map.empty
  , validationReferenceLoader = Nothing  -- No external reference loading by default
  }

-- | Strict validation (format assertion, all errors collected)
strictValidationConfig :: ValidationConfig
strictValidationConfig = defaultValidationConfig
  { validationFormatAssertion = True
  , validationContentAssertion = True
  , validationShortCircuit = False
  }

-- | Validate a value against a schema (without external references)
validateValue :: ValidationConfig -> Schema -> Value -> ValidationResult
validateValue config schema val =
  let -- Build registry by walking the schema tree
      registry = registerSchemaInRegistry (schemaId schema) schema emptyRegistry
      
      ctx = ValidationContext
        { contextConfig = config
        , contextSchemaRegistry = registry  -- Use built registry for $ref resolution
        , contextRootSchema = Just schema   -- Store root schema for local $ref
        , contextBaseURI = schemaId schema
        , contextDynamicScope = []
        , contextEvaluatedProperties = Set.empty
        , contextEvaluatedItems = Set.empty
        , contextVisitedSchemas = Set.empty
        , contextResolvingRefs = Set.empty
        , contextRecursionDepth = 0
        }
  in validateValueWithContext ctx schema val

-- | Validate with a pre-built registry (enables external $ref resolution)
-- Use 'buildRegistryWithExternalRefs' to build the registry with external schemas loaded
validateValueWithRegistry :: ValidationConfig -> SchemaRegistry -> Schema -> Value -> ValidationResult
validateValueWithRegistry config registry schema val =
  let ctx = ValidationContext
        { contextConfig = config
        , contextSchemaRegistry = registry  -- Use provided registry
        , contextRootSchema = Just schema
        , contextBaseURI = schemaId schema
        , contextDynamicScope = []
        , contextEvaluatedProperties = Set.empty
        , contextEvaluatedItems = Set.empty
        , contextVisitedSchemas = Set.empty
        , contextResolvingRefs = Set.empty
        , contextRecursionDepth = 0
        }
  in validateValueWithContext ctx schema val

-- | Validate with explicit context
validateValueWithContext :: ValidationContext -> Schema -> Value -> ValidationResult
validateValueWithContext ctx schema val =
  -- Note: Cycle detection is handled by contextResolvingRefs in validateAgainstObject
  -- We don't use contextVisitedSchemas here because schemas can be legitimately
  -- referenced multiple times (e.g., a schema referencing itself via $ref)
  let ctxWithBase = applySchemaContext ctx schema
  in case schemaCore schema of
       BooleanSchema True -> ValidationSuccess mempty
       BooleanSchema False -> ValidationFailure $ ValidationErrors $ pure $
         ValidationError "schema" emptyPointer emptyPointer "Schema is 'false'" Nothing
       ObjectSchema obj ->
         -- First validate against standard keywords, then custom keywords
         combineResults
           [ validateAgainstObject ctx ctxWithBase schema obj val
           , validateCustomKeywords ctxWithBase schema val
           ]
  where
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        [] -> ValidationSuccess $ mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es

-- | Get the effective vocabularies for a schema based on its metaschema.
-- Returns Nothing if all vocabularies should be active (pre-2019-09 or no vocabulary restrictions).
-- Returns Just (set of vocab URIs) if vocabularies are explicitly declared.
getEffectiveVocabularies :: ValidationContext -> Schema -> Maybe (Set Text)
getEffectiveVocabularies ctx schema =
  case schemaVersion schema of
    -- Pre-2019-09 drafts don't have $vocabulary concept - all keywords active
    Just Draft04 -> Nothing
    Just Draft06 -> Nothing
    Just Draft07 -> Nothing
    -- 2019-09 and later support $vocabulary
    Just Draft201909 -> getVocabulariesFrom201909 ctx schema
    Just Draft202012 -> getVocabulariesFrom202012 ctx schema
    Nothing -> Nothing  -- No version, assume all vocabularies
  where
    getVocabulariesFrom201909 ctx' schema' =
      case schemaVocabulary schema' of
        Just vocabMap -> Just $ Set.fromList (Map.keys vocabMap)
        Nothing -> getVocabulariesFromMetaschema ctx' schema'

    getVocabulariesFrom202012 ctx' schema' =
      case schemaVocabulary schema' of
        Just vocabMap -> Just $ Set.fromList (Map.keys vocabMap)
        Nothing -> getVocabulariesFromMetaschema ctx' schema'

    -- Look up the metaschema in the registry and extract its vocabularies
    getVocabulariesFromMetaschema ctx' schema' =
      case schemaMetaschemaURI schema' of
        Nothing -> Nothing  -- No custom metaschema
        Just metaURI ->
          case Map.lookup metaURI (registrySchemas $ contextSchemaRegistry ctx') of
            Nothing -> Nothing  -- Metaschema not found in registry
            Just metaschema ->
              case schemaVocabulary metaschema of
                Just vocabMap -> Just $ Set.fromList (Map.keys vocabMap)
                Nothing -> Nothing  -- Metaschema has no vocabulary declaration

-- | Check if a vocabulary is active for validation.
-- Returns True if the vocabulary should be used, False if it should be ignored.
isVocabularyActive :: ValidationContext -> Schema -> Text -> Bool
isVocabularyActive ctx schema vocabURI =
  case getEffectiveVocabularies ctx schema of
    Nothing -> True  -- No restrictions, all vocabularies active
    Just vocabs -> Set.member vocabURI vocabs

-- | Validation vocabulary URIs for 2019-09 and 2020-12
validationVocabularyURI201909 :: Text
validationVocabularyURI201909 = "https://json-schema.org/draft/2019-09/vocab/validation"

validationVocabularyURI202012 :: Text
validationVocabularyURI202012 = "https://json-schema.org/draft/2020-12/vocab/validation"

-- | Check if validation vocabulary is active for a schema
-- Uses the root schema from context to determine vocabulary restrictions
isValidationVocabularyActive :: ValidationContext -> Schema -> Bool
isValidationVocabularyActive ctx _schema =
  -- Use the root schema for vocabulary checking, as sub-schemas inherit
  -- vocabulary restrictions from their root
  case contextRootSchema ctx of
    Nothing -> True  -- No root schema, assume validation is active
    Just rootSchema ->
      case schemaVersion rootSchema of
        Just Draft201909 -> isVocabularyActive ctx rootSchema validationVocabularyURI201909
        Just Draft202012 -> isVocabularyActive ctx rootSchema validationVocabularyURI202012
        _ -> True  -- Pre-2019-09 or no version: validation is always active

-- | Apply schema-specific context updates (base URI, root schema)
applySchemaContext :: ValidationContext -> Schema -> ValidationContext
applySchemaContext ctx schema =
  let parentBase = contextBaseURI ctx
      -- Check if the schema's $id has already been applied to the parent base
      -- This happens when following a $ref to a schema with a relative $id
      idAlreadyApplied = case (schemaId schema, parentBase) of
        (Just idText, Just baseText)
          -- If $id is relative and parent base ends with the same path, assume already applied
          | not (T.isPrefixOf "#" idText)
          , not (T.any (== ':') idText) -- No scheme, so it's relative
          , T.isSuffixOf idText baseText ->
              True
        _ -> False
      newBase = if idAlreadyApplied
                  then parentBase  -- Don't recompute, use existing base
                  else schemaEffectiveBase parentBase schema
      baseChanged = newBase /= parentBase
  in ctx
      { contextBaseURI = newBase
      , contextRootSchema =
          if baseChanged
            then Just schema
            else case contextRootSchema ctx of
                   Just existing -> Just existing
                   Nothing -> Just schema
      }

-- | Compile a validator for repeated use
compileValidator :: ValidationConfig -> Schema -> Either CompileError Validator
compileValidator config schema =
  Right $ Validator $ \val -> validateValue config schema val

-- | Create annotation for evaluated properties at current location
annotateProperties :: Set Text -> ValidationAnnotations
annotateProperties props =
  if Set.null props
    then mempty
    else ValidationAnnotations $ Map.singleton emptyPointer $ 
           Map.singleton "properties" (Aeson.Array $ fromList $ map Aeson.String $ Set.toList props)

-- | Create annotation for evaluated array items at current location
annotateItems :: Set Int -> ValidationAnnotations
annotateItems indices =
  if Set.null indices
    then mempty
    else ValidationAnnotations $ Map.singleton emptyPointer $
           Map.singleton "items" (Aeson.Array $ fromList $ map (Aeson.Number . fromIntegral) $ Set.toList indices)

-- | Shift all annotation pointers by prepending a prefix
-- Used when collecting annotations from child validations
shiftAnnotations :: JSONPointer -> ValidationAnnotations -> ValidationAnnotations
shiftAnnotations prefix (ValidationAnnotations annMap) =
  ValidationAnnotations $ Map.mapKeys (prefix <>) annMap

-- | Create a JSON Pointer for an array index
arrayIndexPointer :: Int -> JSONPointer
arrayIndexPointer idx = JSONPointer [T.pack (show idx)]

-- | Create a JSON Pointer for an object property
propertyPointer :: Text -> JSONPointer
propertyPointer prop = JSONPointer [prop]

-- | Validate against object schema
validateAgainstObject :: ValidationContext -> ValidationContext -> Schema -> SchemaObject -> Value -> ValidationResult
validateAgainstObject parentCtx ctx schema obj val =
  -- In 2019-09+, $ref and $dynamicRef are applicators alongside other keywords
  -- In earlier drafts, $ref short-circuits (only validates against ref, ignores siblings)
  let version = validationVersion (contextConfig ctx)
      refIsApplicator = version >= Draft201909
      -- Update context with dynamic scope BEFORE processing refs
      -- Push schema resources (schemas with $id) onto the scope
      -- For 2020-12: push schemas with $dynamicAnchor or schemas with $id
      -- For 2019-09: push schemas with $recursiveAnchor: true or schemas with $id
      shouldPushToScope = case version of
        Draft201909 -> schemaRecursiveAnchor obj == Just True || isJust (contextBaseURI ctx)
        _ -> isJust (schemaDynamicAnchor obj) || isJust (contextBaseURI ctx)
      ctxWithDynamicScope =
        if shouldPushToScope
          then
            let schema' = Schema
                  { schemaVersion = Just version
                  , schemaMetaschemaURI = Nothing
                  , schemaId = contextBaseURI ctx
                  , schemaCore = ObjectSchema obj
                  , schemaVocabulary = Nothing
                  , schemaExtensions = Map.empty
                  , schemaRawKeywords = Map.empty  -- No raw keywords for dynamically constructed schema
                  }
            in ctx { contextDynamicScope = schema' : contextDynamicScope ctx }
          else ctx
  in if refIsApplicator
    then
      -- 2019-09+: $ref/$dynamicRef are applicators, combine with other keywords
      -- We need to pass ref annotations to content validation so unevaluatedProperties
      -- can see properties evaluated by the ref
      -- Use ctxWithDynamicScope so refs can see this schema in dynamic scope
      let refResult = validateRef parentCtx ctxWithDynamicScope obj val
          refAnnotations = case refResult of
            ValidationSuccess anns -> anns
            ValidationFailure _ -> mempty
          contentResult = validateObjectSchemaContentWithRefAnnotations ctxWithDynamicScope obj val refAnnotations
      in combineResults [refResult, contentResult]
    else
      -- Pre-2019-09: $ref short-circuits, ignores sibling keywords
      case schemaRef obj of
        Just ref ->
          -- RECURSIVE REFERENCE FIX: Same depth-based cycle detection for pre-2019-09 $ref
          if contextRecursionDepth ctx >= 100
            then ValidationSuccess mempty  -- Break recursion at depth limit
            else
              let ctxWithDepth = ctx { contextRecursionDepth = contextRecursionDepth ctx + 1 }
              in case resolveReference ref refCtx of
                Just (resolvedSchema, maybeBase, maybeRootForRefs) ->
                  let ctxForRef = case maybeBase of
                        Just baseDoc ->
                          let baseChanged = contextBaseURI ctxWithDepth /= Just baseDoc
                              -- When base URI changes (remote ref), use the root schema for refs
                              -- This is either the base schema (for fragments) or resolved schema
                              rootValue =
                                if baseChanged
                                  then maybeRootForRefs `mplus` Just resolvedSchema
                                  else case contextRootSchema ctxWithDepth of
                                         Just existing -> Just existing
                                         Nothing -> maybeRootForRefs `mplus` Just resolvedSchema
                          in ctxWithDepth { contextBaseURI = Just baseDoc
                                          , contextRootSchema = rootValue
                                          -- Preserve dynamic scope when following $ref
                                          , contextDynamicScope = contextDynamicScope ctxWithDepth
                                          }
                        Nothing -> ctxWithDepth { contextRootSchema = maybeRootForRefs `mplus` contextRootSchema ctxWithDepth
                                                -- Preserve dynamic scope when following $ref
                                                , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                }
                      -- Update context if crossing draft boundaries
                      ctxForRefWithVersion = updateContextForCrossDraftRef ctxForRef resolvedSchema
                  in validateValueWithContext ctxForRefWithVersion resolvedSchema val
                Nothing ->
                  ValidationFailure $ ValidationErrors $ pure $
                    ValidationError "$ref" emptyPointer emptyPointer
                      ("Unable to resolve reference: " <> showReference ref)
                      Nothing
        Nothing -> validateObjectSchemaContent ctx obj val
  where
    refCtx =
      let inheritedBase = contextBaseURI parentCtx
          currentRoot =
            case contextRootSchema ctx of
              Just root -> Just root
              Nothing -> contextRootSchema parentCtx
      in ctx
           { contextBaseURI = inheritedBase
           , contextRootSchema = currentRoot
           }

    -- Validate $ref and $dynamicRef (treated as applicators in 2019-09+)
    validateRef parentCtx' ctx' obj' val' =
      case schemaRef obj' of
        Just ref ->
          -- RECURSIVE REFERENCE FIX: Check recursion depth limit (prevent infinite recursion)
          -- Previously used Set-based cycle detection which was too aggressive and broke valid
          -- recursive schemas like: tree → node → tree → node → ...
          --
          -- Now using depth-based approach (limit: 100) which allows recursive validation
          -- of nested data structures while still preventing truly infinite recursion.
          -- See detailed design note in Types.hs for ValidationContext.contextRecursionDepth
          if contextRecursionDepth ctx' >= 100
            then ValidationSuccess mempty  -- Break recursion at depth limit
            else
              let ctxWithDepth = ctx' { contextRecursionDepth = contextRecursionDepth ctx' + 1 }
              in case resolveReference ref refCtx of
                Just (resolvedSchema, maybeBase, maybeRootForRefs) ->
                  let ctxForRef = case maybeBase of
                        Just baseDoc ->
                          let baseChanged = contextBaseURI ctxWithDepth /= Just baseDoc
                              -- When base URI changes (remote ref), use the root schema for refs
                              -- This is either the base schema (for fragments) or resolved schema
                              rootValue =
                                if baseChanged
                                  then maybeRootForRefs `mplus` Just resolvedSchema
                                  else case contextRootSchema ctxWithDepth of
                                         Just existing -> Just existing
                                         Nothing -> maybeRootForRefs `mplus` Just resolvedSchema
                          in ctxWithDepth { contextBaseURI = Just baseDoc
                                          , contextRootSchema = rootValue
                                          -- Preserve dynamic scope when following $ref
                                          , contextDynamicScope = contextDynamicScope ctxWithDepth
                                          }
                        Nothing -> ctxWithDepth { contextRootSchema = maybeRootForRefs `mplus` contextRootSchema ctxWithDepth
                                                -- Preserve dynamic scope when following $ref
                                                , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                }
                      -- Update context if crossing draft boundaries
                      ctxForRefWithVersion = updateContextForCrossDraftRef ctxForRef resolvedSchema
                  in validateValueWithContext ctxForRefWithVersion resolvedSchema val'
                Nothing ->
                  ValidationFailure $ ValidationErrors $ pure $
                    ValidationError "$ref" emptyPointer emptyPointer
                      ("Unable to resolve reference: " <> showReference ref)
                      Nothing
        Nothing ->
          -- No $ref, check for $dynamicRef (2020-12+)
          case schemaDynamicRef obj' of
            Just dynRef ->
              -- RECURSIVE REFERENCE FIX: Same depth-based cycle detection for $dynamicRef
              if contextRecursionDepth ctx' >= 100
                then ValidationSuccess mempty  -- Break recursion at depth limit
                else
                  let ctxWithDepth = ctx' { contextRecursionDepth = contextRecursionDepth ctx' + 1 }
                  in case resolveDynamicRef dynRef ctxWithDepth of
                    Just resolvedSchema ->
                      validateValueWithContext ctxWithDepth resolvedSchema val'
                    Nothing ->
                      -- Fallback: treat as regular $ref (resolve statically)
                      case resolveReference dynRef refCtx of
                        Just (resolvedSchema, maybeBase, maybeRootForRefs) ->
                          let ctxForRef = case maybeBase of
                                Just baseDoc ->
                                  let baseChanged = contextBaseURI ctxWithDepth /= Just baseDoc
                                      -- When base URI changes (remote ref), use the root schema for refs
                                      rootValue =
                                        if baseChanged
                                          then maybeRootForRefs `mplus` Just resolvedSchema
                                          else case contextRootSchema ctxWithDepth of
                                                 Just existing -> Just existing
                                                 Nothing -> maybeRootForRefs `mplus` Just resolvedSchema
                                      -- For remote refs with relative $id, keep parent base to avoid
                                      -- double-applying the $id during applySchemaContext
                                      baseToSet =
                                        if baseChanged && schemaId resolvedSchema /= Nothing
                                          then contextBaseURI ctxWithDepth  -- Keep parent base
                                          else Just baseDoc
                                  in ctxWithDepth { contextBaseURI = baseToSet
                                                  , contextRootSchema = rootValue
                                                  -- Preserve dynamic scope when following $dynamicRef fallback
                                                  , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                  }
                                Nothing -> ctxWithDepth { contextRootSchema = maybeRootForRefs `mplus` contextRootSchema ctxWithDepth
                                                        -- Preserve dynamic scope when following $dynamicRef fallback
                                                        , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                      }
                              -- Update context if crossing draft boundaries
                              ctxForRefWithVersion = updateContextForCrossDraftRef ctxForRef resolvedSchema
                          in validateValueWithContext ctxForRefWithVersion resolvedSchema val'
                        Nothing ->
                          ValidationFailure $ ValidationErrors $ pure $
                            ValidationError "$dynamicRef" emptyPointer emptyPointer
                              ("Unable to resolve dynamic reference: " <> showReference dynRef)
                              Nothing
            Nothing ->
              -- No $dynamicRef, check for $recursiveRef (2019-09)
              case schemaRecursiveRef obj' of
                Just recRef ->
                  -- Handle $recursiveRef similar to $dynamicRef but with 2019-09 semantics
                  if contextRecursionDepth ctx' >= 100
                    then ValidationSuccess mempty  -- Break recursion at depth limit
                    else
                      let ctxWithDepth = ctx' { contextRecursionDepth = contextRecursionDepth ctx' + 1 }
                      in case resolveRecursiveRef recRef ctxWithDepth of
                        Just resolvedSchema ->
                          validateValueWithContext ctxWithDepth resolvedSchema val'
                        Nothing ->
                          -- Fallback: treat as regular $ref using current context (not parent)
                          case resolveReference recRef ctxWithDepth of
                            Just (resolvedSchema, maybeBase, maybeRootForRefs) ->
                              let ctxForRef = case maybeBase of
                                    Just baseDoc ->
                                      let baseChanged = contextBaseURI ctxWithDepth /= Just baseDoc
                                          rootValue =
                                            if baseChanged
                                              then maybeRootForRefs `mplus` Just resolvedSchema
                                              else case contextRootSchema ctxWithDepth of
                                                     Just existing -> Just existing
                                                     Nothing -> maybeRootForRefs `mplus` Just resolvedSchema
                                          baseToSet =
                                            if baseChanged && schemaId resolvedSchema /= Nothing
                                              then contextBaseURI ctxWithDepth
                                              else Just baseDoc
                                      in ctxWithDepth { contextBaseURI = baseToSet
                                                      , contextRootSchema = rootValue
                                                      , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                      }
                                    Nothing -> ctxWithDepth { contextRootSchema = maybeRootForRefs `mplus` contextRootSchema ctxWithDepth
                                                            , contextDynamicScope = contextDynamicScope ctxWithDepth
                                                            }
                                  -- Update context if crossing draft boundaries
                                  ctxForRefWithVersion = updateContextForCrossDraftRef ctxForRef resolvedSchema
                              in validateValueWithContext ctxForRefWithVersion resolvedSchema val'
                            Nothing ->
                              ValidationFailure $ ValidationErrors $ pure $
                                ValidationError "$recursiveRef" emptyPointer emptyPointer
                                  ("Unable to resolve recursive reference: " <> showReference recRef)
                                  Nothing
                Nothing -> ValidationSuccess mempty  -- No ref at all

    combineResults :: [ValidationResult] -> ValidationResult
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        [] -> ValidationSuccess $ mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es

    -- Validate basic validation keywords using pluggable keyword system
    -- Handles: type, enum, const, numeric (multipleOf, maximum, exclusiveMaximum, minimum, exclusiveMinimum),
    --          string (maxLength, minLength, pattern)
    -- Note: Array and object keywords are handled separately due to their complexity
    -- 
    -- Falls back to old validation functions for:
    -- - Draft-04 schemas (different exclusiveMinimum/exclusiveMaximum semantics)
    -- - Manually constructed schemas (empty schemaRawKeywords)
    validateBasicKeywords :: ValidationContext -> Schema -> SchemaObject -> Value -> ValidationResult
    validateBasicKeywords ctx schema obj val =
      let rawKeywords = schemaRawKeywords schema
          schemaVersion' = schemaVersion schema
          schemaRegistry = contextSchemaRegistry ctx
          registryMap = registrySchemas schemaRegistry  -- Extract Map Text Schema from SchemaRegistry
          
          -- Select appropriate keyword registry based on schema version
          keywordRegistry = case schemaVersion' of
            Just Draft04 -> draft04Registry
            Just Draft06 -> draft06Registry
            Just Draft07 -> draft07Registry
            Just Draft201909 -> draft201909Registry
            Just Draft202012 -> draft202012Registry
            _ -> standardKeywordRegistry  -- Default to standard (Draft-06+)
          
          -- Filter to only basic validation keywords (exclude array/object/composition keywords)
          basicKeywordNames = 
            [ "type", "enum", "const"
            , "multipleOf", "maximum", "exclusiveMaximum", "minimum", "exclusiveMinimum"
            , "maxLength", "minLength", "pattern"
            ]
          basicKeywords = Map.filterWithKey (\k _ -> k `elem` basicKeywordNames) rawKeywords
      in
        -- Fall back to old validation functions for manually constructed schemas (empty schemaRawKeywords)
        if Map.null rawKeywords
        then
          -- Fall back to old validation functions for backward compatibility
          combineResults
            [ KW.validateTypeConstraint obj val
            , KW.validateEnumConstraint obj val
            , KW.validateConstConstraint obj val
            , KW.validateNumericConstraints obj val
            , KW.validateStringConstraints ctx obj val
            ]
        else if Map.null basicKeywords
        then ValidationSuccess mempty
        else
          -- Use pluggable keyword system with version-appropriate registry
          -- Build compilation context
          let compilationCtx = buildCompilationContext registryMap keywordRegistry schema []
              -- Get keyword definitions from registry
              keywordDefs = keywordMap keywordRegistry
              -- Compile keywords
              compiledResult = compileKeywords keywordDefs basicKeywords schema compilationCtx
          in case compiledResult of
            Left err -> validationFailure "compilation" $ "Failed to compile keywords: " <> err
            Right compiled ->
              -- Build keyword validation context (just paths for now)
              let keywordCtx = KeywordValidate.buildValidationContext [] []
                  -- Validate using compiled keywords
                  errors = KeywordValidate.validateKeywords compiled val keywordCtx
              in if null errors
                then ValidationSuccess mempty
                else ValidationFailure $ ValidationErrors $ NE.fromList $
                  map (\msg -> ValidationError "keyword" emptyPointer emptyPointer msg Nothing) errors

    -- Validate object schema content without ref annotations (for pre-2019-09 or when no ref)
    validateObjectSchemaContent ctx' obj' val' =
      validateObjectSchemaContentWithRefAnnotations ctx' obj' val' mempty

    -- Validate object schema content with additional annotations from refs
    -- Note: Dynamic scope has already been updated in validateAgainstObject
    validateObjectSchemaContentWithRefAnnotations ctx' obj' val' refAnns =
      let ctx'' = ctx'  -- Dynamic scope already set up
          -- Check if validation vocabulary is active
          validationActive = isValidationVocabularyActive ctx'' schema
          -- Validate all keywords EXCEPT unevaluated ones
          -- Only apply validation keywords if validation vocabulary is active
          results =
            (if validationActive
             then [ validateBasicKeywords ctx'' schema obj' val'
                  , KW.validateStringConstraints ctx'' obj' val'  -- Content encoding/media type validation
                  ]
             else [])
            ++
            [ validateComposition ctx'' obj' val'
            , validateConditional ctx'' obj' val'
            , KW.validateFormatConstraints ctx'' obj' val'
            , validateArrayConstraintsWithoutUnevaluated ctx'' obj' val'
            , validateObjectConstraintsWithoutUnevaluated ctx'' obj' val'
            ]
          -- Combine results to get accumulated annotations (including ref annotations)
          combinedResult = combineResults results
          combinedWithRefs = case combinedResult of
            ValidationSuccess anns -> ValidationSuccess (refAnns <> anns)
            ValidationFailure errs -> ValidationFailure errs
      in case combinedWithRefs of
           ValidationSuccess anns ->
             -- Now validate unevaluated keywords with accumulated annotations (including from refs)
             let unevalResults =
                   [ validateUnevaluatedForArray ctx'' obj' val' anns
                   , validateUnevaluatedForObject ctx'' obj' val' anns
                   ]
                 unevalCombined = combineResults unevalResults
             in case unevalCombined of
                  ValidationSuccess unevalAnns -> ValidationSuccess (anns <> unevalAnns)
                  ValidationFailure errs -> ValidationFailure errs
           ValidationFailure errs -> ValidationFailure errs
      where
        validateUnevaluatedForArray ctx''' schemaObj (Array arr) accAnns =
          validateUnevaluatedItems ctx''' schemaObj arr accAnns
        validateUnevaluatedForArray _ _ _ _ = ValidationSuccess mempty

        validateUnevaluatedForObject ctx''' schemaObj (Object objMap) accAnns =
          validateUnevaluatedProperties ctx''' schemaObj objMap accAnns
        validateUnevaluatedForObject _ _ _ _ = ValidationSuccess mempty

-- | Validate composition keywords (allOf, anyOf, oneOf, not)
validateComposition :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateComposition ctx obj val =
  combineResults
    [ maybe (ValidationSuccess mempty) (\schemas -> AllOf.validateAllOf validator schemas val) (schemaAllOf obj)
    , maybe (ValidationSuccess mempty) (\schemas -> AnyOf.validateAnyOf validator schemas val) (schemaAnyOf obj)
    , maybe (ValidationSuccess mempty) (\schemas -> OneOf.validateOneOf validator schemas val) (schemaOneOf obj)
    , maybe (ValidationSuccess mempty) (\schema -> Not.validateNot validator schema val) (schemaNot obj)
    ]
  where
    -- Validator function that captures the context
    validator schema value = validateValueWithContext ctx schema value

    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        [] -> ValidationSuccess $ mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es

-- | Validate conditional keywords (if/then/else, draft-07+)
validateConditional :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateConditional ctx obj val = case schemaIf obj of
  Nothing -> ValidationSuccess mempty  -- No conditional
  Just ifSchema ->
    Cond.validateConditional validator ifSchema (schemaThen obj) (schemaElse obj) val
  where
    -- Validator function that captures the context
    validator schema value = validateValueWithContext ctx schema value

-- | Validate array constraints WITHOUT unevaluatedItems (for use when collecting annotations)
validateArrayConstraintsWithoutUnevaluated :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateArrayConstraintsWithoutUnevaluated ctx obj (Array arr) =
  let validation = schemaValidation obj
      arrLength = length arr
      itemValidations =
        [ maybe (ValidationSuccess mempty) (\max' ->
            if fromIntegral arrLength <= max'
              then ValidationSuccess mempty
              else validationFailure "maxItems" "Array length exceeds maxItems"
          ) (validationMaxItems validation)
        , maybe (ValidationSuccess mempty) (\min' ->
            if fromIntegral arrLength >= min'
              then ValidationSuccess mempty
              else validationFailure "minItems" "Array length below minItems"
          ) (validationMinItems validation)
        , validateItems ctx obj arr
        , validateContains ctx obj arr
        , validateUniqueItems obj arr
        ]
  in combineResults itemValidations
  where
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        [] -> ValidationSuccess $ mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es

    validateItems ctx' schemaObj array =
      -- In 2020-12+, prefixItems takes precedence for tuple validation
      -- But only apply prefixItems if we're validating against 2020-12 or later
      let currentVersion = validationVersion (contextConfig ctx')
          isPrefixItemsSupported = currentVersion >= Draft202012
      in case validationPrefixItems (schemaValidation schemaObj) of
        Just prefixSchemas | isPrefixItemsSupported ->
          -- 2020-12+ mode: prefixItems validates positional items
          let prefixResults = zipWith (validateValueWithContext ctx') (NE.toList prefixSchemas) (toList array)
              prefixFailures = [errs | ValidationFailure errs <- prefixResults]
              prefixIndices = Set.fromList [0 .. length prefixSchemas - 1]
              -- Shift annotations from prefixItems validations to correct instance locations
              prefixAnnotations =
                [ shiftAnnotations (arrayIndexPointer idx) anns
                | (idx, ValidationSuccess anns) <- zip [0..] prefixResults
                ]
              -- In 2020-12, "items" applies to items beyond prefixItems
              additionalItemsFromPrefix = drop (length prefixSchemas) (toList array)
              additionalStartIdx = length prefixSchemas
              (additionalResults, additionalIndices, additionalAnnotations) = case validationItems (schemaValidation schemaObj) of
                Just (ItemsSchema itemSchema) ->
                  let results = zipWith (\idx item -> (idx, validateValueWithContext ctx' itemSchema item))
                                  [additionalStartIdx..] additionalItemsFromPrefix
                      indices = if null additionalItemsFromPrefix
                                then Set.empty
                                else Set.fromList [additionalStartIdx .. length array - 1]
                      -- Shift annotations from items validations
                      anns = [ shiftAnnotations (arrayIndexPointer idx) ann
                             | (idx, ValidationSuccess ann) <- results
                             ]
                  in (map snd results, indices, anns)
                _ -> ([], Set.empty, [])  -- No items schema, don't annotate additional items
              additionalFailures = [errs | ValidationFailure errs <- additionalResults]
              allFailures = prefixFailures <> additionalFailures
              allIndices = prefixIndices <> additionalIndices
              allAnnotations = prefixAnnotations <> additionalAnnotations
          in case allFailures of
            [] -> ValidationSuccess $ annotateItems allIndices <> mconcat allAnnotations
            (e:es) -> ValidationFailure $ foldl (<>) e es
        Nothing ->
          -- Pre-2020-12 mode: use old items behavior
          case validationItems (schemaValidation schemaObj) of
            Nothing -> ValidationSuccess mempty
            Just (ItemsSchema itemSchema) ->
              -- All items must validate against the schema
              let results = zipWith (\idx item -> (idx, validateValueWithContext ctx' itemSchema item))
                              [0..] (toList array)
                  failures = [errs | (_, ValidationFailure errs) <- results]
                  allIndices = Set.fromList [0 .. length array - 1]
                  -- Shift annotations from each item validation
                  annotations = [ shiftAnnotations (arrayIndexPointer idx) anns
                                | (idx, ValidationSuccess anns) <- results
                                ]
              in case failures of
                [] -> ValidationSuccess $ annotateItems allIndices <> mconcat annotations
                (e:es) -> ValidationFailure $ foldl (<>) e es
            Just (ItemsTuple tupleSchemas maybeAdditional) ->
              -- Positional validation + optional additional items
              let tupleResults = zipWith (validateValueWithContext ctx') (NE.toList tupleSchemas) (toList array)
                  tupleFailures = [errs | ValidationFailure errs <- tupleResults]
                  tupleIndices = Set.fromList [0 .. length tupleSchemas - 1]
                  -- Shift annotations from tuple validations
                  tupleAnnotations =
                    [ shiftAnnotations (arrayIndexPointer idx) anns
                    | (idx, ValidationSuccess anns) <- zip [0..] tupleResults
                    ]
                  -- Handle additional items beyond tuple length
                  additionalItems = drop (length tupleSchemas) (toList array)
                  additionalStartIdx = length tupleSchemas
                  -- In draft 2019-09/2020-12, additional items are allowed by default when items is an array
                  -- They are only rejected if additionalItems is explicitly false
                  (additionalResults, additionalIndices, additionalAnnotations) = case maybeAdditional of
                    Just addlSchema ->
                      -- additionalItems is explicitly set, validate against it
                      let results = zipWith (\idx item -> (idx, validateValueWithContext ctx' addlSchema item))
                                      [additionalStartIdx..] additionalItems
                          indices = Set.fromList [additionalStartIdx .. length array - 1]
                          anns = [ shiftAnnotations (arrayIndexPointer idx) ann
                                 | (idx, ValidationSuccess ann) <- results
                                 ]
                      in (map snd results, if null additionalItems then Set.empty else indices, anns)
                    Nothing ->
                      -- additionalItems not specified - don't validate or annotate (let unevaluatedItems handle it)
                      ([], Set.empty, [])
                  additionalFailures = [errs | ValidationFailure errs <- additionalResults]
                  allFailures = tupleFailures <> additionalFailures
                  allIndices = tupleIndices <> additionalIndices
                  allAnnotations = tupleAnnotations <> additionalAnnotations
              in case allFailures of
                [] -> ValidationSuccess $ annotateItems allIndices <> mconcat allAnnotations
                (e:es) -> ValidationFailure $ foldl (<>) e es

    validateContains ctx' schemaObj array =
      let validation = schemaValidation schemaObj
          maybeContains = validationContains validation
          maybeMinContains = validationMinContains validation
          maybeMaxContains = validationMaxContains validation
      in case maybeContains of
        Nothing ->
          -- No contains constraint: minContains and maxContains are ignored per spec
          ValidationSuccess mempty
        Just containsSchema ->
          let results = [(idx, validateValueWithContext ctx' containsSchema item) | (idx, item) <- zip [0..] (toList array)]
              matchingIndices = Set.fromList [idx | (idx, result) <- results, isSuccess result]
              matchCount = Set.size matchingIndices
              -- When contains is present, minContains defaults to 1 (not 0)
              minRequired = maybe 1 fromIntegral maybeMinContains
              maxAllowed = fmap fromIntegral maybeMaxContains
              minCheck = matchCount >= minRequired
              maxCheck = maybe True (matchCount <=) maxAllowed
          in if not minCheck
            then validationFailure "contains" $
              "Array has " <> T.pack (show matchCount) <> " items matching contains, but minContains requires " <> T.pack (show minRequired)
            else if not maxCheck
              then validationFailure "maxContains" $
                "Array has " <> T.pack (show matchCount) <> " items matching contains, but maxContains allows at most " <> T.pack (show (fromJust maxAllowed))
              else ValidationSuccess $ annotateItems matchingIndices

    validateUniqueItems schemaObj array = case validationUniqueItems (schemaValidation schemaObj) of
      Nothing -> ValidationSuccess mempty
      Just True ->
        let items = toList array
            uniqueItems = length items == length (nubOrd items)
        in if uniqueItems
          then ValidationSuccess mempty
          else validationFailure "uniqueItems" "Array contains duplicate items"
      Just False -> ValidationSuccess mempty

    -- Simple deduplication using Ord (works for most JSON values)
    nubOrd :: Ord a => [a] -> [a]
    nubOrd = Set.toList . Set.fromList
validateArrayConstraintsWithoutUnevaluated _ _ _ = ValidationSuccess mempty

-- | Validate array constraints
validateArrayConstraints :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateArrayConstraints ctx obj (Array arr) =
  let validation = schemaValidation obj
      arrLength = length arr
      -- First run item validators to collect annotations
      itemValidations =
        [ maybe (ValidationSuccess mempty) (\max' ->
            if fromIntegral arrLength <= max'
              then ValidationSuccess mempty
              else validationFailure "maxItems" "Array length exceeds maxItems"
          ) (validationMaxItems validation)
        , maybe (ValidationSuccess mempty) (\min' ->
            if fromIntegral arrLength >= min'
              then ValidationSuccess mempty
              else validationFailure "minItems" "Array length below minItems"
          ) (validationMinItems validation)
        , validateItems ctx obj arr
        , validateContains ctx obj arr
        , validateUniqueItems obj arr
        ]
      -- Combine results to get annotations from item validators
      itemResult = combineResults itemValidations
      -- Then run unevaluatedItems with those annotations
      unevaluatedResult = case itemResult of
        ValidationSuccess anns ->
          validateUnevaluatedItems ctx obj arr anns
        ValidationFailure _ -> ValidationSuccess mempty  -- If items failed, skip unevaluated check
  in case (itemResult, unevaluatedResult) of
    (ValidationFailure errs, _) -> ValidationFailure errs
    (ValidationSuccess anns, ValidationSuccess unevalAnns) -> 
      ValidationSuccess (anns <> unevalAnns)
    (ValidationSuccess _, ValidationFailure errs) -> ValidationFailure errs
  where
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        [] -> ValidationSuccess $ mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validateItems ctx' schemaObj array =
      -- In 2020-12+, prefixItems takes precedence for tuple validation
      -- But only apply prefixItems if we're validating against 2020-12 or later
      let currentVersion = validationVersion (contextConfig ctx')
          isPrefixItemsSupported = currentVersion >= Draft202012
      in case validationPrefixItems (schemaValidation schemaObj) of
        Just prefixSchemas | isPrefixItemsSupported ->
          -- 2020-12+ mode: prefixItems validates positional items
          let prefixResults = zipWith (validateValueWithContext ctx') (NE.toList prefixSchemas) (toList array)
              prefixFailures = [errs | ValidationFailure errs <- prefixResults]
              prefixIndices = Set.fromList [0 .. length prefixSchemas - 1]
              -- In 2020-12, "items" applies to items beyond prefixItems
              additionalItemsFromPrefix = drop (length prefixSchemas) (toList array)
              additionalStartIdx = length prefixSchemas
              (additionalResults, additionalIndices) = case validationItems (schemaValidation schemaObj) of
                Just (ItemsSchema itemSchema) ->
                  let results = [validateValueWithContext ctx' itemSchema item | item <- additionalItemsFromPrefix]
                      indices = if null additionalItemsFromPrefix 
                                then Set.empty 
                                else Set.fromList [additionalStartIdx .. length array - 1]
                  in (results, indices)
                _ -> ([], Set.empty)  -- No items schema, don't annotate additional items
              additionalFailures = [errs | ValidationFailure errs <- additionalResults]
              allFailures = prefixFailures <> additionalFailures
              allIndices = prefixIndices <> additionalIndices
              allAnnotations = [anns | ValidationSuccess anns <- prefixResults <> additionalResults]
          in case allFailures of
            [] -> ValidationSuccess $ annotateItems allIndices <> mconcat allAnnotations
            (e:es) -> ValidationFailure $ foldl (<>) e es
        Nothing ->
          -- Pre-2020-12 mode: use old items behavior
          case validationItems (schemaValidation schemaObj) of
            Nothing -> ValidationSuccess mempty
            Just (ItemsSchema itemSchema) ->
              -- All items must validate against the schema
              let results = zipWith (\idx item -> (idx, validateValueWithContext ctx' itemSchema item))
                              [0..] (toList array)
                  failures = [errs | (_, ValidationFailure errs) <- results]
                  allIndices = Set.fromList [0 .. length array - 1]
                  -- Shift annotations from each item validation
                  annotations = [ shiftAnnotations (arrayIndexPointer idx) anns
                                | (idx, ValidationSuccess anns) <- results
                                ]
              in case failures of
                [] -> ValidationSuccess $ annotateItems allIndices <> mconcat annotations
                (e:es) -> ValidationFailure $ foldl (<>) e es
            Just (ItemsTuple tupleSchemas maybeAdditional) ->
              -- Positional validation + optional additional items
              let tupleResults = zipWith (validateValueWithContext ctx') (NE.toList tupleSchemas) (toList array)
                  tupleFailures = [errs | ValidationFailure errs <- tupleResults]
                  tupleIndices = Set.fromList [0 .. length tupleSchemas - 1]
                  -- Shift annotations from tuple validations
                  tupleAnnotations =
                    [ shiftAnnotations (arrayIndexPointer idx) anns
                    | (idx, ValidationSuccess anns) <- zip [0..] tupleResults
                    ]
                  -- Handle additional items beyond tuple length
                  additionalItems = drop (length tupleSchemas) (toList array)
                  additionalStartIdx = length tupleSchemas
                  -- In draft 2019-09/2020-12, additional items are allowed by default when items is an array
                  -- They are only rejected if additionalItems is explicitly false
                  (additionalResults, additionalIndices, additionalAnnotations) = case maybeAdditional of
                    Just addlSchema ->
                      -- additionalItems is explicitly set, validate against it
                      let results = zipWith (\idx item -> (idx, validateValueWithContext ctx' addlSchema item))
                                      [additionalStartIdx..] additionalItems
                          indices = Set.fromList [additionalStartIdx .. length array - 1]
                          anns = [ shiftAnnotations (arrayIndexPointer idx) ann
                                 | (idx, ValidationSuccess ann) <- results
                                 ]
                      in (map snd results, if null additionalItems then Set.empty else indices, anns)
                    Nothing ->
                      -- additionalItems not specified - don't validate or annotate (let unevaluatedItems handle it)
                      ([], Set.empty, [])
                  additionalFailures = [errs | ValidationFailure errs <- additionalResults]
                  allFailures = tupleFailures <> additionalFailures
                  allIndices = tupleIndices <> additionalIndices
                  allAnnotations = tupleAnnotations <> additionalAnnotations
              in case allFailures of
                [] -> ValidationSuccess $ annotateItems allIndices <> mconcat allAnnotations
                (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validateContains ctx' schemaObj array =
      let validation = schemaValidation schemaObj
          maybeContains = validationContains validation
          maybeMinContains = validationMinContains validation
          maybeMaxContains = validationMaxContains validation
      in case maybeContains of
        Nothing ->
          -- No contains constraint: minContains and maxContains are ignored per spec
          ValidationSuccess mempty
        Just containsSchema ->
          let results = [(idx, validateValueWithContext ctx' containsSchema item) | (idx, item) <- zip [0..] (toList array)]
              matchingIndices = Set.fromList [idx | (idx, result) <- results, isSuccess result]
              matchCount = Set.size matchingIndices
              -- When contains is present, minContains defaults to 1 (not 0)
              minRequired = maybe 1 fromIntegral maybeMinContains
              maxAllowed = fmap fromIntegral maybeMaxContains
              minCheck = matchCount >= minRequired
              maxCheck = maybe True (matchCount <=) maxAllowed
          in if not minCheck
            then validationFailure "contains" $
              "Array has " <> T.pack (show matchCount) <> " items matching contains, but minContains requires " <> T.pack (show minRequired)
            else if not maxCheck
              then validationFailure "maxContains" $
                "Array has " <> T.pack (show matchCount) <> " items matching contains, but maxContains allows at most " <> T.pack (show (fromJust maxAllowed))
              else ValidationSuccess $ annotateItems matchingIndices
    
    validateUniqueItems schemaObj array = case validationUniqueItems (schemaValidation schemaObj) of
      Nothing -> ValidationSuccess mempty
      Just True ->
        let items = toList array
            uniqueItems = length items == length (nubOrd items)
        in if uniqueItems
          then ValidationSuccess mempty
          else validationFailure "uniqueItems" "Array contains duplicate items"
      Just False -> ValidationSuccess mempty
    
    -- Simple deduplication using Ord (works for most JSON values)
    nubOrd :: Ord a => [a] -> [a]
    nubOrd = Set.toList . Set.fromList
validateArrayConstraints _ _ _ = ValidationSuccess mempty

-- | Shared helper to validate object property constraints (without unevaluatedProperties)
-- This extracts the common logic used by both validateObjectConstraints and validateObjectConstraintsWithoutUnevaluated
validateObjectPropertyConstraints :: ValidationContext -> SchemaObject -> KeyMap.KeyMap Value -> ValidationResult
validateObjectPropertyConstraints ctx obj objMap =
  let validation = schemaValidation obj
      objSize = KeyMap.size objMap
      -- Run all property validators to collect annotations (NOT including unevaluatedProperties)
      propertyValidations =
        [ maybe (ValidationSuccess mempty) (\max' ->
            if fromIntegral objSize <= max'
              then ValidationSuccess mempty
              else validationFailure "maxProperties" "Object has too many properties"
          ) (validationMaxProperties validation)
        , maybe (ValidationSuccess mempty) (\min' ->
            if fromIntegral objSize >= min'
              then ValidationSuccess mempty
              else validationFailure "minProperties" "Object has too few properties"
          ) (validationMinProperties validation)
        , validateRequired obj objMap
        , validatePropertyNames ctx obj objMap
        , validateProperties ctx obj objMap
        , validateAdditionalProperties ctx obj objMap
        , validateDependencies ctx validation objMap
        , validateDependentRequired ctx validation objMap
        , validateDependentSchemas ctx validation objMap
        ]
  in combineResults propertyValidations
  where
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        [] -> ValidationSuccess $ mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validateRequired schemaObj om = case validationRequired (schemaValidation schemaObj) of
      Nothing -> ValidationSuccess mempty
      Just requiredProps ->
        let presentProps = Set.fromList [Key.toText k | k <- KeyMap.keys om]
            missingProps = Set.difference requiredProps presentProps
        in if Set.null missingProps
          then ValidationSuccess mempty
          else ValidationFailure $ ValidationErrors $ pure $
            ValidationError "required" emptyPointer emptyPointer
              ("Missing required properties: " <> T.intercalate ", " (Set.toList missingProps))
              Nothing
    
    validatePropertyNames ctx' schemaObj om = case validationPropertyNames (schemaValidation schemaObj) of
      Nothing -> ValidationSuccess mempty
      Just nameSchema ->
        -- Validate each property name (as a string) against the schema
        let propNames = [Key.toText k | k <- KeyMap.keys om]
            results = [validateValueWithContext ctx' nameSchema (Aeson.String propName) | propName <- propNames]
            failures = [errs | ValidationFailure errs <- results]
        in case failures of
          [] -> ValidationSuccess mempty
          (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validateProperties ctx' schemaObj om =
      let -- Get properties from 'properties' keyword
          maybePropSchemas = validationProperties (schemaValidation schemaObj)
          evaluatedProps = case maybePropSchemas of
            Nothing -> Set.empty
            Just propSchemas -> Set.fromList
              [ propName
              | propName <- Map.keys propSchemas
              , KeyMap.member (Key.fromText propName) om
              ]
          propertyResults = case maybePropSchemas of
            Nothing -> []
            Just propSchemas ->
              [ (propName, validateValueWithContext ctx' propSchema propValue)
              | (propName, propSchema) <- Map.toList propSchemas
              , Just propValue <- [KeyMap.lookup (Key.fromText propName) om]
              ]
          -- Shift annotations from property validations to correct locations
          propertyAnnotations =
            [ shiftAnnotations (propertyPointer propName) anns
            | (propName, ValidationSuccess anns) <- propertyResults
            ]

          -- Also check pattern properties (independent of 'properties' keyword)
          patternCoveredProps = case validationPatternProperties (schemaValidation schemaObj) of
            Nothing -> Set.empty
            Just patternSchemas -> Set.fromList
              [ Key.toText k
              | k <- KeyMap.keys om
              , let propName = Key.toText k
              , (Regex pattern, _) <- Map.toList patternSchemas
              , case Regex.compileRegex pattern of
                  Right regex -> Regex.matchRegex regex propName
                  Left _ -> False
              ]
          patternResultsList = case validationPatternProperties (schemaValidation schemaObj) of
            Nothing -> []
            Just patternSchemas ->
              [ (Key.toText k, validateValueWithContext ctx' patternSchema propValue)
              | (k, propValue) <- KeyMap.toList om
              , let propName = Key.toText k
              , (Regex pattern, patternSchema) <- Map.toList patternSchemas
              , case Regex.compileRegex pattern of
                  Right regex -> Regex.matchRegex regex propName
                  Left _ -> False
              ]
          -- Shift annotations from pattern property validations
          patternAnnotations =
            [ shiftAnnotations (propertyPointer propName) anns
            | (propName, ValidationSuccess anns) <- patternResultsList
            ]

          allResults = map snd propertyResults <> map snd patternResultsList
          allEvaluatedProps = evaluatedProps <> patternCoveredProps
          allAnnotations = propertyAnnotations <> patternAnnotations
          failures = [errs | ValidationFailure errs <- allResults]
      in case failures of
        [] -> ValidationSuccess $ annotateProperties allEvaluatedProps <> mconcat allAnnotations
        (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validateAdditionalProperties ctx' schemaObj om = case validationAdditionalProperties (schemaValidation schemaObj) of
      Nothing -> ValidationSuccess mempty
      Just addlSchema ->
        -- Properties covered by 'properties' keyword
        let definedProps = maybe Set.empty Map.keysSet (validationProperties $ schemaValidation schemaObj)

            -- Properties covered by 'patternProperties' keyword
            patternCoveredProps = case validationPatternProperties (schemaValidation schemaObj) of
              Nothing -> Set.empty
              Just patternSchemas -> Set.fromList
                [ Key.toText k
                | k <- KeyMap.keys om
                , let propName = Key.toText k
                , (Regex pattern, _) <- Map.toList patternSchemas
                , case Regex.compileRegex pattern of
                    Right regex -> Regex.matchRegex regex propName
                    Left _ -> False
                ]

            -- Additional properties are those not covered by either
            allCoveredProps = definedProps <> patternCoveredProps
            additionalPropsList = [(k, v) | (k, v) <- KeyMap.toList om
                              , not $ Set.member (Key.toText k) allCoveredProps]
            additionalPropNames = Set.fromList [Key.toText k | (k, _) <- additionalPropsList]
            results = [(Key.toText k, validateValueWithContext ctx' addlSchema v) | (k, v) <- additionalPropsList]
            -- Shift annotations from additional property validations
            annotations = [ shiftAnnotations (propertyPointer propName) anns
                          | (propName, ValidationSuccess anns) <- results
                          ]
            failures = [errs | (_, ValidationFailure errs) <- results]
        in case failures of
          [] -> ValidationSuccess $ annotateProperties additionalPropNames <> mconcat annotations
          (e:es) -> ValidationFailure $ foldl (<>) e es

-- | Validate object constraints WITHOUT unevaluatedProperties (for use when collecting annotations)
validateObjectConstraintsWithoutUnevaluated :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateObjectConstraintsWithoutUnevaluated ctx obj (Object objMap) =
  validateObjectPropertyConstraints ctx obj objMap
validateObjectConstraintsWithoutUnevaluated _ _ _ = ValidationSuccess mempty

-- | Validate object constraints (with unevaluatedProperties)
validateObjectConstraints :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateObjectConstraints ctx obj (Object objMap) =
  -- First validate property constraints
  let propertyResult = validateObjectPropertyConstraints ctx obj objMap
      -- Then run unevaluatedProperties with collected annotations
      unevaluatedResult = case propertyResult of
        ValidationSuccess anns ->
          validateUnevaluatedProperties ctx obj objMap anns
        ValidationFailure _ -> ValidationSuccess mempty  -- If properties failed, skip unevaluated check
  in case (propertyResult, unevaluatedResult) of
    (ValidationFailure errs, _) -> ValidationFailure errs
    (ValidationSuccess anns, ValidationSuccess unevalAnns) ->
      ValidationSuccess (anns <> unevalAnns)
    (ValidationSuccess _, ValidationFailure errs) -> ValidationFailure errs
validateObjectConstraints _ _ _ = ValidationSuccess mempty

-- | Validate unevaluatedProperties (2019-09+)
-- Properties that weren't evaluated by properties, patternProperties, additionalProperties,
-- or any applicator keywords must validate against this schema
validateUnevaluatedProperties :: ValidationContext -> SchemaObject -> KeyMap.KeyMap Value -> ValidationAnnotations -> ValidationResult
validateUnevaluatedProperties ctx obj objMap collectedAnnotations = 
  case validationUnevaluatedProperties (schemaValidation obj) of
    Nothing -> ValidationSuccess mempty
    Just unevalSchema ->
      let -- Extract evaluated properties from annotations
          evaluatedProps = extractEvaluatedProperties collectedAnnotations
          -- All properties in the object
          allProps = Set.fromList [Key.toText k | k <- KeyMap.keys objMap]
          -- Unevaluated properties are those not in the evaluated set
          unevaluatedProps = Set.difference allProps evaluatedProps
          -- Validate unevaluated properties against the schema
          results = 
            [ validateValueWithContext ctx unevalSchema propValue
            | propName <- Set.toList unevaluatedProps
            , Just propValue <- [KeyMap.lookup (Key.fromText propName) objMap]
            ]
          failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        [] -> ValidationSuccess $ annotateProperties unevaluatedProps <> mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es

-- | Extract evaluated properties from collected annotations
-- Only considers annotations at the current instance location (empty pointer)
extractEvaluatedProperties :: ValidationAnnotations -> Set Text
extractEvaluatedProperties (ValidationAnnotations annMap) =
  -- Only look at annotations at the current location (empty pointer)
  case Map.lookup emptyPointer annMap of
    Nothing -> Set.empty
    Just innerMap -> case Map.lookup "properties" innerMap of
      Just (Aeson.Array arr) -> Set.fromList
        [ txt
        | Aeson.String txt <- toList arr
        ]
      _ -> Set.empty

-- | Validate unevaluatedItems (2019-09+)
-- Array items that weren't evaluated by items, prefixItems, or contains
-- must validate against this schema
validateUnevaluatedItems :: ValidationContext -> SchemaObject -> Vector Value -> ValidationAnnotations -> ValidationResult
validateUnevaluatedItems ctx obj arr collectedAnnotations =
  case validationUnevaluatedItems (schemaValidation obj) of
    Nothing -> ValidationSuccess mempty
    Just unevalSchema ->
      let -- Extract evaluated item indices from annotations
          evaluatedIndices = extractEvaluatedItems collectedAnnotations
          -- All item indices
          allIndices = Set.fromList [0 .. length arr - 1]
          -- Unevaluated items are those not in the evaluated set
          unevaluatedIndices = Set.difference allIndices evaluatedIndices
          -- Validate unevaluated items against the schema
          results =
            [ validateValueWithContext ctx unevalSchema (arr ! idx)
            | idx <- Set.toList unevaluatedIndices
            , idx < length arr  -- Safety check
            ]
          failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        [] -> ValidationSuccess $ annotateItems unevaluatedIndices <> mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es

-- | Extract evaluated items indices from collected annotations
-- Only considers annotations at the current instance location (empty pointer)
extractEvaluatedItems :: ValidationAnnotations -> Set Int
extractEvaluatedItems (ValidationAnnotations annMap) =
  -- Only look at annotations at the current location (empty pointer)
  case Map.lookup emptyPointer annMap of
    Nothing -> Set.empty
    Just innerMap -> case Map.lookup "items" innerMap of
      Just (Aeson.Array arr) -> Set.fromList
        [ idx
        | Aeson.Number n <- toList arr
        , Just idx <- [Sci.toBoundedInteger n]
        ]
      _ -> Set.empty

-- | Validate dependentRequired (2019-09+)
-- If a property exists, then the properties in the value set must also exist
validateDependentRequired :: ValidationContext -> SchemaValidation -> KeyMap.KeyMap Value -> ValidationResult
validateDependentRequired ctx validation objMap =
  let currentVersion = validationVersion (contextConfig ctx)
      isDependentRequiredSupported = currentVersion >= Draft201909
  in case validationDependentRequired validation of
    Nothing -> ValidationSuccess mempty
    Just depReqMap | not isDependentRequiredSupported -> ValidationSuccess mempty  -- Ignore if not supported
    Just depReqMap ->
      let presentProps = Set.fromList [Key.toText k | k <- KeyMap.keys objMap]
          results =
            [ case KeyMap.lookup (Key.fromText propName) objMap of
                Nothing -> ValidationSuccess mempty  -- Property not present, rule doesn't apply
                Just _ ->
                  -- Property is present, check if all required dependencies are present
                  let missingDeps = Set.difference requiredDeps presentProps
                  in if Set.null missingDeps
                    then ValidationSuccess mempty
                    else ValidationFailure $ ValidationErrors $ pure $
                      ValidationError "dependentRequired" emptyPointer emptyPointer
                        ("Property '" <> propName <> "' requires these properties: " <>
                         T.intercalate ", " (Set.toList missingDeps))
                        Nothing
            | (propName, requiredDeps) <- Map.toList depReqMap
            ]
          failures = [errs | ValidationFailure errs <- results]
      in case failures of
        [] -> ValidationSuccess mempty
        (e:es) -> ValidationFailure $ foldl (<>) e es

-- | Validate dependentSchemas (2019-09+)
-- If a property exists, then the instance must validate against the dependent schema
validateDependentSchemas :: ValidationContext -> SchemaValidation -> KeyMap.KeyMap Value -> ValidationResult
validateDependentSchemas ctx validation objMap = case validationDependentSchemas validation of
  Nothing -> ValidationSuccess mempty
  Just depSchemaMap ->
    let results =
          [ case KeyMap.lookup (Key.fromText propName) objMap of
              Nothing -> ValidationSuccess mempty  -- Property not present, rule doesn't apply
              Just _ ->
                -- Property is present, validate entire object against dependent schema
                validateValueWithContext ctx depSchema (Object objMap)
          | (propName, depSchema) <- Map.toList depSchemaMap
          ]
        failures = [errs | ValidationFailure errs <- results]
        annotations = [anns | ValidationSuccess anns <- results]
    in case failures of
      [] -> ValidationSuccess $ mconcat annotations
      (e:es) -> ValidationFailure $ foldl (<>) e es

-- | Validate dependencies (draft-04 through draft-07)
-- Combines property dependencies and schema dependencies in one keyword
validateDependencies :: ValidationContext -> SchemaValidation -> KeyMap.KeyMap Value -> ValidationResult
validateDependencies ctx validation objMap = case validationDependencies validation of
  Nothing -> ValidationSuccess mempty
  Just depsMap ->
    let presentProps = Set.fromList [Key.toText k | k <- KeyMap.keys objMap]
        results =
          [ case KeyMap.lookup (Key.fromText propName) objMap of
              Nothing -> ValidationSuccess mempty  -- Property not present, rule doesn't apply
              Just _ ->
                case dep of
                  -- Property dependency: check if required properties are present
                  DependencyProperties requiredDeps ->
                    let missingDeps = Set.difference requiredDeps presentProps
                    in if Set.null missingDeps
                      then ValidationSuccess mempty
                      else ValidationFailure $ ValidationErrors $ pure $
                        ValidationError "dependencies" emptyPointer emptyPointer
                          ("Property '" <> propName <> "' requires these properties: " <>
                           T.intercalate ", " (Set.toList missingDeps))
                          Nothing
                  -- Schema dependency: validate entire object against the schema
                  DependencySchema depSchema ->
                    validateValueWithContext ctx depSchema (Object objMap)
          | (propName, dep) <- Map.toList depsMap
          ]
        failures = [errs | ValidationFailure errs <- results]
        annotations = [anns | ValidationSuccess anns <- results]
    in case failures of
      [] -> ValidationSuccess $ mconcat annotations
      (e:es) -> ValidationFailure $ foldl (<>) e es

-- | Update validation context when crossing draft boundaries
-- When a schema references another schema with a different $schema version,
-- we need to validate the referenced schema using its own draft rules
updateContextForCrossDraftRef :: ValidationContext -> Schema -> ValidationContext
updateContextForCrossDraftRef ctx resolvedSchema =
  case schemaVersion resolvedSchema of
    Just refVersion ->
      let currentVersion = validationVersion (contextConfig ctx)
      in if refVersion /= currentVersion
         then -- Switch to the referenced schema's draft version
              let newConfig = (contextConfig ctx) { validationVersion = refVersion }
              in ctx { contextConfig = newConfig }
         else ctx
    Nothing -> ctx  -- No $schema specified, use current context's version

-- | Resolve a $ref reference using the schema registry and base URI
-- Supports:
-- - Root reference: #
-- - JSON Pointer references: #/definitions/foo
-- - Anchor references: #anchor-name
-- - Relative URI references: definitions.json#/foo
-- - Absolute URI references: http://example.com/schema#/foo
-- Also handles URL-encoded fragments (e.g., #/definitions/percent%25field)
-- Returns: (resolved schema, base URI, root schema for internal refs)
resolveReference :: Reference -> ValidationContext -> Maybe (Schema, Maybe Text, Maybe Schema)
resolveReference (Reference refText) ctx
  | T.null refText = Nothing
  | refText == "#" =
      -- Root schema reference
      fmap (\schema -> (schema, contextBaseURI ctx, Just schema)) (contextRootSchema ctx)
  | T.isPrefixOf "#/" refText =
      -- JSON Pointer reference within current document
      -- parsePointer handles URL decoding internally
      let pointer = T.drop 1 refText  -- Remove #
          rootSchema = contextRootSchema ctx
      -- IMPORTANT: Use resolvePointerInSchemaWithBase to track base URI changes
      -- from $id fields encountered while navigating the pointer
      in case rootSchema of
           Nothing -> Nothing
           Just root -> resolvePointerInSchemaWithBase pointer root (contextBaseURI ctx) >>= \(schema, effectiveBase) ->
             -- If the resolved schema has $id, it becomes the new root for subsequent references
             let newRoot = case schemaId schema of
                             Just _ -> Just schema  -- Schema has $id, it's a new schema resource
                             Nothing -> rootSchema  -- No $id, keep the old root
             in Just (schema, effectiveBase, newRoot)
  | T.isPrefixOf "#" refText =
      -- Anchor reference (#foo) or JSON Pointer in compact form
      let anchorName = T.drop 1 refText
          baseURI = contextBaseURI ctx
          registry = contextSchemaRegistry ctx
          rootSchema = contextRootSchema ctx
          -- First try as an anchor (also check dynamic anchors for $ref resolution)
          tryAnchor = case baseURI of
            Just uri ->
              -- Try with explicit base URI first (regular anchors)
              case Map.lookup (uri, anchorName) (registryAnchors registry) of
                Just s -> Just (s, Just uri, Just s)
                Nothing ->
                  -- Also try dynamic anchors (2020-12+: $ref can resolve $dynamicAnchor)
                  case Map.lookup (uri, anchorName) (registryDynamicAnchors registry) of
                    Just s -> Just (s, Just uri, Just s)
                    Nothing ->
                      -- Fallback: try with empty string base (for fragment-only $id)
                      case Map.lookup ("", anchorName) (registryAnchors registry) of
                        Just s -> Just (s, Nothing, Just s)
                        Nothing ->
                          case Map.lookup ("", anchorName) (registryDynamicAnchors registry) of
                            Just s -> Just (s, Nothing, Just s)
                            Nothing -> lookupAnyAnchor anchorName registry
            Nothing ->
              -- No base URI, try with empty string base first
              case Map.lookup ("", anchorName) (registryAnchors registry) of
                Just s -> Just (s, Nothing, Just s)
                Nothing ->
                  case Map.lookup ("", anchorName) (registryDynamicAnchors registry) of
                    Just s -> Just (s, Nothing, Just s)
                    Nothing ->
                      -- Fallback: try looking up with any base
                      lookupAnyAnchor anchorName registry
          -- If not found as anchor, try as JSON Pointer (without leading slash)
          tryPointer = resolvePointerInCurrentSchema ("/" <> anchorName) ctx >>= \schema ->
            Just (schema, contextBaseURI ctx, rootSchema)
      in case tryAnchor of
        Just result -> Just result
        Nothing -> tryPointer
  | T.any (== '#') refText =
      -- URI with fragment
      let (uriPart, fragment) = T.breakOn "#" refText
          fragmentPart = T.drop 1 fragment
          registry = contextSchemaRegistry ctx
          -- Resolve the URI part against the base URI before lookup
          resolvedUriPart = if T.null uriPart
                              then ""
                              else resolveAgainstBaseURI (contextBaseURI ctx) uriPart
          resolvedFullRef = if T.null uriPart
                              then refText
                              else resolvedUriPart <> "#" <> fragmentPart
          baseContext = if T.null uriPart then Nothing else Just resolvedUriPart
      in case Map.lookup resolvedFullRef (registrySchemas registry) of
            Just schema ->
              Just (schema, baseContext, Just schema)
            Nothing ->
              case Map.lookup resolvedUriPart (registrySchemas registry) of
                Just baseSchema
                  | T.null fragmentPart ->
                      Just (baseSchema, baseContext, Just baseSchema)
                  | T.isPrefixOf "/" fragmentPart ->
                      -- When resolving a fragment, return the resolved fragment as the schema
                      -- to validate against, but return the base schema as the root for
                      -- resolving subsequent internal references.
                      -- Track base URI changes from nested $id during pointer resolution
                      resolvePointerInSchemaWithBase fragmentPart baseSchema baseContext >>= \(resolved, effectiveBase) ->
                        Just (resolved, effectiveBase, Just baseSchema)
                  | otherwise ->
                      -- Try regular anchors first, then dynamic anchors
                      let anchors = registryAnchors registry
                          dynamicAnchors = registryDynamicAnchors registry
                          anchorResult = Map.lookup (resolvedUriPart, fragmentPart) anchors
                          dynamicAnchorResult = Map.lookup (resolvedUriPart, fragmentPart) dynamicAnchors
                      in case anchorResult of
                        Just resolved -> Just (resolved, baseContext, Just baseSchema)
                        Nothing -> case dynamicAnchorResult of
                          Just resolved -> Just (resolved, baseContext, Just baseSchema)
                          Nothing -> lookupAnyAnchor fragmentPart registry
                Nothing ->
                  lookupAnyAnchor fragmentPart registry
  | otherwise =
      -- Plain URI reference
      let registry = contextSchemaRegistry ctx
          resolved = resolveAgainstBaseURI (contextBaseURI ctx) refText
          baseForContext =
            if T.null resolved
              then contextBaseURI ctx
              else Just resolved
      in Map.lookup resolved (registrySchemas registry) >>= \schema ->
           Just (schema, baseForContext, Just schema)
  where
    lookupAnyAnchor :: Text -> SchemaRegistry -> Maybe (Schema, Maybe Text, Maybe Schema)
    lookupAnyAnchor anchorName registry =
      -- Try regular anchors first
      case [ (base, schema)
           | ((base, name), schema) <- Map.toList (registryAnchors registry)
           , name == anchorName
           ] of
        ((baseUri, schema):_) ->
          let baseResult = if T.null baseUri then Nothing else Just baseUri
          in Just (schema, baseResult, Just schema)
        [] ->
          -- Also check dynamic anchors (2020-12+: $ref can resolve $dynamicAnchor)
          case [ (base, schema)
               | ((base, name), schema) <- Map.toList (registryDynamicAnchors registry)
               , name == anchorName
               ] of
            ((baseUri, schema):_) ->
              let baseResult = if T.null baseUri then Nothing else Just baseUri
              in Just (schema, baseResult, Just schema)
            [] -> Nothing

-- | Resolve a JSON Pointer within the current schema context
resolvePointerInCurrentSchema :: Text -> ValidationContext -> Maybe Schema
resolvePointerInCurrentSchema pointer ctx =
  case contextRootSchema ctx of
    Nothing -> Nothing
    Just rootSchema -> resolvePointerInSchema pointer rootSchema

-- | Resolve a JSON Pointer within a specific schema
resolvePointerInSchema :: Text -> Schema -> Maybe Schema
resolvePointerInSchema pointer schema =
  fmap fst (resolvePointerInSchemaWithBase pointer schema Nothing)

-- | Resolve a JSON Pointer within a specific schema, tracking base URI changes
-- Returns (resolved schema, effective base URI after navigating through schemas with $id)
--
-- This now uses the pluggable keyword navigation system, allowing custom keywords
-- to define their own schema containment and navigation behavior.
resolvePointerInSchemaWithBase :: Text -> Schema -> Maybe Text -> Maybe (Schema, Maybe Text)
resolvePointerInSchemaWithBase pointer schema currentBase =
  case parsePointer pointer of
    Left _ -> Nothing
    Right (JSONPointer segments) -> followPointer segments schema currentBase
  where
    -- Get the appropriate keyword registry based on schema version
    getRegistry :: Schema -> Keyword.KeywordRegistry
    getRegistry s = case schemaVersion s of
      Just Draft04 -> draft04Registry
      _ -> standardKeywordRegistry

    followPointer :: [Text] -> Schema -> Maybe Text -> Maybe (Schema, Maybe Text)
    followPointer [] s base = Just (s, base)
    followPointer (seg:rest) s base = case schemaCore s of
      BooleanSchema _ -> Nothing
      ObjectSchema obj -> navigateKeyword seg rest s obj base

    navigateKeyword :: Text -> [Text] -> Schema -> SchemaObject -> Maybe Text -> Maybe (Schema, Maybe Text)
    navigateKeyword seg rest parentSchema obj currentBase =
      let registry = getRegistry parentSchema
          keywordDef = Keyword.lookupKeyword seg registry
      in case keywordDef of
        -- If keyword has navigation, use it
        Just kwDef -> case Keyword.keywordNavigation kwDef of
          NoNavigation -> Nothing
          
          SingleSchema navFunc ->
            case navFunc parentSchema of
              Just subSchema -> updateBaseAndFollow subSchema
              Nothing -> Nothing
          
          SchemaMap navFunc ->
            case rest of
              (key:remaining) ->
                case navFunc parentSchema of
                  Just schemaMap ->
                    case Map.lookup key schemaMap of
                      Just subSchema -> followPointerFrom remaining subSchema
                      Nothing -> Nothing
                  Nothing -> Nothing
              [] -> Nothing  -- Need a key for SchemaMap navigation
          
          SchemaArray navFunc ->
            case rest of
              (idx:remaining) ->
                case reads (T.unpack idx) :: [(Int, String)] of
                  [(n, "")] | n >= 0 ->
                    case navFunc parentSchema of
                      Just schemas | n < length schemas ->
                        followPointerFrom remaining (schemas !! n)
                      _ -> Nothing
                  _ -> Nothing
              [] -> Nothing  -- Need an index for SchemaArray navigation
          
          CustomNavigation navFunc ->
            -- CustomNavigation gets the current segment and remaining path
            -- and returns (nextSchema, remainingPath)
            case navFunc parentSchema seg rest of
              Just (nextSchema, remaining) -> followPointerFrom remaining nextSchema
              Nothing -> Nothing
        
        -- No registered keyword - check special cases and extensions
        Nothing -> navigateFallback seg rest parentSchema obj currentBase
      where
        -- Update base URI if subschema has $id, then continue following
        updateBaseAndFollow :: Schema -> Maybe (Schema, Maybe Text)
        updateBaseAndFollow subSchema =
          let newBase = case schemaId subSchema of
                Just sid -> Just $ resolveAgainstBaseURI currentBase sid
                Nothing -> currentBase
          in followPointer rest subSchema newBase
        
        followPointerFrom :: [Text] -> Schema -> Maybe (Schema, Maybe Text)
        followPointerFrom remaining subSchema =
          let newBase = case schemaId subSchema of
                Just sid -> Just $ resolveAgainstBaseURI currentBase sid
                Nothing -> currentBase
          in followPointer remaining subSchema newBase
    
    -- Fallback for special cases not handled by registered keywords
    navigateFallback :: Text -> [Text] -> Schema -> SchemaObject -> Maybe Text -> Maybe (Schema, Maybe Text)
    navigateFallback seg rest parentSchema obj currentBase
      -- Special case: $defs and definitions are aliases (handled as SchemaMap)
      | seg == "$defs" || seg == "definitions" =
          case rest of
            (defName:remaining) ->
              case Map.lookup defName (schemaDefs obj) of
                Just subSchema ->
                  let newBase = case schemaId subSchema of
                        Just sid -> Just $ resolveAgainstBaseURI currentBase sid
                        Nothing -> currentBase
                  in followPointer remaining subSchema newBase
                Nothing -> Nothing
            [] -> Nothing
      
      -- Check schemaExtensions for arbitrary/unknown keywords
      | otherwise =
          case Map.lookup seg (schemaExtensions parentSchema) of
            Just val ->
              let version = fromMaybe Draft07 (schemaVersion parentSchema)
              in case Parser.parseSchemaWithVersion version val of
                Right schema -> followPointer rest schema currentBase
                Left _ ->
                  -- Not a direct schema - might be an array of schemas
                  case (val, rest) of
                    (Aeson.Array arr, (idx:remaining)) ->
                      case reads (T.unpack idx) :: [(Int, String)] of
                        [(n, "")] | n >= 0 && n < length arr ->
                          case Parser.parseSchemaWithVersion version (arr ! n) of
                            Right schema -> followPointer remaining schema currentBase
                            Left _ -> Nothing
                        _ -> Nothing
                    _ -> Nothing
            Nothing -> Nothing

-- | Show a reference for error messages
showReference :: Reference -> Text
showReference (Reference t) = t

-- | Resolve a $dynamicRef by searching the dynamic scope stack (2020-12+)
-- $dynamicRef references are resolved by looking for a schema with a matching
-- $dynamicAnchor in the dynamic scope stack (most recent first)
-- Supports both simple anchors (#anchor) and URI references (uri#anchor)
resolveDynamicRef :: Reference -> ValidationContext -> Maybe Schema
resolveDynamicRef (Reference refText) ctx
  | T.any (== '#') refText =
      -- Reference contains a fragment - could be "#anchor" or "uri#anchor"
      let (uriPart, fragment) = T.breakOn "#" refText
          anchorName = T.drop 1 fragment  -- Remove the # prefix
          registry = contextSchemaRegistry ctx
          baseURI = contextBaseURI ctx

          -- BOOKENDING REQUIREMENT: For dynamic resolution to work, the initial
          -- resolution target must have a $dynamicAnchor. If it doesn't, return Nothing
          -- to trigger fallback to static $ref resolution.
          checkBookending schemaToCheck =
            case schemaCore schemaToCheck of
              ObjectSchema obj -> isJust (schemaDynamicAnchor obj)
              _ -> False

      in if T.null uriPart
           then
             -- Simple fragment reference: #anchor
             -- First check if there's a bookending $dynamicAnchor at the target
             let staticTarget = case baseURI of
                   Just uri -> Map.lookup (uri, anchorName) (registryDynamicAnchors registry)
                   Nothing -> Nothing
             in case staticTarget of
               Just targetSchema | checkBookending targetSchema ->
                 -- Target has $dynamicAnchor, search dynamic scope
                 -- Search from outermost (oldest/root) to innermost (most recent) for $dynamicRef
                 case findSchemaWithDynamicAnchor anchorName (reverse (contextDynamicScope ctx)) of
                   Just schema -> Just schema
                   Nothing -> Just targetSchema  -- Use the target itself
               _ ->
                 -- No bookending $dynamicAnchor at target, return Nothing to trigger fallback
                 Nothing
           else
             -- URI with fragment: uri#anchor
             -- First resolve the URI part statically to find the target
             case resolveReference (Reference refText) ctx of
               Just (resolvedSchema, maybeBase, _) | checkBookending resolvedSchema ->
                 -- Target has $dynamicAnchor, search dynamic scope
                 -- Search from outermost (oldest/root) to innermost (most recent) for $dynamicRef
                 case findSchemaWithDynamicAnchor anchorName (reverse (contextDynamicScope ctx)) of
                   Just schema -> Just schema
                   Nothing ->
                     -- If not in dynamic scope, check the registry
                     case maybeBase of
                       Just uri -> Map.lookup (uri, anchorName) (registryDynamicAnchors registry)
                       Nothing -> Just resolvedSchema
               _ ->
                 -- No bookending $dynamicAnchor, return Nothing to trigger fallback
                 Nothing
  | otherwise = Nothing  -- Not a valid $dynamicRef format
  where
    -- Search for a schema with matching $dynamicAnchor in the dynamic scope
    -- Search from outermost (root/oldest) to innermost (most recent)
    -- The list is reversed before calling this function to achieve outermost-first search
    -- According to the spec, $dynamicRef resolves to the FIRST $dynamicAnchor encountered in scope
    findSchemaWithDynamicAnchor :: Text -> [Schema] -> Maybe Schema
    findSchemaWithDynamicAnchor _anchor [] = Nothing
    findSchemaWithDynamicAnchor anchor (schema:rest) = case schemaCore schema of
      ObjectSchema obj ->
        -- Search the entire schema resource for the matching $dynamicAnchor
        case findDynamicAnchorInSchemaResource anchor schema of
          Just foundSchema -> Just foundSchema
          Nothing -> findSchemaWithDynamicAnchor anchor rest
      _ -> findSchemaWithDynamicAnchor anchor rest

    -- Search for a $dynamicAnchor anywhere within a schema resource
    -- This includes the schema itself, its $defs, and recursively through composition keywords
    -- (allOf, anyOf, oneOf) since they're all part of the same schema resource
    findDynamicAnchorInSchemaResource :: Text -> Schema -> Maybe Schema
    findDynamicAnchorInSchemaResource anchor schema = case schemaCore schema of
      ObjectSchema obj ->
        -- First check if this schema itself has the matching $dynamicAnchor
        case schemaDynamicAnchor obj of
          Just dynAnchor | dynAnchor == anchor -> Just schema
          _ ->
            -- Search in $defs
            case findInDefs anchor (schemaDefs obj) of
              Just foundSchema -> Just foundSchema
              Nothing ->
                -- Search in composition keywords (allOf, anyOf, oneOf)
                -- These are part of the same schema resource if they don't have their own $id
                case findInComposition anchor obj of
                  Just foundSchema -> Just foundSchema
                  Nothing -> Nothing
      _ -> Nothing

    -- Search for a $dynamicAnchor in $defs
    -- Only search schemas that don't have their own $id (same schema resource)
    findInDefs :: Text -> Map Text Schema -> Maybe Schema
    findInDefs anchor defs =
      case [ defSchema
           | defSchema <- Map.elems defs
           , not (isJust (schemaId defSchema))  -- Exclude schemas with their own $id
           , case schemaCore defSchema of
               ObjectSchema defObj ->
                 case schemaDynamicAnchor defObj of
                   Just dynAnchor -> dynAnchor == anchor
                   Nothing -> False
               _ -> False
           ] of
        (found:_) -> Just found
        [] -> Nothing

    -- Search for a $dynamicAnchor in composition keywords (allOf, anyOf, oneOf)
    -- Only search in schemas that don't have their own $id (same schema resource)
    findInComposition :: Text -> SchemaObject -> Maybe Schema
    findInComposition anchor obj =
      let searchSchemas schemas = listToMaybe
            [ result
            | schema <- schemas
            , let schemaHasId = isJust (schemaId schema)
            , not schemaHasId  -- Only search if it's part of the same resource
            , Just result <- [findDynamicAnchorInSchemaResource anchor schema]
            ]
      in case schemaAllOf obj of
           Just schemas -> case searchSchemas (NE.toList schemas) of
             Just found -> Just found
             Nothing -> case schemaAnyOf obj of
               Just schemas' -> case searchSchemas (NE.toList schemas') of
                 Just found' -> Just found'
                 Nothing -> case schemaOneOf obj of
                   Just schemas'' -> searchSchemas (NE.toList schemas'')
                   Nothing -> Nothing
               Nothing -> case schemaOneOf obj of
                 Just schemas'' -> searchSchemas (NE.toList schemas'')
                 Nothing -> Nothing
           Nothing -> case schemaAnyOf obj of
             Just schemas' -> case searchSchemas (NE.toList schemas') of
               Just found' -> Just found'
               Nothing -> case schemaOneOf obj of
                 Just schemas'' -> searchSchemas (NE.toList schemas'')
                 Nothing -> Nothing
             Nothing -> case schemaOneOf obj of
               Just schemas'' -> searchSchemas (NE.toList schemas'')
               Nothing -> Nothing

-- | Resolve $recursiveRef (2019-09)
-- Similar to $dynamicRef but uses $recursiveAnchor (boolean) instead of $dynamicAnchor (named)
-- When $recursiveRef points to "#", it resolves to the outermost schema resource with $recursiveAnchor: true
-- BOOKENDING: The current schema must have $recursiveAnchor: true for dynamic resolution
resolveRecursiveRef :: Reference -> ValidationContext -> Maybe Schema
resolveRecursiveRef (Reference refText) ctx
  | refText == "#" =
      let registry = contextSchemaRegistry ctx
          baseURI = contextBaseURI ctx
          -- Check if the current schema (most recent in dynamic scope) has $recursiveAnchor: true
          currentSchemaHasAnchor = case contextDynamicScope ctx of
            (schema:_) -> case schemaCore schema of
              ObjectSchema obj -> schemaRecursiveAnchor obj == Just True
              _ -> False
            [] -> False
          -- Also check the registry for the base URI
          registryHasAnchor = case baseURI of
            Just uri -> Map.member uri (registryRecursiveAnchors registry)
            Nothing -> False
      in if currentSchemaHasAnchor || registryHasAnchor
         then
           -- Target has $recursiveAnchor: true, search dynamic scope
           -- Search from OUTERMOST to INNERMOST
           case findSchemaWithRecursiveAnchor (reverse (contextDynamicScope ctx)) of
             Just schema -> Just schema
             Nothing ->
               -- If not in dynamic scope, check registry
               case baseURI of
                 Just uri -> Map.lookup uri (registryRecursiveAnchors registry)
                 Nothing -> Nothing
         else
           -- No bookending $recursiveAnchor: true, return Nothing to trigger fallback
           Nothing
  | otherwise =
      -- For non-"#" references, treat as regular $ref
      -- This is the fallback behavior in 2019-09
      Nothing
  where
    findSchemaWithRecursiveAnchor :: [Schema] -> Maybe Schema
    findSchemaWithRecursiveAnchor [] = Nothing
    findSchemaWithRecursiveAnchor (schema:rest) = case schemaCore schema of
      ObjectSchema obj ->
        case schemaRecursiveAnchor obj of
          Just True -> Just schema
          _ -> findSchemaWithRecursiveAnchor rest
      _ -> findSchemaWithRecursiveAnchor rest



-- | Validate custom keywords from schema extensions
-- Executes user-provided validators for keywords not in the standard vocabulary
validateCustomKeywords :: ValidationContext -> Schema -> Value -> ValidationResult
validateCustomKeywords ctx schema val =
  let extensions = schemaExtensions schema
      customValidators = validationCustomValidators (contextConfig ctx)
      
      -- Execute validators for extension keywords
      results = 
        [ case Map.lookup keyword customValidators of
            Just validator ->
              -- Execute the custom validator
              case validator val of
                Left err -> ValidationFailure $ ValidationErrors $ pure err
                Right () -> ValidationSuccess mempty
            Nothing ->
              -- No validator registered - treat as annotation only
              ValidationSuccess mempty
        | (keyword, _value) <- Map.toList extensions
        ]
  in combineResults results
  where
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
      in case failures of
        [] -> ValidationSuccess mempty
        (e:es) -> ValidationFailure $ foldl (<>) e es

