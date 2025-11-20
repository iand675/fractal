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
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Scientific as Sci
import Data.Foldable (toList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Vector (Vector, (!), fromList)
import qualified Fractal.JsonSchema.Regex as Regex
import qualified Text.Regex.ECMA262 as R
import qualified Data.UUID as UUID
import qualified Data.Time.Format.ISO8601 as Time
import qualified Data.Time.Clock as Time
import qualified Data.Time.Calendar as Time
import qualified Text.Read as Read
import qualified Data.Time.RFC3339 as RFC3339
import qualified Text.Email.Validate as Email
import qualified Network.URI as URI
import qualified Data.IP as IP
import qualified Data.Text.IDN as IDN

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
  , validationShortCircuit = False     -- Collect all errors
  , validationCollectAnnotations = False
  , validationCustomValidators = Map.empty
  , validationReferenceLoader = Nothing  -- No external reference loading by default
  }

-- | Strict validation (format assertion, all errors collected)
strictValidationConfig :: ValidationConfig
strictValidationConfig = defaultValidationConfig
  { validationFormatAssertion = True
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
           [ validateAgainstObject ctx ctxWithBase obj val
           , validateCustomKeywords ctxWithBase schema val
           ]
  where
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        [] -> ValidationSuccess $ mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es

-- | Apply schema-specific context updates (base URI, root schema)
applySchemaContext :: ValidationContext -> Schema -> ValidationContext
applySchemaContext ctx schema =
  let parentBase = contextBaseURI ctx
      newBase = schemaEffectiveBase parentBase schema
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

-- | Create validation failure with error
validationFailure :: Text -> Text -> ValidationResult
validationFailure keyword message =
  ValidationFailure $ ValidationErrors $ pure $
    ValidationError keyword emptyPointer emptyPointer message Nothing

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

-- | Validate against object schema
validateAgainstObject :: ValidationContext -> ValidationContext -> SchemaObject -> Value -> ValidationResult
validateAgainstObject parentCtx ctx obj val =
  -- In 2019-09+, $ref and $dynamicRef are applicators alongside other keywords
  -- In earlier drafts, $ref short-circuits (only validates against ref, ignores siblings)
  let version = validationVersion (contextConfig ctx)
      refIsApplicator = version >= Draft201909
  in if refIsApplicator
    then
      -- 2019-09+: $ref/$dynamicRef are applicators, combine with other keywords
      -- We need to pass ref annotations to content validation so unevaluatedProperties
      -- can see properties evaluated by the ref
      let refResult = validateRef parentCtx ctx obj val
          refAnnotations = case refResult of
            ValidationSuccess anns -> anns
            ValidationFailure _ -> mempty
          contentResult = validateObjectSchemaContentWithRefAnnotations ctx obj val refAnnotations
      in combineResults [refResult, contentResult]
    else
      -- Pre-2019-09: $ref short-circuits, ignores sibling keywords
      case schemaRef obj of
        Just ref ->
          let refText = showReference ref
          in if Set.member refText (contextResolvingRefs ctx)
            then ValidationSuccess mempty  -- Break the cycle
            else
              let ctxWithRef = ctx { contextResolvingRefs = Set.insert refText (contextResolvingRefs ctx) }
              in case resolveReference ref refCtx of
                Just (resolvedSchema, maybeBase) ->
                  let ctxForRef = case maybeBase of
                        Just baseDoc ->
                          let baseChanged = contextBaseURI ctxWithRef /= Just baseDoc
                              rootValue =
                                if baseChanged
                                  then Just resolvedSchema
                                  else case contextRootSchema ctxWithRef of
                                         Just existing -> Just existing
                                         Nothing -> Just resolvedSchema
                          in ctxWithRef { contextBaseURI = Just baseDoc
                                        , contextRootSchema = rootValue
                                        }
                        Nothing -> ctxWithRef
                  in validateValueWithContext ctxForRef resolvedSchema val
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
          let refText = showReference ref
          in if Set.member refText (contextResolvingRefs ctx')
            then ValidationSuccess mempty  -- Break the cycle
            else
              let ctxWithRef = ctx' { contextResolvingRefs = Set.insert refText (contextResolvingRefs ctx') }
              in case resolveReference ref refCtx of
                Just (resolvedSchema, maybeBase) ->
                  let ctxForRef = case maybeBase of
                        Just baseDoc ->
                          let baseChanged = contextBaseURI ctxWithRef /= Just baseDoc
                              rootValue =
                                if baseChanged
                                  then Just resolvedSchema
                                  else case contextRootSchema ctxWithRef of
                                         Just existing -> Just existing
                                         Nothing -> Just resolvedSchema
                          in ctxWithRef { contextBaseURI = Just baseDoc
                                        , contextRootSchema = rootValue
                                        }
                        Nothing -> ctxWithRef
                  in validateValueWithContext ctxForRef resolvedSchema val'
                Nothing ->
                  ValidationFailure $ ValidationErrors $ pure $
                    ValidationError "$ref" emptyPointer emptyPointer
                      ("Unable to resolve reference: " <> showReference ref)
                      Nothing
        Nothing ->
          -- No $ref, check for $dynamicRef (2020-12+)
          case schemaDynamicRef obj' of
            Just dynRef ->
              let dynRefText = showReference dynRef
              in if Set.member dynRefText (contextResolvingRefs ctx')
                then ValidationSuccess mempty  -- Break the cycle
                else
                  let ctxWithRef = ctx' { contextResolvingRefs = Set.insert dynRefText (contextResolvingRefs ctx') }
                  in case resolveDynamicRef dynRef ctxWithRef of
                    Just resolvedSchema ->
                      validateValueWithContext ctxWithRef resolvedSchema val'
                    Nothing ->
                      -- Fallback: treat as regular $ref (resolve statically)
                      case resolveReference dynRef refCtx of
                        Just (resolvedSchema, maybeBase) ->
                          let ctxForRef = case maybeBase of
                                Just baseDoc ->
                                  let baseChanged = contextBaseURI ctxWithRef /= Just baseDoc
                                      rootValue =
                                        if baseChanged
                                          then Just resolvedSchema
                                          else case contextRootSchema ctxWithRef of
                                                 Just existing -> Just existing
                                                 Nothing -> Just resolvedSchema
                                  in ctxWithRef { contextBaseURI = Just baseDoc
                                                , contextRootSchema = rootValue
                                                }
                                Nothing -> ctxWithRef
                          in validateValueWithContext ctxForRef resolvedSchema val'
                        Nothing ->
                          ValidationFailure $ ValidationErrors $ pure $
                            ValidationError "$dynamicRef" emptyPointer emptyPointer
                              ("Unable to resolve dynamic reference: " <> showReference dynRef)
                              Nothing
            Nothing -> ValidationSuccess mempty  -- No ref at all

    combineResults :: [ValidationResult] -> ValidationResult
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        [] -> ValidationSuccess $ mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es

    -- Validate object schema content without ref annotations (for pre-2019-09 or when no ref)
    validateObjectSchemaContent ctx' obj' val' =
      validateObjectSchemaContentWithRefAnnotations ctx' obj' val' mempty

    -- Validate object schema content with additional annotations from refs
    validateObjectSchemaContentWithRefAnnotations ctx' obj' val' refAnns =
      -- Update context with dynamic anchor if present
      let ctx'' = case schemaDynamicAnchor obj' of
            Just _ ->
              -- Push current schema onto dynamic scope
              let schema' = Schema
                    { schemaVersion = Just (validationVersion $ contextConfig ctx')
                    , schemaId = contextBaseURI ctx'
                    , schemaCore = ObjectSchema obj'
                    , schemaVocabulary = Nothing
                    , schemaExtensions = Map.empty
                    }
              in ctx' { contextDynamicScope = schema' : contextDynamicScope ctx' }
            Nothing -> ctx'
          -- Validate all keywords EXCEPT unevaluated ones
          results =
            [ validateTypeConstraint obj' val'
            , validateEnumConstraint obj' val'
            , validateConstConstraint obj' val'
            , validateComposition ctx'' obj' val'
            , validateConditional ctx'' obj' val'
            , validateNumericConstraints obj' val'
            , validateStringConstraints obj' val'
            , validateFormatConstraints ctx'' obj' val'
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

-- | Validate type constraint
validateTypeConstraint :: SchemaObject -> Value -> ValidationResult
validateTypeConstraint obj val = case schemaType obj of
  Nothing -> ValidationSuccess mempty
  Just (One expectedType) -> validateType expectedType val
  Just (Many types) -> validateTypeUnion (NE.toList types) val
  where
    validateType NullType Null = ValidationSuccess mempty
    validateType NullType _ = validationFailure "type" "Expected null"
    validateType BooleanType (Bool _) = ValidationSuccess mempty
    validateType BooleanType _ = validationFailure "type" "Expected boolean"
    validateType StringType (String _) = ValidationSuccess mempty
    validateType StringType _ = validationFailure "type" "Expected string"
    validateType NumberType (Number _) = ValidationSuccess mempty
    validateType NumberType _ = validationFailure "type" "Expected number"
    validateType IntegerType (Number n) =
      if Sci.isInteger n
        then ValidationSuccess mempty
        else validationFailure "type" "Expected integer"
    validateType IntegerType _ = validationFailure "type" "Expected integer"
    validateType ObjectType (Object _) = ValidationSuccess mempty
    validateType ObjectType _ = validationFailure "type" "Expected object"
    validateType ArrayType (Array _) = ValidationSuccess mempty
    validateType ArrayType _ = validationFailure "type" "Expected array"
    
    validateTypeUnion types v =
      if any (\t -> isSuccess $ validateType t v) types
        then ValidationSuccess mempty
        else validationFailure "type" $ "Expected one of: " <> T.intercalate ", " (map (T.pack . show) types)

-- | Validate enum constraint
validateEnumConstraint :: SchemaObject -> Value -> ValidationResult
validateEnumConstraint obj val = case schemaEnum obj of
  Nothing -> ValidationSuccess mempty
  Just allowedValues ->
    if val `elem` NE.toList allowedValues
      then ValidationSuccess mempty
      else validationFailure "enum" $ "Value not in enum: " <> T.pack (show val)

-- | Validate const constraint (draft-06+)
validateConstConstraint :: SchemaObject -> Value -> ValidationResult
validateConstConstraint obj val = case schemaConst obj of
  Nothing -> ValidationSuccess mempty
  Just expected ->
    if val == expected
      then ValidationSuccess mempty
      else validationFailure "const" $ "Value does not match const"

-- | Validate composition keywords (allOf, anyOf, oneOf, not)
validateComposition :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateComposition ctx obj val =
  combineResults
    [ maybe (ValidationSuccess mempty) (validateAllOf ctx val) (schemaAllOf obj)
    , maybe (ValidationSuccess mempty) (validateAnyOf ctx val) (schemaAnyOf obj)
    , maybe (ValidationSuccess mempty) (validateOneOf ctx val) (schemaOneOf obj)
    , maybe (ValidationSuccess mempty) (validateNot ctx val) (schemaNot obj)
    ]
  where
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        [] -> ValidationSuccess $ mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validateAllOf ctx' v schemas =
      let results = [validateValueWithContext ctx' schema v | schema <- NE.toList schemas]
          failures = [errs | ValidationFailure errs <- results]
          annotations = [anns | ValidationSuccess anns <- results]
      in case failures of
        -- Collect annotations from ALL branches in allOf (all must pass)
        [] -> ValidationSuccess $ mconcat annotations
        (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validateAnyOf ctx' v schemas =
      let results = [validateValueWithContext ctx' schema v | schema <- NE.toList schemas]
          successes = [anns | ValidationSuccess anns <- results]
      in if null successes
        then validationFailure "anyOf" "Value does not match any schema in anyOf"
        -- Collect annotations from ALL passing branches in anyOf
        else ValidationSuccess $ mconcat successes
    
    validateOneOf ctx' v schemas =
      let results = [validateValueWithContext ctx' schema v | schema <- NE.toList schemas]
          successes = [anns | ValidationSuccess anns <- results]
      in case length successes of
        -- Collect annotations from the single passing branch
        1 -> ValidationSuccess $ head successes
        0 -> validationFailure "oneOf" "Value does not match any schema in oneOf"
        _ -> validationFailure "oneOf" "Value matches more than one schema in oneOf"
    
    validateNot ctx' v schema =
      case validateValueWithContext ctx' schema v of
        ValidationSuccess _ -> validationFailure "not" "Value matches schema in 'not'"
        ValidationFailure _ -> ValidationSuccess mempty

-- | Validate conditional keywords (if/then/else, draft-07+)
validateConditional :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateConditional ctx obj val = case schemaIf obj of
  Nothing -> ValidationSuccess mempty  -- No conditional
  Just ifSchema ->
    case validateValueWithContext ctx ifSchema val of
      ValidationSuccess ifAnns ->
        -- If validates, apply then (if present) and combine annotations
        case schemaThen obj of
          Just thenSchema ->
            case validateValueWithContext ctx thenSchema val of
              ValidationSuccess thenAnns -> ValidationSuccess (ifAnns <> thenAnns)
              ValidationFailure errs -> ValidationFailure errs
          Nothing -> ValidationSuccess ifAnns  -- No then, keep if annotations
      ValidationFailure _ ->
        -- If fails, apply else (if present)
        maybe (ValidationSuccess mempty) (\elseSchema -> validateValueWithContext ctx elseSchema val) (schemaElse obj)

-- | Validate numeric constraints
validateNumericConstraints :: SchemaObject -> Value -> ValidationResult
validateNumericConstraints obj (Number n) =
  let validation = schemaValidation obj
      results =
        [ maybe (ValidationSuccess mempty) (checkMultipleOf n) (validationMultipleOf validation)
        , checkMaximumWithExclusive n validation
        , checkMinimumWithExclusive n validation
        ]
  in combineResults results
  where
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
      in case failures of
        [] -> ValidationSuccess mempty
        (e:es) -> ValidationFailure $ foldl (<>) e es
    
    checkMultipleOf num divisor =
      -- Special case: if the number is an integer and divisor <= 1, any integer is a multiple
      -- This handles overflow cases like 1e308 / 0.5
      let numDouble = Sci.toRealFloat num :: Double
          divisorDouble = Sci.toRealFloat divisor :: Double
          isIntegerValue = numDouble == fromIntegral (round numDouble :: Integer)
      in if isIntegerValue && divisorDouble > 0 && divisorDouble <= 1
        then
          -- For integers with divisor <= 1, check if 1/divisor is an integer
          -- e.g., 0.5 -> 2, 0.25 -> 4, etc.
          let reciprocal = 1 / divisorDouble
              isValidDivisor = reciprocal == fromIntegral (round reciprocal :: Integer)
          in if isValidDivisor
             then ValidationSuccess mempty
             else 
               -- Fall back to standard check
               let remainder = numDouble - (fromIntegral (floor (numDouble / divisorDouble) :: Integer) * divisorDouble)
                   epsilon = 1e-10
               in if abs remainder < epsilon || abs (remainder - divisorDouble) < epsilon
                  then ValidationSuccess mempty
                  else validationFailure "multipleOf" $ "Value is not a multiple of " <> T.pack (show divisor)
        else
          -- Standard check for non-integer or divisor > 1
          let remainder = numDouble - (fromIntegral (floor (numDouble / divisorDouble) :: Integer) * divisorDouble)
              epsilon = 1e-10
          in if abs remainder < epsilon || abs (remainder - divisorDouble) < epsilon
            then ValidationSuccess mempty
            else validationFailure "multipleOf" $ "Value is not a multiple of " <> T.pack (show divisor)
    
    -- Check maximum with exclusiveMaximum handling (draft-04 vs draft-06+)
    checkMaximumWithExclusive num validation' =
      case (validationMaximum validation', validationExclusiveMaximum validation') of
        (Just max', Just (Left True)) ->
          -- Draft-04: exclusiveMaximum is boolean, modifies maximum behavior
          if num < max'
            then ValidationSuccess mempty
            else validationFailure "exclusiveMaximum" $ "Value " <> T.pack (show num) <> " must be less than " <> T.pack (show max')
        (Just max', _) ->
          -- No exclusive or exclusiveMaximum = False
          if num <= max'
            then ValidationSuccess mempty
            else validationFailure "maximum" $ "Value " <> T.pack (show num) <> " exceeds maximum " <> T.pack (show max')
        (Nothing, Just (Right exclusiveMax)) ->
          -- Draft-06+: exclusiveMaximum is numeric, standalone
          if num < exclusiveMax
            then ValidationSuccess mempty
            else validationFailure "exclusiveMaximum" $ "Value " <> T.pack (show num) <> " must be less than " <> T.pack (show exclusiveMax)
        _ -> ValidationSuccess mempty
    
    -- Check minimum with exclusiveMinimum handling (draft-04 vs draft-06+)
    checkMinimumWithExclusive num validation' =
      case (validationMinimum validation', validationExclusiveMinimum validation') of
        (Just min', Just (Left True)) ->
          -- Draft-04: exclusiveMinimum is boolean, modifies minimum behavior
          if num > min'
            then ValidationSuccess mempty
            else validationFailure "exclusiveMinimum" $ "Value " <> T.pack (show num) <> " must be greater than " <> T.pack (show min')
        (Just min', _) ->
          -- No exclusive or exclusiveMinimum = False
          if num >= min'
            then ValidationSuccess mempty
            else validationFailure "minimum" $ "Value " <> T.pack (show num) <> " is less than minimum " <> T.pack (show min')
        (Nothing, Just (Right exclusiveMin)) ->
          -- Draft-06+: exclusiveMinimum is numeric, standalone
          if num > exclusiveMin
            then ValidationSuccess mempty
            else validationFailure "exclusiveMinimum" $ "Value " <> T.pack (show num) <> " must be greater than " <> T.pack (show exclusiveMin)
        _ -> ValidationSuccess mempty
validateNumericConstraints _ _ = ValidationSuccess mempty

-- | Validate string constraints  
validateStringConstraints :: SchemaObject -> Value -> ValidationResult
validateStringConstraints obj (String txt) =
  let validation = schemaValidation obj
      textLength = T.length txt
  in combineResults
    [ maybe (ValidationSuccess mempty) (\max' ->
        if fromIntegral textLength <= max'
          then ValidationSuccess mempty
          else validationFailure "maxLength" $ "String length exceeds maxLength"
      ) (validationMaxLength validation)
    , maybe (ValidationSuccess mempty) (\min' ->
        if fromIntegral textLength >= min'
          then ValidationSuccess mempty
          else validationFailure "minLength" $ "String length below minLength"
      ) (validationMinLength validation)
    , validatePattern txt validation
    ]
  where
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
      in case failures of
        [] -> ValidationSuccess mempty
        (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validatePattern text schemaValidation = case validationPattern schemaValidation of
      Nothing -> ValidationSuccess mempty
      Just (Regex pattern) ->
        -- Use regex-tdfa to match pattern
        case compileRegex pattern of
          Right regex ->
            if matchRegex regex text
              then ValidationSuccess mempty
              else validationFailure "pattern" $ "String does not match pattern: " <> pattern
          Left err -> validationFailure "pattern" $ "Invalid regex pattern: " <> err
validateStringConstraints _ _ = ValidationSuccess mempty

-- | Validate format constraints (respects config flag)
validateFormatConstraints :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateFormatConstraints ctx obj (String txt) =
  let validation = schemaValidation obj
      config = contextConfig ctx
  in case validationFormat validation of
    Nothing -> ValidationSuccess mempty
    Just format ->
      if validationFormatAssertion config
        then validateFormatValue format txt  -- Format as assertion
        else ValidationSuccess mempty        -- Format as annotation only
validateFormatConstraints _ _ _ = ValidationSuccess mempty

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
      case validationPrefixItems (schemaValidation schemaObj) of
        Just prefixSchemas ->
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
              let results = [validateValueWithContext ctx' itemSchema item | item <- toList array]
                  failures = [errs | ValidationFailure errs <- results]
                  allIndices = Set.fromList [0 .. length array - 1]
                  annotations = [anns | ValidationSuccess anns <- results]
              in case failures of
                [] -> ValidationSuccess $ annotateItems allIndices <> mconcat annotations
                (e:es) -> ValidationFailure $ foldl (<>) e es
            Just (ItemsTuple tupleSchemas maybeAdditional) ->
              -- Positional validation + optional additional items
              let tupleResults = zipWith (validateValueWithContext ctx') (NE.toList tupleSchemas) (toList array)
                  tupleFailures = [errs | ValidationFailure errs <- tupleResults]
                  tupleIndices = Set.fromList [0 .. length tupleSchemas - 1]
                  -- Handle additional items beyond tuple length
                  additionalItems = drop (length tupleSchemas) (toList array)
                  additionalStartIdx = length tupleSchemas
                  -- In draft 2019-09/2020-12, additional items are allowed by default when items is an array
                  -- They are only rejected if additionalItems is explicitly false
                  (additionalResults, additionalIndices) = case maybeAdditional of
                    Just addlSchema ->
                      -- additionalItems is explicitly set, validate against it
                      let results = [validateValueWithContext ctx' addlSchema item | item <- additionalItems]
                          indices = Set.fromList [additionalStartIdx .. length array - 1]
                      in (results, if null additionalItems then Set.empty else indices)
                    Nothing ->
                      -- additionalItems not specified - don't validate or annotate (let unevaluatedItems handle it)
                      ([], Set.empty)
                  additionalFailures = [errs | ValidationFailure errs <- additionalResults]
                  allFailures = tupleFailures <> additionalFailures
                  allIndices = tupleIndices <> additionalIndices
                  allAnnotations = [anns | ValidationSuccess anns <- tupleResults <> additionalResults]
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
          -- No contains constraint, but check for standalone minContains/maxContains
          case maybeMinContains of
            Nothing -> ValidationSuccess mempty
            Just minC ->
              -- Standalone minContains without contains is valid if minC <= array length
              if fromIntegral (length array) >= minC
                then ValidationSuccess mempty
                else validationFailure "minContains" $
                  "Array has " <> T.pack (show (length array)) <> " items, but minContains requires " <> T.pack (show minC)
        Just containsSchema ->
          let results = [validateValueWithContext ctx' containsSchema item | item <- toList array]
              matchCount = length (filter isSuccess results)
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
              else ValidationSuccess mempty

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
      case validationPrefixItems (schemaValidation schemaObj) of
        Just prefixSchemas ->
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
              let results = [validateValueWithContext ctx' itemSchema item | item <- toList array]
                  failures = [errs | ValidationFailure errs <- results]
                  allIndices = Set.fromList [0 .. length array - 1]
                  annotations = [anns | ValidationSuccess anns <- results]
              in case failures of
                [] -> ValidationSuccess $ annotateItems allIndices <> mconcat annotations
                (e:es) -> ValidationFailure $ foldl (<>) e es
            Just (ItemsTuple tupleSchemas maybeAdditional) ->
              -- Positional validation + optional additional items
              let tupleResults = zipWith (validateValueWithContext ctx') (NE.toList tupleSchemas) (toList array)
                  tupleFailures = [errs | ValidationFailure errs <- tupleResults]
                  tupleIndices = Set.fromList [0 .. length tupleSchemas - 1]
                  -- Handle additional items beyond tuple length
                  additionalItems = drop (length tupleSchemas) (toList array)
                  additionalStartIdx = length tupleSchemas
                  -- In draft 2019-09/2020-12, additional items are allowed by default when items is an array
                  -- They are only rejected if additionalItems is explicitly false
                  (additionalResults, additionalIndices) = case maybeAdditional of
                    Just addlSchema ->
                      -- additionalItems is explicitly set, validate against it
                      let results = [validateValueWithContext ctx' addlSchema item | item <- additionalItems]
                          indices = Set.fromList [additionalStartIdx .. length array - 1]
                      in (results, if null additionalItems then Set.empty else indices)
                    Nothing ->
                      -- additionalItems not specified - don't validate or annotate (let unevaluatedItems handle it)
                      ([], Set.empty)
                  additionalFailures = [errs | ValidationFailure errs <- additionalResults]
                  allFailures = tupleFailures <> additionalFailures
                  allIndices = tupleIndices <> additionalIndices
                  allAnnotations = [anns | ValidationSuccess anns <- tupleResults <> additionalResults]
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
          -- No contains constraint, but check for standalone minContains/maxContains
          case maybeMinContains of
            Nothing -> ValidationSuccess mempty
            Just minC -> 
              -- Standalone minContains without contains is valid if minC <= array length
              if fromIntegral (length array) >= minC
                then ValidationSuccess mempty
                else validationFailure "minContains" $ 
                  "Array has " <> T.pack (show (length array)) <> " items, but minContains requires " <> T.pack (show minC)
        Just containsSchema ->
          let results = [validateValueWithContext ctx' containsSchema item | item <- toList array]
              matchCount = length (filter isSuccess results)
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
              else ValidationSuccess mempty
    
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
        , validateDependentRequired validation objMap
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
          results = case maybePropSchemas of
            Nothing -> []
            Just propSchemas ->
              [ validateValueWithContext ctx' propSchema propValue
              | (propName, propSchema) <- Map.toList propSchemas
              , Just propValue <- [KeyMap.lookup (Key.fromText propName) om]
              ]

          -- Also check pattern properties (independent of 'properties' keyword)
          patternCoveredProps = case validationPatternProperties (schemaValidation schemaObj) of
            Nothing -> Set.empty
            Just patternSchemas -> Set.fromList
              [ Key.toText k
              | k <- KeyMap.keys om
              , let propName = Key.toText k
              , (Regex pattern, _) <- Map.toList patternSchemas
              , case compileRegex pattern of
                  Right regex -> matchRegex regex propName
                  Left _ -> False
              ]
          patternResults = case validationPatternProperties (schemaValidation schemaObj) of
            Nothing -> []
            Just patternSchemas ->
              [ validateValueWithContext ctx' patternSchema propValue
              | (k, propValue) <- KeyMap.toList om
              , let propName = Key.toText k
              , (Regex pattern, patternSchema) <- Map.toList patternSchemas
              , case compileRegex pattern of
                  Right regex -> matchRegex regex propName
                  Left _ -> False
              ]

          allResults = results <> patternResults
          allEvaluatedProps = evaluatedProps <> patternCoveredProps
          failures = [errs | ValidationFailure errs <- allResults]
          annotations = [anns | ValidationSuccess anns <- allResults]
      in case failures of
        [] -> ValidationSuccess $ annotateProperties allEvaluatedProps <> mconcat annotations
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
                , case compileRegex pattern of
                    Right regex -> matchRegex regex propName
                    Left _ -> False
                ]
            
            -- Additional properties are those not covered by either
            allCoveredProps = definedProps <> patternCoveredProps
            additionalPropsList = [(k, v) | (k, v) <- KeyMap.toList om
                              , not $ Set.member (Key.toText k) allCoveredProps]
            additionalPropNames = Set.fromList [Key.toText k | (k, _) <- additionalPropsList]
            results = [validateValueWithContext ctx' addlSchema v | (_, v) <- additionalPropsList]
            failures = [errs | ValidationFailure errs <- results]
            annotations = [anns | ValidationSuccess anns <- results]
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
extractEvaluatedProperties :: ValidationAnnotations -> Set Text
extractEvaluatedProperties (ValidationAnnotations annMap) =
  -- Debug: trace the annotations being processed
  let result = Set.unions
        [ case Map.lookup "properties" innerMap of
            Just (Aeson.Array arr) -> Set.fromList
              [ txt
              | Aeson.String txt <- toList arr
              ]
            _ -> Set.empty
        | innerMap <- Map.elems annMap
        ]
  in result -- Debug output removed for production

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
extractEvaluatedItems :: ValidationAnnotations -> Set Int
extractEvaluatedItems (ValidationAnnotations annMap) =
  Set.unions
    [ case Map.lookup "items" innerMap of
        Just (Aeson.Array arr) -> Set.fromList
          [ idx
          | Aeson.Number n <- toList arr
          , Just idx <- [Sci.toBoundedInteger n]
          ]
        _ -> Set.empty
    | innerMap <- Map.elems annMap
    ]

-- | Validate dependentRequired (2019-09+)
-- If a property exists, then the properties in the value set must also exist
validateDependentRequired :: SchemaValidation -> KeyMap.KeyMap Value -> ValidationResult
validateDependentRequired validation objMap = case validationDependentRequired validation of
  Nothing -> ValidationSuccess mempty
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

-- | Resolve a $ref reference using the schema registry and base URI
-- Supports:
-- - Root reference: #
-- - JSON Pointer references: #/definitions/foo
-- - Anchor references: #anchor-name
-- - Relative URI references: definitions.json#/foo
-- - Absolute URI references: http://example.com/schema#/foo
-- Also handles URL-encoded fragments (e.g., #/definitions/percent%25field)
resolveReference :: Reference -> ValidationContext -> Maybe (Schema, Maybe Text)
resolveReference (Reference refText) ctx
  | T.null refText = Nothing
  | refText == "#" =
      -- Root schema reference
      fmap (\schema -> (schema, contextBaseURI ctx)) (contextRootSchema ctx)
  | T.isPrefixOf "#/" refText =
      -- JSON Pointer reference within current document
      -- parsePointer handles URL decoding internally
      let pointer = T.drop 1 refText  -- Remove #
      in resolvePointerInCurrentSchema pointer ctx >>= \schema ->
           Just (schema, contextBaseURI ctx)
  | T.isPrefixOf "#" refText =
      -- Anchor reference (#foo) or JSON Pointer in compact form
      let anchorName = T.drop 1 refText
          baseURI = contextBaseURI ctx
          registry = contextSchemaRegistry ctx
          -- First try as an anchor
          tryAnchor = case baseURI of
            Just uri ->
              -- Try with explicit base URI first
              case Map.lookup (uri, anchorName) (registryAnchors registry) of
                Just s -> Just (s, Just uri)
                Nothing ->
                  -- Fallback: try with empty string base (for fragment-only $id)
                  case Map.lookup ("", anchorName) (registryAnchors registry) of
                    Just s -> Just (s, Nothing)
                    Nothing -> lookupAnyAnchor anchorName registry
            Nothing ->
              -- No base URI, try with empty string base first
              case Map.lookup ("", anchorName) (registryAnchors registry) of
                Just s -> Just (s, Nothing)
                Nothing ->
                  -- Fallback: try looking up with any base
                  lookupAnyAnchor anchorName registry
          -- If not found as anchor, try as JSON Pointer (without leading slash)
          tryPointer = resolvePointerInCurrentSchema ("/" <> anchorName) ctx >>= \schema ->
            Just (schema, contextBaseURI ctx)
      in case tryAnchor of
        Just result -> Just result
        Nothing -> tryPointer
  | T.any (== '#') refText =
      -- URI with fragment
      let (uriPart, fragment) = T.breakOn "#" refText
          fragmentPart = T.drop 1 fragment
          registry = contextSchemaRegistry ctx
          baseContext = if T.null uriPart then Nothing else Just uriPart
      in case Map.lookup refText (registrySchemas registry) of
            Just schema ->
              Just (schema, baseContext)
            Nothing ->
              case Map.lookup uriPart (registrySchemas registry) of
                Just schema
                  | T.null fragmentPart ->
                      Just (schema, baseContext)
                  | T.isPrefixOf "/" fragmentPart ->
                      resolvePointerInSchema fragmentPart schema >>= \resolved ->
                        Just (resolved, baseContext)
                  | otherwise ->
                      let anchors = registryAnchors registry
                      in maybe
                           (lookupAnyAnchor fragmentPart registry)
                           (\resolved -> Just (resolved, baseContext))
                           (Map.lookup (uriPart, fragmentPart) anchors)
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
           Just (schema, baseForContext)
  where
    lookupAnyAnchor :: Text -> SchemaRegistry -> Maybe (Schema, Maybe Text)
    lookupAnyAnchor anchorName registry =
      case [ (base, schema)
           | ((base, name), schema) <- Map.toList (registryAnchors registry)
           , name == anchorName
           ] of
        ((baseUri, schema):_) ->
          let baseResult = if T.null baseUri then Nothing else Just baseUri
          in Just (schema, baseResult)
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
  case parsePointer pointer of
    Left _ -> Nothing
    Right (JSONPointer segments) -> followPointer segments schema
  where
    followPointer :: [Text] -> Schema -> Maybe Schema
    followPointer [] s = Just s
    followPointer (seg:rest) s = case schemaCore s of
      BooleanSchema _ -> Nothing
      ObjectSchema obj -> navigateObject seg rest s obj
    
    navigateObject :: Text -> [Text] -> Schema -> SchemaObject -> Maybe Schema
    navigateObject seg rest parentSchema obj
      -- Navigate into $defs or definitions
      | seg == "$defs" || seg == "definitions" =
          case rest of
            (defName:remaining) ->
              case Map.lookup defName (schemaDefs obj) of
                Just subSchema -> followPointer remaining subSchema
                Nothing -> Nothing
            [] -> Nothing  -- Need a definition name
      
      -- Navigate into properties
      | seg == "properties" =
          case rest of
            (propName:remaining) ->
              case validationProperties (schemaValidation obj) >>= Map.lookup propName of
                Just propSchema -> followPointer remaining propSchema
                Nothing -> Nothing
            [] -> Nothing  -- Need a property name
      
      -- Navigate into patternProperties
      | seg == "patternProperties" =
          case rest of
            (patternKey:remaining) ->
              case validationPatternProperties (schemaValidation obj) of
                Just patterns ->
                  case [(patSchema, pat) | (Regex pat, patSchema) <- Map.toList patterns, pat == patternKey] of
                    ((patSchema, _):_) -> followPointer remaining patSchema
                    [] -> Nothing
                Nothing -> Nothing
            [] -> Nothing
      
      -- Navigate into additionalProperties
      | seg == "additionalProperties" =
          validationAdditionalProperties (schemaValidation obj) >>= followPointer rest
      
      -- Navigate into items
      | seg == "items" =
          case validationItems (schemaValidation obj) of
            Just (ItemsSchema itemSchema) -> followPointer rest itemSchema
            Just (ItemsTuple schemas _) ->
              case rest of
                (idx:remaining) ->
                  case reads (T.unpack idx) :: [(Int, String)] of
                    [(n, "")] | n >= 0 && n < length schemas ->
                      followPointer remaining (NE.toList schemas !! n)
                    _ -> Nothing
                [] -> Nothing
            Nothing -> Nothing
      
      -- Navigate into prefixItems (2020-12+)
      | seg == "prefixItems" =
          case validationPrefixItems (schemaValidation obj) of
            Just prefixSchemas ->
              case rest of
                (idx:remaining) ->
                  case reads (T.unpack idx) :: [(Int, String)] of
                    [(n, "")] | n >= 0 && n < length prefixSchemas ->
                      followPointer remaining (NE.toList prefixSchemas !! n)
                    _ -> Nothing
                [] -> Nothing
            Nothing -> Nothing
      
      -- Navigate into allOf
      | seg == "allOf" =
          case schemaAllOf obj of
            Just schemas ->
              case rest of
                (idx:remaining) ->
                  case reads (T.unpack idx) :: [(Int, String)] of
                    [(n, "")] | n >= 0 && n < length schemas ->
                      followPointer remaining (NE.toList schemas !! n)
                    _ -> Nothing
                [] -> Nothing
            Nothing -> Nothing
      
      -- Navigate into anyOf
      | seg == "anyOf" =
          case schemaAnyOf obj of
            Just schemas ->
              case rest of
                (idx:remaining) ->
                  case reads (T.unpack idx) :: [(Int, String)] of
                    [(n, "")] | n >= 0 && n < length schemas ->
                      followPointer remaining (NE.toList schemas !! n)
                    _ -> Nothing
                [] -> Nothing
            Nothing -> Nothing
      
      -- Navigate into oneOf
      | seg == "oneOf" =
          case schemaOneOf obj of
            Just schemas ->
              case rest of
                (idx:remaining) ->
                  case reads (T.unpack idx) :: [(Int, String)] of
                    [(n, "")] | n >= 0 && n < length schemas ->
                      followPointer remaining (NE.toList schemas !! n)
                    _ -> Nothing
                [] -> Nothing
            Nothing -> Nothing
      
      -- Navigate into not
      | seg == "not" =
          schemaNot obj >>= followPointer rest
      
      -- Navigate into if/then/else
      | seg == "if" = schemaIf obj >>= followPointer rest
      | seg == "then" = schemaThen obj >>= followPointer rest
      | seg == "else" = schemaElse obj >>= followPointer rest
      
      -- Navigate into dependentSchemas
      | seg == "dependentSchemas" =
          case rest of
            (propName:remaining) ->
              validationDependentSchemas (schemaValidation obj) >>= Map.lookup propName >>= followPointer remaining
            [] -> Nothing
      
      -- Navigate into contains
      | seg == "contains" =
          validationContains (schemaValidation obj) >>= followPointer rest
      
      -- Navigate into propertyNames
      | seg == "propertyNames" =
          validationPropertyNames (schemaValidation obj) >>= followPointer rest
      
      -- Navigate into unevaluatedProperties
      | seg == "unevaluatedProperties" =
          validationUnevaluatedProperties (schemaValidation obj) >>= followPointer rest
      
      -- Fallback: check schemaExtensions for arbitrary keywords
      | otherwise =
          case Map.lookup seg (schemaExtensions parentSchema) of
            Just val ->
              -- Try to parse the value as a schema
              let version = fromMaybe Draft07 (schemaVersion parentSchema)
              in case Parser.parseSchemaWithVersion version val of
                Right schema -> followPointer rest schema
                Left _ ->
                  -- Not a schema - check if we need array indexing
                  case (val, rest) of
                    (Aeson.Array arr, (idx:remaining)) ->
                      case reads (T.unpack idx) :: [(Int, String)] of
                        [(n, "")] | n >= 0 && n < length arr ->
                          -- Try to parse the array element as a schema
                          case Parser.parseSchemaWithVersion version (arr ! n) of
                            Right schema -> followPointer remaining schema
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
resolveDynamicRef :: Reference -> ValidationContext -> Maybe Schema
resolveDynamicRef (Reference refText) ctx
  -- $dynamicRef should start with # to indicate an anchor reference
  | T.isPrefixOf "#" refText =
      let anchorName = T.drop 1 refText  -- Remove the # prefix
      in findSchemaWithDynamicAnchor anchorName (contextDynamicScope ctx)
  | otherwise = Nothing  -- Not a valid $dynamicRef format
  where
    -- Search for a schema with matching $dynamicAnchor in the dynamic scope
    findSchemaWithDynamicAnchor :: Text -> [Schema] -> Maybe Schema
    findSchemaWithDynamicAnchor _anchor [] = Nothing
    findSchemaWithDynamicAnchor anchor (schema:rest) = case schemaCore schema of
      ObjectSchema obj ->
        case schemaDynamicAnchor obj of
          Just dynAnchor | dynAnchor == anchor -> Just schema
          _ -> findSchemaWithDynamicAnchor anchor rest
      _ -> findSchemaWithDynamicAnchor anchor rest

-- | Compile regex pattern
-- Automatically adds Unicode flag if pattern contains Unicode property escapes
compileRegex :: Text -> Either Text (Regex.Regex)
compileRegex pattern =
  let flags = if needsUnicodeMode pattern then [R.Unicode] else []
  in case Regex.compileText pattern flags of
      Left err -> Left $ T.pack $ "Invalid regex: " <> err
      Right regex -> Right regex

-- | Check if a pattern needs Unicode mode
-- Unicode property escapes (\p{...} or \P{...}) require Unicode mode
-- Character class escapes (\d, \D, \w, \W, \s, \S) need Unicode mode for proper multi-byte character matching
-- Also, patterns with non-BMP characters (code points > 0xFFFF) require Unicode mode
--
-- Why character classes need Unicode mode:
-- In byte mode, libregexp treats each UTF-8 byte separately. For example, "é" (U+00E9)
-- is encoded as UTF-8 bytes [0xC3, 0xA9]. The pattern `^\\W$` expects exactly ONE non-word
-- character, but in byte mode it sees TWO bytes. In Unicode mode (UTF-16), "é" is a single
-- code unit, so `^\\W$` matches correctly.
needsUnicodeMode :: Text -> Bool
needsUnicodeMode pattern =
  -- Check for \p{...} or \P{...} patterns (after JSON parsing, these are single backslashes)
  -- We need to match against the actual text, which has the backslash
  let hasUnicodeProperty = any (`T.isInfixOf` pattern) ["\\p{", "\\P{"]
      -- Check if pattern contains any non-BMP characters (code points > 0xFFFF)
      -- These include emoji and other characters outside the Basic Multilingual Plane
      hasNonBMPChar = T.any (\c -> fromEnum c > 0xFFFF) pattern
      -- Check for character class escapes that need Unicode mode
      hasCharClass = any (`T.isInfixOf` pattern) ["\\d", "\\D", "\\w", "\\W", "\\s", "\\S"]
  in hasUnicodeProperty || hasNonBMPChar || hasCharClass

-- | Match text against regex
matchRegex :: Regex.Regex -> Text -> Bool
matchRegex regex text = Regex.test regex (TE.encodeUtf8 text)

-- | Validate format values
validateFormatValue :: Format -> Text -> ValidationResult
validateFormatValue Email text
  | isValidEmail text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid email format"
validateFormatValue IDNEmail text
  | isValidIDNEmail text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid IDN email format"
validateFormatValue URI text
  | isValidURI text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid URI format"
validateFormatValue URIRef text
  | isValidURIReference text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid URI reference"
validateFormatValue IPv4 text
  | isValidIPv4 text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid IPv4 address"
validateFormatValue IPv6 text
  | isValidIPv6 text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid IPv6 address"
validateFormatValue UUID text
  | isValidUUID text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid UUID format"
validateFormatValue DateTime text
  | isValidDateTime text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid date-time format"
validateFormatValue Date text
  | isValidDate text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid date format"
validateFormatValue Time text
  | isValidTime text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid time format"
validateFormatValue Duration text
  | isValidDuration text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid ISO 8601 duration"
validateFormatValue Hostname text
  | isValidHostname text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid hostname"
validateFormatValue IDNHostname text
  | isValidIDNHostname text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid IDN hostname"
validateFormatValue JSONPointerFormat text
  | isValidJSONPointer text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid JSON Pointer"
validateFormatValue RelativeJSONPointerFormat text
  | isValidRelativeJSONPointer text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid relative JSON Pointer"
validateFormatValue RegexFormat text
  | isValidRegex text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid ECMA-262 regex"
validateFormatValue _ _ = ValidationSuccess mempty  -- Other formats: annotation only or not implemented

-- Format validators using proper libraries
isValidEmail :: Text -> Bool
isValidEmail text = Email.isValid (TE.encodeUtf8 text)

-- IDN Email validator - validates internationalized email addresses
-- Per RFC 6531, the domain part must be a valid IDN hostname
isValidIDNEmail :: Text -> Bool
isValidIDNEmail text =
  case T.splitOn "@" text of
    [local, domain] | not (T.null local) && not (T.null domain) ->
      -- Basic structure check: must have exactly one @ with non-empty parts
      -- Validate local part: basic checks (no leading/trailing dots, valid chars)
      let validLocal = not (T.isPrefixOf "." local) &&
                      not (T.isSuffixOf "." local) &&
                      not (T.isInfixOf ".." local) &&
                      T.length local <= 64
          -- Validate domain part using IDN hostname validation
          validDomain = isValidIDNHostname domain
      in validLocal && validDomain
    _ -> False  -- Must have exactly one @

isValidURI :: Text -> Bool
isValidURI text = case URI.parseURI (T.unpack text) of
  Just _ -> True
  Nothing -> False

isValidIPv4 :: Text -> Bool
isValidIPv4 text = 
  let parts = T.splitOn "." text
      isValidOctet part = case reads (T.unpack part) :: [(Int, String)] of
        [(n, "")] -> n >= 0 && n <= 255
        _ -> False
  in length parts == 4 && all isValidOctet parts

isValidIPv6 :: Text -> Bool
isValidIPv6 text =
  let str = T.unpack text
  in case reads str :: [(IP.IPv6, String)] of
    [(_, "")] -> True
    _ -> False

isValidUUID :: Text -> Bool
isValidUUID text = case UUID.fromText text of
  Just _ -> True
  Nothing -> False

-- RFC3339 date-time validator
-- Validates according to RFC3339 as required by JSON Schema
-- Uses the timerep library for strict RFC3339 parsing with additional checks
isValidDateTime :: Text -> Bool
isValidDateTime text =
  -- First check with RFC3339 parser
  case RFC3339.parseTimeRFC3339 (T.unpack text) of
    Nothing -> False
    Just _ ->
      -- Additional validation for edge cases not caught by timerep:
      -- 1. Leap seconds must be at XX:59:60 (not other minutes like :58:60)
      -- 2. Leap seconds in UTC must be at 23:59:60
      -- 3. Timezone offsets must be -23:59 to +23:59 (not -24:00 or +24:00)
      let normalized = T.map (\c -> if c == 't' then 'T' else if c == 'z' then 'Z' else c) text
          hasLeapSecond = T.isInfixOf ":60" normalized || T.isInfixOf ":60." normalized
      in if hasLeapSecond
        then
          -- Check that leap second is at :59:60
          let hasValidMinute = T.isInfixOf ":59:60" normalized
              parts = T.splitOn "T" normalized
          in if length parts /= 2 || not hasValidMinute
            then False
            else
              let timePart = parts !! 1
                  isUTC = T.isSuffixOf "Z" timePart
                  timeStr = if isUTC
                           then T.dropEnd 1 timePart
                           else case T.breakOnEnd "+" timePart of
                                  (before, after) | not (T.null after) ->
                                    T.dropEnd (T.length after + 1) before
                                  _ -> case T.breakOnEnd "-" timePart of
                                         (before, after) | T.length after == 5 && T.elem ':' after ->
                                           T.dropEnd 6 before
                                         _ -> timePart
                  hour = case T.splitOn ":" timeStr of
                           (h:_) -> Read.readMaybe (T.unpack h) :: Maybe Int
                           _ -> Nothing
              in case (isUTC, hour) of
                   (True, Just h) -> h == 23  -- UTC leap seconds must be at 23:59:60
                   (False, _) -> True         -- Non-UTC can be at any XX:59:60
                   _ -> False
        else
          -- Check for invalid timezone offsets (hour must be < 24)
          let hasOffset = T.isInfixOf "+" normalized || (T.count "-" normalized > 2)
          in if hasOffset && not (T.isSuffixOf "Z" normalized)
            then
              let offsetStr = T.takeEnd 6 normalized  -- e.g., "+24:00" or "-24:00"
                  offsetHour = Read.readMaybe (T.unpack $ T.take 2 $ T.drop 1 offsetStr) :: Maybe Int
              in case offsetHour of
                   Just h -> h < 24
                   Nothing -> True  -- Couldn't parse, let RFC3339 parser decision stand
            else True

-- RFC3339 date validator
isValidDate :: Text -> Bool
isValidDate text = 
  case Time.iso8601ParseM (T.unpack text) :: Maybe Time.Day of
    Just _ -> True
    Nothing -> False

-- Hostname validator (RFC 1123)
isValidHostname :: Text -> Bool
isValidHostname text =
  let labels = T.splitOn "." text
      validLabel lbl =
        not (T.null lbl) &&
        T.length lbl <= 63 &&
        not (T.isPrefixOf "-" lbl) &&
        not (T.isSuffixOf "-" lbl) &&
        T.all (\c -> c == '-' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) lbl
  in not (null labels) &&
     T.length text <= 253 &&
     all validLabel labels

-- IDN Hostname validator (RFC 5890-5893)
isValidIDNHostname :: Text -> Bool
isValidIDNHostname text =
  -- Try to convert to ASCII using IDNA2008
  case IDN.toASCII text of
    Right ascii -> isValidHostname ascii  -- Validate the ASCII form
    Left _ -> False  -- IDN conversion failed

-- URI Reference validator (allows relative references)
isValidURIReference :: Text -> Bool
isValidURIReference text = case URI.parseURIReference (T.unpack text) of
  Just _ -> True
  Nothing -> False

-- Time validator (RFC3339 time format)
isValidTime :: Text -> Bool
isValidTime text =
  -- Simple check for HH:MM:SS or HH:MM:SS.sss format with optional timezone
  let timePattern = "^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})?$"
      matches = case Regex.compileText timePattern [] of
                  Left _ -> False
                  Right regex -> Regex.test regex (TE.encodeUtf8 text)
  in matches && not (T.isInfixOf ":60" text)  -- Reject leap seconds

-- ISO 8601 Duration validator
isValidDuration :: Text -> Bool
isValidDuration text =
  -- Simple regex for ISO 8601 duration: P[nY][nM][nD][T[nH][nM][nS]]
  let durationPattern = "^P(?:[0-9]+Y)?(?:[0-9]+M)?(?:[0-9]+D)?(?:T(?:[0-9]+H)?(?:[0-9]+M)?(?:[0-9]+(?:\\.[0-9]+)?S)?)?$"
      matches = case Regex.compileText durationPattern [] of
                  Left _ -> False
                  Right regex -> Regex.test regex (TE.encodeUtf8 text)
  in T.isPrefixOf "P" text && matches

-- JSON Pointer validator (RFC 6901)
isValidJSONPointer :: Text -> Bool
isValidJSONPointer text =
  -- Either empty string or starts with /
  T.null text || T.isPrefixOf "/" text

-- Relative JSON Pointer validator
isValidRelativeJSONPointer :: Text -> Bool
isValidRelativeJSONPointer text =
  case T.uncons text of
    Just (c, _) | c >= '0' && c <= '9' ->
      -- Starts with a digit, followed by optional # or JSON pointer
      let (digits, remainder) = T.span (\x -> x >= '0' && x <= '9') text
      in not (T.null digits) &&
         (T.null remainder || T.isPrefixOf "#" remainder || T.isPrefixOf "/" remainder)
    _ -> False

-- ECMA-262 Regex validator
isValidRegex :: Text -> Bool
isValidRegex text =
  case compileRegex text of
    Right _ -> True
    Left _ -> False

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

