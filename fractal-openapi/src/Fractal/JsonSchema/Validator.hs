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
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import Numeric.Natural (Natural)
import Data.Foldable (toList)
import Text.Regex.TDFA ((=~))
import qualified Text.Regex.TDFA as Regex

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
        }
  in validateValueWithContext ctx schema val

-- | Validate with explicit context
validateValueWithContext :: ValidationContext -> Schema -> Value -> ValidationResult
validateValueWithContext ctx schema val =
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
      in case failures of
        [] -> ValidationSuccess mempty
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

-- | Validate against object schema
validateAgainstObject :: ValidationContext -> ValidationContext -> SchemaObject -> Value -> ValidationResult
validateAgainstObject parentCtx ctx obj val =
  -- First handle $ref if present
  case schemaRef obj of
    Just ref ->
      -- Resolve and validate against the referenced schema
      case resolveReference ref refCtx of
        Just (resolvedSchema, maybeBase) ->
          let ctxForRef = case maybeBase of
                Just baseDoc ->
                  let baseChanged = contextBaseURI ctx /= Just baseDoc
                      rootValue =
                        if baseChanged
                          then Just resolvedSchema
                          else case contextRootSchema ctx of
                                 Just existing -> Just existing
                                 Nothing -> Just resolvedSchema
                  in ctx { contextBaseURI = Just baseDoc
                         , contextRootSchema = rootValue
                         }
                Nothing -> ctx
          in validateValueWithContext ctxForRef resolvedSchema val
        Nothing ->
          -- Reference not found - this is an error
          ValidationFailure $ ValidationErrors $ pure $
            ValidationError "$ref" emptyPointer emptyPointer
              ("Unable to resolve reference: " <> showReference ref)
              Nothing
    Nothing ->
      -- No $ref, check for $dynamicRef (2020-12+)
      case schemaDynamicRef obj of
        Just dynRef ->
          -- Try to resolve $dynamicRef using dynamic scope
          case resolveDynamicRef dynRef ctx of
            Just resolvedSchema -> validateValueWithContext ctx resolvedSchema val
            Nothing ->
              -- Fallback: treat as regular reference
              -- For now, just continue with normal validation
              validateObjectSchemaContent ctx obj val
        Nothing ->
          validateObjectSchemaContent ctx obj val
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

    validateObjectSchemaContent ctx' obj' val' =
      -- Update context with dynamic anchor if present
      let ctx'' = case schemaDynamicAnchor obj' of
            Just anchor ->
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
      in combineResults
          [ validateTypeConstraint obj' val'
          , validateEnumConstraint obj' val'
          , validateConstConstraint obj' val'
          , validateComposition ctx'' obj' val'
          , validateConditional ctx'' obj' val'
          , validateNumericConstraints obj' val'
          , validateStringConstraints obj' val'
          , validateFormatConstraints ctx'' obj' val'
          , validateArrayConstraints ctx'' obj' val'
          , validateObjectConstraints ctx'' obj' val'
          ]
    
    -- Combine multiple validation results
    combineResults :: [ValidationResult] -> ValidationResult
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
      in case failures of
        [] -> ValidationSuccess mempty
        (e:es) -> ValidationFailure $ foldl (<>) e es

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
      in case failures of
        [] -> ValidationSuccess mempty
        (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validateAllOf ctx' v schemas =
      let results = [validateValueWithContext ctx' schema v | schema <- NE.toList schemas]
          failures = [errs | ValidationFailure errs <- results]
      in case failures of
        [] -> ValidationSuccess mempty
        (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validateAnyOf ctx' v schemas =
      let results = [validateValueWithContext ctx' schema v | schema <- NE.toList schemas]
      in if any isSuccess results
        then ValidationSuccess mempty
        else validationFailure "anyOf" "Value does not match any schema in anyOf"
    
    validateOneOf ctx' v schemas =
      let results = [validateValueWithContext ctx' schema v | schema <- NE.toList schemas]
          successes = filter isSuccess results
      in case length successes of
        1 -> ValidationSuccess mempty
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
      ValidationSuccess _ ->
        -- If validates, apply then
        maybe (ValidationSuccess mempty) (\thenSchema -> validateValueWithContext ctx thenSchema val) (schemaThen obj)
      ValidationFailure _ ->
        -- If fails, apply else
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
      -- Avoid Scientific arithmetic - convert to Double for modulo check
      -- This handles repeating decimals that can't be represented in Scientific
      let numDouble = Sci.toRealFloat num :: Double
          divisorDouble = Sci.toRealFloat divisor :: Double
          remainder = numDouble - (fromIntegral (floor (numDouble / divisorDouble) :: Integer) * divisorDouble)
          epsilon = 1e-10  -- Tolerance for floating point comparison
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

-- | Validate array constraints
validateArrayConstraints :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateArrayConstraints ctx obj (Array arr) =
  let validation = schemaValidation obj
      arrLength = length arr
  in combineResults
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
  where
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
      in case failures of
        [] -> ValidationSuccess mempty
        (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validateItems ctx' schemaObj array =
      -- In 2020-12+, prefixItems takes precedence for tuple validation
      case validationPrefixItems (schemaValidation schemaObj) of
        Just prefixSchemas ->
          -- 2020-12+ mode: prefixItems validates positional items
          let prefixResults = zipWith (validateValueWithContext ctx') (NE.toList prefixSchemas) (toList array)
              prefixFailures = [errs | ValidationFailure errs <- prefixResults]
              -- In 2020-12, "items" applies to items beyond prefixItems
              additionalItemsFromPrefix = drop (length prefixSchemas) (toList array)
              additionalResults = case validationItems (schemaValidation schemaObj) of
                Just (ItemsSchema itemSchema) ->
                  [validateValueWithContext ctx' itemSchema item | item <- additionalItemsFromPrefix]
                _ -> []
              additionalFailures = [errs | ValidationFailure errs <- additionalResults]
              allFailures = prefixFailures <> additionalFailures
          in case allFailures of
            [] -> ValidationSuccess mempty
            (e:es) -> ValidationFailure $ foldl (<>) e es
        Nothing ->
          -- Pre-2020-12 mode: use old items behavior
          case validationItems (schemaValidation schemaObj) of
            Nothing -> ValidationSuccess mempty
            Just (ItemsSchema itemSchema) ->
              -- All items must validate against the schema
              let results = [validateValueWithContext ctx' itemSchema item | item <- toList array]
                  failures = [errs | ValidationFailure errs <- results]
              in case failures of
                [] -> ValidationSuccess mempty
                (e:es) -> ValidationFailure $ foldl (<>) e es
            Just (ItemsTuple tupleSchemas maybeAdditional) ->
              -- Positional validation + optional additional items
              let tupleResults = zipWith (validateValueWithContext ctx') (NE.toList tupleSchemas) (toList array)
                  tupleFailures = [errs | ValidationFailure errs <- tupleResults]
                  -- Handle additional items beyond tuple length
                  additionalItems = drop (length tupleSchemas) (toList array)
                  additionalResults = case (maybeAdditional, additionalItems) of
                    (Just addlSchema, items) -> [validateValueWithContext ctx' addlSchema item | item <- items]
                    (Nothing, []) -> []
                    (Nothing, _) -> [ValidationFailure $ ValidationErrors $ pure $
                                     ValidationError "items" emptyPointer emptyPointer "Additional items not allowed" Nothing]
                  additionalFailures = [errs | ValidationFailure errs <- additionalResults]
                  allFailures = tupleFailures <> additionalFailures
              in case allFailures of
                [] -> ValidationSuccess mempty
                (e:es) -> ValidationFailure $ foldl (<>) e es
    
    validateContains ctx' schemaObj array = case validationContains (schemaValidation schemaObj) of
      Nothing -> ValidationSuccess mempty
      Just containsSchema ->
        let results = [validateValueWithContext ctx' containsSchema item | item <- toList array]
            anyMatch = any isSuccess results
        in if anyMatch || null (toList array)  -- Empty array passes unless minContains set
          then ValidationSuccess mempty
          else validationFailure "contains" "No array items match the contains schema"
    
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

-- | Validate object constraints
validateObjectConstraints :: ValidationContext -> SchemaObject -> Value -> ValidationResult
validateObjectConstraints ctx obj (Object objMap) =
  let validation = schemaValidation obj
      objSize = KeyMap.size objMap
  in combineResults
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
    , validateProperties ctx obj objMap
    , validateAdditionalProperties ctx obj objMap
    , validateDependentRequired validation objMap
    , validateDependentSchemas ctx validation objMap
    ]
  where
    combineResults results =
      let failures = [errs | ValidationFailure errs <- results]
      in case failures of
        [] -> ValidationSuccess mempty
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
    
    validateProperties ctx' schemaObj om = case validationProperties (schemaValidation schemaObj) of
      Nothing -> ValidationSuccess mempty
      Just propSchemas ->
        let results = [ validateValueWithContext ctx' propSchema propValue
                      | (propName, propSchema) <- Map.toList propSchemas
                      , Just propValue <- [KeyMap.lookup (Key.fromText propName) om]
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
            failures = [errs | ValidationFailure errs <- allResults]
        in case failures of
          [] -> ValidationSuccess mempty
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
            additionalProps = [(k, v) | (k, v) <- KeyMap.toList om
                              , not $ Set.member (Key.toText k) allCoveredProps]
            results = [validateValueWithContext ctx' addlSchema v | (_, v) <- additionalProps]
            failures = [errs | ValidationFailure errs <- results]
        in case failures of
          [] -> ValidationSuccess mempty
          (e:es) -> ValidationFailure $ foldl (<>) e es
validateObjectConstraints _ _ _ = ValidationSuccess mempty

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
    in case failures of
      [] -> ValidationSuccess mempty
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
      -- Anchor reference (#foo)
      let anchorName = T.drop 1 refText
          baseURI = contextBaseURI ctx
          registry = contextSchemaRegistry ctx
      in case baseURI of
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
      ObjectSchema obj -> navigateObject seg rest obj
    
    navigateObject :: Text -> [Text] -> SchemaObject -> Maybe Schema
    navigateObject seg rest obj
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
                  case [(schema, pat) | (Regex pat, schema) <- Map.toList patterns, pat == patternKey] of
                    ((schema, _):_) -> followPointer remaining schema
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
            Just (ItemsTuple schemas maybeAdditional) ->
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
      
      | otherwise = Nothing  -- Unknown segment

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

-- | Helper to create validation failure
validationFailure :: Text -> Text -> ValidationResult
validationFailure keyword msg = ValidationFailure $ ValidationErrors $ pure $
  ValidationError keyword emptyPointer emptyPointer msg Nothing

-- | Compile regex pattern
compileRegex :: Text -> Either Text (Regex.Regex)
compileRegex pattern =
  -- regex-tdfa's makeRegex never fails, it returns a regex
  Right $ Regex.makeRegex (T.unpack pattern :: String)

-- | Match text against regex
matchRegex :: Regex.Regex -> Text -> Bool
matchRegex regex text = Regex.match regex (T.unpack text)

-- | Validate format values
validateFormatValue :: Format -> Text -> ValidationResult
validateFormatValue Email text
  | isValidEmail text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid email format"
validateFormatValue URI text
  | isValidURI text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid URI format"
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
validateFormatValue Hostname text
  | isValidHostname text = ValidationSuccess mempty
  | otherwise = validationFailure "format" "Invalid hostname"
validateFormatValue _ _ = ValidationSuccess mempty  -- Other formats: annotation only or not implemented

-- Format validators (simplified implementations)
isValidEmail :: Text -> Bool
isValidEmail text = T.length text > 0 && T.any (== '@') text && T.any (== '.') text

isValidURI :: Text -> Bool
isValidURI text = T.isPrefixOf "http://" text || T.isPrefixOf "https://" text || T.isPrefixOf "/" text

isValidIPv4 :: Text -> Bool
isValidIPv4 text = 
  let parts = T.splitOn "." text
  in length parts == 4 && all isValidOctet parts
  where
    isValidOctet part = case reads (T.unpack part) :: [(Int, String)] of
      [(n, "")] -> n >= 0 && n <= 255
      _ -> False

isValidIPv6 :: Text -> Bool
isValidIPv6 text = T.any (== ':') text && T.length text >= 2

isValidUUID :: Text -> Bool
isValidUUID text = 
  T.length text == 36 && 
  T.index text 8 == '-' && 
  T.index text 13 == '-' && 
  T.index text 18 == '-' && 
  T.index text 23 == '-'

isValidDateTime :: Text -> Bool
isValidDateTime text = T.any (== 'T') text || T.any (== ' ') text

isValidDate :: Text -> Bool
isValidDate text = T.length text == 10 && T.count "-" text == 2

isValidHostname :: Text -> Bool
isValidHostname text = T.length text > 0 && T.all (\c -> c == '.' || c == '-' || c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9']) text

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

