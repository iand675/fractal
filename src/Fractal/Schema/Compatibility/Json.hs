{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | JSON Schema compatibility checking following schema evolution best practices.
--
-- This module implements compatibility checking between JSON Schemas according to
-- common schema evolution patterns. The compatibility checks ensure that schemas
-- can evolve while maintaining the ability to validate data across different
-- schema versions.
--
-- == Compatibility Types
--
-- The module supports several compatibility levels:
--
-- * __BACKWARD__: A new schema is backward compatible if it can validate data that was valid under the old schema.
--   This is the most common compatibility mode. You can:
--
--     * Add optional fields (not required)
--     * Remove required constraints (make fields optional)
--     * Widen types (e.g., from integer to number)
--     * Add new enum values (if used as input)
--
-- * __FORWARD__: A new schema is forward compatible if the old schema can validate data that is valid under the new schema.
--   You can:
--
--     * Remove optional fields
--     * Add required constraints (make fields required)
--     * Narrow types (e.g., from number to integer)
--     * Remove enum values
--
-- * __FULL__: A schema is fully compatible if it is both backward and forward compatible.
--   This is the most restrictive mode.
--
-- * __TRANSITIVE__ variants: These check compatibility across all historical versions,
--   not just the immediate predecessor.
--
-- == Compatibility Rules
--
-- === Types
-- * Identical types are always compatible
-- * Type widening allowed for backward compatibility:
--     * integer → number
--     * More restrictive → less restrictive (e.g., removing format constraints)
-- * Type narrowing allowed for forward compatibility:
--     * number → integer (with appropriate constraints)
--
-- === Required Fields
-- * For backward compatibility:
--     * Can add optional fields
--     * Cannot add required fields
--     * Can remove required constraint (make fields optional)
-- * For forward compatibility:
--     * Can remove optional fields
--     * Can add required constraint (make fields optional → required)
--     * Cannot remove required fields
--
-- === Properties
-- * For backward compatibility: new schema must accept all valid old data
-- * For forward compatibility: old schema must accept all valid new data
--
-- === Enums
-- * For backward compatibility: can add new enum values
-- * For forward compatibility: can remove enum values
-- * Removing values in backward mode or adding in forward mode breaks compatibility
--
-- === Constraints
-- * Relaxing constraints (increasing max, decreasing min) is backward compatible
-- * Tightening constraints is forward compatible
--
-- == Examples
--
-- Backward compatible changes:
--
-- @
-- -- Old schema
-- {
--   "type": "object",
--   "properties": {
--     "id": {"type": "integer"}
--   },
--   "required": ["id"]
-- }
--
-- -- New schema (backward compatible)
-- {
--   "type": "object",
--   "properties": {
--     "id": {"type": "integer"},
--     "name": {"type": "string"}
--   },
--   "required": ["id"]
-- }
-- @
--
-- Forward compatible changes:
--
-- @
-- -- Old schema
-- {
--   "type": "object",
--   "properties": {
--     "id": {"type": "integer"},
--     "email": {"type": "string"}
--   },
--   "required": ["id"]
-- }
--
-- -- New schema (forward compatible)
-- {
--   "type": "object",
--   "properties": {
--     "id": {"type": "integer"}
--   },
--   "required": ["id"]
-- }
-- @
module Fractal.Schema.Compatibility.Json where

import Data.Aeson (Value(..), Object, decode)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Fractal.Schema.Types (CompatibilityLevel(..))
import qualified Data.Vector as V
import Data.String (IsString(..))
import Data.Scientific (Scientific, isInteger)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust)

-- | Represents the result of a schema compatibility check
data CompatibilityResult
  = Compatible
  | Incompatible (NonEmpty CompatibilityError)
  deriving stock (Show, Eq, Generic)

-- | A path component in the schema
data PathComponent
  = PathProperty Text
  | PathItem
  | PathType
  | PathEnum
  | PathRequired
  | PathMinimum
  | PathMaximum
  | PathMinLength
  | PathMaxLength
  | PathPattern
  | PathFormat
  | PathAdditionalProperties
  deriving stock (Show, Eq, Generic)

instance IsString PathComponent where
  fromString = PathProperty . T.pack

type SchemaPath = [PathComponent]

-- | Detailed error information for schema compatibility issues
data CompatibilityError = CompatibilityError
  { errorPath :: SchemaPath
  , errorType :: ErrorType
  , errorDetails :: Text
  } deriving stock (Show, Eq, Generic)

-- | Types of compatibility errors
data ErrorType
  = TypeMismatch
  | RequiredFieldAdded
  | RequiredFieldRemoved
  | EnumValueRemoved
  | EnumValueAdded
  | ConstraintTightened
  | ConstraintRelaxed
  | PropertyRemoved
  | PropertyAdded
  | TypeNarrowed
  | TypeWidened
  | FormatChanged
  deriving stock (Show, Eq, Generic)

-- | JSON Schema representation
data JsonSchema = JsonSchema
  { jsType :: Maybe JsonType
  , jsProperties :: Maybe Object
  , jsRequired :: Maybe [Text]
  , jsEnum :: Maybe [Value]
  , jsMinimum :: Maybe Scientific
  , jsMaximum :: Maybe Scientific
  , jsMinLength :: Maybe Int
  , jsMaxLength :: Maybe Int
  , jsPattern :: Maybe Text
  , jsFormat :: Maybe Text
  , jsItems :: Maybe Value
  , jsAdditionalProperties :: Maybe Value
  , jsAllOf :: Maybe [Value]
  , jsAnyOf :: Maybe [Value]
  , jsOneOf :: Maybe [Value]
  , jsNot :: Maybe Value
  } deriving stock (Show, Eq, Generic)

data JsonType
  = JTString
  | JTNumber
  | JTInteger
  | JTBoolean
  | JTObject
  | JTArray
  | JTNull
  | JTMultiple [JsonType]
  deriving stock (Show, Eq, Generic)

-- | Parse a JSON Schema from a Value
parseSchema :: Value -> Maybe JsonSchema
parseSchema (Object obj) = Just JsonSchema
  { jsType = parseType =<< KM.lookup "type" obj
  , jsProperties = case KM.lookup "properties" obj of
      Just (Object props) -> Just props
      _ -> Nothing
  , jsRequired = case KM.lookup "required" obj of
      Just (Array arr) -> Just $ V.toList $ V.mapMaybe (\case String s -> Just s; _ -> Nothing) arr
      _ -> Nothing
  , jsEnum = case KM.lookup "enum" obj of
      Just (Array arr) -> Just $ V.toList arr
      _ -> Nothing
  , jsMinimum = case KM.lookup "minimum" obj of
      Just (Number n) -> Just n
      _ -> Nothing
  , jsMaximum = case KM.lookup "maximum" obj of
      Just (Number n) -> Just n
      _ -> Nothing
  , jsMinLength = case KM.lookup "minLength" obj of
      Just (Number n) | isInteger n -> Just $ floor n
      _ -> Nothing
  , jsMaxLength = case KM.lookup "maxLength" obj of
      Just (Number n) | isInteger n -> Just $ floor n
      _ -> Nothing
  , jsPattern = case KM.lookup "pattern" obj of
      Just (String s) -> Just s
      _ -> Nothing
  , jsFormat = case KM.lookup "format" obj of
      Just (String s) -> Just s
      _ -> Nothing
  , jsItems = KM.lookup "items" obj
  , jsAdditionalProperties = KM.lookup "additionalProperties" obj
  , jsAllOf = case KM.lookup "allOf" obj of
      Just (Array arr) -> Just $ V.toList arr
      _ -> Nothing
  , jsAnyOf = case KM.lookup "anyOf" obj of
      Just (Array arr) -> Just $ V.toList arr
      _ -> Nothing
  , jsOneOf = case KM.lookup "oneOf" obj of
      Just (Array arr) -> Just $ V.toList arr
      _ -> Nothing
  , jsNot = KM.lookup "not" obj
  }
parseSchema _ = Nothing

parseType :: Value -> Maybe JsonType
parseType (String s) = case s of
  "string" -> Just JTString
  "number" -> Just JTNumber
  "integer" -> Just JTInteger
  "boolean" -> Just JTBoolean
  "object" -> Just JTObject
  "array" -> Just JTArray
  "null" -> Just JTNull
  _ -> Nothing
parseType (Array arr) = do
  types <- traverse parseType (V.toList arr)
  pure $ JTMultiple types
parseType _ = Nothing

-- | Check if a sequence of schemas satisfies the given compatibility level
-- Schemas should be ordered newest to oldest
checkCompatibilityLevel :: CompatibilityLevel -> NonEmpty Value -> CompatibilityResult
checkCompatibilityLevel level schemas = case level of
  NONE -> Compatible
  BACKWARD -> checkBackwardCompatibility (NE.head schemas) (NE.last schemas)
  FORWARD -> checkForwardCompatibility (NE.head schemas) (NE.last schemas)
  FULL -> checkFullCompatibility (NE.head schemas) (NE.last schemas)
  BACKWARD_TRANSITIVE -> checkBackwardTransitive schemas
  FORWARD_TRANSITIVE -> checkForwardTransitive schemas
  FULL_TRANSITIVE -> checkFullTransitive schemas

-- | Check backward compatibility across all adjacent pairs in the sequence
checkBackwardTransitive :: NonEmpty Value -> CompatibilityResult
checkBackwardTransitive schemas =
  case NE.tail schemas of
    [] -> Compatible
    rest -> case checkBackwardCompatibility (NE.head schemas) (NE.head $ NE.fromList rest) of
      Compatible -> checkBackwardTransitive (NE.fromList rest)
      Incompatible errs -> Incompatible $ NE.map (\err -> err { errorPath = PathProperty "backward_transitive" : errorPath err }) errs

-- | Check forward compatibility across all adjacent pairs in the sequence
checkForwardTransitive :: NonEmpty Value -> CompatibilityResult
checkForwardTransitive schemas =
  case NE.tail schemas of
    [] -> Compatible
    rest -> case checkForwardCompatibility (NE.head schemas) (NE.head $ NE.fromList rest) of
      Compatible -> checkForwardTransitive (NE.fromList rest)
      Incompatible errs -> Incompatible $ NE.map (\err -> err { errorPath = PathProperty "forward_transitive" : errorPath err }) errs

-- | Check full compatibility across all adjacent pairs in the sequence
checkFullTransitive :: NonEmpty Value -> CompatibilityResult
checkFullTransitive schemas =
  case NE.tail schemas of
    [] -> Compatible
    rest -> case checkFullCompatibility (NE.head schemas) (NE.head $ NE.fromList rest) of
      Compatible -> checkFullTransitive (NE.fromList rest)
      Incompatible errs -> Incompatible $ NE.map (\err -> err { errorPath = PathProperty "full_transitive" : errorPath err }) errs

-- | Check if a schema is backward compatible with another schema
-- Backward compatibility means that data valid under the old schema is still valid under the new schema
-- (new schema is more permissive or the same)
checkBackwardCompatibility :: Value -> Value -> CompatibilityResult
checkBackwardCompatibility oldSchemaVal newSchemaVal =
  case (parseSchema oldSchemaVal, parseSchema newSchemaVal) of
    (Just oldSchema, Just newSchema) -> checkBackward [] oldSchema newSchema
    (Nothing, _) -> Incompatible $ NE.singleton $ CompatibilityError [] TypeMismatch "Failed to parse old schema"
    (_, Nothing) -> Incompatible $ NE.singleton $ CompatibilityError [] TypeMismatch "Failed to parse new schema"

-- | Check if a schema is forward compatible with another schema
-- Forward compatibility means that data valid under the new schema is still valid under the old schema
-- (old schema is more permissive or the same as new schema)
checkForwardCompatibility :: Value -> Value -> CompatibilityResult
checkForwardCompatibility oldSchemaVal newSchemaVal =
  case (parseSchema oldSchemaVal, parseSchema newSchemaVal) of
    (Just oldSchema, Just newSchema) -> checkForward [] oldSchema newSchema
    (Nothing, _) -> Incompatible $ NE.singleton $ CompatibilityError [] TypeMismatch "Failed to parse old schema"
    (_, Nothing) -> Incompatible $ NE.singleton $ CompatibilityError [] TypeMismatch "Failed to parse new schema"

-- | Check if two schemas are fully compatible (both forward and backward)
checkFullCompatibility :: Value -> Value -> CompatibilityResult
checkFullCompatibility schema1 schema2 =
  case (checkBackwardCompatibility schema1 schema2, checkForwardCompatibility schema1 schema2) of
    (Compatible, Compatible) -> Compatible
    (Incompatible backwardErrs, Compatible) ->
      Incompatible $ NE.map (\err -> err { errorPath = PathProperty "backward" : errorPath err }) backwardErrs
    (Compatible, Incompatible forwardErrs) ->
      Incompatible $ NE.map (\err -> err { errorPath = PathProperty "forward" : errorPath err }) forwardErrs
    (Incompatible backwardErrs, Incompatible forwardErrs) ->
      Incompatible $ NE.map (\err -> err { errorPath = PathProperty "backward" : errorPath err }) backwardErrs
        <> NE.map (\err -> err { errorPath = PathProperty "forward" : errorPath err }) forwardErrs

-- | Check backward compatibility between two parsed schemas
checkBackward :: SchemaPath -> JsonSchema -> JsonSchema -> CompatibilityResult
checkBackward path oldSchema newSchema =
  let errors = concat
        [ checkTypeBackward path oldSchema newSchema
        , checkRequiredBackward path oldSchema newSchema
        , checkPropertiesBackward path oldSchema newSchema
        , checkEnumBackward path oldSchema newSchema
        , checkConstraintsBackward path oldSchema newSchema
        ]
  in case errors of
    [] -> Compatible
    (e:es) -> Incompatible (e :| es)

-- | Check forward compatibility between two parsed schemas
checkForward :: SchemaPath -> JsonSchema -> JsonSchema -> CompatibilityResult
checkForward path oldSchema newSchema =
  let errors = concat
        [ checkTypeForward path oldSchema newSchema
        , checkRequiredForward path oldSchema newSchema
        , checkPropertiesForward path oldSchema newSchema
        , checkEnumForward path oldSchema newSchema
        , checkConstraintsForward path oldSchema newSchema
        ]
  in case errors of
    [] -> Compatible
    (e:es) -> Incompatible (e :| es)

-- Type checking
checkTypeBackward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkTypeBackward path oldSchema newSchema =
  case (jsType oldSchema, jsType newSchema) of
    (Just oldType, Just newType) ->
      if typesBackwardCompatible oldType newType
      then []
      else [CompatibilityError (PathType : path) TypeMismatch $
            "Type changed from " <> formatType oldType <> " to " <> formatType newType <> " (not backward compatible)"]
    (Just _, Nothing) -> [] -- New schema accepts all types (more permissive)
    (Nothing, Just _) -> [CompatibilityError (PathType : path) TypeMismatch "Type constraint added in new schema"]
    (Nothing, Nothing) -> []

checkTypeForward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkTypeForward path oldSchema newSchema =
  case (jsType oldSchema, jsType newSchema) of
    (Just oldType, Just newType) ->
      if typesForwardCompatible oldType newType
      then []
      else [CompatibilityError (PathType : path) TypeMismatch $
            "Type changed from " <> formatType oldType <> " to " <> formatType newType <> " (not forward compatible)"]
    (Nothing, Just _) -> [] -- New schema is more restrictive (acceptable for forward compat)
    (Just _, Nothing) -> [CompatibilityError (PathType : path) TypeMismatch "Type constraint removed in new schema"]
    (Nothing, Nothing) -> []

typesBackwardCompatible :: JsonType -> JsonType -> Bool
typesBackwardCompatible oldType newType = case (oldType, newType) of
  (t1, t2) | t1 == t2 -> True
  (JTInteger, JTNumber) -> True -- Widening: integer -> number is backward compatible
  (JTMultiple oldTypes, JTMultiple newTypes) ->
    all (\ot -> any (\nt -> typesBackwardCompatible ot nt) newTypes) oldTypes
  _ -> False

typesForwardCompatible :: JsonType -> JsonType -> Bool
typesForwardCompatible oldType newType = case (oldType, newType) of
  (t1, t2) | t1 == t2 -> True
  (JTNumber, JTInteger) -> True -- Narrowing: number -> integer is forward compatible
  (JTMultiple oldTypes, JTMultiple newTypes) ->
    all (\nt -> any (\ot -> typesForwardCompatible ot nt) oldTypes) newTypes
  _ -> False

-- Required fields checking
checkRequiredBackward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkRequiredBackward path oldSchema newSchema =
  let oldRequired = Set.fromList $ fromMaybe [] $ jsRequired oldSchema
      newRequired = Set.fromList $ fromMaybe [] $ jsRequired newSchema
      addedRequired = Set.difference newRequired oldRequired
  in if Set.null addedRequired
     then []
     else [CompatibilityError (PathRequired : path) RequiredFieldAdded $
           "Required fields added: " <> T.intercalate ", " (Set.toList addedRequired)]

checkRequiredForward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkRequiredForward path oldSchema newSchema =
  let oldRequired = Set.fromList $ fromMaybe [] $ jsRequired oldSchema
      newRequired = Set.fromList $ fromMaybe [] $ jsRequired newSchema
      removedRequired = Set.difference oldRequired newRequired
  in if Set.null removedRequired
     then []
     else [CompatibilityError (PathRequired : path) RequiredFieldRemoved $
           "Required fields removed: " <> T.intercalate ", " (Set.toList removedRequired)]

-- Properties checking
checkPropertiesBackward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkPropertiesBackward path oldSchema newSchema =
  case (jsProperties oldSchema, jsProperties newSchema) of
    (Just oldProps, Just newProps) ->
      -- For backward compatibility, all old properties should still be acceptable in new schema
      -- New properties can be added as long as they're optional
      let oldKeys = Set.fromList $ KM.keys oldProps
          newKeys = Set.fromList $ KM.keys newProps
          removedKeys = Set.difference oldKeys newKeys
          oldRequired = Set.fromList $ fromMaybe [] $ jsRequired oldSchema
          removedRequiredProps = Set.intersection removedKeys oldRequired
      in if not (Set.null removedRequiredProps)
         then [CompatibilityError (PathProperty "properties" : path) PropertyRemoved $
               "Required properties removed: " <> T.intercalate ", " (map (\k -> T.pack $ show k) $ Set.toList removedRequiredProps)]
         else []
    _ -> []

checkPropertiesForward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkPropertiesForward path oldSchema newSchema =
  case (jsProperties oldSchema, jsProperties newSchema) of
    (Just oldProps, Just newProps) ->
      -- For forward compatibility, new data (validated by new schema) should be acceptable to old schema
      -- This means we cannot add required properties that old schema doesn't know about
      let oldKeys = Set.fromList $ KM.keys oldProps
          newKeys = Set.fromList $ KM.keys newProps
          addedKeys = Set.difference newKeys oldKeys
          newRequired = Set.fromList $ fromMaybe [] $ jsRequired newSchema
          addedRequiredProps = Set.intersection addedKeys newRequired
      in if not (Set.null addedRequiredProps)
         then [CompatibilityError (PathProperty "properties" : path) PropertyAdded $
               "Required properties added: " <> T.intercalate ", " (map (\k -> T.pack $ show k) $ Set.toList addedRequiredProps)]
         else []
    _ -> []

-- Enum checking
checkEnumBackward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkEnumBackward path oldSchema newSchema =
  case (jsEnum oldSchema, jsEnum newSchema) of
    (Just oldEnum, Just newEnum) ->
      let oldSet = Set.fromList oldEnum
          newSet = Set.fromList newEnum
          removed = Set.difference oldSet newSet
      in if Set.null removed
         then []
         else [CompatibilityError (PathEnum : path) EnumValueRemoved $
               "Enum values removed: " <> T.pack (show $ Set.toList removed)]
    (Just _, Nothing) -> [] -- Enum constraint removed (more permissive)
    (Nothing, Just _) -> [CompatibilityError (PathEnum : path) EnumValueAdded "Enum constraint added"]
    (Nothing, Nothing) -> []

checkEnumForward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkEnumForward path oldSchema newSchema =
  case (jsEnum oldSchema, jsEnum newSchema) of
    (Just oldEnum, Just newEnum) ->
      let oldSet = Set.fromList oldEnum
          newSet = Set.fromList newEnum
          added = Set.difference newSet oldSet
      in if Set.null added
         then []
         else [CompatibilityError (PathEnum : path) EnumValueAdded $
               "Enum values added: " <> T.pack (show $ Set.toList added)]
    (Nothing, Just _) -> [] -- Enum constraint added (more restrictive)
    (Just _, Nothing) -> [CompatibilityError (PathEnum : path) EnumValueRemoved "Enum constraint removed"]
    (Nothing, Nothing) -> []

-- Constraints checking
checkConstraintsBackward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkConstraintsBackward path oldSchema newSchema =
  concat
    [ checkMinimumBackward path oldSchema newSchema
    , checkMaximumBackward path oldSchema newSchema
    , checkMinLengthBackward path oldSchema newSchema
    , checkMaxLengthBackward path oldSchema newSchema
    , checkPatternBackward path oldSchema newSchema
    , checkFormatBackward path oldSchema newSchema
    ]

checkConstraintsForward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkConstraintsForward path oldSchema newSchema =
  concat
    [ checkMinimumForward path oldSchema newSchema
    , checkMaximumForward path oldSchema newSchema
    , checkMinLengthForward path oldSchema newSchema
    , checkMaxLengthForward path oldSchema newSchema
    , checkPatternForward path oldSchema newSchema
    , checkFormatForward path oldSchema newSchema
    ]

checkMinimumBackward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkMinimumBackward path oldSchema newSchema =
  case (jsMinimum oldSchema, jsMinimum newSchema) of
    (Just oldMin, Just newMin) | newMin > oldMin ->
      [CompatibilityError (PathMinimum : path) ConstraintTightened $
       "Minimum increased from " <> T.pack (show oldMin) <> " to " <> T.pack (show newMin)]
    (Just _, Nothing) -> [] -- Constraint removed (more permissive)
    (Nothing, Just _) -> [CompatibilityError (PathMinimum : path) ConstraintTightened "Minimum constraint added"]
    _ -> []

checkMinimumForward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkMinimumForward path oldSchema newSchema =
  case (jsMinimum oldSchema, jsMinimum newSchema) of
    (Just oldMin, Just newMin) | newMin < oldMin ->
      [CompatibilityError (PathMinimum : path) ConstraintRelaxed $
       "Minimum decreased from " <> T.pack (show oldMin) <> " to " <> T.pack (show newMin)]
    (Nothing, Just _) -> [] -- Constraint added (more restrictive)
    (Just _, Nothing) -> [CompatibilityError (PathMinimum : path) ConstraintRelaxed "Minimum constraint removed"]
    _ -> []

checkMaximumBackward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkMaximumBackward path oldSchema newSchema =
  case (jsMaximum oldSchema, jsMaximum newSchema) of
    (Just oldMax, Just newMax) | newMax < oldMax ->
      [CompatibilityError (PathMaximum : path) ConstraintTightened $
       "Maximum decreased from " <> T.pack (show oldMax) <> " to " <> T.pack (show newMax)]
    (Just _, Nothing) -> [] -- Constraint removed (more permissive)
    (Nothing, Just _) -> [CompatibilityError (PathMaximum : path) ConstraintTightened "Maximum constraint added"]
    _ -> []

checkMaximumForward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkMaximumForward path oldSchema newSchema =
  case (jsMaximum oldSchema, jsMaximum newSchema) of
    (Just oldMax, Just newMax) | newMax > oldMax ->
      [CompatibilityError (PathMaximum : path) ConstraintRelaxed $
       "Maximum increased from " <> T.pack (show oldMax) <> " to " <> T.pack (show newMax)]
    (Nothing, Just _) -> [] -- Constraint added (more restrictive)
    (Just _, Nothing) -> [CompatibilityError (PathMaximum : path) ConstraintRelaxed "Maximum constraint removed"]
    _ -> []

checkMinLengthBackward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkMinLengthBackward path oldSchema newSchema =
  case (jsMinLength oldSchema, jsMinLength newSchema) of
    (Just oldMin, Just newMin) | newMin > oldMin ->
      [CompatibilityError (PathMinLength : path) ConstraintTightened $
       "MinLength increased from " <> T.pack (show oldMin) <> " to " <> T.pack (show newMin)]
    (Just _, Nothing) -> [] -- Constraint removed (more permissive)
    (Nothing, Just _) -> [CompatibilityError (PathMinLength : path) ConstraintTightened "MinLength constraint added"]
    _ -> []

checkMinLengthForward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkMinLengthForward path oldSchema newSchema =
  case (jsMinLength oldSchema, jsMinLength newSchema) of
    (Just oldMin, Just newMin) | newMin < oldMin ->
      [CompatibilityError (PathMinLength : path) ConstraintRelaxed $
       "MinLength decreased from " <> T.pack (show oldMin) <> " to " <> T.pack (show newMin)]
    (Nothing, Just _) -> [] -- Constraint added (more restrictive)
    (Just _, Nothing) -> [CompatibilityError (PathMinLength : path) ConstraintRelaxed "MinLength constraint removed"]
    _ -> []

checkMaxLengthBackward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkMaxLengthBackward path oldSchema newSchema =
  case (jsMaxLength oldSchema, jsMaxLength newSchema) of
    (Just oldMax, Just newMax) | newMax < oldMax ->
      [CompatibilityError (PathMaxLength : path) ConstraintTightened $
       "MaxLength decreased from " <> T.pack (show oldMax) <> " to " <> T.pack (show newMax)]
    (Just _, Nothing) -> [] -- Constraint removed (more permissive)
    (Nothing, Just _) -> [CompatibilityError (PathMaxLength : path) ConstraintTightened "MaxLength constraint added"]
    _ -> []

checkMaxLengthForward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkMaxLengthForward path oldSchema newSchema =
  case (jsMaxLength oldSchema, jsMaxLength newSchema) of
    (Just oldMax, Just newMax) | newMax > oldMax ->
      [CompatibilityError (PathMaxLength : path) ConstraintRelaxed $
       "MaxLength increased from " <> T.pack (show oldMax) <> " to " <> T.pack (show newMax)]
    (Nothing, Just _) -> [] -- Constraint added (more restrictive)
    (Just _, Nothing) -> [CompatibilityError (PathMaxLength : path) ConstraintRelaxed "MaxLength constraint removed"]
    _ -> []

checkPatternBackward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkPatternBackward path oldSchema newSchema =
  case (jsPattern oldSchema, jsPattern newSchema) of
    (Just oldPat, Just newPat) | oldPat /= newPat ->
      [CompatibilityError (PathPattern : path) ConstraintTightened "Pattern constraint changed"]
    (Just _, Nothing) -> [] -- Constraint removed (more permissive)
    (Nothing, Just _) -> [CompatibilityError (PathPattern : path) ConstraintTightened "Pattern constraint added"]
    _ -> []

checkPatternForward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkPatternForward path oldSchema newSchema =
  case (jsPattern oldSchema, jsPattern newSchema) of
    (Just oldPat, Just newPat) | oldPat /= newPat ->
      [CompatibilityError (PathPattern : path) ConstraintRelaxed "Pattern constraint changed"]
    (Nothing, Just _) -> [] -- Constraint added (more restrictive)
    (Just _, Nothing) -> [CompatibilityError (PathPattern : path) ConstraintRelaxed "Pattern constraint removed"]
    _ -> []

checkFormatBackward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkFormatBackward path oldSchema newSchema =
  case (jsFormat oldSchema, jsFormat newSchema) of
    (Just oldFmt, Just newFmt) | oldFmt /= newFmt ->
      [CompatibilityError (PathFormat : path) FormatChanged $
       "Format changed from " <> oldFmt <> " to " <> newFmt]
    (Just _, Nothing) -> [] -- Format constraint removed (more permissive)
    (Nothing, Just _) -> [CompatibilityError (PathFormat : path) FormatChanged "Format constraint added"]
    _ -> []

checkFormatForward :: SchemaPath -> JsonSchema -> JsonSchema -> [CompatibilityError]
checkFormatForward path oldSchema newSchema =
  case (jsFormat oldSchema, jsFormat newSchema) of
    (Just oldFmt, Just newFmt) | oldFmt /= newFmt ->
      [CompatibilityError (PathFormat : path) FormatChanged $
       "Format changed from " <> oldFmt <> " to " <> newFmt]
    (Nothing, Just _) -> [] -- Format constraint added (more restrictive)
    (Just _, Nothing) -> [CompatibilityError (PathFormat : path) FormatChanged "Format constraint removed"]
    _ -> []

-- | Format a schema path into a JSONPath-like string
formatPath :: SchemaPath -> Text
formatPath = mconcat . map formatComponent
  where
    formatComponent = \case
      PathProperty name -> "." <> name
      PathItem -> "[]"
      PathType -> ".type"
      PathEnum -> ".enum"
      PathRequired -> ".required"
      PathMinimum -> ".minimum"
      PathMaximum -> ".maximum"
      PathMinLength -> ".minLength"
      PathMaxLength -> ".maxLength"
      PathPattern -> ".pattern"
      PathFormat -> ".format"
      PathAdditionalProperties -> ".additionalProperties"

-- | Format a JSON type into a human-readable string
formatType :: JsonType -> Text
formatType = \case
  JTString -> "string"
  JTNumber -> "number"
  JTInteger -> "integer"
  JTBoolean -> "boolean"
  JTObject -> "object"
  JTArray -> "array"
  JTNull -> "null"
  JTMultiple types -> "[" <> T.intercalate ", " (map formatType types) <> "]"

-- | Format a compatibility error into a human-readable string
formatError :: CompatibilityError -> Text
formatError CompatibilityError{..} =
  formatPath errorPath <> ": " <> formatErrorType errorType <> " - " <> errorDetails

-- | Format an error type into a human-readable string
formatErrorType :: ErrorType -> Text
formatErrorType = \case
  TypeMismatch -> "Type mismatch"
  RequiredFieldAdded -> "Required field added"
  RequiredFieldRemoved -> "Required field removed"
  EnumValueRemoved -> "Enum value removed"
  EnumValueAdded -> "Enum value added"
  ConstraintTightened -> "Constraint tightened"
  ConstraintRelaxed -> "Constraint relaxed"
  PropertyRemoved -> "Property removed"
  PropertyAdded -> "Property added"
  TypeNarrowed -> "Type narrowed"
  TypeWidened -> "Type widened"
  FormatChanged -> "Format changed"
