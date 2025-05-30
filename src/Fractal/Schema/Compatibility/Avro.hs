{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | Avro schema compatibility checking following the Confluent Schema Registry algorithm.
--
-- This module implements compatibility checking between Avro schemas according to the
-- rules defined by Confluent Schema Registry. The compatibility checks ensure that
-- schemas can evolve while maintaining the ability to read data written with different
-- schema versions.
--
-- == Compatibility Types
--
-- The module supports several compatibility levels:
--
-- * __BACKWARD__: A new schema is backward compatible if it can read data written with the old schema.
--   This is the most common compatibility mode. You can:
--
--     * Add fields with defaults
--     * Remove fields (the reader will ignore them)
--     * Promote types: int → long/float/double, long → float/double, float → double, string ↔ bytes
--
-- * __FORWARD__: A new schema is forward compatible if the old schema can read data written with the new schema.
--   You can:
--
--     * Remove fields
--     * Add fields only if the old schema has defaults for them
--     * Demote types where supported
--
-- * __FULL__: A schema is fully compatible if it is both backward and forward compatible.
--   This is the most restrictive mode.
--
-- * __TRANSITIVE__ variants: These check compatibility across all historical versions,
--   not just the immediate predecessor.
--
-- == Reader and Writer Schemas
--
-- In Avro terminology:
--
-- * __Writer schema__: The schema used to write/encode the data
-- * __Reader schema__: The schema used to read/decode the data
--
-- For compatibility checking:
--
-- * Backward compatibility: new schema (reader) must be able to read data written with old schema (writer)
-- * Forward compatibility: old schema (reader) must be able to read data written with new schema (writer)
--
-- == Compatibility Rules
--
-- === Primitive Types
-- * Identical primitive types are always compatible
-- * Type promotions allowed:
--     * int → long, float, or double
--     * long → float or double
--     * float → double
--     * string ↔ bytes
-- * Different primitive types are incompatible
--
-- === Records
-- * Record names must match (considering aliases)
-- * For backward compatibility:
--     * Reader fields not in writer must have defaults
--     * Reader can have fewer fields than writer
-- * For forward compatibility:
--     * Writer fields not in reader are ignored
--     * Writer cannot have fields missing from reader (unless reader has defaults)
--
-- === Enums
-- * Enum names must match
-- * Writer symbols must exist in reader's enum (reader can have additional symbols)
-- * If writer has a symbol not in reader, an error occurs (enum defaults not currently supported)
--
-- === Unions
-- * For backward compatibility: each reader branch must match some writer branch
-- * For forward compatibility: each writer branch must match some reader branch
--
-- === Arrays and Maps
-- * Element/value types must be compatible
--
-- === Fixed
-- * Names and sizes must match exactly
--
-- == Field Resolution
--
-- Fields are matched by name, considering aliases. A field matches if:
--
-- * Names are identical, or
-- * Reader field name is in writer field aliases, or
-- * Writer field name is in reader field aliases
--
-- == Examples
--
-- Backward compatible changes:
--
-- @
-- -- Old schema
-- {"type": "record", "name": "User", "fields": [
--   {"name": "id", "type": "int"}
-- ]}
--
-- -- New schema (backward compatible)
-- {"type": "record", "name": "User", "fields": [
--   {"name": "id", "type": "int"},
--   {"name": "name", "type": "string", "default": "Unknown"}
-- ]}
-- @
--
-- Forward compatible changes:
--
-- @
-- -- Old schema
-- {"type": "record", "name": "User", "fields": [
--   {"name": "id", "type": "int"},
--   {"name": "email", "type": "string"}
-- ]}
--
-- -- New schema (forward compatible)
-- {"type": "record", "name": "User", "fields": [
--   {"name": "id", "type": "int"}
-- ]}
-- @
module Fractal.Schema.Compatibility.Avro where

import Data.Avro (Schema(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Validation (Validation(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Fractal.Schema.Types (CompatibilityLevel(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Avro.Schema.Schema as AvroSchema
import Data.String (IsString(..))
import Data.Foldable
import Data.Hashable (Hashable(..))
import Data.List (sortBy, intersperse)
import Data.Ord (comparing)
import Data.List (find)

-- | Represents the result of a schema compatibility check
data CompatibilityResult
  = Compatible
  | Incompatible (NonEmpty CompatibilityError)
  deriving stock (Show, Eq, Generic)

-- | A path component in the schema
data PathComponent
  = PathField Text
  | ArrayIndex
  | MapKey
  | MapValue
  | UnionVariant Int
  | Reference Text
  deriving stock (Show, Eq, Generic)

instance IsString PathComponent where
  fromString = PathField . T.pack

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
  | MissingField
  | MissingDefault
  | NameMismatch
  | SizeMismatch
  | EnumSymbolMismatch
  | UnionVariantMismatch
  | PromotionError Schema Schema  -- ^ Failed promotion from reader type to writer type
  | ReferenceError
  | AliasError
  deriving stock (Show, Eq, Generic)

-- | A field in a record schema
type Field = AvroSchema.Field

-- | A schema reference
data SchemaReference = SchemaReference
  { refName :: Text
  , refNamespace :: Maybe Text
  } deriving stock (Show, Eq, Generic)

-- | Environment for schema resolution
data SchemaEnv = SchemaEnv
  { envReferences :: HashMap Text Schema
  , envNamespaces :: HashMap Text Text
  } deriving stock (Show, Eq, Generic)

-- | Check if a sequence of schemas satisfies the given compatibility level
-- Schemas should be ordered newest to oldest
checkCompatibilityLevel :: CompatibilityLevel -> NonEmpty Schema -> CompatibilityResult
checkCompatibilityLevel level schemas = case level of
  NONE -> Compatible
  BACKWARD -> checkBackwardCompatibility (NE.head schemas) (NE.last schemas)
  FORWARD -> checkForwardCompatibility (NE.head schemas) (NE.last schemas)
  FULL -> checkFullCompatibility (NE.head schemas) (NE.last schemas)
  BACKWARD_TRANSITIVE -> checkBackwardTransitive schemas
  FORWARD_TRANSITIVE -> checkForwardTransitive schemas
  FULL_TRANSITIVE -> checkFullTransitive schemas

-- | Check if a type can be promoted to another type
-- According to Avro spec, the writer's schema may be promoted to the reader's as follows:
-- * int is promotable to long, float, or double
-- * long is promotable to float or double
-- * float is promotable to double
-- * string is promotable to bytes
-- * bytes is promotable to string
canPromote :: Schema -> Schema -> Bool
canPromote readerSchema writerSchema = case (readerSchema, writerSchema) of
  -- int promotions
  (Long _, Int _) -> True
  (Float, Int _) -> True
  (Double, Int _) -> True
  -- long promotions
  (Float, Long _) -> True
  (Double, Long _) -> True
  -- float promotions
  (Double, Float) -> True
  -- string/bytes promotions
  (Bytes _, String _) -> True
  (String _, Bytes _) -> True
  _ -> False

-- | Resolve a schema reference
resolveReference :: SchemaEnv -> SchemaReference -> Maybe Schema
resolveReference env ref = do
  let fullName = case refNamespace ref of
        Just ns -> ns <> "." <> refName ref
        Nothing -> refName ref
  HM.lookup fullName (envReferences env)

-- | Check backward compatibility across all adjacent pairs in the sequence
checkBackwardTransitive :: NonEmpty Schema -> CompatibilityResult
checkBackwardTransitive schemas =
  case NE.tail schemas of
    [] -> Compatible
    rest -> case checkBackwardCompatibility (NE.head schemas) (NE.head $ NE.fromList rest) of
      Compatible -> checkBackwardTransitive (NE.fromList rest)
      Incompatible errs -> Incompatible $ NE.map (\err -> err { errorPath = "backward_transitive" : errorPath err }) errs

-- | Check forward compatibility across all adjacent pairs in the sequence
checkForwardTransitive :: NonEmpty Schema -> CompatibilityResult
checkForwardTransitive schemas =
  case NE.tail schemas of
    [] -> Compatible
    rest -> case checkForwardCompatibility (NE.head schemas) (NE.head $ NE.fromList rest) of
      Compatible -> checkForwardTransitive (NE.fromList rest)
      Incompatible errs -> Incompatible $ NE.map (\err -> err { errorPath = "forward_transitive" : errorPath err }) errs

-- | Check full compatibility across all adjacent pairs in the sequence
checkFullTransitive :: NonEmpty Schema -> CompatibilityResult
checkFullTransitive schemas =
  case NE.tail schemas of
    [] -> Compatible
    rest -> case checkFullCompatibility (NE.head schemas) (NE.head $ NE.fromList rest) of
      Compatible -> checkFullTransitive (NE.fromList rest)
      Incompatible errs -> Incompatible $ NE.map (\err -> err { errorPath = "full_transitive" : errorPath err }) errs

-- | Check if two names match, considering aliases
namesMatch :: Text -> [Text] -> Text -> [Text] -> Bool
namesMatch name1 aliases1 name2 aliases2 =
  name1 == name2 ||
  name1 `elem` aliases2 ||
  name2 `elem` aliases1

-- | Check if two schemas are compatible
checkSchemaCompatibility :: SchemaEnv -> Schema -> Schema -> CompatibilityResult
checkSchemaCompatibility env readerSchema writerSchema =
  case compareSchemas env [] readerSchema writerSchema of
    Success _ -> Compatible
    Failure errs -> Incompatible errs

-- | Compare two schemas for compatibility
compareSchemas :: SchemaEnv -> SchemaPath -> Schema -> Schema -> Validation (NonEmpty CompatibilityError) ()
compareSchemas env path = compareSchemasWithDefault env path Nothing

-- | Compare two schemas for compatibility, with an optional default value for the reader schema
compareSchemasWithDefault :: SchemaEnv -> SchemaPath -> Maybe AvroSchema.DefaultValue -> Schema -> Schema -> Validation (NonEmpty CompatibilityError) ()
compareSchemasWithDefault env path mDefault readerSchema writerSchema =
  case (readerSchema, writerSchema) of
    (Null, Null) -> Success ()
    (Boolean, Boolean) -> Success ()
    (Int _, Int _) -> Success ()
    (Long _, Long _) -> Success ()
    (Float, Float) -> Success ()
    (Double, Double) -> Success ()
    (Bytes _, Bytes _) -> Success ()
    (String _, String _) -> Success ()
    (Array r, Array w) -> compareSchemasWithDefault env (ArrayIndex : path) Nothing r w
    (Map r, Map w) -> compareSchemasWithDefault env (MapValue : path) Nothing r w
    (Union rs, Union ws) ->
      let results = zipWith (\i w ->
            case find (\r -> case compareSchemasWithDefault env (UnionVariant i : path) Nothing r w of
                               Success _ -> True
                               Failure _ -> False) (V.toList rs) of
              Just _ -> Success ()
              Nothing -> Failure $ NE.singleton $ CompatibilityError
                { errorPath = UnionVariant i : path
                , errorType = UnionVariantMismatch
                , errorDetails = "Writer union variant " <> formatSchemaType w <> " has no compatible reader variant"
                }
            ) [0..] (V.toList ws)
      in sequenceA_ results
    (Union rs, _) ->
      case find (\r -> case compareSchemasWithDefault env [] Nothing r writerSchema of
                         Success _ -> True
                         Failure _ -> False) (V.toList rs) of
        Just r -> compareSchemasWithDefault env path Nothing r writerSchema
        Nothing -> Failure $ pure $ CompatibilityError
          { errorPath = path
          , errorType = UnionVariantMismatch
          , errorDetails = "No reader union variant matches writer type " <> formatSchemaType writerSchema
          }
    (_, Union ws) ->
      let results = zipWith (\i w ->
            case compareSchemasWithDefault env (UnionVariant i : path) Nothing readerSchema w of
              Success _ -> Success ()
              Failure errs -> Failure errs
            ) [0..] (V.toList ws)
          -- Reader must be compatible with at least one writer variant
      in case find (\case Success _ -> True; _ -> False) results of
        Just _ -> Success ()
        Nothing -> Failure $ pure $ CompatibilityError
          { errorPath = path
          , errorType = UnionVariantMismatch
          , errorDetails = "Reader type " <> formatSchemaType readerSchema <> " cannot read any writer union variant"
          }
    (Record rName rAliases _rDoc rFields, Record wName wAliases _wDoc wFields) ->
      if namesMatch (AvroSchema.baseName rName) (map AvroSchema.baseName rAliases) (AvroSchema.baseName wName) (map AvroSchema.baseName wAliases)
      then compareRecordFields env path rFields wFields
      else Failure $ pure $ CompatibilityError
        { errorPath = path
        , errorType = NameMismatch
        , errorDetails = "Record names don't match: " <> AvroSchema.baseName rName <> " vs " <> AvroSchema.baseName wName
        }
    (Enum rName rAliases _rDoc rSymbols, Enum wName wAliases _wDoc wSymbols) ->
      if namesMatch (AvroSchema.baseName rName) (map AvroSchema.baseName rAliases) (AvroSchema.baseName wName) (map AvroSchema.baseName wAliases)
      then
        let writerNotInReader = filter (not . (`elem` rSymbols)) (V.toList wSymbols)
            -- If we have a default value, we only need to ensure the default symbol exists
            -- and that any writer symbols are present in the reader
            checkSymbols = case mDefault of
              Just (AvroSchema.DEnum _ _ defSym) ->
                -- For enums with defaults, we only need to ensure the default symbol exists
                if defSym `elem` rSymbols
                then Success ()
                else Failure $ NE.singleton $ CompatibilityError
                  { errorPath = path
                  , errorType = EnumSymbolMismatch
                  , errorDetails = "Default enum symbol '" <> T.pack (show defSym) <> "' not present in reader's enum"
                  }
              _ ->
                -- Without a default, all writer symbols must be in reader
                if null writerNotInReader
                then Success ()
                else Failure $ NE.singleton $ CompatibilityError
                  { errorPath = path
                  , errorType = EnumSymbolMismatch
                  , errorDetails = "Writer enum contains symbols not in reader enum: " <> T.intercalate ", " writerNotInReader
                  }
        in checkSymbols
      else Failure $ NE.singleton $ CompatibilityError
        { errorPath = path
        , errorType = NameMismatch
        , errorDetails = "Enum names don't match: " <> AvroSchema.baseName rName <> " vs " <> AvroSchema.baseName wName
        }
    (Fixed rName _rAliases rSize rLogicalType, Fixed wName _wAliases wSize wLogicalType) ->
      if rName == wName
      then if rSize == wSize && rLogicalType == wLogicalType
        then Success ()
        else Failure $ NE.singleton $ CompatibilityError
          { errorPath = path
          , errorType = SizeMismatch
          , errorDetails = "Fixed size mismatch: " <> AvroSchema.baseName rName
          }
      else Failure $ NE.singleton $ CompatibilityError
        { errorPath = path
        , errorType = NameMismatch
        , errorDetails = "Fixed names don't match: " <> AvroSchema.baseName rName <> " vs " <> AvroSchema.baseName wName
        }
    (NamedType ref, _) ->
      case resolveReference env (SchemaReference (AvroSchema.baseName ref) Nothing) of
        Just resolvedSchema -> compareSchemasWithDefault env (Reference (AvroSchema.baseName ref) : path) Nothing resolvedSchema writerSchema
        Nothing -> Failure $ NE.singleton $ CompatibilityError
          { errorPath = Reference (AvroSchema.baseName ref) : path
          , errorType = ReferenceError
          , errorDetails = "Could not resolve schema reference: " <> AvroSchema.baseName ref
          }
    (_, NamedType ref) ->
      case resolveReference env (SchemaReference (AvroSchema.baseName ref) Nothing) of
        Just resolvedSchema -> compareSchemasWithDefault env path mDefault readerSchema resolvedSchema
        Nothing -> Failure $ NE.singleton $ CompatibilityError
          { errorPath = Reference (AvroSchema.baseName ref) : path
          , errorType = ReferenceError
          , errorDetails = "Could not resolve schema reference: " <> AvroSchema.baseName ref
          }
    _ -> if canPromote readerSchema writerSchema
      then Success ()
      else Failure $ NE.singleton $ CompatibilityError
        { errorPath = path
        , errorType = PromotionError readerSchema writerSchema
        , errorDetails = "Cannot read " <> formatSchemaType writerSchema <> " as " <> formatSchemaType readerSchema
        }

-- | Compare record fields for compatibility
compareRecordFields :: SchemaEnv -> SchemaPath -> [AvroSchema.Field] -> [AvroSchema.Field] -> Validation (NonEmpty CompatibilityError) ()
compareRecordFields env path readerFields writerFields =
  let
    checkField rField =
      case findMatchingField rField writerFields of
        Just wField ->
          compareSchemasWithDefault
            env
              (PathField (AvroSchema.fldName rField) : path)
              (AvroSchema.fldDefault rField)
              (AvroSchema.fldType rField)
              (AvroSchema.fldType wField)
        Nothing -> case AvroSchema.fldDefault rField of
          Just _ -> Success ()
          Nothing -> Failure $ NE.singleton $ CompatibilityError
            { errorPath = PathField (AvroSchema.fldName rField) : path
            , errorType = MissingDefault
            , errorDetails = "Reader field not found in writer schema and has no default value"
            }
    readerResults = map checkField readerFields
    writerResults = map (\wField ->
      case findMatchingField wField readerFields of
        Just _ -> Success ()
        Nothing -> Failure $ NE.singleton $ CompatibilityError
          { errorPath = PathField (AvroSchema.fldName wField) : path
          , errorType = MissingField
          , errorDetails = "Writer field not found in reader schema"
          }) writerFields
  in sequenceA_ (readerResults <> writerResults)

-- | Find a matching field in the writer schema, considering aliases
findMatchingField :: AvroSchema.Field -> [AvroSchema.Field] -> Maybe AvroSchema.Field
findMatchingField rField writerFields =
  let matches wField =
        AvroSchema.fldName rField == AvroSchema.fldName wField ||
        AvroSchema.fldName rField `Set.member` Set.fromList (AvroSchema.fldAliases wField) ||
        AvroSchema.fldName wField `Set.member` Set.fromList (AvroSchema.fldAliases rField)
  in find matches writerFields

-- | Format a schema path into a JSONPath-like string
formatPath :: SchemaPath -> Text
formatPath = mconcat . map formatComponent
  where
    formatComponent = \case
      PathField name -> "." <> name
      ArrayIndex -> "[]"
      MapKey -> ".key"
      MapValue -> ".value"
      UnionVariant i -> "[" <> T.pack (show i) <> "]"
      Reference name -> "@" <> name

-- | Format a schema type into a concise human-readable string
formatSchemaType :: Schema -> Text
formatSchemaType = \case
  Null -> "null"
  Boolean -> "boolean"
  Int _ -> "int"
  Long _ -> "long"
  Float -> "float"
  Double -> "double"
  Bytes _ -> "bytes"
  String _ -> "string"
  Array item -> "array<" <> formatSchemaType item <> ">"
  Map values -> "map<" <> formatSchemaType values <> ">"
  NamedType name -> AvroSchema.baseName name
  Record name _ _ _ -> "record:" <> AvroSchema.baseName name
  Enum name _ _ _ -> "enum:" <> AvroSchema.baseName name
  Union options -> "union[" <> T.intercalate "," (map formatSchemaType $ V.toList options) <> "]"
  Fixed name _ _ _ -> "fixed:" <> AvroSchema.baseName name

-- | Format a compatibility error into a human-readable string
formatError :: CompatibilityError -> Text
formatError CompatibilityError{..} =
  formatPath errorPath <> ": " <> formatErrorType errorType <> " - " <> errorDetails

-- | Format an error type into a human-readable string
formatErrorType :: ErrorType -> Text
formatErrorType = \case
  TypeMismatch -> "Type mismatch"
  MissingField -> "Missing field"
  MissingDefault -> "Missing default value"
  NameMismatch -> "Name mismatch"
  SizeMismatch -> "Size mismatch"
  EnumSymbolMismatch -> "Enum symbol mismatch"
  UnionVariantMismatch -> "Union variant mismatch"
  PromotionError from to -> "Type promotion error (" <> formatSchemaType from <> " -> " <> formatSchemaType to <> ")"
  ReferenceError -> "Schema reference error"
  AliasError -> "Field alias error"

-- | Check if a schema is backward compatible with another schema
-- Backward compatibility means that data written with the old schema can be read by the new schema
-- (new schema is the reader, old schema is the writer)
checkBackwardCompatibility :: Schema -> Schema -> CompatibilityResult
checkBackwardCompatibility oldSchema newSchema =
  checkSchemaCompatibility (SchemaEnv mempty mempty) newSchema oldSchema

-- | Check if a schema is forward compatible with another schema
-- Forward compatibility means that data written with the new schema can be read by the old schema
-- (old schema is the reader, new schema is the writer)
checkForwardCompatibility :: Schema -> Schema -> CompatibilityResult
checkForwardCompatibility =
  checkSchemaCompatibility (SchemaEnv mempty mempty)

-- | Check if two schemas are fully compatible (both forward and backward)
checkFullCompatibility :: Schema -> Schema -> CompatibilityResult
checkFullCompatibility schema1 schema2 =
  case (checkBackwardCompatibility schema1 schema2, checkForwardCompatibility schema1 schema2) of
    (Compatible, Compatible) -> Compatible
    (Incompatible backwardErrs, Compatible) ->
      Incompatible $ NE.map (\err -> err { errorPath = "backward" : errorPath err }) backwardErrs
    (Compatible, Incompatible forwardErrs) ->
      Incompatible $ NE.map (\err -> err { errorPath = "forward" : errorPath err }) forwardErrs
    (Incompatible backwardErrs, Incompatible forwardErrs) ->
      Incompatible $ NE.map (\err -> err { errorPath = "backward" : errorPath err }) backwardErrs
        <> NE.map (\err -> err { errorPath = "forward" : errorPath err }) forwardErrs
