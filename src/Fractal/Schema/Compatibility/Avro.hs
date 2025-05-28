{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Fractal.Schema.Compatibility.Avro where

import Data.Avro (Schema)
import Data.Avro.Schema (SchemaType(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Validation (Validation(..), validation)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Fractal.Schema.Types (CompatibilityLevel(..))

-- | Represents the result of a schema compatibility check
data CompatibilityResult
  = Compatible
  | Incompatible (NonEmpty CompatibilityError)
  deriving stock (Show, Eq, Generic)

-- | A path component in the schema
data PathComponent
  = Field Text
  | ArrayIndex
  | MapKey
  | MapValue
  | UnionVariant Int
  deriving stock (Show, Eq, Generic)

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
  | NameMismatch
  | SizeMismatch
  | EnumSymbolMismatch
  | UnionVariantMismatch
  deriving stock (Show, Eq, Generic)

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

-- | Check backward compatibility across all adjacent pairs in the sequence
checkBackwardTransitive :: NonEmpty Schema -> CompatibilityResult
checkBackwardTransitive schemas =
  case NE.tail schemas of
    [] -> Compatible
    rest -> case checkBackwardCompatibility (NE.head schemas) (NE.head rest) of
      Compatible -> checkBackwardTransitive rest
      Incompatible errs -> Incompatible $ NE.map (\err -> err { errorPath = "backward_transitive" : errorPath err }) errs

-- | Check forward compatibility across all adjacent pairs in the sequence
checkForwardTransitive :: NonEmpty Schema -> CompatibilityResult
checkForwardTransitive schemas =
  case NE.tail schemas of
    [] -> Compatible
    rest -> case checkForwardCompatibility (NE.head schemas) (NE.head rest) of
      Compatible -> checkForwardTransitive rest
      Incompatible errs -> Incompatible $ NE.map (\err -> err { errorPath = "forward_transitive" : errorPath err }) errs

-- | Check full compatibility across all adjacent pairs in the sequence
checkFullTransitive :: NonEmpty Schema -> CompatibilityResult
checkFullTransitive schemas =
  case NE.tail schemas of
    [] -> Compatible
    rest -> case checkFullCompatibility (NE.head schemas) (NE.head rest) of
      Compatible -> checkFullTransitive rest
      Incompatible errs -> Incompatible $ NE.map (\err -> err { errorPath = "full_transitive" : errorPath err }) errs

-- | Check if two schemas are compatible
checkSchemaCompatibility :: Schema -> Schema -> CompatibilityResult
checkSchemaCompatibility readerSchema writerSchema =
  case compareSchemas [] readerSchema writerSchema of
    Success _ -> Compatible
    Failure errs -> Incompatible errs

-- | Compare two schemas for compatibility
compareSchemas :: SchemaPath -> Schema -> Schema -> Validation (NonEmpty CompatibilityError) ()
compareSchemas path readerSchema writerSchema =
  case (readerSchema, writerSchema) of
    (Null, Null) -> Success ()
    (Boolean, Boolean) -> Success ()
    (Int _, Int _) -> Success ()
    (Long _, Long _) -> Success ()
    (Float, Float) -> Success ()
    (Double, Double) -> Success ()
    (Bytes _, Bytes _) -> Success ()
    (String _, String _) -> Success ()
    (Array r, Array w) -> compareSchemas (ArrayIndex : path) r w
    (Map r, Map w) -> compareSchemas (MapValue : path) r w
    (Union rs, Union ws) ->
      let results = zipWith (\i r ->
            if any (\w -> case compareSchemas (UnionVariant i : path) r w of
                           Success _ -> True
                           Failure _ -> False) ws
              then Success ()
              else Failure $ NE.singleton $ CompatibilityError
                { errorPath = UnionVariant i : path
                , errorType = UnionVariantMismatch
                , errorDetails = "Union variant is not compatible with any writer variant"
                }
          ) [0..] rs
      in foldr (<*>) (Success ()) results
    (Record rName rFields, Record wName wFields) ->
      if rName == wName
        then compareRecordFields path rFields wFields
        else Failure $ NE.singleton $ CompatibilityError
          { errorPath = path
          , errorType = NameMismatch
          , errorDetails = "Record names don't match: " <> rName <> " vs " <> wName
          }
    (Enum rName rSymbols, Enum wName wSymbols) ->
      if rName == wName
        then if all (`elem` wSymbols) rSymbols
          then Success ()
          else Failure $ NE.singleton $ CompatibilityError
            { errorPath = path
            , errorType = EnumSymbolMismatch
            , errorDetails = "Reader enum contains symbols not in writer enum: " <> rName
            }
        else Failure $ NE.singleton $ CompatibilityError
          { errorPath = path
          , errorType = NameMismatch
          , errorDetails = "Enum names don't match: " <> rName <> " vs " <> wName
          }
    (Fixed rName rSize, Fixed wName wSize) ->
      if rName == wName
        then if rSize == wSize
          then Success ()
          else Failure $ NE.singleton $ CompatibilityError
            { errorPath = path
            , errorType = SizeMismatch
            , errorDetails = "Fixed size mismatch: " <> rName
            }
        else Failure $ NE.singleton $ CompatibilityError
          { errorPath = path
          , errorType = NameMismatch
          , errorDetails = "Fixed names don't match: " <> rName <> " vs " <> wName
          }
    _ -> Failure $ NE.singleton $ CompatibilityError
      { errorPath = path
      , errorType = TypeMismatch
      , errorDetails = "Schema types are incompatible"
      }

-- | Compare record fields for compatibility
compareRecordFields :: SchemaPath -> [(Text, Schema)] -> [(Text, Schema)] -> Validation (NonEmpty CompatibilityError) ()
compareRecordFields path readerFields writerFields =
  let results = map (\(rName, rSchema) ->
        case lookup rName writerFields of
          Just wSchema -> compareSchemas (Field rName : path) rSchema wSchema
          Nothing -> Failure $ NE.singleton $ CompatibilityError
            { errorPath = Field rName : path
            , errorType = MissingField
            , errorDetails = "Reader field not found in writer schema"
            }
      ) readerFields
  in foldr (<*>) (Success ()) results

-- | Format a schema path into a JSONPath-like string
formatPath :: SchemaPath -> Text
formatPath = mconcat . map formatComponent
  where
    formatComponent = \case
      Field name -> "." <> name
      ArrayIndex -> "[]"
      MapKey -> ".key"
      MapValue -> ".value"
      UnionVariant i -> "[" <> show i <> "]"

-- | Format a compatibility error into a human-readable string
formatError :: CompatibilityError -> Text
formatError CompatibilityError{..} =
  formatPath errorPath <> ": " <> formatErrorType errorType <> " - " <> errorDetails

-- | Format an error type into a human-readable string
formatErrorType :: ErrorType -> Text
formatErrorType = \case
  TypeMismatch -> "Type mismatch"
  MissingField -> "Missing field"
  NameMismatch -> "Name mismatch"
  SizeMismatch -> "Size mismatch"
  EnumSymbolMismatch -> "Enum symbol mismatch"
  UnionVariantMismatch -> "Union variant mismatch"

-- | Check if a schema is backward compatible with another schema
-- Backward compatibility means that data written with the new schema can be read by the old schema
checkBackwardCompatibility :: Schema -> Schema -> CompatibilityResult
checkBackwardCompatibility oldSchema newSchema =
  checkSchemaCompatibility oldSchema newSchema

-- | Check if a schema is forward compatible with another schema
-- Forward compatibility means that data written with the old schema can be read by the new schema
checkForwardCompatibility :: Schema -> Schema -> CompatibilityResult
checkForwardCompatibility newSchema oldSchema =
  checkSchemaCompatibility newSchema oldSchema

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
