{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Example: Creating custom vocabularies for JSON Schema
--
-- This example demonstrates how to create domain-specific vocabularies
-- with custom keywords for validation and annotation.
module CustomVocabulary where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import Fractal.JsonSchema.Vocabulary
import Fractal.JsonSchema.Dialect
import Fractal.JsonSchema.Types
import Fractal.JsonSchema

-- | Example 1: Business Domain Vocabulary
--
-- This vocabulary adds business-specific validation keywords.

-- Custom keyword data types
data CreditCardKeyword = CreditCardKeyword
  { ccProvider :: Maybe Text  -- "visa", "mastercard", etc.
  , ccLuhnCheck :: Bool
  } deriving (Eq, Show, Typeable)

data TaxIDKeyword = TaxIDKeyword
  { taxIDCountry :: Text
  , taxIDFormat :: Text
  } deriving (Eq, Show, Typeable)

-- Create the business vocabulary
businessVocabulary :: Vocabulary
businessVocabulary = Vocabulary
  { vocabularyURI = "https://example.com/vocabs/business/v1"
  , vocabularyRequired = False
  , vocabularyKeywords = Map.fromList
      [ ("x-credit-card", creditCardKeywordDef)
      , ("x-tax-id", taxIDKeywordDef)
      , ("x-currency", currencyKeywordDef)
      ]
  , vocabularyMetaSchema = Nothing
  }

-- Define the credit card keyword
creditCardKeywordDef :: KeywordDefinition
creditCardKeywordDef = KeywordDefinition
  { keywordName = "x-credit-card"
  , keywordAppliesTo = StringSchemaOnly
  , keywordPriority = 100
  }

-- Define the tax ID keyword
taxIDKeywordDef :: KeywordDefinition
taxIDKeywordDef = KeywordDefinition
  { keywordName = "x-tax-id"
  , keywordAppliesTo = StringSchemaOnly
  , keywordPriority = 100
  }

-- Define the currency keyword
currencyKeywordDef :: KeywordDefinition
currencyKeywordDef = KeywordDefinition
  { keywordName = "x-currency"
  , keywordAppliesTo = AnySchema
  , keywordPriority = 50
  }

-- | Example 2: Security Vocabulary
--
-- This vocabulary adds security and compliance keywords.

data PIIKeyword = PIIKeyword
  { piiCategory :: Text
  , piiRetentionDays :: Int
  } deriving (Eq, Show, Typeable)

securityVocabulary :: Vocabulary
securityVocabulary = Vocabulary
  { vocabularyURI = "https://example.com/vocabs/security/v1"
  , vocabularyRequired = True  -- This vocabulary is required
  , vocabularyKeywords = Map.fromList
      [ ("x-sensitive", sensitiveKeywordDef)
      , ("x-pii", piiKeywordDef)
      , ("x-encryption-required", encryptionKeywordDef)
      , ("x-access-level", accessLevelKeywordDef)
      ]
  , vocabularyMetaSchema = Nothing
  }

sensitiveKeywordDef :: KeywordDefinition
sensitiveKeywordDef = KeywordDefinition
  { keywordName = "x-sensitive"
  , keywordAppliesTo = AnySchema
  , keywordPriority = 200  -- High priority for security
  }

piiKeywordDef :: KeywordDefinition
piiKeywordDef = KeywordDefinition
  { keywordName = "x-pii"
  , keywordAppliesTo = AnySchema
  , keywordPriority = 200
  }

encryptionKeywordDef :: KeywordDefinition
encryptionKeywordDef = KeywordDefinition
  { keywordName = "x-encryption-required"
  , keywordAppliesTo = AnySchema
  , keywordPriority = 200
  }

accessLevelKeywordDef :: KeywordDefinition
accessLevelKeywordDef = KeywordDefinition
  { keywordName = "x-access-level"
  , keywordAppliesTo = AnySchema
  , keywordPriority = 200
  }

-- | Example 3: Code Generation Vocabulary
--
-- This vocabulary adds hints for code generation.

codegenVocabulary :: Vocabulary
codegenVocabulary = Vocabulary
  { vocabularyURI = "https://example.com/vocabs/codegen/v1"
  , vocabularyRequired = False  -- Optional - won't fail if unknown
  , vocabularyKeywords = Map.fromList
      [ ("x-haskell-type", haskellTypeKeywordDef)
      , ("x-haskell-module", haskellModuleKeywordDef)
      , ("x-haskell-deriving", haskellDerivingKeywordDef)
      , ("x-haskell-newtype", haskellNewtypeKeywordDef)
      ]
  , vocabularyMetaSchema = Nothing
  }

haskellTypeKeywordDef :: KeywordDefinition
haskellTypeKeywordDef = KeywordDefinition
  { keywordName = "x-haskell-type"
  , keywordAppliesTo = AnySchema
  , keywordPriority = 10  -- Low priority - doesn't affect validation
  }

haskellModuleKeywordDef :: KeywordDefinition
haskellModuleKeywordDef = KeywordDefinition
  { keywordName = "x-haskell-module"
  , keywordAppliesTo = AnySchema
  , keywordPriority = 10
  }

haskellDerivingKeywordDef :: KeywordDefinition
haskellDerivingKeywordDef = KeywordDefinition
  { keywordName = "x-haskell-deriving"
  , keywordAppliesTo = AnySchema
  , keywordPriority = 10
  }

haskellNewtypeKeywordDef :: KeywordDefinition
haskellNewtypeKeywordDef = KeywordDefinition
  { keywordName = "x-haskell-newtype"
  , keywordAppliesTo = AnySchema
  , keywordPriority = 10
  }

-- | Example 4: Creating a Custom Dialect
--
-- Combine standard and custom vocabularies into a company dialect.

{-
-- NOTE: This is a conceptual example. The actual implementation
-- requires the Schema type and meta-schemas to be defined.

acmeCorpDialect :: Dialect
acmeCorpDialect = Dialect
  { dialectURI = "https://acme.com/json-schema/2024"
  , dialectName = "ACME Corp JSON Schema"
  , dialectVersion = "2024.1"
  , dialectVocabularies = Map.fromList
      -- Standard vocabularies
      [ ("https://json-schema.org/draft/2020-12/vocab/core", True)
      , ("https://json-schema.org/draft/2020-12/vocab/applicator", True)
      , ("https://json-schema.org/draft/2020-12/vocab/validation", True)
      , ("https://json-schema.org/draft/2020-12/vocab/meta-data", True)

      -- Custom vocabularies
      , ("https://example.com/vocabs/business/v1", True)
      , ("https://example.com/vocabs/security/v1", True)
      , ("https://example.com/vocabs/codegen/v1", False)
      ]
  , dialectMetaSchema = undefined  -- Would be the actual meta-schema
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = WarnUnknown
  }

-- Register vocabularies and create dialect
exampleSetup :: IO VocabularyRegistry
exampleSetup = do
  let registry = emptyRegistry
  let registry' = registerVocabulary businessVocabulary registry
  let registry'' = registerVocabulary securityVocabulary registry'
  let registry''' = registerVocabulary codegenVocabulary registry''
  let registry'''' = registerDialect acmeCorpDialect registry'''
  pure registry''''
-}

{-
Example JSON Schema using the custom dialect:

{
  "$schema": "https://acme.com/json-schema/2024",
  "type": "object",
  "properties": {
    "creditCard": {
      "type": "string",
      "x-credit-card": {
        "provider": "visa",
        "luhn-check": true
      },
      "x-sensitive": true,
      "x-encryption-required": "AES-256",
      "x-haskell-newtype": {
        "constructor": "CreditCardNumber",
        "module": "Acme.Payment.Types"
      }
    },
    "ssn": {
      "type": "string",
      "x-tax-id": {
        "country": "US",
        "format": "XXX-XX-XXXX"
      },
      "x-pii": {
        "category": "government-id",
        "retention-days": 2555
      },
      "x-access-level": "confidential",
      "x-haskell-newtype": {
        "constructor": "SSN"
      }
    }
  }
}
-}

-- | Example 4: Using Custom Validators
--
-- This shows how to register custom validators and use them with validation

-- Custom validator for credit card numbers (Luhn algorithm check)
creditCardValidator :: CustomValidator
creditCardValidator (String text) =
  if isValidLuhn text
    then Right ()
    else Left $ ValidationError "x-credit-card" emptyPointer emptyPointer
           "Invalid credit card number (Luhn check failed)" Nothing
creditCardValidator _ = Left $ ValidationError "x-credit-card" emptyPointer emptyPointer
  "x-credit-card can only validate strings" Nothing

-- Simplified Luhn algorithm check
isValidLuhn :: Text -> Bool
isValidLuhn text =
  let digits = T.filter (\c -> c >= '0' && c <= '9') text
      nums = map (\c -> read [c] :: Int) (T.unpack digits)
      checksum = sum $ zipWith (*) (cycle [1, 2]) $ map luhnDouble $ reverse nums
  in length nums >= 13 && checksum `mod` 10 == 0
  where
    luhnDouble n = let doubled = n * 2 in if doubled > 9 then doubled - 9 else doubled

-- Custom validator for currency codes (ISO 4217)
currencyValidator :: CustomValidator
currencyValidator (String text) =
  if T.toUpper text `elem` ["USD", "EUR", "GBP", "JPY", "CHF", "CAD", "AUD", "NZD"]
    then Right ()
    else Left $ ValidationError "x-currency" emptyPointer emptyPointer
           ("Invalid currency code: " <> text) Nothing
currencyValidator _ = Left $ ValidationError "x-currency" emptyPointer emptyPointer
  "x-currency expects a string value" Nothing

-- Example: Validating with custom keywords
exampleValidation :: IO ()
exampleValidation = do
  -- Define a schema with custom keywords
  let schemaJson = Aeson.object
        [ "type" Aeson..= String "object"
        , "properties" Aeson..= Aeson.object
            [ "cardNumber" Aeson..= Aeson.object
                [ "type" Aeson..= String "string"
                , "x-credit-card" Aeson..= Aeson.object
                    [ "luhn-check" Aeson..= Bool True ]
                ]
            , "currency" Aeson..= Aeson.object
                [ "type" Aeson..= String "string"
                , "x-currency" Aeson..= Bool True
                ]
            ]
        ]
  
  case Aeson.fromJSON schemaJson of
    Aeson.Error err -> putStrLn $ "Schema parse error: " <> err
    Aeson.Success schemaValue -> do
      case parseSchema schemaValue of
        Left parseErr -> print parseErr
        Right schema -> do
          -- Create validation config with custom validators
          let config = defaultValidationConfig
                { validationCustomValidators = Map.fromList
                    [ ("x-credit-card", creditCardValidator)
                    , ("x-currency", currencyValidator)
                    ]
                }
          
          -- Valid test data
          let validData = Aeson.object
                [ "cardNumber" Aeson..= String "4532015112830366"  -- Valid Luhn
                , "currency" Aeson..= String "USD"
                ]
          
          -- Invalid test data
          let invalidData = Aeson.object
                [ "cardNumber" Aeson..= String "1234567890123456"  -- Invalid Luhn
                , "currency" Aeson..= String "XYZ"  -- Invalid currency
                ]
          
          -- Validate
          putStrLn "Validating valid data:"
          print $ validateValue config schema validData
          
          putStrLn "\nValidating invalid data:"
          print $ validateValue config schema invalidData
