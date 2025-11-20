{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core types for IDNA2008 implementation
module Data.Text.IDNA2008.Types
  ( -- * Error Types
    IDNA2008Error(..)
  , HyphenError(..)
  , PunycodeError(..)
  
    -- * Domain Types
  , Label(..)
  , DomainName(..)
  , mkLabel
  , mkDomainName
  
    -- * Unicode Property Types
  , CodePointStatus(..)
  , BidiClass(..)
  , ContextRule(..)
  , BidiRule(..)
  , Script(..)
  
    -- * Punycode Types
  , PunycodeState(..)
  , PunycodeParams(..)
  , initialState
  , punycodeParams
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | IDNA2008 processing errors with detailed context
data IDNA2008Error
  = EmptyLabel
  | LabelTooLong 
      { actualLength :: Int
      , maxLength :: Int
      }
  | TotalDomainTooLong
      { actualLength :: Int
      , maxLength :: Int  
      }
  | InvalidHyphenPosition HyphenError
  | StartsWithCombiningMark
      { character :: Char
      , codepoint :: Int
      }
  | DisallowedCodePoint
      { character :: Char
      , codepoint :: Int
      }
  | InvalidContext
      { character :: Char
      , contextRule :: ContextRule
      , position :: Int
      , context :: Text
      }
  | BidiViolation
      { bidiRule :: BidiRule
      , label :: Text
      }
  | PunycodeDecodeError
      { input :: Text
      , reason :: Text
      }
  deriving (Show, Eq, Generic)

instance NFData IDNA2008Error

-- | Hyphen position errors
data HyphenError
  = StartsWithHyphen
  | EndsWithHyphen
  | HyphensAt3And4NotPunycode
  deriving (Show, Eq, Generic)

instance NFData HyphenError

-- | Punycode encoding/decoding errors
data PunycodeError
  = InvalidPunycode Text  -- ^ Malformed Punycode with reason
  | Overflow              -- ^ Integer overflow during encoding/decoding
  deriving (Show, Eq, Generic)

instance NFData PunycodeError

-- | Domain label in either Unicode or ASCII/Punycode form
data Label
  = ULabel Text    -- ^ Unicode label (U-label)
  | ALabel Text    -- ^ ASCII/Punycode label (A-label with xn-- prefix)
  deriving (Show, Eq, Generic)

instance NFData Label

-- | Complete domain name consisting of one or more labels
newtype DomainName = DomainName 
  { labels :: NonEmpty Label }
  deriving (Show, Eq, Generic)

instance NFData DomainName

-- | Smart constructor for Label with invariant enforcement
mkLabel :: Text -> Either IDNA2008Error Label
mkLabel text
  | T.null text = Left EmptyLabel
  | T.length text > 63 = Left (LabelTooLong (T.length text) 63)
  | "xn--" `T.isPrefixOf` text = Right (ALabel text)
  | otherwise = Right (ULabel text)

-- | Smart constructor for DomainName with invariant enforcement
mkDomainName :: Text -> Either IDNA2008Error DomainName
mkDomainName text = do
  if T.null text 
    then Left EmptyLabel
    else if T.length text > 253
      then Left (TotalDomainTooLong (T.length text) 253)
      else do
        let labelTexts = T.splitOn "." text
        labelList <- traverse mkLabel labelTexts
        case NE.nonEmpty labelList of
          Nothing -> Left EmptyLabel
          Just ne -> Right (DomainName ne)

-- | IDNA2008 code point status per RFC 5892
data CodePointStatus
  = PVALID      -- ^ Always valid in domain names
  | DISALLOWED  -- ^ Never valid in domain names
  | CONTEXTJ    -- ^ Valid only in specific contexts (Join_Control)
  | CONTEXTO    -- ^ Valid only in specific contexts (Other)
  | UNASSIGNED  -- ^ Not assigned in this Unicode version
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData CodePointStatus

-- | Unicode bidirectional character class per UCD
data BidiClass
  = L    -- ^ Left-to-Right
  | R    -- ^ Right-to-Left
  | AL   -- ^ Right-to-Left Arabic
  | AN   -- ^ Arabic Number
  | EN   -- ^ European Number
  | ES   -- ^ European Number Separator
  | ET   -- ^ European Number Terminator
  | CS   -- ^ Common Number Separator
  | NSM  -- ^ Nonspacing Mark
  | BN   -- ^ Boundary Neutral
  | B    -- ^ Paragraph Separator
  | S    -- ^ Segment Separator
  | WS   -- ^ Whitespace
  | ON   -- ^ Other Neutral
  | LRE  -- ^ Left-to-Right Embedding
  | LRO  -- ^ Left-to-Right Override
  | RLE  -- ^ Right-to-Left Embedding
  | RLO  -- ^ Right-to-Left Override
  | PDF  -- ^ Pop Directional Format
  | LRI  -- ^ Left-to-Right Isolate
  | RLI  -- ^ Right-to-Left Isolate
  | FSI  -- ^ First Strong Isolate
  | PDI  -- ^ Pop Directional Isolate
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData BidiClass

-- | Contextual validation rules from RFC 5892 Appendix A
data ContextRule
  = ZWNJRule                      -- ^ A.1: Zero Width Non-Joiner
  | ZWJRule                       -- ^ A.2: Zero Width Joiner  
  | MiddleDotRule                 -- ^ A.3: Middle Dot
  | GreekKeraiaRule               -- ^ A.4: Greek Lower Numeral Sign
  | HebrewGereshRule              -- ^ A.5: Hebrew Punctuation Geresh
  | HebrewGershayimRule           -- ^ A.6: Hebrew Punctuation Gershayim
  | KatakanaMiddleDotRule         -- ^ A.7: Katakana Middle Dot
  | ArabicIndicDigitsRule         -- ^ A.8: Arabic-Indic Digits
  | ExtendedArabicIndicDigitsRule -- ^ A.9: Extended Arabic-Indic Digits
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData ContextRule

-- | Bidirectional text rules from RFC 5893
data BidiRule
  = Rule1  -- ^ Presence of RTL characters triggers RTL validation
  | Rule2  -- ^ First character must be R or AL
  | Rule3  -- ^ Last character must be R, AL, AN, or EN  
  | Rule4  -- ^ NSM may only follow allowed character types
  | Rule5  -- ^ ES and CS constraints in RTL labels
  | Rule6  -- ^ ON, BN constraints in RTL labels
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData BidiRule

-- | Unicode script assignment
data Script
  = Latin
  | Greek
  | Cyrillic
  | Hebrew
  | Arabic
  | Devanagari
  | Bengali
  | Gurmukhi
  | Gujarati
  | Oriya
  | Tamil
  | Telugu
  | Kannada
  | Malayalam
  | Thai
  | Lao
  | Tibetan
  | Myanmar
  | Georgian
  | Hangul
  | Hiragana
  | Katakana
  | Han
  | OtherScript
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData Script

-- | Punycode algorithm state per RFC 3492
data PunycodeState = PunycodeState
  { n :: Int      -- ^ Next code point to encode
  , delta :: Int  -- ^ Delta value for encoding
  , bias :: Int   -- ^ Bias for variable-length integer encoding
  , h :: Int      -- ^ Number of code points handled
  } deriving (Show, Eq, Generic)

instance NFData PunycodeState

-- | Initial Punycode state per RFC 3492
initialState :: PunycodeState
initialState = PunycodeState
  { n = 0x80      -- Initial code point (128)
  , delta = 0
  , bias = 72     -- Initial bias
  , h = 0
  }

-- | Punycode algorithm parameters per RFC 3492
data PunycodeParams = PunycodeParams
  { base :: Int   -- ^ 36
  , tmin :: Int   -- ^ 1
  , tmax :: Int   -- ^ 26
  , skew :: Int   -- ^ 38
  , damp :: Int   -- ^ 700
  } deriving (Show, Eq, Generic)

instance NFData PunycodeParams

-- | Standard Punycode parameters from RFC 3492
punycodeParams :: PunycodeParams
punycodeParams = PunycodeParams
  { base = 36
  , tmin = 1
  , tmax = 26
  , skew = 38
  , damp = 700
  }

