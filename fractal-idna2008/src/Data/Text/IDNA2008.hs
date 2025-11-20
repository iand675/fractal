{-# LANGUAGE OverloadedStrings #-}

-- | High-level IDNA2008 API for domain name processing
module Data.Text.IDNA2008
  ( -- * Main API
    toASCII
  , toUnicode
  , validateLabel
  
    -- * Types
  , module Data.Text.IDNA2008.Types
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IDNA2008.Types
import Data.Text.IDNA2008.Unicode
import Data.Text.IDNA2008.Validation
import Data.Text.IDNA2008.Punycode (encodePunycode, decodePunycode)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Char (ord, isAscii)

-- | Convert a Unicode domain name to ASCII (A-label) form per RFC 5891
toASCII :: Text -> Either IDNA2008Error Text
toASCII input = do
  domain <- mkDomainName input
  processedLabels <- traverse processLabelToASCII (labels domain)
  return $ T.intercalate "." $ map labelText (NE.toList processedLabels)
  where
    labelText (ULabel t) = t
    labelText (ALabel t) = t

-- | Convert an ASCII domain name to Unicode (U-label) form per RFC 5891
toUnicode :: Text -> Either IDNA2008Error Text
toUnicode input = do
  domain <- mkDomainName input
  processedLabels <- traverse processLabelToUnicode (labels domain)
  return $ T.intercalate "." $ map labelText (NE.toList processedLabels)
  where
    labelText (ULabel t) = t
    labelText (ALabel t) = t

-- | Process a single label to ASCII form
processLabelToASCII :: Label -> Either IDNA2008Error Label
processLabelToASCII (ALabel t) = 
  -- Already ASCII, validate it
  validateLabel t >> return (ALabel t)
  
processLabelToASCII (ULabel t)
  | isAsciiLabel t = do
      -- Pure ASCII label, validate and return as-is
      validateLabel t
      return (ULabel t)
  
  | otherwise = do
      -- Unicode label, needs Punycode encoding
      validateLabel t
      encoded <- case encodePunycode t of
        Left err -> Left (PunycodeDecodeError t (T.pack (show err)))
        Right e -> Right e
      let aLabel = "xn--" <> encoded
      
      -- Validate length constraints
      if T.length aLabel > 63
        then Left (LabelTooLong (T.length aLabel) 63)
        else return (ALabel aLabel)

-- | Process a single label to Unicode form  
processLabelToUnicode :: Label -> Either IDNA2008Error Label
processLabelToUnicode (ULabel t) = 
  -- Already Unicode, validate and return
  validateLabel t >> return (ULabel t)
  
processLabelToUnicode (ALabel t)
  | "xn--" `T.isPrefixOf` t = do
      -- Punycode A-label, decode it
      let encoded = T.drop 4 t
      decoded <- case decodePunycode encoded of
        Left err -> Left (PunycodeDecodeError encoded (T.pack (show err)))
        Right d -> Right d
      validateLabel decoded
      return (ULabel decoded)
  
  | otherwise =
      -- Pure ASCII label, validate and return
      validateLabel t >> return (ALabel t)

-- | Validate a single domain label per IDNA2008 rules
validateLabel :: Text -> Either IDNA2008Error ()
validateLabel label = do
  -- Check empty
  if T.null label
    then Left EmptyLabel
    else pure ()
  
  -- Check length (before encoding)
  if T.length label > 63
    then Left (LabelTooLong (T.length label) 63)
    else pure ()
  
  -- Check hyphen positions
  checkHyphens label
  
  -- Check combining marks at start
  case T.uncons label of
    Just (c, _) | isCombiningMark c ->
      Left (StartsWithCombiningMark c (ord c))
    _ -> pure ()
  
  -- Validate each code point
  mapM_ validateCodePoint (T.unpack label)
  
  -- Validate contextual rules for CONTEXTJ/CONTEXTO
  validateContextualRules label
  
  -- Validate bidi rules
  validateBidiRules label
  
  return ()

-- | Check hyphen position rules
checkHyphens :: Text -> Either IDNA2008Error ()
checkHyphens label = do
  case T.uncons label of
    Just ('-', _) -> Left (InvalidHyphenPosition StartsWithHyphen)
    _ -> pure ()
  
  case T.unsnoc label of
    Just (_, '-') -> Left (InvalidHyphenPosition EndsWithHyphen)
    _ -> pure ()
  
  -- Check for "--" in positions 3-4 (unless it's an A-label with xn--)
  if T.length label >= 4 && not ("xn--" `T.isPrefixOf` label)
    then if T.index label 2 == '-' && T.index label 3 == '-'
      then Left (InvalidHyphenPosition HyphensAt3And4NotPunycode)
      else pure ()
    else pure ()

-- | Validate a single code point
validateCodePoint :: Char -> Either IDNA2008Error ()
validateCodePoint c =
  case codePointStatus c of
    PVALID -> pure ()
    DISALLOWED -> Left (DisallowedCodePoint c (ord c))
    CONTEXTJ -> pure ()  -- Will be checked in context validation
    CONTEXTO -> pure ()  -- Will be checked in context validation
    UNASSIGNED -> Left (DisallowedCodePoint c (ord c))

-- | Check if a label is pure ASCII
isAsciiLabel :: Text -> Bool
isAsciiLabel = T.all isAscii
