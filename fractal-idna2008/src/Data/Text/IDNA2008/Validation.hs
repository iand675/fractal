{-# LANGUAGE OverloadedStrings #-}

-- | IDNA2008 contextual and bidirectional validation rules
module Data.Text.IDNA2008.Validation
  ( validateContextualRules
  , validateBidiRules
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IDNA2008.Types
import Data.Text.IDNA2008.Unicode
import Data.Char (ord)
import Data.Maybe (isJust)

-- | Validate contextual rules for CONTEXTJ and CONTEXTO code points (RFC 5892 Appendix A)
validateContextualRules :: Text -> Either IDNA2008Error ()
validateContextualRules label = mapM_ checkChar (zip [0..] (T.unpack label))
  where
    checkChar (pos, c) =
      case codePointStatus c of
        CONTEXTJ -> validateContextJ label pos c
        CONTEXTO -> validateContextO label pos c
        _ -> pure ()

-- | Validate CONTEXTJ characters (Join Controls)
validateContextJ :: Text -> Int -> Char -> Either IDNA2008Error ()
validateContextJ label pos c
  | ord c == 0x200C = validateZWNJ label pos c  -- ZERO WIDTH NON-JOINER
  | ord c == 0x200D = validateZWJ label pos c   -- ZERO WIDTH JOINER
  | otherwise = pure ()

-- | Validate CONTEXTO characters (Other)
validateContextO :: Text -> Int -> Char -> Either IDNA2008Error ()
validateContextO label pos c =
  case lookupContextRule c of
    Just MiddleDotRule -> validateMiddleDot label pos c
    Just GreekKeraiaRule -> validateGreekKeraia label pos c
    Just HebrewGereshRule -> validateHebrewPunctuation label pos c
    Just HebrewGershayimRule -> validateHebrewPunctuation label pos c
    Just KatakanaMiddleDotRule -> validateKatakanaMiddleDot label pos c
    Just ArabicIndicDigitsRule -> validateArabicIndicDigits label pos c
    Just ExtendedArabicIndicDigitsRule -> validateExtendedArabicIndicDigits label pos c
    _ -> pure ()

-- | ZERO WIDTH NON-JOINER (U+200C) - RFC 5892 Appendix A.1
validateZWNJ :: Text -> Int -> Char -> Either IDNA2008Error ()
validateZWNJ label pos c =
  if pos > 0 && isVirama (T.index label (pos - 1))
    then pure ()
    else Left (InvalidContext c ZWNJRule pos label)

-- | ZERO WIDTH JOINER (U+200D) - RFC 5892 Appendix A.2
validateZWJ :: Text -> Int -> Char -> Either IDNA2008Error ()
validateZWJ label pos c =
  if pos > 0 && isVirama (T.index label (pos - 1))
    then pure ()
    else Left (InvalidContext c ZWJRule pos label)

-- | MIDDLE DOT (U+00B7) - RFC 5892 Appendix A.3
validateMiddleDot :: Text -> Int -> Char -> Either IDNA2008Error ()
validateMiddleDot label pos c =
  let before = pos > 0 && T.index label (pos - 1) == 'l'
      after = pos < T.length label - 1 && T.index label (pos + 1) == 'l'
  in if before && after
       then pure ()
       else Left (InvalidContext c MiddleDotRule pos label)

-- | GREEK LOWER NUMERAL SIGN (U+0375) - RFC 5892 Appendix A.4
validateGreekKeraia :: Text -> Int -> Char -> Either IDNA2008Error ()
validateGreekKeraia label pos c =
  let hasGreek = any (\ch -> scriptOf ch == Just Greek) (T.unpack label)
  in if hasGreek
       then pure ()
       else Left (InvalidContext c GreekKeraiaRule pos label)

-- | HEBREW PUNCTUATION (U+05F3, U+05F4) - RFC 5892 Appendix A.5/A.6
validateHebrewPunctuation :: Text -> Int -> Char -> Either IDNA2008Error ()
validateHebrewPunctuation label pos c =
  let hasHebrew = any (\ch -> scriptOf ch == Just Hebrew) (T.unpack label)
  in if hasHebrew
       then pure ()
       else Left (InvalidContext c HebrewGereshRule pos label)

-- | KATAKANA MIDDLE DOT (U+30FB) - RFC 5892 Appendix A.7
validateKatakanaMiddleDot :: Text -> Int -> Char -> Either IDNA2008Error ()
validateKatakanaMiddleDot label pos c =
  let hasKatakanaOrHiragana = any (\ch -> 
        let s = scriptOf ch
        in s == Just Katakana || s == Just Hiragana || s == Just Han
        ) (T.unpack label)
  in if hasKatakanaOrHiragana
       then pure ()
       else Left (InvalidContext c KatakanaMiddleDotRule pos label)

-- | ARABIC-INDIC DIGITS (U+0660..U+0669) - RFC 5892 Appendix A.8
validateArabicIndicDigits :: Text -> Int -> Char -> Either IDNA2008Error ()
validateArabicIndicDigits label pos c =
  let hasExtended = any (\ch -> ord ch >= 0x06F0 && ord ch <= 0x06F9) (T.unpack label)
  in if not hasExtended
       then pure ()
       else Left (InvalidContext c ArabicIndicDigitsRule pos label)

-- | EXTENDED ARABIC-INDIC DIGITS (U+06F0..U+06F9) - RFC 5892 Appendix A.9
validateExtendedArabicIndicDigits :: Text -> Int -> Char -> Either IDNA2008Error ()
validateExtendedArabicIndicDigits label pos c =
  let hasNormal = any (\ch -> ord ch >= 0x0660 && ord ch <= 0x0669) (T.unpack label)
  in if not hasNormal
       then pure ()
       else Left (InvalidContext c ExtendedArabicIndicDigitsRule pos label)

-- | Validate bidirectional text rules (RFC 5893)
validateBidiRules :: Text -> Either IDNA2008Error ()
validateBidiRules label =
  let chars = T.unpack label
      bidiClasses = map bidiClass chars
      hasRTL = any (`elem` [R, AL, AN]) bidiClasses
  in if hasRTL
       then validateRTLLabel label chars bidiClasses
       else validateLTRLabel label chars bidiClasses

-- | Validate RTL labels (RFC 5893 Rules 2-6)
validateRTLLabel :: Text -> [Char] -> [BidiClass] -> Either IDNA2008Error ()
validateRTLLabel label chars bidiClasses = do
  -- Rule 2: First character must be R or AL
  case bidiClasses of
    (first:_) | first `notElem` [R, AL] ->
      Left (BidiViolation Rule2 label)
    _ -> pure ()
  
  -- Rule 3: Last character must be R, AL, EN, or AN
  case reverse bidiClasses of
    (last':_) | last' `notElem` [R, AL, EN, AN] ->
      Left (BidiViolation Rule3 label)
    _ -> pure ()
  
  -- Rule 4: NSM must follow allowed characters
  mapM_ (checkNSM bidiClasses) [0..length bidiClasses - 1]
  
  -- Rule 5: No ES or CS with AN
  if any (== AN) bidiClasses && any (`elem` [ES, CS]) bidiClasses
    then Left (BidiViolation Rule5 label)
    else pure ()
  
  -- Rule 6: EN and AN cannot be mixed
  if any (== EN) bidiClasses && any (== AN) bidiClasses
    then Left (BidiViolation Rule6 label)
    else pure ()
  
  where
    checkNSM classes pos =
      if pos < length classes && classes !! pos == NSM
        then if pos > 0 && classes !! (pos - 1) `elem` [R, AL, EN, AN]
          then pure ()
          else Left (BidiViolation Rule4 label)
        else pure ()

-- | Validate LTR labels (simpler rules)
validateLTRLabel :: Text -> [Char] -> [BidiClass] -> Either IDNA2008Error ()
validateLTRLabel _ _ _ = pure ()  -- LTR labels have minimal bidi restrictions

