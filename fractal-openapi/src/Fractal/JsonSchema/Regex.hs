{-# LANGUAGE Trustworthy #-}
-- | Pure interface to ECMA262 regular expressions
--
-- This module provides an observably pure interface to the underlying
-- ECMA262 regex engine with a simple PCRE-like API.
--
-- The IO operations are encapsulated using unsafePerformIO which is safe because:
--
-- 1. Regex compilation is deterministic - same pattern and flags always produce the same result
-- 2. Regex matching is deterministic - same regex and input always produce the same result
-- 3. No observable side effects occur (no mutation, no external state changes)
-- 4. The FFI operations are thread-safe
module Fractal.JsonSchema.Regex
  ( -- * Types
    Regex
  , RegexFlag(..)
    -- * Compilation
  , compile
  , compileText
  , makeRegex
    -- * Matching
  , test
  , matchFirst
  , (=~)
  ) where

import qualified Text.Regex.ECMA262 as R
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO.Unsafe (unsafePerformIO)

-- | Re-export Regex type
type Regex = R.Regex

-- | Re-export RegexFlag type
type RegexFlag = R.RegexFlag

-- | Compile a regular expression pattern from a ByteString
--
-- This is observably pure: same input always produces same output,
-- no side effects, deterministic behavior.
compile :: BS.ByteString -> [RegexFlag] -> Either String Regex
compile pat flags = unsafePerformIO $ R.compile pat flags
{-# NOINLINE compile #-}

-- | Compile a regular expression pattern from a Text value
compileText :: T.Text -> [RegexFlag] -> Either String Regex
compileText pat flags = unsafePerformIO $ R.compileText pat flags
{-# NOINLINE compileText #-}

-- | Test if a regex matches a subject string
--
-- This is observably pure: same regex and input always produces same result.
test :: Regex -> BS.ByteString -> Bool
test regex subject = unsafePerformIO $ R.test regex subject
{-# NOINLINE test #-}

-- | Find the first match of a regex in a subject string
--
-- This is observably pure: same regex and input always produces same result.
matchFirst :: Regex -> BS.ByteString -> Maybe R.Match
matchFirst regex subject = unsafePerformIO $ R.match regex subject
{-# NOINLINE matchFirst #-}

-- | Make a regex from a string (throws error on invalid pattern)
makeRegex :: String -> Regex
makeRegex pat = case compile (BS8.pack pat) [] of
  Right r -> r
  Left err -> error $ "makeRegex: " ++ err

-- | PCRE-like matching operator
--
-- Usage:
-- @
--   "hello" =~ "^h.*o$" :: Bool
-- @
class MatchLike source pattern where
  (=~) :: source -> pattern -> Bool

-- Match String against String pattern
instance MatchLike String String where
  source =~ pattern = case compile (BS8.pack pattern) [] of
    Right regex -> test regex (BS8.pack source)
    Left _ -> False

-- Match Text against Text pattern
instance MatchLike T.Text T.Text where
  source =~ pattern = case compileText pattern [] of
    Right regex -> test regex (TE.encodeUtf8 source)
    Left _ -> False

-- Match String against compiled Regex
instance MatchLike String Regex where
  source =~ regex = test regex (BS8.pack source)

-- Match Text against compiled Regex
instance MatchLike T.Text Regex where
  source =~ regex = test regex (TE.encodeUtf8 source)

infixl 9 =~
