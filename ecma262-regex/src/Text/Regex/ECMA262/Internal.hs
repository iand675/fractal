{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

-- | Low-level FFI bindings to libregexp from QuickJS
module Text.Regex.ECMA262.Internal
  ( -- * Opaque types
    Regex
  , RegexPtr
    -- * Compilation
  , compileRegex
  , freeRegex
  , c_free
    -- * Inspection
  , isValid
  , getError
  , getCaptureCount
  , getFlags
  , getGroupNames
    -- * Execution
  , execRegex
    -- * Flags
  , RegexFlag(..)
  , flagsToInt
  , intToFlags
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Bits
import Control.Monad (when)
import Control.Exception (bracket)

-- | Opaque pointer to compiled regex
data Regex
type RegexPtr = Ptr Regex

-- | ECMAScript regex flags
data RegexFlag
  = Global        -- ^ g flag: global matching
  | IgnoreCase    -- ^ i flag: case-insensitive
  | Multiline     -- ^ m flag: multiline mode
  | DotAll        -- ^ s flag: dot matches newline
  | Unicode       -- ^ u flag: Unicode mode
  | Sticky        -- ^ y flag: sticky matching
  | Indices       -- ^ d flag: generate indices for substring matches
  | HasNamedGroups -- ^ internal: regex has named groups
  | UnicodeSets   -- ^ v flag: Unicode sets mode
  deriving (Eq, Show, Enum, Bounded)

-- | Convert flags to integer representation
flagsToInt :: [RegexFlag] -> CInt
flagsToInt = fromIntegral . foldr (\flag acc -> acc .|. flagBit flag) (0 :: Int)
  where
    flagBit :: RegexFlag -> Int
    flagBit Global        = 1 `shiftL` 0
    flagBit IgnoreCase    = 1 `shiftL` 1
    flagBit Multiline     = 1 `shiftL` 2
    flagBit DotAll        = 1 `shiftL` 3
    flagBit Unicode       = 1 `shiftL` 4
    flagBit Sticky        = 1 `shiftL` 5
    flagBit Indices       = 1 `shiftL` 6
    flagBit HasNamedGroups = 1 `shiftL` 7
    flagBit UnicodeSets   = 1 `shiftL` 8

-- | Convert integer representation to flags
intToFlags :: CInt -> [RegexFlag]
intToFlags n = filter (testBit (fromIntegral n :: Int) . flagIndex) [minBound .. maxBound]
  where
    flagIndex :: RegexFlag -> Int
    flagIndex Global        = 0
    flagIndex IgnoreCase    = 1
    flagIndex Multiline     = 2
    flagIndex DotAll        = 3
    flagIndex Unicode       = 4
    flagIndex Sticky        = 5
    flagIndex Indices       = 6
    flagIndex HasNamedGroups = 7
    flagIndex UnicodeSets   = 8

-- | FFI imports
foreign import ccall unsafe "ecma262_regex_wrapper.c ecma262_regex_compile"
  c_compile :: CString -> CSize -> CInt -> IO RegexPtr

foreign import ccall unsafe "ecma262_regex_wrapper.c &ecma262_regex_free"
  c_free :: FunPtr (RegexPtr -> IO ())

foreign import ccall unsafe "ecma262_regex_wrapper.c ecma262_regex_is_valid"
  c_is_valid :: RegexPtr -> IO CInt

foreign import ccall unsafe "ecma262_regex_wrapper.c ecma262_regex_get_error"
  c_get_error :: RegexPtr -> IO CString

foreign import ccall unsafe "ecma262_regex_wrapper.c ecma262_regex_get_capture_count"
  c_get_capture_count :: RegexPtr -> IO CInt

foreign import ccall unsafe "ecma262_regex_wrapper.c ecma262_regex_get_flags"
  c_get_flags :: RegexPtr -> IO CInt

foreign import ccall unsafe "ecma262_regex_wrapper.c ecma262_regex_get_groupnames"
  c_get_groupnames :: RegexPtr -> IO CString

foreign import ccall unsafe "ecma262_regex_wrapper.c ecma262_regex_exec"
  c_exec :: RegexPtr -> Ptr Word8 -> CInt -> CInt -> Ptr (Ptr Word8) -> IO CInt

-- | Compile a regular expression pattern
compileRegex :: BS.ByteString -> [RegexFlag] -> IO (Either String RegexPtr)
compileRegex pattern flags = do
  BSU.unsafeUseAsCStringLen pattern $ \(patternStr, patternLen) -> do
    ptr <- c_compile patternStr (fromIntegral patternLen) (flagsToInt flags)
    if ptr == nullPtr
      then return $ Left "Failed to allocate memory for regex"
      else do
        valid <- c_is_valid ptr
        if valid /= 0
          then return $ Right ptr
          else do
            errPtr <- c_get_error ptr
            if errPtr == nullPtr
              then return $ Left "Unknown compilation error"
              else do
                errMsg <- peekCString errPtr
                c_free_direct ptr
                return $ Left errMsg

-- | Direct call to free (needed when compilation fails)
foreign import ccall unsafe "ecma262_regex_wrapper.c ecma262_regex_free"
  c_free_direct :: RegexPtr -> IO ()

-- | Free a compiled regex
freeRegex :: RegexPtr -> IO ()
freeRegex = c_free_direct

-- | Check if regex is valid
isValid :: RegexPtr -> IO Bool
isValid ptr = (/= 0) <$> c_is_valid ptr

-- | Get compilation error message
getError :: RegexPtr -> IO String
getError ptr = do
  errPtr <- c_get_error ptr
  if errPtr == nullPtr
    then return ""
    else peekCString errPtr

-- | Get number of capture groups
getCaptureCount :: RegexPtr -> IO Int
getCaptureCount ptr = fromIntegral <$> c_get_capture_count ptr

-- | Get regex flags
getFlags :: RegexPtr -> IO [RegexFlag]
getFlags ptr = intToFlags <$> c_get_flags ptr

-- | Get named group names (if any)
getGroupNames :: RegexPtr -> IO (Maybe String)
getGroupNames ptr = do
  namePtr <- c_get_groupnames ptr
  if namePtr == nullPtr
    then return Nothing
    else Just <$> peekCString namePtr

-- | Execute regex against a subject string
-- Returns Nothing if no match, Just (startIdx, endIdx, captures) if match
-- Note: libregexp's capture_count INCLUDES the full match at index 0
execRegex :: RegexPtr -> BS.ByteString -> Int -> IO (Maybe (Int, Int, [(Int, Int)]))
execRegex ptr subject startIdx = do
  captureCount <- getCaptureCount ptr
  -- captureCount already includes the full match, so no need to add 1
  let captureArraySize = captureCount * 2

  BSU.unsafeUseAsCStringLen subject $ \(subjectStr, subjectLen) -> do
    allocaArray captureArraySize $ \captureArray -> do
      result <- c_exec ptr (castPtr subjectStr) (fromIntegral startIdx)
                      (fromIntegral subjectLen) captureArray

      if result == 1
        then do
          -- Match found, extract captures (including full match at index 0)
          captures <- extractCaptures captureArray captureCount (castPtr subjectStr)
          case captures of
            ((start, end):rest) -> return $ Just (start, end, rest)
            [] -> return Nothing
        else return Nothing

-- | Extract capture group positions from the C array
extractCaptures :: Ptr (Ptr Word8) -> Int -> Ptr Word8 -> IO [(Int, Int)]
extractCaptures captureArray numCaptures basePtr = go 0
  where
    go i
      | i >= numCaptures = return []
      | otherwise = do
          startPtr <- peekElemOff captureArray (i * 2)
          endPtr <- peekElemOff captureArray (i * 2 + 1)

          let start = if startPtr == nullPtr then -1 else startPtr `minusPtr` basePtr
              end = if endPtr == nullPtr then -1 else endPtr `minusPtr` basePtr

          rest <- go (i + 1)
          return $ (start, end) : rest
