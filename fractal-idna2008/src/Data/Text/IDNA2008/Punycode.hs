{-# LANGUAGE OverloadedStrings #-}

-- | Punycode encoding/decoding per RFC 3492
module Data.Text.IDNA2008.Punycode
  ( encodePunycode
  , decodePunycode
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (ord, chr, isAscii)
import Data.Text.IDNA2008.Types (PunycodeError(..))

-- RFC 3492 parameters
base :: Int
base = 36

tmin :: Int
tmin = 1

tmax :: Int
tmax = 26

skew :: Int
skew = 38

damp :: Int
damp = 700

initialBias :: Int
initialBias = 72

initialN :: Int
initialN = 0x80

delimiter :: Char
delimiter = '-'

-- | Encode Unicode text to Punycode
encodePunycode :: Text -> Either PunycodeError Text
encodePunycode input = do
      let inputList = T.unpack input
          basic = filter isBasic inputList
          b = length basic
          output = basic ++ if b > 0 then [delimiter] else []
      
      result <- encodeLoop inputList output b initialN 0 initialBias (length inputList) b
      return $ T.pack result

-- | Main encoding loop - RFC 3492 Section 6.3 pseudocode
encodeLoop :: [Char] -> String -> Int -> Int -> Int -> Int -> Int -> Int -> Either PunycodeError String
encodeLoop input output h n delta bias inputLen b
  | h >= inputLen = Right output
  | otherwise = do
      -- Find next code point m to insert
      let nonBasicCodes = [ord c | c <- input, not (isBasic c)]
          candidates = filter (>= n) nonBasicCodes
      
      if null candidates
        then Right output
        else do
          let m = minimum candidates
              delta' = delta + (m - n) * (h + 1)
          
          if delta' < 0
            then Left Overflow
            else do
              -- Encode all occurrences of m
              (output', h', delta'', bias') <- 
                encodeCodePoint input output h m delta' bias b
              
              encodeLoop input output' h' (m + 1) (delta'' + 1) bias' inputLen b

-- | Encode all occurrences of a specific code point
encodeCodePoint :: [Char] -> String -> Int -> Int -> Int -> Int -> Int ->
                   Either PunycodeError (String, Int, Int, Int)
encodeCodePoint [] output h _ delta bias _ = 
  Right (output, h, delta, bias)
encodeCodePoint (c:cs) output h m delta bias b
  | ord c < m = 
      if delta == maxBound
        then Left Overflow
        else encodeCodePoint cs output h m (delta + 1) bias b
  | ord c == m = do
      -- Encode delta
      let (output', newBias) = encodeVarInt output delta bias (h + 1) (h == b)
      encodeCodePoint cs output' (h + 1) m 0 newBias b
  | otherwise = 
      encodeCodePoint cs output h m delta bias b

-- | Encode variable-length integer
encodeVarInt :: String -> Int -> Int -> Int -> Bool -> (String, Int)
encodeVarInt output q bias numPoints firstTime = 
  let (out, _) = go output q base
      newBias = adapt q numPoints firstTime
  in (out, newBias)
  where
    go out q' k =
      let t = if k <= bias then tmin
              else if k >= bias + tmax then tmax
              else k - bias
      in if q' < t
           then (out ++ [encodeDigit q'], q')
           else let digit = t + ((q' - t) `mod` (base - t))
                    q'' = (q' - t) `div` (base - t)
                in go (out ++ [encodeDigit digit]) q'' (k + base)

-- | Encode a single digit
encodeDigit :: Int -> Char
encodeDigit d
  | d < 26 = chr (ord 'a' + d)
  | otherwise = chr (ord '0' + d - 26)

-- | Decode Punycode to Unicode text  
decodePunycode :: Text -> Either PunycodeError Text
decodePunycode input
  | T.null input = Right input
  | otherwise =
      -- Find the last delimiter
      case T.findIndex (== delimiter) (T.reverse input) of
        Nothing ->
          -- No delimiter - decode as all-encoded with empty basic part
          -- Even if it's all ASCII, it might be Punycode-encoded
          if T.all isBasic input
            then do
              -- Try to decode - if it's not valid Punycode, it's plain ASCII
              result <- decodeLoop (T.unpack input) [] initialN 0 initialBias
              return $ T.pack result
            else Left (InvalidPunycode "Non-basic chars without delimiter")
        Just revPos ->
          -- Found delimiter at position (length - revPos - 1)
          let delimPos = T.length input - revPos - 1
              basicPart = T.take delimPos input
              encodedPart = T.drop (delimPos + 1) input
          in if not (T.all isBasic basicPart)
               then Left (InvalidPunycode "Non-basic in basic part")
               else if T.null encodedPart
                      -- Trailing delimiter with no encoded part - just return basic part
                      then Right basicPart
                      else do
                        result <- decodeLoop (T.unpack encodedPart) (T.unpack basicPart) 
                                           initialN 0 initialBias
                        return $ T.pack result

-- | Main decoding loop - RFC 3492 Section 6.2
decodeLoop :: String -> String -> Int -> Int -> Int -> Either PunycodeError String
decodeLoop [] output _ _ _ = Right output
decodeLoop encoded output n i bias = do
  (delta, rest, _) <- decodeVarInt encoded bias base
  
  let outLen = length output
  
  -- Increment i by decoded delta
  let i' = i + delta
  
  -- Calculate how many times we wrap around the output length
  let numPoints = outLen + 1
      wraps = i' `div` numPoints
      insertPos = i' `mod` numPoints
  
  -- Increment n by the number of wraps
  let n' = n + wraps
  
  if n' > 0x10FFFF
    then Left Overflow
    else do
      let c = chr n'
          -- Insert character at position insertPos
          output' = take insertPos output ++ [c] ++ drop insertPos output
          -- Adapt bias (firsttime = true only on very first call when i was 0 initially)
          bias' = adapt delta (outLen + 1) (i == 0)
          -- Continue with updated i position (wrapped) + 1
          i'' = (i' `mod` (outLen + 1)) + 1
      
      decodeLoop rest output' n' i'' bias'

-- | Decode variable-length integer
decodeVarInt :: String -> Int -> Int -> Either PunycodeError (Int, String, Int)
decodeVarInt encoded bias baseVal = go encoded 0 1 baseVal
  where
    go [] _ _ _ = Left (InvalidPunycode "Incomplete variable-length integer")
    go (c:cs) value weight k = do
      d <- decodeDigit c
      let t = if k <= bias then tmin
              else if k >= bias + tmax then tmax
              else k - bias
          value' = value + d * weight
      
      if d < t
        then Right (value', cs, k)
        else go cs value' (weight * (baseVal - t)) (k + baseVal)

decodeDigit :: Char -> Either PunycodeError Int
decodeDigit c
  | c >= 'a' && c <= 'z' = Right (ord c - ord 'a')
  | c >= 'A' && c <= 'Z' = Right (ord c - ord 'A')
  | c >= '0' && c <= '9' = Right (ord c - ord '0' + 26)
  | otherwise = Left (InvalidPunycode $ "Invalid digit: " <> T.singleton c)

-- | Adapt bias after each delta encoding
adapt :: Int -> Int -> Bool -> Int
adapt delta numPoints firstTime =
  let delta' = if firstTime then delta `div` damp else delta `div` 2
      delta'' = delta' + (delta' `div` numPoints)
  in go delta'' 0
  where
    go d k
      | d > ((base - tmin) * tmax) `div` 2 =
          go (d `div` (base - tmin)) (k + base)
      | otherwise = k + (base - tmin + 1) * d `div` (d + skew)

-- | Check if character is basic (ASCII)
isBasic :: Char -> Bool
isBasic = isAscii
