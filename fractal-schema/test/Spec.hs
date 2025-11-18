{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

-- Import the Avro schema compatibility tests
import qualified Fractal.Schema.Compatibility.AvroSpec as Avro
-- Import the client tests
import qualified Fractal.Schema.ClientSpec as Client

main :: IO ()
main = hspec $ do
  -- Include the Avro schema compatibility tests
  Avro.spec
  -- Include the client tests
  Client.spec
