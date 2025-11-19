module Fractal.OpenApi.JsonSchema.ValidatorSpec (spec) where

import Test.Hspec
import Fractal.OpenApi.JsonSchema.Validator

spec :: Spec
spec = do
  describe "JSON Schema Validator" $ do
    describe "validatePattern (ECMAScript 262 regex)" $ do
      it "validates simple patterns" $ do
        validatePattern "^[a-z]+$" "hello" `shouldBe` Right True
        validatePattern "^[a-z]+$" "Hello" `shouldBe` Right False
        validatePattern "^[a-z]+$" "123" `shouldBe` Right False

      it "validates patterns with Unicode" $ do
        validatePattern "^[\\u0061-\\u007a]+$" "hello" `shouldBe` Right True
        -- Note: Unicode property escapes require Unicode flag in ECMAScript regex
        validatePattern "^[A-Z]+$" "HELLO" `shouldBe` Right True

      it "validates patterns with character classes" $ do
        validatePattern "^\\d{3}-\\d{4}$" "123-4567" `shouldBe` Right True
        validatePattern "^\\d{3}-\\d{4}$" "abc-defg" `shouldBe` Right False

      it "validates patterns with lookahead" $ do
        validatePattern "^(?=.*[a-z])(?=.*[A-Z]).+$" "helloWORLD" `shouldBe` Right True
        validatePattern "^(?=.*[a-z])(?=.*[A-Z]).+$" "hello" `shouldBe` Right False

      it "handles invalid patterns" $ do
        case validatePattern "^[" "test" of
          Left _ -> True `shouldBe` True
          Right _ -> expectationFailure "Should have failed with invalid pattern"
