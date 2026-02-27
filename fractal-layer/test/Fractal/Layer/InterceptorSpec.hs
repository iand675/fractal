{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fractal.Layer.InterceptorSpec (spec) where

import Test.Hspec
import Data.Typeable (Proxy(..), typeRep)
import Fractal.Layer.Interceptor
import UnliftIO

spec :: Spec
spec = do
  describe "CompositionType" $ do
    it "Sequential show" $
      show Sequential `shouldBe` "Sequential"

    it "Parallel show" $
      show Parallel `shouldBe` "Parallel"

    it "Sequential == Sequential" $
      Sequential `shouldBe` Sequential

    it "Parallel == Parallel" $
      Parallel `shouldBe` Parallel

    it "Sequential /= Parallel" $
      Sequential `shouldNotBe` Parallel

  describe "OperationContext" $ do
    it "show includes all fields" $ do
      let ctx = OperationContext
            { operationName = "test"
            , operationType = Just (typeRep (Proxy @Int))
            , operationMetadata = [("key", "value")]
            }
      let s = show ctx
      s `shouldContain` "test"
      s `shouldContain` "key"
      s `shouldContain` "value"

    it "show with Nothing type" $ do
      let ctx = simpleContext "bare"
      show ctx `shouldContain` "bare"

  describe "nullInterceptor" $ do
    it "all callbacks are no-ops" $ do
      let ni = nullInterceptor :: LayerInterceptor IO
      onResourceAcquire ni (simpleContext "test")
      onResourceRelease ni "test" 0
      onEffectRun ni (simpleContext "test")
      onEffectComplete ni "test" 0
      onServiceCreate ni (simpleContext "test")
      onServiceReuse ni "test" (typeRep (Proxy @Int))
      onCompositionStart ni Sequential
      onCompositionEnd ni Sequential 0

  describe "combineInterceptors" $ do
    it "empty list behaves like nullInterceptor" $ do
      let combined = combineInterceptors ([] :: [LayerInterceptor IO])
      onResourceAcquire combined (simpleContext "test")
      onCompositionStart combined Sequential
      onCompositionEnd combined Parallel 1.0

    it "preserves order of callbacks" $ do
      ref <- newIORef ([] :: [Int])
      let i1 = nullInterceptor { onEffectRun = \_ -> modifyIORef ref (++ [1]) }
      let i2 = nullInterceptor { onEffectRun = \_ -> modifyIORef ref (++ [2]) }
      let i3 = nullInterceptor { onEffectRun = \_ -> modifyIORef ref (++ [3]) }
      let combined = combineInterceptors [i1, i2, i3]
      onEffectRun combined (simpleContext "test")
      readIORef ref >>= (`shouldBe` [1, 2, 3])

    it "combines onCompositionStart callbacks" $ do
      ref <- newIORef ([] :: [String])
      let i1 = nullInterceptor { onCompositionStart = \t -> modifyIORef ref (++ ["i1:" ++ show t]) }
      let i2 = nullInterceptor { onCompositionStart = \t -> modifyIORef ref (++ ["i2:" ++ show t]) }
      let combined = combineInterceptors [i1, i2]

      onCompositionStart combined Sequential
      onCompositionStart combined Parallel

      logs <- readIORef ref
      logs `shouldBe` ["i1:Sequential", "i2:Sequential", "i1:Parallel", "i2:Parallel"]

    it "combines onCompositionEnd callbacks" $ do
      ref <- newIORef ([] :: [String])
      let i1 = nullInterceptor { onCompositionEnd = \t _ -> modifyIORef ref (++ ["i1:" ++ show t]) }
      let i2 = nullInterceptor { onCompositionEnd = \t _ -> modifyIORef ref (++ ["i2:" ++ show t]) }
      let combined = combineInterceptors [i1, i2]

      onCompositionEnd combined Sequential 1.0
      logs <- readIORef ref
      logs `shouldBe` ["i1:Sequential", "i2:Sequential"]

    it "combines all callback types" $ do
      ref <- newIORef ([] :: [String])
      let tracker :: String -> LayerInterceptor IO
          tracker tag = (nullInterceptor :: LayerInterceptor IO)
            { onResourceAcquire = \_ -> modifyIORef ref (++ [tag ++ ":acq"])
            , onResourceRelease = \_ _ -> modifyIORef ref (++ [tag ++ ":rel"])
            , onEffectRun = \_ -> modifyIORef ref (++ [tag ++ ":eff"])
            , onEffectComplete = \_ _ -> modifyIORef ref (++ [tag ++ ":done"])
            , onServiceCreate = \_ -> modifyIORef ref (++ [tag ++ ":svc"])
            , onServiceReuse = \_ _ -> modifyIORef ref (++ [tag ++ ":reuse"])
            , onCompositionStart = \_ -> modifyIORef ref (++ [tag ++ ":cstart"])
            , onCompositionEnd = \_ _ -> modifyIORef ref (++ [tag ++ ":cend"])
            }
      let combined = combineInterceptors [tracker "a", tracker "b"]

      onResourceAcquire combined (simpleContext "r")
      onResourceRelease combined "r" 0
      onEffectRun combined (simpleContext "e")
      onEffectComplete combined "e" 0
      onServiceCreate combined (simpleContext "s")
      onServiceReuse combined "s" (typeRep (Proxy @Int))
      onCompositionStart combined Sequential
      onCompositionEnd combined Sequential 0

      logs <- readIORef ref
      logs `shouldBe`
        [ "a:acq", "b:acq"
        , "a:rel", "b:rel"
        , "a:eff", "b:eff"
        , "a:done", "b:done"
        , "a:svc", "b:svc"
        , "a:reuse", "b:reuse"
        , "a:cstart", "b:cstart"
        , "a:cend", "b:cend"
        ]

  describe "simpleContext" $ do
    it "creates context with name only" $ do
      let ctx = simpleContext "my-op"
      operationName ctx `shouldBe` "my-op"
      operationType ctx `shouldBe` Nothing
      operationMetadata ctx `shouldBe` []

  describe "withType" $ do
    it "adds type to context" $ do
      let ctx = withType (typeRep (Proxy @Bool)) (simpleContext "op")
      operationType ctx `shouldBe` Just (typeRep (Proxy @Bool))
      operationName ctx `shouldBe` "op"
      operationMetadata ctx `shouldBe` []

  describe "withMetadata" $ do
    it "adds metadata to context" $ do
      let ctx = withMetadata [("k1", "v1"), ("k2", "v2")] (simpleContext "op")
      operationMetadata ctx `shouldBe` [("k1", "v1"), ("k2", "v2")]

    it "appends to existing metadata" $ do
      let ctx0 = OperationContext "op" Nothing [("existing", "data")]
      let ctx1 = withMetadata [("new", "field")] ctx0
      operationMetadata ctx1 `shouldBe` [("existing", "data"), ("new", "field")]
