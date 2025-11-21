module Fractal.JsonSchema.EmbeddedMetaschemasSpec (spec) where

import Test.Hspec
import Fractal.JsonSchema.EmbeddedMetaschemas
import qualified Data.Text as T

spec :: Spec
spec = describe "Embedded Metaschemas" $ do
  it "has embedded files" $ do
    putStrLn $ "\nNumber of embedded files: " ++ show (length embeddedFiles)
    putStrLn "Embedded file paths:"
    mapM_ (putStrLn . ("  - " ++) . fst) embeddedFiles
    embeddedFiles `shouldSatisfy` (not . null)

  it "embeds at least one metaschema" $ do
    embeddedMetaschemaURIs `shouldSatisfy` (not . null)

  it "prints all embedded URIs for debugging" $ do
    putStrLn "\nEmbedded metaschema URIs:"
    mapM_ (putStrLn . ("  - " ++) . T.unpack) embeddedMetaschemaURIs
    True `shouldBe` True

  it "can look up 2020-12 core metaschema" $ do
    let uri = "https://json-schema.org/draft/2020-12/meta/core"
    lookupEmbeddedMetaschema uri `shouldSatisfy` maybe False (const True)

  it "can look up 2020-12 applicator metaschema" $ do
    let uri = "https://json-schema.org/draft/2020-12/meta/applicator"
    lookupEmbeddedMetaschema uri `shouldSatisfy` maybe False (const True)

  it "can look up 2019-09 core metaschema" $ do
    let uri = "https://json-schema.org/draft/2019-09/meta/core"
    lookupEmbeddedMetaschema uri `shouldSatisfy` maybe False (const True)

  it "can look up 2019-09 applicator metaschema" $ do
    let uri = "https://json-schema.org/draft/2019-09/meta/applicator"
    lookupEmbeddedMetaschema uri `shouldSatisfy` maybe False (const True)
