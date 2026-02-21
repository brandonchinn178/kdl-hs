{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module KDL.ParserSpec (spec) where

import Control.Monad (forM_)
import Data.Text.IO qualified as Text
import KDL qualified
import Skeletest
import Skeletest.Predicate qualified as P
import System.Directory (findExecutable, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

spec :: Spec
spec = do
  let exampleConfig = "foo hello=world 1.0 { bar; }"

  describe "parse" $ do
    it "parses a KDL document" $ do
      KDL.parse exampleConfig `shouldSatisfy` P.right P.matchesSnapshot

    describe "error messages" $ do
      let test msg input = do
            it msg $ KDL.parse input `shouldSatisfy` P.left P.matchesSnapshot

      -- TODO: add more
      test "Unquoted numeric prop name" "foo 123=123"

  describe "parseWith" $ do
    it "parses a KDL document with spans" $ do
      KDL.parseWith KDL.def{KDL.includeSpans = True} "foo 1 2 {\n  bar 3\n}"
        `shouldSatisfy` P.right P.matchesSnapshot

  -- Most behavior tested in `parse` tests
  describe "parseFile" $ do
    it "parses a KDL document from a filepath" $ do
      withSystemTempDirectory "" $ \tmpdir -> do
        let file = tmpdir ++ "/test.kdl"
        Text.writeFile file exampleConfig
        Right actual <- KDL.parseFile file
        Right expected <- pure $ KDL.parse exampleConfig
        -- tested in `parse`
        actual `shouldBe` expected

  describe "kdl-test examples" $ do
    it "decodes correctly" $ do
      decoder <- findExecutable "kdl-hs-test-decoder" >>= maybe (error "Could not find kdl-hs-test-decoder") pure
      callProcess "scripts/kdl-test" ["run", "--decoder", decoder]

    it "roundtrips successfully" $ do
      FixtureTmpDir tmpdir <- getFixture
      let dir = tmpdir </> "kdl-examples"
      callProcess "scripts/kdl-test" ["extract", "--dir", dir]
      files <- filter ((== ".kdl") . takeExtension) <$> listDirectory (dir </> "valid")
      forM_ files $ \file -> do
        context file $ do
          content <- Text.readFile (dir </> "valid" </> file)
          (fmap KDL.render . KDL.parse) content `shouldBe` Right content
