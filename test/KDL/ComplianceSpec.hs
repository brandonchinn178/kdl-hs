{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module KDL.ComplianceSpec (spec) where

import Control.Monad (forM_, unless)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import KDL qualified
import Skeletest
import System.Directory (
  createDirectoryIfMissing,
  findExecutable,
  getTemporaryDirectory,
  removePathForcibly,
 )
import System.FilePath (takeBaseName, takeFileName, (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callProcess, readProcess)

spec :: Spec
spec =
  describe "Compliance with upstream test cases" $ do
    describe "kdl-test" $ do
      it "passes" $ do
        decoder <- findExecutable "kdl-hs-test-decoder" >>= maybe (error "Could not find kdl-hs-test-decoder") pure
        callProcess "scripts/kdl-test" ["--decoder", decoder]

    forM_ (unsafePerformIO getComplianceFiles) $ \file -> do
      describe (takeFileName file) $ do
        let isFail = "_fail" `Text.isSuffixOf` (Text.pack . takeBaseName) file

        unless isFail . it "should round trip" $ do
          FixtureComplianceTests dir <- getFixture
          content <- Text.readFile (dir </> file)
          (fmap KDL.render . KDL.parse) content `shouldBe` Right content

getComplianceFiles :: IO [FilePath]
getComplianceFiles = lines <$> readProcess "tar" ["tzf", complianceTestArchive, "input/*.kdl"] ""

complianceTestArchive :: FilePath
complianceTestArchive = "test/compliance-tests/tests.tar.gz"

newtype FixtureComplianceTests = FixtureComplianceTests FilePath

instance Fixture FixtureComplianceTests where
  fixtureScope = PerSessionFixture
  fixtureAction = do
    -- Can't use FixtureTmpDir: https://github.com/brandonchinn178/skeletest/issues/FIXME
    tmpdir <- getTemporaryDirectory
    let dir = tmpdir </> "kdl-compliance"
    createDirectoryIfMissing True dir
    callProcess "tar" ["xzf", complianceTestArchive, "-C", dir]
    pure . withCleanup (FixtureComplianceTests dir) $ do
      removePathForcibly dir
