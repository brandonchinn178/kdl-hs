{-# LANGUAGE OverloadedStrings #-}

module KDL.DecoderSpec (spec) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import KDL qualified
import KDL.Types (Node)
import Skeletest
import Skeletest.Predicate qualified as P
import System.FilePath ((</>))

decodeErrorMsgSnapshot :: Maybe FilePath -> Predicate IO (Either KDL.DecodeError a)
decodeErrorMsgSnapshot mfile = P.left (KDL.renderDecodeError P.>>> sanitize P.>>> P.matchesSnapshot)
 where
  sanitize =
    case mfile of
      Nothing -> id
      Just file -> Text.replace (Text.pack file) "test_config.kdl"

spec :: Spec
spec = do
  describe "decodeWith" $ do
    it "fails with helpful error if parsing fails" $ do
      let config = "foo 123=123"
          decoder = KDL.document $ KDL.node @Node "foo"
      KDL.decodeWith decoder config `shouldSatisfy` decodeErrorMsgSnapshot Nothing

    it "fails with user-defined error" $ do
      let config = "foo -1"
          decoder =
            KDL.document . KDL.argAtWith "foo" $
              KDL.withDecoder KDL.number $ \x -> do
                when (x < 0) $ do
                  KDL.failM $ "Got negative number: " <> (Text.pack . show) x
                pure x
      KDL.decodeWith decoder config `shouldSatisfy` decodeErrorMsgSnapshot Nothing

    it "shows context in deeply nested error" $ do
      let config = "foo; foo { bar { baz; baz; baz; baz a=1; }; }"
          decoder =
            KDL.document
              . (KDL.many . KDL.nodeWith "foo" . KDL.children)
              . (KDL.many . KDL.nodeWith "bar" . KDL.children)
              . (KDL.many . KDL.nodeWith "baz")
              $ KDL.optional (KDL.prop @Text "a")
      KDL.decodeWith decoder config `shouldSatisfy` decodeErrorMsgSnapshot Nothing

  describe "decodeFileWith" $ do
    it "fails with helpful error if parsing fails" $ do
      FixtureKdlFile file <- getFixture
      writeFile file "foo 123=123"
      let decoder = KDL.document $ KDL.node @Node "foo"
      KDL.decodeFileWith decoder file `shouldSatisfy` P.returns (decodeErrorMsgSnapshot (Just file))

    it "fails with user-defined error" $ do
      FixtureKdlFile file <- getFixture
      writeFile file "foo -1"
      let decoder =
            KDL.document . KDL.argAtWith "foo" $
              KDL.withDecoder KDL.number $ \x -> do
                when (x < 0) $ do
                  KDL.failM $ "Got negative number: " <> (Text.pack . show) x
                pure x
      KDL.decodeFileWith decoder file `shouldSatisfy` P.returns (decodeErrorMsgSnapshot (Just file))

    it "shows context in deeply nested error" $ do
      FixtureKdlFile file <- getFixture
      writeFile file "foo; foo { bar { baz; baz; baz; baz a=1; }; }"
      let decoder =
            KDL.document
              . (KDL.many . KDL.nodeWith "foo" . KDL.children)
              . (KDL.many . KDL.nodeWith "bar" . KDL.children)
              . (KDL.many . KDL.nodeWith "baz")
              $ KDL.optional (KDL.prop @Text "a")
      KDL.decodeFileWith decoder file `shouldSatisfy` P.returns (decodeErrorMsgSnapshot (Just file))

newtype FixtureKdlFile = FixtureKdlFile FilePath

instance Fixture FixtureKdlFile where
  fixtureAction = do
    FixtureTmpDir tmpdir <- getFixture
    pure . noCleanup $ FixtureKdlFile (tmpdir </> "kdl-hs-test.kdl")
