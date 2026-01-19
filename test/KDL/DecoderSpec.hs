{-# LANGUAGE OverloadedStrings #-}

module KDL.DecoderSpec (spec) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import KDL qualified
import KDL.TestUtils.Error (decodeErrorMsg)
import KDL.Types (Node)
import Skeletest
import Skeletest.Predicate qualified as P
import System.FilePath ((</>))

spec :: Spec
spec = do
  describe "decodeWith" $ do
    it "fails with helpful error if parsing fails" $ do
      let config = "foo hello= 123"
          decoder = KDL.document $ KDL.node @Node "foo"
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "1:10:"
          , "  |"
          , "1 | foo hello= 123"
          , "  |          ^^"
          , "unexpected \"= \""
          , "expecting Node Child, Node Space, or Node Terminator"
          ]

    it "fails with user-defined error" $ do
      let config = "foo -1"
          decoder =
            KDL.document . KDL.argAtWith "foo" $
              KDL.withDecoder KDL.number $ \x -> do
                when (x < 0) $ do
                  KDL.failM $ "Got negative number: " <> (Text.pack . show) x
                pure x
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "At: foo #0 > arg #0"
          , "  Got negative number: -1.0"
          ]

    it "shows context in deeply nested error" $ do
      let config = "foo; foo { bar { baz; baz; baz; baz a=1; }; }"
          decoder =
            KDL.document
              . (KDL.many . KDL.nodeWith "foo" . KDL.children)
              . (KDL.many . KDL.nodeWith "bar" . KDL.children)
              . (KDL.many . KDL.nodeWith "baz")
              $ KDL.optional (KDL.prop @Text "a")
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "At: foo #1 > bar #0 > baz #3 > prop a"
          , "  Expected text, got: 1"
          ]

  describe "decodeFileWith" $ do
    it "fails with helpful error if parsing fails" $ do
      FixtureKdlFile file <- getFixture
      writeFile file "foo hello= 123"
      let decoder = KDL.document $ KDL.node @Node "foo"
      KDL.decodeFileWith decoder file
        `shouldSatisfy` (P.returns . decodeErrorMsg)
          [ "Failed to decode " <> Text.pack file <> ":"
          , "1:10:"
          , "  |"
          , "1 | foo hello= 123"
          , "  |          ^^"
          , "unexpected \"= \""
          , "expecting Node Child, Node Space, or Node Terminator"
          ]

    it "fails with user-defined error" $ do
      FixtureKdlFile file <- getFixture
      writeFile file "foo -1"
      let decoder =
            KDL.document . KDL.argAtWith "foo" $
              KDL.withDecoder KDL.number $ \x -> do
                when (x < 0) $ do
                  KDL.failM $ "Got negative number: " <> (Text.pack . show) x
                pure x
      KDL.decodeFileWith decoder file
        `shouldSatisfy` (P.returns . decodeErrorMsg)
          [ "Failed to decode " <> Text.pack file <> ":"
          , "At: foo #0 > arg #0"
          , "  Got negative number: -1.0"
          ]

    it "shows context in deeply nested error" $ do
      FixtureKdlFile file <- getFixture
      writeFile file "foo; foo { bar { baz; baz; baz; baz a=1; }; }"
      let decoder =
            KDL.document
              . (KDL.many . KDL.nodeWith "foo" . KDL.children)
              . (KDL.many . KDL.nodeWith "bar" . KDL.children)
              . (KDL.many . KDL.nodeWith "baz")
              $ KDL.optional (KDL.prop @Text "a")
      KDL.decodeFileWith decoder file
        `shouldSatisfy` (P.returns . decodeErrorMsg)
          [ "Failed to decode " <> Text.pack file <> ":"
          , "At: foo #1 > bar #0 > baz #3 > prop a"
          , "  Expected text, got: 1"
          ]

newtype FixtureKdlFile = FixtureKdlFile FilePath

instance Fixture FixtureKdlFile where
  fixtureAction = do
    FixtureTmpDir tmpdir <- getFixture
    pure . noCleanup $ FixtureKdlFile (tmpdir </> "kdl-hs-test.kdl")
