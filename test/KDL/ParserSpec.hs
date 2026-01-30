{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module KDL.ParserSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import KDL qualified
import KDL.Parser
import KDL.Types (
  Entry (..),
  Identifier (..),
  Node (..),
  NodeList (..),
  Value (..),
  ValueData (..),
 )
import Skeletest
import Skeletest.Predicate qualified as P
import System.Directory (findExecutable, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses a KDL document" $ do
      let expected =
            newNodeList
              [ newNode
                  "foo"
                  [ newArg $ Number 1.0
                  , newProp "hello" $ Text "world"
                  ]
                  ( Just
                      [ newNode "bar" [] (Just [])
                      ]
                  )
              ]
      parse "foo hello=world 1.0 { bar; }" `shouldBe` Right expected

    it "returns a textual error on parse failure" $ do
      let msg =
            Text.intercalate "\n" $
              [ "1:10:"
              , "  |"
              , "1 | foo hello= 123"
              , "  |          ^^"
              , "unexpected \"= \""
              , "expecting Node Child, Node Space, or Node Terminator"
              ]
      parse "foo hello= 123" `shouldBe` Left msg

  describe "parseFile" $ do
    it "parses a KDL document from a filepath" $ do
      withSystemTempDirectory "" $ \tmpdir -> do
        let file = tmpdir ++ "/test.kdl"
        writeFile file "foo hello=world 1.0 { bar; }"
        let expected =
              newNodeList
                [ newNode
                    "foo"
                    [ newArg $ Number 1.0
                    , newProp "hello" $ Text "world"
                    ]
                    ( Just
                        [ newNode "bar" [] (Just [])
                        ]
                    )
                ]
        parseFile file `shouldSatisfy` P.returns (P.right (P.eq expected))

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

{----- Helpers -----}

newNodeList :: [Node] -> NodeList
newNodeList nodes =
  NodeList
    { nodes = nodes
    , format = Nothing
    }

newNode :: Text -> [Entry] -> Maybe [Node] -> Node
newNode name entries children =
  Node
    { ann = Nothing
    , name = newIdentifier name
    , entries = entries
    , children = newNodeList <$> children
    , format = Nothing
    }

newArg :: ValueData -> Entry
newArg = newEntry Nothing

newProp :: Text -> ValueData -> Entry
newProp = newEntry . Just

newEntry :: Maybe Text -> ValueData -> Entry
newEntry mName data_ =
  Entry
    { name = newIdentifier <$> mName
    , value =
        Value
          { ann = Nothing
          , data_ = data_
          , format = Nothing
          }
    , format = Nothing
    }

newIdentifier :: Text -> Identifier
newIdentifier value =
  Identifier
    { value = value
    , format = Nothing
    }
