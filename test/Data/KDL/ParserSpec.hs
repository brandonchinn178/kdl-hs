{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.KDL.ParserSpec (spec) where

import Data.KDL.Parser
import Data.KDL.Types (
  Entry (..),
  Identifier (..),
  Node (..),
  NodeList (..),
  Value (..),
  ValueData (..),
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Skeletest
import Skeletest.Predicate qualified as P
import System.IO.Temp (withSystemTempDirectory)

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
