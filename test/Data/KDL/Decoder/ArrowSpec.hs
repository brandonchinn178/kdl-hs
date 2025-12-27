{-# LANGUAGE Arrows #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.KDL.Decoder.ArrowSpec (spec) where

import Control.Arrow (returnA)
import Data.KDL.Decoder.Arrow qualified as KDL
import Data.KDL.Types (
  Ann (..),
  BaseNode (..),
  BaseValue (..),
  Entry (..),
  Identifier (..),
  NodeList (..),
 )
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Skeletest
import Skeletest.Predicate qualified as P

decodeErrorEq :: [Text] -> Predicate IO (Either KDL.DecodeError a)
decodeErrorEq msgs = P.left (KDL.renderDecodeError P.>>> P.eq msg)
 where
  msg = Text.intercalate "\n" msgs

spec :: Spec
spec = do
  describe "NodeListDecoder" $ do
    describe "node" $ do
      it "decodes a node" $ do
        let config = "foo 123"
            decoder = KDL.document $ proc () -> do
              KDL.node "foo" -< ()
            expected =
              BaseNode
                { name = Identifier{value = "foo", format = Nothing}
                , entries =
                    [ Entry
                        { name = Nothing
                        , value = Ann{ann = Nothing, obj = Number 123, format = Nothing}
                        , format = Nothing
                        }
                    ]
                , children = Just NodeList{nodes = [], format = Nothing}
                , format = Nothing
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "decodes multiple nodes" $ do
        let config = "foo; foo"
            decoder = KDL.document $ proc () -> do
              KDL.many $ KDL.node "foo" -< ()
            expected = [fooNode, fooNode]
            fooNode =
              BaseNode
                { name = Identifier{value = "foo", format = Nothing}
                , entries = []
                , children = Just NodeList{nodes = [], format = Nothing}
                , format = Nothing
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "decodes nodes in any order" $ do
        let config = "foo; bar"
            decoder = KDL.document $ proc () -> do
              bar <- KDL.node "bar" -< ()
              foo <- KDL.node "foo" -< ()
              returnA -< (bar, foo)
            expected = (node "bar", node "foo")
            node name =
              BaseNode
                { name = Identifier{value = name, format = Nothing}
                , entries = []
                , children = Just NodeList{nodes = [], format = Nothing}
                , format = Nothing
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "fails when not enough nodes" $ do
        let config = "foo"
            decoder = KDL.document $ proc () -> do
              foo1 <- KDL.node @BaseNode "foo" -< ()
              foo2 <- KDL.node @BaseNode "foo" -< ()
              returnA -< (foo1, foo2)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorEq
            [ "At: <root>"
            , "  Expected another node: foo"
            ]

    describe "remainingNodes" $ do
      it "returns all remaining nodes" $ do
        let config = "foo 1; foo 2; bar"
            decoder = KDL.document $ proc () -> do
              _ <- KDL.node @BaseNode "foo" -< ()
              KDL.remainingNodes -< ()
            expected =
              Map.fromList
                [ ("foo", [fooNode2])
                , ("bar", [barNode])
                ]
            fooNode2 =
              BaseNode
                { name = Identifier{value = "foo", format = Nothing}
                , entries =
                    [ Entry
                        { name = Nothing
                        , value = Ann{ann = Nothing, obj = Number 2, format = Nothing}
                        , format = Nothing
                        }
                    ]
                , children = Just NodeList{nodes = [], format = Nothing}
                , format = Nothing
                }
            barNode =
              BaseNode
                { name = Identifier{value = "bar", format = Nothing}
                , entries = []
                , children = Just NodeList{nodes = [], format = Nothing}
                , format = Nothing
                }

        KDL.decodeWith decoder config `shouldBe` Right expected

    describe "argAt" $ do
      it "gets argument at a node" $ do
        let config = "foo \"bar\"; hello \"world\""
            decoder = KDL.document $ proc () -> do
              hello <- KDL.argAt "hello" -< ()
              foo <- KDL.argAt "foo" -< ()
              returnA -< (hello, foo)
        KDL.decodeWith decoder config `shouldBe` Right (Text.pack "world", Text.pack "bar")

      it "fails if no node" $ do
        pure ()

      it "fails if node has no args" $ do
        pure ()

    describe "argsAt" $ do
      pure ()

    describe "dashChildrenAt" $ do
      pure ()

    describe "dashNodesAt" $ do
      pure ()
