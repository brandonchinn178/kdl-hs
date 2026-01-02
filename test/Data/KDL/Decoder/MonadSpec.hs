{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.KDL.Decoder.MonadSpec (spec) where

import Control.Monad (forM_, unless, when)
import Data.KDL qualified as KDL
import Data.KDL.Types (
  Entry (..),
  Identifier (..),
  Node (..),
  NodeList (..),
  Value (..),
  ValueData (..),
 )
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Skeletest
import Skeletest.Predicate qualified as P

decodeErrorMsg :: [Text] -> Predicate IO (Either KDL.DecodeError a)
decodeErrorMsg msgs = P.left (KDL.renderDecodeError P.>>> P.eq msg)
 where
  msg = Text.intercalate "\n" msgs

spec :: Spec
spec = do
  apiSpec
  decodeNodeSpec
  decodeValueSpec

apiSpec :: Spec
apiSpec = do
  describe "decodeWith" $ do
    it "fails with helpful error if parsing fails" $ do
      let config = "foo hello= 123"
          decoder = KDL.document $ KDL.node @Node "foo"
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "At: <root>"
          , "  1:10:"
          , "    |"
          , "  1 | foo hello= 123"
          , "    |          ^^"
          , "  unexpected \"= \""
          , "  expecting Node Child, Node Space, or Node Terminator"
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

  describe "NodeListDecoder" $ do
    describe "node" $ do
      it "decodes a node" $ do
        let config = "foo 1.0"
            decoder = KDL.document $ do
              KDL.node "foo"
            expected =
              Node
                { ann = Nothing
                , name = Identifier{value = "foo", format = Nothing}
                , entries =
                    [ Entry
                        { name = Nothing
                        , value = Value{ann = Nothing, data_ = Number 1.0, format = Nothing}
                        , format = Nothing
                        }
                    ]
                , children = Just NodeList{nodes = [], format = Nothing}
                , format = Nothing
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "decodes multiple nodes" $ do
        let config = "foo; foo"
            decoder = KDL.document $ do
              KDL.many $ KDL.node "foo"
            expected = [fooNode, fooNode]
            fooNode =
              Node
                { ann = Nothing
                , name = Identifier{value = "foo", format = Nothing}
                , entries = []
                , children = Just NodeList{nodes = [], format = Nothing}
                , format = Nothing
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "decodes nodes in any order" $ do
        let config = "foo; bar"
            decoder = KDL.document $ do
              bar <- KDL.node "bar"
              foo <- KDL.node "foo"
              pure (bar, foo)
            expected = (node "bar", node "foo")
            node name =
              Node
                { ann = Nothing
                , name = Identifier{value = name, format = Nothing}
                , entries = []
                , children = Just NodeList{nodes = [], format = Nothing}
                , format = Nothing
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "fails when not enough nodes" $ do
        let config = "foo"
            decoder = KDL.document $ do
              foo1 <- KDL.node @Node "foo"
              foo2 <- KDL.node @Node "foo"
              pure (foo1, foo2)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: <root>"
            , "  Expected another node: foo"
            ]

    -- Most behaviors tested with `node`
    describe "nodeWith" $ do
      it "decodes a node" $ do
        let config = "foo 1.0 { hello \"world\"; }"
            decodeFoo = do
              arg <- KDL.arg @Int
              child <- KDL.children $ KDL.argAt @Text "hello"
              pure (arg, child)
            decoder = KDL.document $ do
              KDL.nodeWith "foo" decodeFoo
        KDL.decodeWith decoder config `shouldBe` Right (1, "world")

      it "fails when node fails to parse" $ do
        let config = "foo 1.0"
            decodeFoo = do
              KDL.arg @Text
            decoder = KDL.document $ do
              KDL.nodeWith "foo" decodeFoo
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected text, got: 1.0"
            ]

    -- Most behaviors tested with `nodeWith`
    describe "nodeWith'" $ do
      it "decodes a node with an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "(test)foo 1"
              decoder = KDL.document $ do
                KDL.nodeWith' "foo" anns $ KDL.arg @Int
          KDL.decodeWith decoder config `shouldBe` Right 1

      it "decodes a node without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo 1"
              decoder = KDL.document $ do
                KDL.nodeWith' "foo" anns $ KDL.arg @Int
          KDL.decodeWith decoder config `shouldBe` Right 1

      it "fails when node has unexpected annotation" $ do
        let config = "(test)foo 2"
            decoder = KDL.document $ do
              KDL.nodeWith' "foo" ["FOO"] $ KDL.arg @Int
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Expected annotation to be one of [\"FOO\"], got: test"
            ]

    describe "remainingNodes" $ do
      it "returns all remaining nodes" $ do
        let config = "foo 1.0; foo 2.0; bar"
            decoder = KDL.document $ do
              _ <- KDL.node @Node "foo"
              KDL.remainingNodes
            expected =
              Map.fromList
                [ ("foo", [fooNode2])
                , ("bar", [barNode])
                ]
            fooNode2 =
              Node
                { ann = Nothing
                , name = Identifier{value = "foo", format = Nothing}
                , entries =
                    [ Entry
                        { name = Nothing
                        , value = Value{ann = Nothing, data_ = Number 2.0, format = Nothing}
                        , format = Nothing
                        }
                    ]
                , children = Just NodeList{nodes = [], format = Nothing}
                , format = Nothing
                }
            barNode =
              Node
                { ann = Nothing
                , name = Identifier{value = "bar", format = Nothing}
                , entries = []
                , children = Just NodeList{nodes = [], format = Nothing}
                , format = Nothing
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

    -- Most behaviors tested with `remainingNodes`
    describe "remainingNodesWith" $ do
      it "returns all remaining nodes" $ do
        let config = "foo 1.0; foo 2.0; bar"
            decodeNode = do
              KDL.optional $ KDL.arg @Int
            decoder = KDL.document $ do
              _ <- KDL.node @Node "foo"
              KDL.remainingNodesWith decodeNode
            expected =
              Map.fromList
                [ ("foo", [Just 2])
                , ("bar", [Nothing])
                ]
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "fails when node fails to parse" $ do
        let config = "foo 1; bar 1; bar \"hello\""
            decodeNode = do
              KDL.arg @Int
            decoder = KDL.document $ do
              KDL.remainingNodesWith decodeNode
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: bar #1 > arg #0"
            , "  Expected number, got: hello"
            ]

    -- Most behaviors tested with `remainingNodesWith`
    describe "remainingNodesWith'" $ do
      it "decodes a node with an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "(test)foo 1"
              decoder = KDL.document $ do
                KDL.remainingNodesWith' anns $ KDL.arg @Int
          KDL.decodeWith decoder config
            `shouldBe` (Right . Map.fromList) [("foo", [1])]

      it "decodes a node without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo 1"
              decoder = KDL.document $ do
                KDL.remainingNodesWith' anns $ KDL.arg @Int
          KDL.decodeWith decoder config
            `shouldBe` (Right . Map.fromList) [("foo", [1])]

      it "fails when node has unexpected annotation" $ do
        let config = "(FOO)foo 1; (test)foo 2"
            decoder = KDL.document $ do
              KDL.remainingNodesWith' ["FOO"] $ KDL.arg @Int
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #1"
            , "  Expected annotation to be one of [\"FOO\"], got: test"
            ]

    describe "argAt" $ do
      it "gets argument at a node" $ do
        let config = "foo \"bar\"; hello \"world\""
            decoder = KDL.document $ do
              hello <- KDL.argAt @Text "hello"
              foo <- KDL.argAt @Text "foo"
              pure (hello, foo)
        KDL.decodeWith decoder config `shouldBe` Right ("world", "bar")

      it "fails if no node" $ do
        let config = "other_node"
            decoder = KDL.document $ do
              KDL.argAt @Int "foo"
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: <root>"
            , "  Expected node: foo"
            ]

      it "fails if node has no args" $ do
        let config = "foo"
            decoder = KDL.document $ do
              KDL.argAt @Int "foo"
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Expected arg #0"
            ]

      it "fails if arg fails to parse" $ do
        let config = "foo 1"
            decoder = KDL.document $ do
              KDL.argAt @Text "foo"
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected text, got: 1"
            ]

    -- Most behaviors tested with `argAt`
    describe "argAtWith" $ do
      it "gets argument at a node" $ do
        let config = "foo 1"
            decoder = KDL.document $ do
              KDL.argAtWith "foo" $ show . (* 10) <$> KDL.number
        KDL.decodeWith decoder config `shouldBe` Right "10.0"

    -- Most behaviors tested with `argAtWith`
    describe "argAtWith'" $ do
      it "decodes argument with an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo (test)a"
              decoder = KDL.document $ do
                KDL.argAtWith' "foo" anns KDL.text
          KDL.decodeWith decoder config `shouldBe` Right "a"

      it "decodes argument without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo a"
              decoder = KDL.document $ do
                KDL.argAtWith' "foo" anns KDL.text
          KDL.decodeWith decoder config `shouldBe` Right "a"

      it "fails when argument has unexpected annotation" $ do
        let config = "foo (test)a"
            decoder = KDL.document $ do
              KDL.argAtWith' "foo" ["VAL"] KDL.text
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

    describe "argsAt" $ do
      it "gets arguments at a node" $ do
        let config = "foo 1 2 3"
            decoder = KDL.document $ do
              KDL.argsAt @Int "foo"
        KDL.decodeWith decoder config `shouldBe` Right [1, 2, 3]

      it "returns empty list if no node" $ do
        let config = ""
            decoder = KDL.document $ do
              KDL.argsAt @Int "foo"
        KDL.decodeWith decoder config `shouldBe` Right []

      it "returns empty list if node has no args" $ do
        let config = "foo"
            decoder = KDL.document $ do
              KDL.argsAt @Int "foo"
        KDL.decodeWith decoder config `shouldBe` Right []

      it "fails if any arg fails to parse" $ do
        let config = "foo 1 \"asdf\""
            decoder = KDL.document $ do
              KDL.argsAt @Int "foo"
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #1"
            , "  Expected number, got: asdf"
            ]

    -- Most behaviors tested with `argsAt`
    describe "argsAtWith" $ do
      it "gets arguments at a node" $ do
        let config = "foo 1 2"
            decoder = KDL.document $ do
              KDL.argsAtWith "foo" $ show . (* 10) <$> KDL.number
        KDL.decodeWith decoder config `shouldBe` Right ["10.0", "20.0"]

    -- Most behaviors tested with `argsAtWith`
    describe "argsAtWith'" $ do
      it "decodes arguments with an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo (test)a (test)b"
              decoder = KDL.document $ do
                KDL.argsAtWith' "foo" anns KDL.text
          KDL.decodeWith decoder config `shouldBe` Right ["a", "b"]

      it "decodes arguments without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo a b"
              decoder = KDL.document $ do
                KDL.argsAtWith' "foo" anns KDL.text
          KDL.decodeWith decoder config `shouldBe` Right ["a", "b"]

      it "fails when argument has unexpected annotation" $ do
        let config = "foo (VAL)a (test)b"
            decoder = KDL.document $ do
              KDL.argsAtWith' "foo" ["VAL"] KDL.text
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #1"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

    describe "dashChildrenAt" $ do
      it "gets dash children at a node" $ do
        let config = "foo { - 1; - 2; - 3; }"
            decoder = KDL.document $ do
              KDL.dashChildrenAt @Int "foo"
        KDL.decodeWith decoder config `shouldBe` Right [1, 2, 3]

      it "returns empty list if no node" $ do
        let config = ""
            decoder = KDL.document $ do
              KDL.dashChildrenAt @Int "foo"
        KDL.decodeWith decoder config `shouldBe` Right []

      it "returns empty list if node has no dash children" $ do
        forM_ ["foo", "foo {}"] $ \config -> do
          let decoder = KDL.document $ do
                KDL.dashChildrenAt @Int "foo"
          KDL.decodeWith decoder config `shouldBe` Right []

      it "fails if dash children have multiple args" $ do
        let config = "foo { - 1 2; - 3 4; }"
            decoder = KDL.document $ do
              KDL.dashChildrenAt @Int "foo"
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > - #0"
            , "  Unexpected arg #1: 2"
            ]

      it "fails if node has non-dash children" $ do
        let config = "foo { - 1; bar 1 2 3; }"
            decoder = KDL.document $ do
              KDL.dashChildrenAt @Int "foo"
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Unexpected node: bar #0"
            ]

      it "fails if any child fails to parse" $ do
        let config = "foo { - 1; - \"asdf\"; }"
            decoder = KDL.document $ do
              KDL.dashChildrenAt @Int "foo"
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > - #1 > arg #0"
            , "  Expected number, got: asdf"
            ]

    -- Most behaviors tested with `dashChildrenAt`
    describe "dashChildrenAtWith" $ do
      it "gets dash children at a node" $ do
        let config = "foo { - 1; - 2; }"
            decoder = KDL.document $ do
              KDL.dashChildrenAtWith "foo" $ show . (* 10) <$> KDL.number
        KDL.decodeWith decoder config `shouldBe` Right ["10.0", "20.0"]

    -- Most behaviors tested with `dashChildrenAtWith`
    describe "dashChildrenAtWith'" $ do
      it "decodes dash children with an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo { - (test)a; - (test)b; }"
              decoder = KDL.document $ do
                KDL.dashChildrenAtWith' "foo" anns KDL.text
          KDL.decodeWith decoder config `shouldBe` Right ["a", "b"]

      it "decodes dash children without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo { - a; - b; }"
              decoder = KDL.document $ do
                KDL.dashChildrenAtWith' "foo" anns KDL.text
          KDL.decodeWith decoder config `shouldBe` Right ["a", "b"]

      it "fails when child has unexpected annotation" $ do
        let config = "foo { - (test)a; }"
            decoder = KDL.document $ do
              KDL.dashChildrenAtWith' "foo" ["VAL"] KDL.text
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > - #0 > arg #0"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

    describe "dashNodesAt" $ do
      it "gets dash nodes at a node" $ do
        let config = "foo { - { bar; }; - { baz; }; }"
            decoder = KDL.document $ do
              KDL.dashNodesAt "foo"
            expected = [node "-" [node "bar" []], node "-" [node "baz" []]]
            node name children =
              Node
                { ann = Nothing
                , name = Identifier{value = name, format = Nothing}
                , entries = []
                , children = Just NodeList{nodes = children, format = Nothing}
                , format = Nothing
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "returns empty list if no node" $ do
        let config = ""
            decoder = KDL.document $ do
              KDL.dashNodesAt @Node "foo"
        KDL.decodeWith decoder config `shouldBe` Right []

      it "returns empty list if node has no dash nodes" $ do
        forM_ ["foo", "foo {}"] $ \config -> do
          let decoder = KDL.document $ do
                KDL.dashNodesAt @Node "foo"
          KDL.decodeWith decoder config `shouldBe` Right []

      it "fails if node has non-dash nodes" $ do
        let config = "foo { - 1; bar 1 2 3; }"
            decoder = KDL.document $ do
              KDL.dashNodesAt @Node "foo"
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Unexpected node: bar #0"
            ]

    -- Most behaviors tested with `dashNodesAt`
    describe "dashNodesAtWith" $ do
      it "gets dash nodes at a node" $ do
        let config = "foo { - 1 { bar \"hello\"; }; - 2 { bar \"world\"; }; }"
            decodeChild = do
              arg <- KDL.arg @Int
              child <- KDL.children $ KDL.nodeWith "bar" $ KDL.arg @Text
              pure (arg, child)
            decoder = KDL.document $ do
              KDL.dashNodesAtWith "foo" decodeChild
        KDL.decodeWith decoder config `shouldBe` Right [(1, "hello"), (2, "world")]

      it "fails if any child fails to parse" $ do
        let config = "foo { - { bar 1; }; - { bar \"test\"; }; }"
            decoder = KDL.document $ do
              KDL.dashNodesAtWith "foo" $ KDL.children $ KDL.argAt @Int "bar"
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > - #1 > bar #0 > arg #0"
            , "  Expected number, got: test"
            ]

  describe "NodeDecoder" $ do
    let decodeNode name decoder config =
          KDL.decodeWith
            ( KDL.document $ do
                KDL.nodeWith name decoder
            )
            config

    describe "arg" $ do
      it "decodes an argument" $ do
        let config = "foo 1 \"bar\""
            decoder = do
              arg1 <- KDL.arg @Int
              arg2 <- KDL.arg @Text
              pure (arg1, arg2)
        decodeNode "foo" decoder config `shouldBe` Right (1, "bar")

      it "decodes multiple arguments" $ do
        let config = "foo 1 2 3"
            decoder = do
              KDL.many $ KDL.arg @Int
        decodeNode "foo" decoder config `shouldBe` Right [1, 2, 3]

      it "fails if argument doesn't exist" $ do
        let config = "foo"
            decoder = do
              KDL.arg @Int
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Expected arg #0"
            ]

      it "fails if argument fails to parse" $ do
        let config = "foo \"test\""
            decoder = do
              KDL.arg @Int
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected number, got: test"
            ]

      it "fails if not all arguments are decoded" $ do
        let config = "foo 1 2 3"
            decoder = do
              KDL.arg @Int
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Unexpected arg #1: 2"
            ]

    -- Most behaviors tested with `arg`
    describe "argWith" $ do
      it "decodes an argument" $ do
        let config = "foo \"bar\""
            decoder = do
              KDL.argWith KDL.text
        decodeNode "foo" decoder config `shouldBe` Right "bar"

    -- Most behaviors tested with `argWith`
    describe "argWith'" $ do
      it "decodes argument with an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo (test)a"
              decoder = do
                KDL.argWith' anns KDL.text
          decodeNode "foo" decoder config `shouldBe` Right "a"

      it "decodes argument without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo a"
              decoder = do
                KDL.argWith' anns KDL.text
          decodeNode "foo" decoder config `shouldBe` Right "a"

      it "fails when argument has unexpected annotation" $ do
        let config = "foo (test)a"
            decoder = do
              KDL.argWith' ["VAL"] KDL.text
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

    describe "prop" $ do
      it "decodes a prop" $ do
        let config = "foo test1=1 test2=\"hello\""
            decoder = do
              prop1 <- KDL.prop @Text "test2"
              prop2 <- KDL.prop @Int "test1"
              pure (prop1, prop2)
        decodeNode "foo" decoder config `shouldBe` Right ("hello", 1)

      it "can optionally decode a prop" $ do
        let config = "foo a=1"
            decoder = do
              a <- KDL.optional $ KDL.prop @Int "a"
              b <- KDL.optional $ KDL.prop @Int "b"
              pure (a, b)
        decodeNode "foo" decoder config `shouldBe` Right (Just 1, Nothing)

      it "decodes last prop" $ do
        let config = "foo test=1 test=2"
            decoder = do
              KDL.prop @Int "test"
        decodeNode "foo" decoder config `shouldBe` Right 2

      it "fails if prop doesn't exist" $ do
        let config = "foo 123"
            decoder = do
              KDL.prop @Int "test"
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Expected prop: test"
            ]

      it "fails if prop fails to parse" $ do
        let config = "foo hello=world"
            decoder = do
              KDL.prop @Int "hello"
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > prop hello"
            , "  Expected number, got: world"
            ]

      it "fails if not all props are decoded" $ do
        let config = "foo a=1 b=2"
            decoder = do
              KDL.prop @Int "a"
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Unexpected prop: b=2"
            ]

    -- Most behaviors tested with `prop`
    describe "propWith" $ do
      it "decodes a prop" $ do
        let config = "foo a=1"
            decoder = do
              KDL.propWith "a" KDL.number
        decodeNode "foo" decoder config `shouldBe` Right 1

    -- Most behaviors tested with `propWith`
    describe "propWith'" $ do
      it "decodes prop with an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo a=(test)1"
              decoder = do
                KDL.propWith' "a" anns KDL.number
          decodeNode "foo" decoder config `shouldBe` Right 1

      it "decodes prop without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo a=1"
              decoder = do
                KDL.propWith' "a" anns KDL.number
          decodeNode "foo" decoder config `shouldBe` Right 1

      it "fails when prop has unexpected annotation" $ do
        let config = "foo a=(test)1"
            decoder = do
              KDL.propWith' "a" ["VAL"] KDL.number
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > prop a"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

    describe "remainingProps" $ do
      it "decodes remaining props" $ do
        let config = "foo a=1 b=2 c=3 b=4"
            decoder = do
              _ <- KDL.prop @Int "a"
              KDL.remainingProps @Int
        decodeNode "foo" decoder config
          `shouldBe` (Right . Map.fromList) [("b", 4), ("c", 3)]

      it "returns empty map if no props left" $ do
        let config = "foo a=1"
            decoder = do
              _ <- KDL.prop @Int "a"
              KDL.remainingProps @Int
        decodeNode "foo" decoder config `shouldBe` Right Map.empty

      it "fails if prop fails to parse" $ do
        let config = "foo a=1 b=1 c=2 c=test"
            decoder = do
              _ <- KDL.prop @Int "a"
              KDL.remainingProps @Int
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > prop c"
            , "  Expected number, got: test"
            ]

    -- Most behaviors tested with `remainingProps`
    describe "remainingPropsWith" $ do
      it "decodes remaining props" $ do
        let config = "foo a=1 b=2 c=3 b=4"
            decoder = do
              _ <- KDL.prop @Int "a"
              KDL.remainingPropsWith KDL.number
        decodeNode "foo" decoder config
          `shouldBe` (Right . Map.fromList) [("b", 4), ("c", 3)]

    -- Most behaviors tested with `remainingPropsWith`
    describe "remainingPropsWith'" $ do
      it "decodes props with an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo a=(test)1 b=(test)2"
              decoder = do
                KDL.remainingPropsWith' anns KDL.number
          decodeNode "foo" decoder config
            `shouldBe` (Right . Map.fromList) [("a", 1), ("b", 2)]

      it "decodes props without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo a=1 b=2"
              decoder = do
                KDL.remainingPropsWith' anns KDL.number
          decodeNode "foo" decoder config
            `shouldBe` (Right . Map.fromList) [("a", 1), ("b", 2)]

      it "fails when prop has unexpected annotation" $ do
        let config = "foo a=(VAL)1 b=(test)2"
            decoder = do
              KDL.remainingPropsWith' ["VAL"] KDL.number
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > prop b"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

    describe "children" $ do
      it "decodes children" $ do
        let config = "foo { bar test; }"
            decoder = do
              KDL.children $ KDL.node @Node "bar"
            expected =
              Node
                { ann = Nothing
                , name = Identifier{value = "bar", format = Nothing}
                , entries =
                    [ Entry
                        { name = Nothing
                        , value = Value{ann = Nothing, data_ = Text "test", format = Nothing}
                        , format = Nothing
                        }
                    ]
                , children = Just NodeList{nodes = [], format = Nothing}
                , format = Nothing
                }
        decodeNode "foo" decoder config `shouldBe` Right expected

      it "can be re-entered" $ do
        let config = "foo { bar a; baz b; }"
            decoder = do
              arg1 <- KDL.children $ KDL.argAt @Text "bar"
              arg2 <- KDL.children $ KDL.argAt @Text "baz"
              pure (arg1, arg2)
        decodeNode "foo" decoder config `shouldBe` Right ("a", "b")

      it "fails if not all children are decoded" $ do
        let config = "foo { asdf; bar; }"
            decoder = do
              KDL.children $ KDL.node @Node "bar"
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Unexpected node: asdf #0"
            ]

  describe "ValueDecoder" $ do
    describe "any" $ do
      it "decodes any value" $ do
        let config = "foo 1.0 asdf true"
            decoder = KDL.document $ do
              KDL.argsAtWith "foo" KDL.any
            val data_ =
              Value
                { ann = Nothing
                , data_ = data_
                , format = Nothing
                }
        KDL.decodeWith decoder config
          `shouldBe` Right [val $ Number 1, val $ Text "asdf", val $ Bool True]

    describe "text" $ do
      it "decodes text value" $ do
        let config = "foo asdf"
            decoder = KDL.document $ do
              KDL.argAtWith "foo" KDL.text
        KDL.decodeWith decoder config `shouldBe` Right "asdf"

      it "fails when value is not text" $ do
        let config = "foo 1"
            decoder = KDL.document $ do
              KDL.argAtWith "foo" KDL.text
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected text, got: 1"
            ]

    describe "number" $ do
      it "decodes number value" $ do
        let config = "foo 1"
            decoder = KDL.document $ do
              KDL.argAtWith "foo" KDL.number
        KDL.decodeWith decoder config `shouldBe` Right 1

      it "fails when value is not number" $ do
        let config = "foo asdf"
            decoder = KDL.document $ do
              KDL.argAtWith "foo" KDL.number
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected number, got: asdf"
            ]

    describe "bool" $ do
      it "decodes bool value" $ do
        let config = "foo true"
            decoder = KDL.document $ do
              KDL.argAtWith "foo" KDL.bool
        KDL.decodeWith decoder config `shouldBe` Right True

      it "fails when value is not bool" $ do
        let config = "foo 1"
            decoder = KDL.document $ do
              KDL.argAtWith "foo" KDL.bool
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected bool, got: 1"
            ]

    describe "null" $ do
      it "decodes null value" $ do
        let config = "foo null"
            decoder = KDL.document $ do
              KDL.argAtWith "foo" KDL.null
        KDL.decodeWith decoder config `shouldBe` Right ()

      it "fails when value is not null" $ do
        let config = "foo 1"
            decoder = KDL.document $ do
              KDL.argAtWith "foo" KDL.null
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected null, got: 1"
            ]

  describe "Combinators" $ do
    describe "oneOf" $ do
      it "decodes one of the options" $ do
        let config = "foo 123 hello"
            decoder = KDL.document $ do
              KDL.nodeWith "foo" . KDL.many . KDL.argWith $
                KDL.oneOf [Left <$> KDL.number, Right <$> KDL.text]
        KDL.decodeWith decoder config `shouldBe` Right [Left 123, Right "hello"]

      it "fails if none can be decoded" $ do
        let config = "foo 123 hello"
            decoder = KDL.document $ do
              KDL.nodeWith "foo" . KDL.many . KDL.argWith $
                KDL.oneOf [Left <$> KDL.number, Right <$> KDL.bool]
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #1"
            , "  Expected bool, got: hello"
            , "  Expected number, got: hello"
            ]

    describe "option" $ do
      it "defaults to the given value" $ do
        let config = "foo"
            decoder = KDL.document $ do
              KDL.nodeWith "foo" $ KDL.option 123 $ KDL.arg @Int
        KDL.decodeWith decoder config `shouldBe` Right 123

newtype MyNode = MyNode Int
  deriving (Eq)

instance KDL.DecodeNode MyNode where
  validNodeTypeAnns _ = ["MyNode"]
  nodeDecoder = KDL.noSchema $ do
    x <- KDL.arg
    unless (0 < x && x < 10) $ do
      KDL.fail $ "Invalid argument: " <> (Text.pack . show) x
    pure $ MyNode x

decodeNodeSpec :: Spec
decodeNodeSpec = do
  describe "DecodeNode" $ do
    it "decodes a custom node" $ do
      let config = "foo 1"
          decoder = KDL.document $ do
            KDL.node "foo"
      KDL.decodeWith decoder config `shouldBe` Right (MyNode 1)

    it "throws user-specified error" $ do
      let config = "foo 100"
          decoder = KDL.document $ do
            KDL.node @MyNode "foo"
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "At: foo #0"
          , "  Invalid argument: 100"
          ]

    it "decodes valid type ann" $ do
      let config = "(MyNode)foo 1"
          decoder = KDL.document $ do
            KDL.node "foo"
      KDL.decodeWith decoder config `shouldBe` Right (MyNode 1)

    it "fails on invalid type ann" $ do
      let config = "(bad)foo 1"
          decoder = KDL.document $ do
            KDL.node @MyNode "foo"
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "At: foo #0"
          , "  Expected annotation to be one of [\"MyNode\"], got: bad"
          ]

newtype MyVal = MyVal Double
  deriving (Eq)

instance KDL.DecodeValue MyVal where
  validValueTypeAnns _ = ["MyVal"]
  valueDecoder = KDL.noSchema . KDL.withDecoder KDL.number $ \x -> do
    unless (0 < x && x < 10) $ do
      KDL.failM $ "Invalid value: " <> (Text.pack . show) x
    pure $ MyVal (realToFrac x)

decodeValueSpec :: Spec
decodeValueSpec = do
  describe "DecodeValue" $ do
    it "decodes a custom value" $ do
      let config = "foo 1"
          decoder = KDL.document $ do
            KDL.argAt "foo"
      KDL.decodeWith decoder config `shouldBe` Right (MyVal 1)

    it "throws user-specified error" $ do
      let config = "foo 100.0"
          decoder = KDL.document $ do
            KDL.argAt @MyVal "foo"
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "At: foo #0 > arg #0"
          , "  Invalid value: 100.0"
          ]

    it "decodes valid type ann" $ do
      let config = "foo (MyVal)1"
          decoder = KDL.document $ do
            KDL.argAt "foo"
      KDL.decodeWith decoder config `shouldBe` Right (MyVal 1)

    it "fails on invalid type ann" $ do
      let config = "foo (bad)1"
          decoder = KDL.document $ do
            KDL.argAt @MyVal "foo"
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "At: foo #0 > arg #0"
          , "  Expected annotation to be one of [\"MyVal\"], got: bad"
          ]
