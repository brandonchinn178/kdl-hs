{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- Apostrophes confuse CPP macros
#define _APOS_(x) x'

#ifdef IS_MONAD
#define _DO_ do
#define _RETURN_(x) pure x
#define _STMT_(x) x
#endif

#ifdef IS_ARROW
#define _DO_ proc () -> do
#define _RETURN_(x) returnA -< x
#define _STMT_(x) x -< ()
#endif

#ifdef IS_ARROW
{-# LANGUAGE Arrows #-}
#endif

module KDL.Decoder.SharedSpec.MODULE_NAME (
  apiSpec,
  decodeNodeSpec,
  decodeValueSpec,
) where

import Control.Monad (forM_, unless)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import KDL.TestUtils.AST (scrubFormat)
import KDL.TestUtils.Error (decodeErrorMsg)
import KDL.Types (
  Entry (..),
  Identifier (..),
  Node (..),
  NodeList (..),
  Value (..),
  ValueData (..),
 )
import Skeletest

#ifdef IS_MONAD
import KDL qualified
#endif

#ifdef IS_ARROW
import Control.Arrow (returnA)
import KDL.Arrow qualified as KDL
#endif

apiSpec :: Spec
apiSpec = do
  describe "NodeListDecoder" $ do
    describe "node" $ do
      it "decodes a node" $ do
        let config = "foo 1.0"
            decoder = KDL.document $ _DO_
              _STMT_(scrubFormat <$> KDL.node "foo")
            expected =
              Node
                { ann = Nothing
                , name = Identifier{value = "foo", ext = KDL.def}
                , entries =
                    [ Entry
                        { name = Nothing
                        , value = Value{ann = Nothing, data_ = Number 1.0, ext = KDL.def}
                        , ext = KDL.def
                        }
                    ]
                , children = Nothing
                , ext = KDL.def
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "decodes multiple nodes" $ do
        let config = "foo; foo"
            decoder = KDL.document $ _DO_
              _STMT_(fmap (map scrubFormat) . KDL.many $ KDL.node "foo")
            expected = [fooNode, fooNode]
            fooNode =
              Node
                { ann = Nothing
                , name = Identifier{value = "foo", ext = KDL.def}
                , entries = []
                , children = Nothing
                , ext = KDL.def
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "decodes nodes in any order" $ do
        let config = "foo; bar"
            decoder = KDL.document $ _DO_
              bar <- _STMT_(scrubFormat <$> KDL.node "bar")
              foo <- _STMT_(scrubFormat <$> KDL.node "foo")
              _RETURN_((bar, foo))
            expected = (node "bar", node "foo")
            node name =
              Node
                { ann = Nothing
                , name = Identifier{value = name, ext = KDL.def}
                , entries = []
                , children = Nothing
                , ext = KDL.def
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "fails when not enough nodes" $ do
        let config = "foo"
            decoder = KDL.document $ _DO_
              foo1 <- _STMT_(scrubFormat <$> KDL.node @Node "foo")
              foo2 <- _STMT_(scrubFormat <$> KDL.node @Node "foo")
              _RETURN_((foo1, foo2))
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: <root>"
            , "  Expected another node: foo"
            ]

    -- Most behaviors tested with `node`
    describe "nodeWith" $ do
      it "decodes a node" $ do
        let config = "foo 1.0 { hello world; }"
            decodeFoo = _DO_
              arg <- _STMT_(KDL.arg @Int)
              child <- _STMT_(KDL.children $ KDL.argAt @Text "hello")
              _RETURN_((arg, child))
            decoder = KDL.document $ _DO_
              _STMT_(KDL.nodeWith "foo" decodeFoo)
        KDL.decodeWith decoder config `shouldBe` Right (1, "world")

      it "fails when node fails to parse" $ do
        let config = "foo 1.0"
            decodeFoo = _DO_
              _STMT_(KDL.arg @Text)
            decoder = KDL.document $ _DO_
              _STMT_(KDL.nodeWith "foo" decodeFoo)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected string, got: 1.0"
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
              decoder = KDL.document $ _DO_
                _STMT_(_APOS_(KDL.nodeWith) "foo" anns $ KDL.arg @Int)
          KDL.decodeWith decoder config `shouldBe` Right 1

      it "decodes a node without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo 1"
              decoder = KDL.document $ _DO_
                _STMT_(_APOS_(KDL.nodeWith) "foo" anns $ KDL.arg @Int)
          KDL.decodeWith decoder config `shouldBe` Right 1

      it "fails when node has unexpected annotation" $ do
        let config = "(test)foo 2"
            decoder = KDL.document $ _DO_
              _STMT_(_APOS_(KDL.nodeWith) "foo" ["FOO"] $ KDL.arg @Int)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Expected annotation to be one of [\"FOO\"], got: test"
            ]

    describe "remainingNodes" $ do
      it "returns all remaining nodes" $ do
        let config = "foo 1.0; foo 2.0; bar"
            decoder = KDL.document $ _DO_
              _ <- _STMT_(KDL.node @Node "foo")
              _STMT_(fmap (map scrubFormat) <$> KDL.remainingNodes)
            expected =
              Map.fromList
                [ ("foo", [fooNode2])
                , ("bar", [barNode])
                ]
            fooNode2 =
              Node
                { ann = Nothing
                , name = Identifier{value = "foo", ext = KDL.def}
                , entries =
                    [ Entry
                        { name = Nothing
                        , value = Value{ann = Nothing, data_ = Number 2.0, ext = KDL.def}
                        , ext = KDL.def
                        }
                    ]
                , children = Nothing
                , ext = KDL.def
                }
            barNode =
              Node
                { ann = Nothing
                , name = Identifier{value = "bar", ext = KDL.def}
                , entries = []
                , children = Nothing
                , ext = KDL.def
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

    -- Most behaviors tested with `remainingNodes`
    describe "remainingNodesWith" $ do
      it "returns all remaining nodes" $ do
        let config = "foo 1.0; foo 2.0; bar"
            decodeNode = _DO_
              _STMT_(KDL.optional $ KDL.arg @Int)
            decoder = KDL.document $ _DO_
              _ <- _STMT_(KDL.node @Node "foo")
              _STMT_(KDL.remainingNodesWith decodeNode)
            expected =
              Map.fromList
                [ ("foo", [Just 2])
                , ("bar", [Nothing])
                ]
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "fails when node fails to parse" $ do
        let config = "foo 1; bar 1; bar hello"
            decodeNode = _DO_
              _STMT_(KDL.arg @Int)
            decoder = KDL.document $ _DO_
              _STMT_(KDL.remainingNodesWith decodeNode)
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
              decoder = KDL.document $ _DO_
                _STMT_(_APOS_(KDL.remainingNodesWith) anns $ KDL.arg @Int)
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
              decoder = KDL.document $ _DO_
                _STMT_(_APOS_(KDL.remainingNodesWith) anns $ KDL.arg @Int)
          KDL.decodeWith decoder config
            `shouldBe` (Right . Map.fromList) [("foo", [1])]

      it "fails when node has unexpected annotation" $ do
        let config = "(FOO)foo 1; (test)foo 2"
            decoder = KDL.document $ _DO_
              _STMT_(_APOS_(KDL.remainingNodesWith) ["FOO"] $ KDL.arg @Int)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #1"
            , "  Expected annotation to be one of [\"FOO\"], got: test"
            ]

    describe "argAt" $ do
      it "gets argument at a node" $ do
        let config = "foo bar; hello world"
            decoder = KDL.document $ _DO_
              hello <- _STMT_(KDL.argAt @Text "hello")
              foo <- _STMT_(KDL.argAt @Text "foo")
              _RETURN_((hello, foo))
        KDL.decodeWith decoder config `shouldBe` Right ("world", "bar")

      it "fails if no node" $ do
        let config = "other_node"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAt @Int "foo")
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: <root>"
            , "  Expected node: foo"
            ]

      it "fails if node has no args" $ do
        let config = "foo"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAt @Int "foo")
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Expected arg #0"
            ]

      it "fails if arg fails to parse" $ do
        let config = "foo 1"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAt @Text "foo")
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected string, got: 1"
            ]

    -- Most behaviors tested with `argAt`
    describe "argAtWith" $ do
      it "gets argument at a node" $ do
        let config = "foo 1"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAtWith "foo" $ show . (* 10) <$> KDL.number)
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
              decoder = KDL.document $ _DO_
                _STMT_(_APOS_(KDL.argAtWith) "foo" anns KDL.string)
          KDL.decodeWith decoder config `shouldBe` Right "a"

      it "decodes argument without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo a"
              decoder = KDL.document $ _DO_
                _STMT_(_APOS_(KDL.argAtWith) "foo" anns KDL.string)
          KDL.decodeWith decoder config `shouldBe` Right "a"

      it "fails when argument has unexpected annotation" $ do
        let config = "foo (test)a"
            decoder = KDL.document $ _DO_
              _STMT_(_APOS_(KDL.argAtWith) "foo" ["VAL"] KDL.string)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

    describe "argsAt" $ do
      it "gets arguments at a node" $ do
        let config = "foo 1 2 3"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argsAt @Int "foo")
        KDL.decodeWith decoder config `shouldBe` Right [1, 2, 3]

      it "returns empty list if no node" $ do
        let config = ""
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argsAt @Int "foo")
        KDL.decodeWith decoder config `shouldBe` Right []

      it "returns empty list if node has no args" $ do
        let config = "foo"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argsAt @Int "foo")
        KDL.decodeWith decoder config `shouldBe` Right []

      it "fails if any arg fails to parse" $ do
        let config = "foo 1 asdf"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argsAt @Int "foo")
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #1"
            , "  Expected number, got: asdf"
            ]

    -- Most behaviors tested with `argsAt`
    describe "argsAtWith" $ do
      it "gets arguments at a node" $ do
        let config = "foo 1 2"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argsAtWith "foo" $ show . (* 10) <$> KDL.number)
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
              decoder = KDL.document $ _DO_
                _STMT_(_APOS_(KDL.argsAtWith) "foo" anns KDL.string)
          KDL.decodeWith decoder config `shouldBe` Right ["a", "b"]

      it "decodes arguments without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo a b"
              decoder = KDL.document $ _DO_
                _STMT_(_APOS_(KDL.argsAtWith) "foo" anns KDL.string)
          KDL.decodeWith decoder config `shouldBe` Right ["a", "b"]

      it "fails when argument has unexpected annotation" $ do
        let config = "foo (VAL)a (test)b"
            decoder = KDL.document $ _DO_
              _STMT_(_APOS_(KDL.argsAtWith) "foo" ["VAL"] KDL.string)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #1"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

    describe "dashChildrenAt" $ do
      it "gets dash children at a node" $ do
        let config = "foo { - 1; - 2; - 3; }"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.dashChildrenAt @Int "foo")
        KDL.decodeWith decoder config `shouldBe` Right [1, 2, 3]

      it "returns empty list if no node" $ do
        let config = ""
            decoder = KDL.document $ _DO_
              _STMT_(KDL.dashChildrenAt @Int "foo")
        KDL.decodeWith decoder config `shouldBe` Right []

      it "returns empty list if node has no dash children" $ do
        forM_ ["foo", "foo {}"] $ \config -> do
          let decoder = KDL.document $ _DO_
                _STMT_(KDL.dashChildrenAt @Int "foo")
          KDL.decodeWith decoder config `shouldBe` Right []

      it "fails if dash children have multiple args" $ do
        let config = "foo { - 1 2; - 3 4; }"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.dashChildrenAt @Int "foo")
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > - #0"
            , "  Unexpected arg #1: 2"
            ]

      it "fails if node has non-dash children" $ do
        let config = "foo { - 1; bar 1 2 3; }"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.dashChildrenAt @Int "foo")
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Unexpected node: bar #0"
            ]

      it "fails if any child fails to parse" $ do
        let config = "foo { - 1; - asdf; }"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.dashChildrenAt @Int "foo")
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > - #1 > arg #0"
            , "  Expected number, got: asdf"
            ]

    -- Most behaviors tested with `dashChildrenAt`
    describe "dashChildrenAtWith" $ do
      it "gets dash children at a node" $ do
        let config = "foo { - 1; - 2; }"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.dashChildrenAtWith "foo" $ show . (* 10) <$> KDL.number)
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
              decoder = KDL.document $ _DO_
                _STMT_(_APOS_(KDL.dashChildrenAtWith) "foo" anns KDL.string)
          KDL.decodeWith decoder config `shouldBe` Right ["a", "b"]

      it "decodes dash children without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo { - a; - b; }"
              decoder = KDL.document $ _DO_
                _STMT_(_APOS_(KDL.dashChildrenAtWith) "foo" anns KDL.string)
          KDL.decodeWith decoder config `shouldBe` Right ["a", "b"]

      it "fails when child has unexpected annotation" $ do
        let config = "foo { - (test)a; }"
            decoder = KDL.document $ _DO_
              _STMT_(_APOS_(KDL.dashChildrenAtWith) "foo" ["VAL"] KDL.string)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > - #0 > arg #0"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

    describe "dashNodesAt" $ do
      it "gets dash nodes at a node" $ do
        let config = "foo { - { bar; }; - { baz; }; }"
            decoder = KDL.document $ _DO_
              _STMT_(map scrubFormat <$> KDL.dashNodesAt "foo")
            expected = [node "-" [node "bar" []], node "-" [node "baz" []]]
            node name children =
              Node
                { ann = Nothing
                , name = Identifier{value = name, ext = KDL.def}
                , entries = []
                , children =
                    if null children
                      then Nothing
                      else Just NodeList{nodes = children, ext = KDL.def}
                , ext = KDL.def
                }
        KDL.decodeWith decoder config `shouldBe` Right expected

      it "returns empty list if no node" $ do
        let config = ""
            decoder = KDL.document $ _DO_
              _STMT_(KDL.dashNodesAt @Node "foo")
        KDL.decodeWith decoder config `shouldBe` Right []

      it "returns empty list if node has no dash nodes" $ do
        forM_ ["foo", "foo {}"] $ \config -> do
          let decoder = KDL.document $ _DO_
                _STMT_(KDL.dashNodesAt @Node "foo")
          KDL.decodeWith decoder config `shouldBe` Right []

      it "fails if node has non-dash nodes" $ do
        let config = "foo { - 1; bar 1 2 3; }"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.dashNodesAt @Node "foo")
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Unexpected node: bar #0"
            ]

    -- Most behaviors tested with `dashNodesAt`
    describe "dashNodesAtWith" $ do
      it "gets dash nodes at a node" $ do
        let config = "foo { - 1 { bar hello; }; - 2 { bar world; }; }"
            decodeChild = _DO_
              arg <- _STMT_(KDL.arg @Int)
              child <- _STMT_(KDL.children $ KDL.nodeWith "bar" $ KDL.arg @Text)
              _RETURN_((arg, child))
            decoder = KDL.document $ _DO_
              _STMT_(KDL.dashNodesAtWith "foo" decodeChild)
        KDL.decodeWith decoder config `shouldBe` Right [(1, "hello"), (2, "world")]

      it "fails if any child fails to parse" $ do
        let config = "foo { - { bar 1; }; - { bar test; }; }"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.dashNodesAtWith "foo" $ KDL.children $ KDL.argAt @Int "bar")
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > - #1 > bar #0 > arg #0"
            , "  Expected number, got: test"
            ]

  describe "NodeDecoder" $ do
    let decodeNode name decoder config =
          KDL.decodeWith
            ( KDL.document $ _DO_
                _STMT_(KDL.nodeWith name decoder)
            )
            config

    describe "arg" $ do
      it "decodes an argument" $ do
        let config = "foo 1 bar"
            decoder = _DO_
              arg1 <- _STMT_(KDL.arg @Int)
              arg2 <- _STMT_(KDL.arg @Text)
              _RETURN_((arg1, arg2))
        decodeNode "foo" decoder config `shouldBe` Right (1, "bar")

      it "decodes multiple arguments" $ do
        let config = "foo 1 2 3"
            decoder = _DO_
              _STMT_(KDL.many $ KDL.arg @Int)
        decodeNode "foo" decoder config `shouldBe` Right [1, 2, 3]

      it "fails if argument doesn't exist" $ do
        let config = "foo"
            decoder = _DO_
              _STMT_(KDL.arg @Int)
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Expected arg #0"
            ]

      it "fails if argument fails to parse" $ do
        let config = "foo test"
            decoder = _DO_
              _STMT_(KDL.arg @Int)
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected number, got: test"
            ]

      it "fails if not all arguments are decoded" $ do
        let config = "foo 1 2 3"
            decoder = _DO_
              _STMT_(KDL.arg @Int)
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Unexpected arg #1: 2"
            ]

    -- Most behaviors tested with `arg`
    describe "argWith" $ do
      it "decodes an argument" $ do
        let config = "foo bar"
            decoder = _DO_
              _STMT_(KDL.argWith KDL.string)
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
              decoder = _DO_
                _STMT_(_APOS_(KDL.argWith) anns KDL.string)
          decodeNode "foo" decoder config `shouldBe` Right "a"

      it "decodes argument without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo a"
              decoder = _DO_
                _STMT_(_APOS_(KDL.argWith) anns KDL.string)
          decodeNode "foo" decoder config `shouldBe` Right "a"

      it "fails when argument has unexpected annotation" $ do
        let config = "foo (test)a"
            decoder = _DO_
              _STMT_(_APOS_(KDL.argWith) ["VAL"] KDL.string)
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

      it "supports backtracking annotations" $ do
        let config = "foo (l)1 (r)2"
            decodeArg =
              KDL.oneOf
                [ Left <$> KDL.argWith' ["l"] KDL.number
                , Right <$> KDL.argWith' ["r"] KDL.number
                ]
            decoder = KDL.document $ _DO_
              _STMT_(KDL.nodeWith "foo" . KDL.many $ decodeArg)
        KDL.decodeWith decoder config `shouldBe` Right [Left 1, Right 2]

    describe "prop" $ do
      it "decodes a prop" $ do
        let config = "foo test1=1 test2=hello"
            decoder = _DO_
              prop1 <- _STMT_(KDL.prop @Text "test2")
              prop2 <- _STMT_(KDL.prop @Int "test1")
              _RETURN_((prop1, prop2))
        decodeNode "foo" decoder config `shouldBe` Right ("hello", 1)

      it "can optionally decode a prop" $ do
        let config = "foo a=1"
            decoder = _DO_
              a <- _STMT_(KDL.optional $ KDL.prop @Int "a")
              b <- _STMT_(KDL.optional $ KDL.prop @Int "b")
              _RETURN_((a, b))
        decodeNode "foo" decoder config `shouldBe` Right (Just 1, Nothing)

      it "decodes last prop" $ do
        let config = "foo test=1 test=2"
            decoder = _DO_
              _STMT_(KDL.prop @Int "test")
        decodeNode "foo" decoder config `shouldBe` Right 2

      it "fails if prop doesn't exist" $ do
        let config = "foo 123"
            decoder = _DO_
              _STMT_(KDL.prop @Int "test")
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Expected prop: test"
            ]

      it "fails if prop fails to parse" $ do
        let config = "foo hello=world"
            decoder = _DO_
              _STMT_(KDL.prop @Int "hello")
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > prop hello"
            , "  Expected number, got: world"
            ]

      it "fails if not all props are decoded" $ do
        let config = "foo a=1 b=2"
            decoder = _DO_
              _STMT_(KDL.prop @Int "a")
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Unexpected prop: b=2"
            ]

    -- Most behaviors tested with `prop`
    describe "propWith" $ do
      it "decodes a prop" $ do
        let config = "foo a=1"
            decoder = _DO_
              _STMT_(KDL.propWith "a" KDL.number)
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
              decoder = _DO_
                _STMT_(_APOS_(KDL.propWith) "a" anns KDL.number)
          decodeNode "foo" decoder config `shouldBe` Right 1

      it "decodes prop without an annotation" $ do
        let testCases =
              [ []
              , ["test"]
              , ["test", "other"]
              ]
        forM_ testCases $ \anns -> do
          let config = "foo a=1"
              decoder = _DO_
                _STMT_(_APOS_(KDL.propWith) "a" anns KDL.number)
          decodeNode "foo" decoder config `shouldBe` Right 1

      it "fails when prop has unexpected annotation" $ do
        let config = "foo a=(test)1"
            decoder = _DO_
              _STMT_(_APOS_(KDL.propWith) "a" ["VAL"] KDL.number)
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > prop a"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

    describe "remainingProps" $ do
      it "decodes remaining props" $ do
        let config = "foo a=1 b=2 c=3 b=4"
            decoder = _DO_
              _ <- _STMT_(KDL.prop @Int "a")
              _STMT_(KDL.remainingProps @Int)
        decodeNode "foo" decoder config
          `shouldBe` (Right . Map.fromList) [("b", 4), ("c", 3)]

      it "returns empty map if no props left" $ do
        let config = "foo a=1"
            decoder = _DO_
              _ <- _STMT_(KDL.prop @Int "a")
              _STMT_(KDL.remainingProps @Int)
        decodeNode "foo" decoder config `shouldBe` Right Map.empty

      it "fails if prop fails to parse" $ do
        let config = "foo a=1 b=1 c=2 c=test"
            decoder = _DO_
              _ <- _STMT_(KDL.prop @Int "a")
              _STMT_(KDL.remainingProps @Int)
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > prop c"
            , "  Expected number, got: test"
            ]

    -- Most behaviors tested with `remainingProps`
    describe "remainingPropsWith" $ do
      it "decodes remaining props" $ do
        let config = "foo a=1 b=2 c=3 b=4"
            decoder = _DO_
              _ <- _STMT_(KDL.prop @Int "a")
              _STMT_(KDL.remainingPropsWith KDL.number)
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
              decoder = _DO_
                _STMT_(_APOS_(KDL.remainingPropsWith) anns KDL.number)
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
              decoder = _DO_
                _STMT_(_APOS_(KDL.remainingPropsWith) anns KDL.number)
          decodeNode "foo" decoder config
            `shouldBe` (Right . Map.fromList) [("a", 1), ("b", 2)]

      it "fails when prop has unexpected annotation" $ do
        let config = "foo a=(VAL)1 b=(test)2"
            decoder = _DO_
              _STMT_(_APOS_(KDL.remainingPropsWith) ["VAL"] KDL.number)
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > prop b"
            , "  Expected annotation to be one of [\"VAL\"], got: test"
            ]

    describe "children" $ do
      it "decodes children" $ do
        let config = "foo { bar test; }"
            decoder = _DO_
              _STMT_(fmap scrubFormat . KDL.children $ KDL.node @Node "bar")
            expected =
              Node
                { ann = Nothing
                , name = Identifier{value = "bar", ext = KDL.def}
                , entries =
                    [ Entry
                        { name = Nothing
                        , value = Value{ann = Nothing, data_ = String "test", ext = KDL.def}
                        , ext = KDL.def
                        }
                    ]
                , children = Nothing
                , ext = KDL.def
                }
        decodeNode "foo" decoder config `shouldBe` Right expected

      it "can be re-entered" $ do
        let config = "foo { bar a; baz b; }"
            decoder = _DO_
              arg1 <- _STMT_(KDL.children $ KDL.argAt @Text "bar")
              arg2 <- _STMT_(KDL.children $ KDL.argAt @Text "baz")
              _RETURN_((arg1, arg2))
        decodeNode "foo" decoder config `shouldBe` Right ("a", "b")

      it "fails if not all children are decoded" $ do
        let config = "foo { asdf; bar; }"
            decoder = _DO_
              _STMT_(KDL.children $ KDL.node @Node "bar")
        decodeNode "foo" decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0"
            , "  Unexpected node: asdf #0"
            ]

  describe "ValueDecoder" $ do
    describe "any" $ do
      it "decodes any value" $ do
        let config = "foo 1.0 asdf #true"
            decoder = KDL.document $ _DO_
              _STMT_(map scrubFormat <$> KDL.argsAtWith "foo" KDL.any)
            val data_ =
              Value
                { ann = Nothing
                , data_ = data_
                , ext = KDL.def
                }
        KDL.decodeWith decoder config
          `shouldBe` Right [val $ Number 1, val $ String "asdf", val $ Bool True]

    describe "string" $ do
      it "decodes string value" $ do
        let config = "foo asdf"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAtWith "foo" KDL.string)
        KDL.decodeWith decoder config `shouldBe` Right "asdf"

      it "fails when value is not string" $ do
        let config = "foo 1"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAtWith "foo" KDL.string)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected string, got: 1"
            ]

    describe "number" $ do
      it "decodes number value" $ do
        let config = "foo 1"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAtWith "foo" KDL.number)
        KDL.decodeWith decoder config `shouldBe` Right 1

      it "fails when value is not number" $ do
        let config = "foo asdf"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAtWith "foo" KDL.number)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected number, got: asdf"
            ]

    describe "bool" $ do
      it "decodes bool value" $ do
        let config = "foo #true"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAtWith "foo" KDL.bool)
        KDL.decodeWith decoder config `shouldBe` Right True

      it "fails when value is not bool" $ do
        let config = "foo 1"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAtWith "foo" KDL.bool)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected bool, got: 1"
            ]

    describe "null" $ do
      it "decodes null value" $ do
        let config = "foo #null"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAtWith "foo" KDL.null)
        KDL.decodeWith decoder config `shouldBe` Right ()

      it "fails when value is not null" $ do
        let config = "foo 1"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.argAtWith "foo" KDL.null)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #0"
            , "  Expected null, got: 1"
            ]

  describe "Combinators" $ do
    describe "oneOf" $ do
      it "decodes one of the options" $ do
        let config = "foo 123 hello"
            decodeVal = [Left <$> KDL.number, Right <$> KDL.string]
            decoder = KDL.document $ _DO_
              _STMT_(KDL.nodeWith "foo" . KDL.many . KDL.argWith $ KDL.oneOf decodeVal)
        KDL.decodeWith decoder config `shouldBe` Right [Left 123, Right "hello"]

      it "fails if none can be decoded" $ do
        let config = "foo 123 hello"
            decodeVal = [Left <$> KDL.number, Right <$> KDL.bool]
            decoder = KDL.document $ _DO_
              _STMT_(KDL.nodeWith "foo" . KDL.many . KDL.argWith $ KDL.oneOf decodeVal)
        KDL.decodeWith decoder config
          `shouldSatisfy` decodeErrorMsg
            [ "At: foo #0 > arg #1"
            , "  Expected bool, got: hello"
            , "  Expected number, got: hello"
            ]

    describe "option" $ do
      it "defaults to the given value" $ do
        let config = "foo"
            decoder = KDL.document $ _DO_
              _STMT_(KDL.nodeWith "foo" $ KDL.option 123 $ KDL.arg @Int)
        KDL.decodeWith decoder config `shouldBe` Right 123

newtype MyNode = MyNode Int
  deriving (Eq)

instance KDL.DecodeNode MyNode where
  validNodeTypeAnns _ = ["MyNode"]
  nodeDecoder = _DO_
    x <- _STMT_(KDL.arg)
#ifdef IS_ARROW
    if not (0 < x && x < 10)
      then KDL.fail -< "Invalid argument: " <> (Text.pack . show) x
      else returnA -< ()
    returnA -< MyNode x
#endif
#ifdef IS_MONAD
    unless (0 < x && x < 10) $ do
      KDL.fail $ "Invalid argument: " <> (Text.pack . show) x
    pure $ MyNode x
#endif

decodeNodeSpec :: Spec
decodeNodeSpec = do
  describe "DecodeNode" $ do
    it "decodes a custom node" $ do
      let config = "foo 1"
          decoder = KDL.document $ _DO_
            _STMT_(KDL.node "foo")
      KDL.decodeWith decoder config `shouldBe` Right (MyNode 1)

    it "throws user-specified error" $ do
      let config = "foo 100"
          decoder = KDL.document $ _DO_
            _STMT_(KDL.node @MyNode "foo")
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "At: foo #0"
          , "  Invalid argument: 100"
          ]

    it "decodes valid type ann" $ do
      let config = "(MyNode)foo 1"
          decoder = KDL.document $ _DO_
            _STMT_(KDL.node "foo")
      KDL.decodeWith decoder config `shouldBe` Right (MyNode 1)

    it "fails on invalid type ann" $ do
      let config = "(bad)foo 1"
          decoder = KDL.document $ _DO_
            _STMT_(KDL.node @MyNode "foo")
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "At: foo #0"
          , "  Expected annotation to be one of [\"MyNode\"], got: bad"
          ]

newtype MyVal = MyVal Double
  deriving (Eq)

instance KDL.DecodeValue MyVal where
  validValueTypeAnns _ = ["MyVal"]
  valueDecoder = KDL.withDecoder KDL.number $ \x -> do
    unless (0 < x && x < 10) $ do
      KDL.failM $ "Invalid value: " <> (Text.pack . show) x
    pure $ MyVal (realToFrac x)

decodeValueSpec :: Spec
decodeValueSpec = do
  describe "DecodeValue" $ do
    it "decodes a custom value" $ do
      let config = "foo 1"
          decoder = KDL.document $ _DO_
            _STMT_(KDL.argAt "foo")
      KDL.decodeWith decoder config `shouldBe` Right (MyVal 1)

    it "throws user-specified error" $ do
      let config = "foo 100.0"
          decoder = KDL.document $ _DO_
            _STMT_(KDL.argAt @MyVal "foo")
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "At: foo #0 > arg #0"
          , "  Invalid value: 100.0"
          ]

    it "decodes valid type ann" $ do
      let config = "foo (MyVal)1"
          decoder = KDL.document $ _DO_
            _STMT_(KDL.argAt "foo")
      KDL.decodeWith decoder config `shouldBe` Right (MyVal 1)

    it "fails on invalid type ann" $ do
      let config = "foo (bad)1"
          decoder = KDL.document $ _DO_
            _STMT_(KDL.argAt @MyVal "foo")
      KDL.decodeWith decoder config
        `shouldSatisfy` decodeErrorMsg
          [ "At: foo #0 > arg #0"
          , "  Expected annotation to be one of [\"MyVal\"], got: bad"
          ]
