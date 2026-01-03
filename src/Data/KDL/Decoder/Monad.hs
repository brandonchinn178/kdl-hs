{-# LANGUAGE LambdaCase #-}

{-|
This module defines the Monad interface for decoding a KDL document. Intended to
be imported from "Data.KDL" as:

> import Data.KDL qualified as KDL

For most use-cases, this Monad interface is sufficient. You may wish to use
"Data.KDL.Decoder.Arrow" if you would like to statically analyze a decoder's
schema, e.g. to generate documentation.

= Quickstart

See "Data.KDL"
-}
module Data.KDL.Decoder.Monad (
  -- * Decoding entrypoint
  decodeWith,
  decodeFileWith,
  decodeDocWith,

  -- * Decoder
  Decoder,
  module Data.KDL.Decoder.DecodeM,
  fail,
  withDecoder,
  debug,

  -- * Decode type classes
  -- $decodeTypeClassesDoc
  noSchema,
  Arrow.DecodeNode (..),
  Arrow.DecodeValue (..),

  -- * Document
  DocumentDecoder,
  document,

  -- * NodeList
  NodeListDecoder,
  node,
  remainingNodes,
  argAt,
  argsAt,
  dashChildrenAt,
  dashNodesAt,

  -- ** Explicitly specify decoders
  nodeWith,
  remainingNodesWith,
  dashChildrenAtWith,
  dashNodesAtWith,
  argAtWith,
  argsAtWith,

  -- ** Explicitly specify decoders and type annotations
  nodeWith',
  remainingNodesWith',
  dashChildrenAtWith',
  argAtWith',
  argsAtWith',

  -- * Node
  NodeDecoder,
  arg,
  prop,
  remainingProps,
  children,

  -- ** Explicitly specify decoders
  argWith,
  propWith,
  remainingPropsWith,

  -- ** Explicitly specify decoders and type annotations
  argWith',
  propWith',
  remainingPropsWith',

  -- * Value
  ValueDecoder,
  any,
  text,
  number,
  bool,
  null,

  -- * Combinators
  Arrow.oneOf,
  Arrow.many,
  Arrow.optional,
  Arrow.option,
  Arrow.some,
) where

import Control.Applicative (Alternative)
import Control.Arrow qualified as Arrow
import Data.Coerce (coerce)
import Data.KDL.Decoder.Arrow qualified as Arrow
import Data.KDL.Decoder.DecodeM
import Data.KDL.Decoder.Schema (
  Schema (..),
 )
import Data.KDL.Types (
  Document,
  Node,
  NodeList,
  Value,
 )
import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Prelude hiding (any, fail, null)

-- | Decode the given KDL configuration with the given decoder.
decodeWith :: forall a. DocumentDecoder a -> Text -> Either DecodeError a
decodeWith = coerce (Arrow.decodeWith @a)

-- | Read KDL configuration from the given file path and decode it with the given decoder.
decodeFileWith :: forall a. DocumentDecoder a -> FilePath -> IO (Either DecodeError a)
decodeFileWith = coerce (Arrow.decodeFileWith @a)

-- | Decode an already-parsed 'Document' with the given decoder.
decodeDocWith :: forall a. DocumentDecoder a -> Document -> Either DecodeError a
decodeDocWith = coerce (Arrow.decodeDocWith @a)

{----- Decoder -----}

-- | @Decoder o a@ represents an action that decodes a KDL object of type @o@
-- and returns a value of type @a@.
--
-- Exactly the same as the Arrow 'Data.KDL.Decoder.Arrow.Decoder', except
-- provides a Monad instance that's useful for do-notation. The only downside of
-- using this over the Arrow 'Data.KDL.Decoder.Arrow.Decoder' is you don't get a
-- statically derived schema of the entire document. If you want that, you must
-- use either arrows notation with the Arrow 'Data.KDL.Decoder.Arrow.Decoder' or
-- use do-notation with @-XApplicativeDo@.
newtype Decoder o a = Decoder (Arrow.Decoder o () a)
  deriving
    ( Functor
    , Applicative
    , Alternative
    )

instance Monad (Decoder o) where
  Decoder (Arrow.Decoder _ run1) >>= k =
    Decoder . Arrow.Decoder SchemaUnknown $ \a -> do
      x <- run1 a
      let Decoder (Arrow.Decoder _ run2) = k x
      run2 a

-- $decodeTypeClassesDoc
-- To avoid code duplication, the same 'DecodeNode' and 'DecodeValue' type classes
-- are used for both the Arrow and Monad decoders. However, the type classes are
-- implemented with the Arrow decoder, as the Monad decoder is lossy regarding the
-- schema. If you're implementing instances that may be used with the Arrow
-- decoder, you should probably implement the decoder with
-- the "Data.KDL.Decoder.Arrow" API.
--
-- Otherwise, use the normal Monad decoder API and use 'noSchema' at the very end,
-- which indicates that this instance doesn't provide any schema information.
--
-- @
-- instance KDL.DecodeNode Person where
--   nodeDecoder = KDL.noSchema $ do
--     name <- KDL.arg
--     age <- KDL.prop "age"
--     pure Person{..}
--
-- instance KDL.DecodeValue MyVal where
--   valueDecoder =
--     KDL.noSchema . KDL.oneOf $
--       [ MyText \<$> KDL.text
--       , KDL.withDecoder KDL.number $ \x -> do
--           if x == 0
--             then pure MyZero
--             else KDL.failM "integer value must be zero"
--       ]
-- @

-- | Drop the Monad Decoder back down to an Arrow Decoder.
noSchema :: Decoder o a -> Arrow.Decoder o () a
noSchema (Decoder decoder) = decoder

-- | Unconditionally fail the decoder.
--
-- === __Example__
--
-- @
-- decoder = do
--   x <- KDL.arg
--   when (x > 100) $ do
--     KDL.fail $ "argument is too large: " <> (Text.pack . show) x
--   pure x
-- @
fail :: forall a o. Text -> Decoder o a
fail msg = coerce (Arrow.arr (\() -> msg) Arrow.>>> Arrow.fail @a)

-- | Run actions within a t'Decoder'. Useful for adding post-processing logic.
--
-- === __Example__
--
-- @
-- decoder = KDL.withDecoder KDL.number $ \\x -> do
--   when (x > 100)
--     KDL.failM $ "argument is too large: " <> (Text.pack . show) x
--   pure $ MyVal x
-- @
withDecoder :: forall o a b. Decoder o a -> (a -> DecodeM b) -> Decoder o b
withDecoder = coerce (Arrow.withDecoder @o @() @a @b)

-- | Debug the current state of the object being decoded.
--
-- === __Example__
--
-- @
-- decoder = do
--   KDL.debug    -- Node{entries = [Entry{}, Entry{}]}
--   x <- KDL.arg
--   KDL.debug    -- Node{entries = [Entry{}]}
--   y <- KDL.arg
--   KDL.debug    -- Node{entries = []}
--   pure (x, y)
-- @
debug :: forall o. (Show o) => Decoder o ()
debug = coerce (Arrow.debug @o @())

{----- Decoding Document -----}

newtype DocumentDecoder a = DocumentDecoder (NodeListDecoder a)

-- | Finalize a 'NodeListDecoder' as a 'DocumentDecoder' to use with 'decodeWith'.
--
-- Ensures that all nodes have been decoded (e.g. error if the user specified
-- unrecognized nodes, or misspelled a node name). To allow unrecognized nodes,
-- use @remainingNodes \@Node@ and ignore the result.
document :: forall a. NodeListDecoder a -> DocumentDecoder a
document = coerce (Arrow.document @a)

{----- Decoding NodeList -----}

type NodeListDecoder = Decoder NodeList

-- | Decode a node with the given name and decoder.
--
-- == __Example__
--
-- @
-- instance KDL.DecodeNode Person where
--   nodeTypeAnns _ = ["Person"]
--   nodeDecoder = KDL.noSchema $ do
--     name <- KDL.arg
--     pure Person{..}
--
-- let
--   config =
--     """
--     person "Alice"
--     person "Bob"
--     person "Charlie"
--     (Person)person "Danielle"
--     (Dog)person "Fido"
--     """
--   decoder = KDL.document $ do
--     many $ KDL.node "person"
-- KDL.decodeWith decoder config == Right ["Alice", "Bob", "Charlie", "Danielle"]
-- @
node :: forall a. (Arrow.DecodeNode a) => Text -> NodeListDecoder a
node = coerce (Arrow.node @a)

-- | Same as 'node', except explicitly specify the 'NodeDecoder' instead of using 'Arrow.DecodeNode'.
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     person "Alice"
--     person "Bob"
--     person "Charlie"
--     (Person)person "Danielle"
--     (Dog)person "Fido"
--     """
--   decoder = KDL.document $ do
--     many . KDL.nodeWith "person" ["Person"] $ KDL.arg
-- KDL.decodeWith decoder config == Right ["Alice", "Bob", "Charlie", "Danielle"]
-- @
nodeWith :: forall a. (Typeable a) => Text -> NodeDecoder a -> NodeListDecoder a
nodeWith = coerce (Arrow.nodeWith @() @a)

-- | Same as 'nodeWith', except allow specifying type annotations.
nodeWith' :: forall a. (Typeable a) => Text -> [Text] -> NodeDecoder a -> NodeListDecoder a
nodeWith' = coerce (Arrow.nodeWith' @() @a)

-- | Decode all remaining nodes with the given decoder.
--
-- == __Example__
--
-- @
-- instance KDL.DecodeNode MyArg where
--   nodeDecoder = KDL.noSchema $ do
--     name <- KDL.arg
--     pure MyArg{..}
--
-- let
--   config =
--     """
--     build "pkg1"
--     build "pkg2"
--     lint "pkg1"
--     """
--   decoder = KDL.document $ do
--     KDL.remainingNodes
-- KDL.decodeWith decoder config == Right (Map.fromList [("build", [MyArg "pkg1", MyArg "pkg2"]), ("lint", [MyArg "pkg1"])])
-- @
remainingNodes :: forall a. (Arrow.DecodeNode a) => NodeListDecoder (Map Text [a])
remainingNodes = coerce (Arrow.remainingNodes @a)

-- | Same as 'remainingNodes', except explicitly specify the 'NodeDecoder' instead of using 'Arrow.DecodeNode'
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     build "pkg1"
--     build "pkg2"
--     lint "pkg1"
--     """
--   decoder = KDL.document $ do
--     KDL.remainingNodesWith [] KDL.arg
-- KDL.decodeWith decoder config == Right (Map.fromList [("build", ["pkg1", "pkg2"]), ("lint", ["pkg1"])])
-- @
remainingNodesWith :: forall a. (Typeable a) => NodeDecoder a -> NodeListDecoder (Map Text [a])
remainingNodesWith = coerce (Arrow.remainingNodesWith @() @a)

-- | Same as 'remainingNodesWith', except allow specifying type annotations.
remainingNodesWith' :: forall a. (Typeable a) => [Text] -> NodeDecoder a -> NodeListDecoder (Map Text [a])
remainingNodesWith' = coerce (Arrow.remainingNodesWith' @() @a)

-- | A helper to decode the first argument of the first node with the given name.
-- A utility for nodes that are acting like a key-value store.
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     verbose #true
--     """
--   decoder = KDL.document $ do
--     KDL.argAt "verbose"
-- KDL.decodeWith decoder config == Right True
-- @
argAt :: forall a. (Arrow.DecodeValue a) => Text -> NodeListDecoder a
argAt = coerce (Arrow.argAt @a)

-- | Same as 'argAt', except explicitly specify the 'ValueDecoder' instead of using 'Arrow.DecodeValue'
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     verbose #true
--     """
--   decoder = KDL.document $ do
--     KDL.argAtWith "verbose" [] KDL.bool
-- KDL.decodeWith decoder config == Right True
-- @
argAtWith :: forall a. (Typeable a) => Text -> ValueDecoder a -> NodeListDecoder a
argAtWith = coerce (Arrow.argAtWith @() @a)

-- | Same as 'argAtWith', except allow specifying type annotations.
argAtWith' :: forall a. (Typeable a) => Text -> [Text] -> ValueDecoder a -> NodeListDecoder a
argAtWith' = coerce (Arrow.argAtWith' @() @a)

-- | A helper to decode all the arguments of the first node with the given name.
-- A utility for nodes that are acting like a key-value store with a list of values.
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     email "a@example.com" "b@example.com"
--     """
--   decoder = KDL.document $ do
--     KDL.argsAt "email"
-- KDL.decodeWith decoder config == Right ["a@example.com", "b@example.com"]
-- @
argsAt :: forall a. (Arrow.DecodeValue a) => Text -> NodeListDecoder [a]
argsAt = coerce (Arrow.argsAt @a)

-- | Same as 'argsAt', except explicitly specify the 'ValueDecoder' instead of using 'Arrow.DecodeValue'
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     email "a@example.com" "b@example.com"
--     """
--   decoder = KDL.document $ do
--     KDL.argsAtWith "email" [] KDL.text
-- KDL.decodeWith decoder config == Right ["a@example.com", "b@example.com"]
-- @
argsAtWith :: forall a. (Typeable a) => Text -> ValueDecoder a -> NodeListDecoder [a]
argsAtWith = coerce (Arrow.argsAtWith @() @a)

-- | Same as 'argsAtWith', except allow specifying type annotations.
argsAtWith' :: forall a. (Typeable a) => Text -> [Text] -> ValueDecoder a -> NodeListDecoder [a]
argsAtWith' = coerce (Arrow.argsAtWith' @() @a)

-- | A helper for decoding child values in a list following the KDL convention of being named "-".
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     attendees {
--       - "Alice"
--       - "Bob"
--     }
--     """
--   decoder = KDL.document $ do
--     KDL.dashChildrenAt "attendees"
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
dashChildrenAt :: forall a. (Arrow.DecodeValue a) => Text -> NodeListDecoder [a]
dashChildrenAt = coerce (Arrow.dashChildrenAt @a)

-- | Same as 'dashChildrenAt', except explicitly specify the 'ValueDecoder' instead of using 'Arrow.DecodeValue'
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     attendees {
--       - "Alice"
--       - "Bob"
--     }
--     """
--   decoder = KDL.document $ do
--     KDL.dashChildrenAtWith "attendees" [] KDL.text
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
dashChildrenAtWith :: forall a. (Typeable a) => Text -> ValueDecoder a -> NodeListDecoder [a]
dashChildrenAtWith = coerce (Arrow.dashChildrenAtWith @() @a)

-- | Same as 'dashChildrenAtWith', except allow specifying type annotations.
dashChildrenAtWith' :: forall a. (Typeable a) => Text -> [Text] -> ValueDecoder a -> NodeListDecoder [a]
dashChildrenAtWith' = coerce (Arrow.dashChildrenAtWith' @() @a)

-- | A helper for decoding child values in a list following the KDL convention of being named "-".
--
-- == __Example__
--
-- @
-- instance KDL.DecodeNode Attendee where
--   nodeDecoder = KDL.noSchema $ do
--     name <- KDL.arg
--     pure Attendee{..}
--
-- let
--   config =
--     """
--     attendees {
--       - "Alice"
--       - "Bob"
--     }
--     """
--   decoder = KDL.document $ do
--     KDL.dashNodesAt "attendees"
-- KDL.decodeWith decoder config == Right [Attendee "Alice", Attendee "Bob"]
-- @
dashNodesAt :: forall a. (Arrow.DecodeNode a) => Text -> NodeListDecoder [a]
dashNodesAt = coerce (Arrow.dashNodesAt @a)

-- | Same as 'dashChildrenAt', except explicitly specify the 'NodeDecoder' instead of using 'Arrow.DecodeNode'
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     attendees {
--       - "Alice"
--       - "Bob"
--     }
--     """
--   decoder = KDL.document $ do
--     KDL.dashNodesAtWith "attendees" [] KDL.arg
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
dashNodesAtWith :: forall a. (Typeable a) => Text -> NodeDecoder a -> NodeListDecoder [a]
dashNodesAtWith = coerce (Arrow.dashNodesAtWith @() @a)

{----- Decoding Node -----}

type NodeDecoder a = Decoder Node a

-- | Decode an argument in the node.
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     person \"Alice" 1 2 3
--     """
--   decoder = KDL.document $ do
--     KDL.nodeWith "person" decodePerson
--   decodePerson = do
--     name <- KDL.arg
--     vals <- KDL.many KDL.arg
--     pure (name, vals)
-- KDL.decodeWith decoder config == Right (\"Alice", [1, 2, 3])
-- @
arg :: forall a. (Arrow.DecodeValue a) => NodeDecoder a
arg = coerce (Arrow.arg @a)

-- | Same as 'arg', except explicitly specify the 'ValueDecoder' instead of using 'DecodeValue'
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     person \"Alice" 1 2 3
--     """
--   decoder = KDL.document $ do
--     KDL.nodeWith "person" $ decodePerson
--   decodePerson = do
--     name \<- KDL.argWith $ Text.toUpper \<$> KDL.text
--     vals \<- KDL.many $ KDL.argWith $ show \<$> KDL.valueDecoder @Int
--     pure (name, vals)
-- KDL.decodeWith decoder config == Right (\"ALICE", ["1", "2", "3"])
-- @
argWith :: forall a. (Typeable a) => ValueDecoder a -> NodeDecoder a
argWith = coerce (Arrow.argWith @() @a)

-- | Same as 'argWith', except allow specifying type annotations.
argWith' :: forall a. (Typeable a) => [Text] -> ValueDecoder a -> NodeDecoder a
argWith' = coerce (Arrow.argWith' @() @a)

-- | Decode the property with the given name in the node.
--
-- If the property appears multiple times, the last value is returned, as
-- defined in the spec.
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     my-node a=1 b=2 a=3
--     """
--   decoder = KDL.document $ do
--     KDL.nodeWith "my-node" $ KDL.prop @Int "a"
-- KDL.decodeWith decoder config == Right 3
-- @
prop :: forall a. (Arrow.DecodeValue a) => Text -> NodeDecoder a
prop = coerce (Arrow.prop @a)

-- | Same as 'prop', except explicitly specify the 'ValueDecoder' instead of using 'DecodeValue'
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     my-node a=1 b=2 a=3
--     """
--   decoder = KDL.document $ do
--     KDL.nodeWith "my-node" $ do
--       KDL.propWith "a" $ show \<$> KDL.number
-- KDL.decodeWith decoder config == Right "3.0"
-- @
propWith :: forall a. (Typeable a) => Text -> ValueDecoder a -> NodeDecoder a
propWith = coerce (Arrow.propWith @() @a)

-- | Same as 'propWith', except allow specifying type annotations.
propWith' :: forall a. (Typeable a) => Text -> [Text] -> ValueDecoder a -> NodeDecoder a
propWith' = coerce (Arrow.propWith' @() @a)

-- | Decode all remaining props
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     my-node a=1 b=2 a=3
--     """
--   decoder = KDL.document $ do
--     KDL.nodeWith "my-node" $ KDL.remainingProps @Int
-- KDL.decodeWith decoder config == (Right . Map.fromList) [("a", 3), ("b", 2)]
-- @
remainingProps :: forall a. (Arrow.DecodeValue a) => NodeDecoder (Map Text a)
remainingProps = coerce (Arrow.remainingProps @a)

-- | Same as 'remainingProps', except explicitly specify the 'ValueDecoder' instead of using 'DecodeValue'
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     my-node a=1 b=2 a=3
--     """
--   decoder = KDL.document $ do
--     KDL.nodeWith "my-node" $ do
--       KDL.remainingPropsWith $ show \<$> KDL.number
-- KDL.decodeWith decoder config == (Right . Map.fromList) [("a", "3.0"), ("b", "2.0")]
-- @
remainingPropsWith :: forall a. (Typeable a) => ValueDecoder a -> NodeDecoder (Map Text a)
remainingPropsWith = coerce (Arrow.remainingPropsWith @() @a)

-- | Same as 'remainingPropsWith', except allow specifying type annotations.
remainingPropsWith' :: forall a. (Typeable a) => [Text] -> ValueDecoder a -> NodeDecoder (Map Text a)
remainingPropsWith' = coerce (Arrow.remainingPropsWith' @() @a)

-- | Decode the children of the node.
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     person \"Alice" {
--       email "alice@example.com"
--     }
--     """
--   decoder = KDL.document $ do
--     KDL.nodeWith "person" $ do
--       name <- KDL.arg
--       email <- KDL.children $ KDL.argAt "email"
--       pure Person{..}
-- KDL.decodeWith decoder config == Right Person{name = \"Alice", email = "alice\@example.com"}
-- @
children :: forall a. NodeListDecoder a -> NodeDecoder a
children = coerce (Arrow.children @() @a)

{----- Decoding Value -----}

type ValueDecoder a = Decoder Value a

-- | Decode any value, without any possibility of failure.
any :: ValueDecoder Value
any = coerce (Arrow.any @())

-- | Decode a KDL text value.
text :: ValueDecoder Text
text = coerce (Arrow.text @())

-- | Decode a KDL number value.
number :: ValueDecoder Scientific
number = coerce (Arrow.number @())

-- | Decode a KDL bool value.
bool :: ValueDecoder Bool
bool = coerce (Arrow.bool @())

-- | Decode a KDL null value.
null :: ValueDecoder ()
null = coerce (Arrow.null @())
