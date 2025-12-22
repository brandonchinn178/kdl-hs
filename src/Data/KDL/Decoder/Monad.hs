{-# LANGUAGE LambdaCase #-}

{-|
This module defines the Monad interface for decoding a KDL document. Intended to
be imported from 'Data.KDL' as:

> import Data.KDL qualified as KDL

For most use-cases, this Monad interface is sufficient. You may wish to use
'Data.KDL.Decoder.Arrow' if you would like to statically analyze a decoder's
schema, e.g. to generate documentation.

=== Quickstart

FIXME: quickstart
-}
module Data.KDL.Decoder.Monad (
  decodeWith,
  decodeFileWith,
  
  -- * Decoder
  Decoder,
  DecodeError (..),
  module Data.KDL.Decoder.DecodeM,
  fail,

  -- * Decode type classes
  withoutSchema,
  Arrow.DecodeBaseNode (..),
  Arrow.DecodeBaseValue (..),

  -- * Document
  DocumentDecoder,
  document,
  documentSchema,

  -- * NodeList
  NodeListDecoder,
  nodeAt,
  remainingNodes,
  argAt,
  argsAt,
  dashChildrenAt,
  dashNodesAt,

  -- ** Explicitly specify NodeDecoder
  nodeAtWith,
  remainingNodesWith,
  dashChildrenAtWith,
  dashNodesAtWith,

  -- * Node
  NodeDecoder,
  node,

  -- ** Explicitly specify BaseNodeDecoder
  nodeWith,

  -- * BaseNode
  BaseNodeDecoder,
  arg,
  prop,
  remainingProps,
  children,

  -- ** Explicitly specify ValueDecoder
  argWith,
  propWith,
  remainingPropsWith,

  -- * Value
  ValueDecoder,
  value,

  -- ** Explicitly specify BaseValueDecoder
  valueWith,

  -- * BaseValue
  Arrow.BaseValueDecoder,
  Arrow.any,
  Arrow.text,
  Arrow.number,
  Arrow.bool,
  Arrow.null,
  Arrow.oneOf,

  -- * Combinators
  Arrow.many,
  Arrow.optional,
  Arrow.option,
  Arrow.some,
) where

import Control.Applicative (Alternative)
import Control.Arrow qualified as Arrow
import Data.Coerce (coerce)
import Data.KDL.Decoder.Arrow qualified as Arrow
import Data.KDL.Decoder.DecodeM (
  DecodeError (..),
 )
import Data.KDL.Decoder.Schema (
  Schema (..),
  SchemaOf,
 )
import Data.KDL.Types (
  BaseNode,
  BaseValue,
  Node,
  NodeList,
  Value,
 )
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Prelude hiding (fail)

decodeWith :: forall a. DocumentDecoder a -> Text -> Either DecodeError a
decodeWith = coerce (Arrow.decodeWith @a)

decodeFileWith :: forall a. DocumentDecoder a -> FilePath -> IO (Either DecodeError a)
decodeFileWith = coerce (Arrow.decodeFileWith @a)

{----- Decoder -----}

-- | @Decoder o a@ represents an action that decodes a KDL object of type @o@ and returns
-- a value of type @a@.
--
-- Exactly the same as the Arrow 'Arrow.Decoder', except provides a Monad instance that's
-- useful for do-notation. The only downside of using this over Arrow.Decoder is you don't
-- get a statically derived schema of the entire document. If you want that, you must use
-- either arrows notation with the Arrow 'Arrow.Decoder' or use do-notation with 'ApplicativeDo'.
newtype Decoder o a = Decoder (Arrow.Decoder o () a)
  deriving
    ( Functor
    , Applicative
    , Alternative
    )

instance Monad (Decoder o) where
  Decoder (Arrow.Decoder _ run1) >>= k =
    Decoder . Arrow.Decoder SchemaUnknown $ \(o0, a) -> do
      (o1, x) <- run1 (o0, a)
      let Decoder (Arrow.Decoder _ run2) = k x
      run2 (o1, a)

-- | Drop the Monad Decoder back down to an Arrow Decoder.Content.
--
-- See 'DecodeBaseNode' for more information.
withoutSchema :: Decoder o a -> Arrow.Decoder o () a
withoutSchema (Decoder decoder) = decoder

fail :: forall a o. Text -> Decoder o a
fail msg = coerce (Arrow.arr (\() -> msg) Arrow.>>> Arrow.fail @a)

{----- DocumentDecoder -----}

newtype DocumentDecoder a = DocumentDecoder (NodeListDecoder a)

document :: forall a. NodeListDecoder a -> DocumentDecoder a
document = coerce (Arrow.document @a)

documentSchema :: forall a. DocumentDecoder a -> SchemaOf NodeList
documentSchema = coerce (Arrow.documentSchema @a)

{----- NodeListDecoder -----}

type NodeListDecoder = Decoder NodeList

-- | Decode a node with the given name and decoder.
--
-- == __Example__
--
-- @
-- instance KDL.DecodeBaseNode Person where
--   baseNodeTypeAnns _ = ["Person"]
--   baseNodeDecoder = KDL.withoutSchema $ do
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
--     many $ KDL.nodeAt "person"
-- KDL.decodeWith decoder config == Right ["Alice", "Bob", "Charlie", "Danielle"]
-- @
nodeAt :: forall a. (Arrow.DecodeBaseNode a) => Text -> NodeListDecoder a
nodeAt = coerce (Arrow.nodeAt @a)

-- | Same as 'nodeAt', except explicitly specify the 'NodeDecoder' instead of using 'DecodeBaseNode'.
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
--     many . KDL.nodeAtWith "person" . KDL.nodeWith ["Person"] $ KDL.arg
-- KDL.decodeWith decoder config == Right ["Alice", "Bob", "Charlie", "Danielle"]
-- @
nodeAtWith :: forall a. Text -> NodeDecoder a -> NodeListDecoder a
nodeAtWith = coerce (Arrow.nodeAtWith @() @a)

-- | Decode all remaining nodes with the given decoder.
--
-- == __Example__
--
-- @
-- instance KDL.DecodeBaseNode MyArg where
--   baseNodeDecoder = KDL.withoutSchema $ do
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
remainingNodes :: forall a. (Arrow.DecodeBaseNode a) => NodeListDecoder (Map Text [a])
remainingNodes = coerce (Arrow.remainingNodes @a)

-- | Same as 'remainingNodes', except explicitly specify the 'NodeDecoder' instead of using 'DecodeBaseNode'
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
--     KDL.remainingNodes . KDL.nodeWith [] $ KDL.arg
-- KDL.decodeWith decoder config == Right (Map.fromList [("build", ["pkg1", "pkg2"]), ("lint", ["pkg1"])])
-- @
remainingNodesWith :: forall a. NodeDecoder a -> NodeListDecoder (Map Text [a])
remainingNodesWith = coerce (Arrow.remainingNodesWith @() @a)

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
argAt :: forall a. (Arrow.DecodeBaseValue a) => Text -> NodeListDecoder a
argAt = coerce (Arrow.argAt @a)

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
argsAt :: forall a. (Arrow.DecodeBaseValue a) => Text -> NodeListDecoder [a]
argsAt = coerce (Arrow.argsAt @a)

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
dashChildrenAt :: forall a. (Arrow.DecodeBaseValue a) => Text -> NodeListDecoder [a]
dashChildrenAt = coerce (Arrow.dashChildrenAt @a)

-- | Same as 'dashChildrenAt', except explicitly specify the 'ValueDecoder' instead of using 'DecodeBaseValue'
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
--     KDL.dashChildrenAtWith "attendees" $ KDL.valueWith [] KDL.text
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
dashChildrenAtWith :: forall a. (Typeable a) => Text -> ValueDecoder a -> NodeListDecoder [a]
dashChildrenAtWith = coerce (Arrow.dashChildrenAtWith @() @a)

-- | A helper for decoding child values in a list following the KDL convention of being named "-".
--
-- == __Example__
--
-- @
-- instance KDL.DecodeBaseNode Attendee where
--   baseNodeDecoder = KDL.withoutSchema $ do
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
dashNodesAt :: forall a. (Arrow.DecodeBaseNode a) => Text -> NodeListDecoder [a]
dashNodesAt = coerce (Arrow.dashNodesAt @a)

-- | Same as 'dashChildrenAt', except explicitly specify the 'NodeDecoder' instead of using 'DecodeBaseNode'
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
--     KDL.dashNodesAtWith "attendees" $ KDL.nodeWith [] KDL.arg
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
dashNodesAtWith :: forall a. (Typeable a) => Text -> NodeDecoder a -> NodeListDecoder [a]
dashNodesAtWith = coerce (Arrow.dashNodesAtWith @() @a)

{----- NodeDecoder -----}

type NodeDecoder a = Decoder Node a

-- | FIXME: document
node :: forall a. (Arrow.DecodeBaseNode a) => NodeDecoder a
node = coerce (Arrow.node @a)

-- | FIXME: document
nodeWith :: forall a. (Typeable a) => [Text] -> BaseNodeDecoder a -> NodeDecoder a
nodeWith = coerce (Arrow.nodeWith @() @a)

{----- BaseNodeDecoder -----}

type BaseNodeDecoder a = Decoder BaseNode a

-- FIXME: document
arg :: forall a. (Arrow.DecodeBaseValue a) => BaseNodeDecoder a
arg = coerce (Arrow.arg @a)

-- FIXME: document
argWith :: forall a. ValueDecoder a -> BaseNodeDecoder a
argWith = coerce (Arrow.argWith @() @a)

-- | FIXME: document
prop :: forall a. (Arrow.DecodeBaseValue a) => Text -> BaseNodeDecoder a
prop = coerce (Arrow.prop @a)

-- | FIXME: document
propWith :: forall a. Text -> ValueDecoder a -> BaseNodeDecoder a
propWith = coerce (Arrow.propWith @() @a)

-- | FIXME: document
remainingProps :: forall a. (Arrow.DecodeBaseValue a) => BaseNodeDecoder (Map Text a)
remainingProps = coerce (Arrow.remainingProps @a)

-- | FIXME: document
remainingPropsWith :: forall a. ValueDecoder a -> BaseNodeDecoder (Map Text a)
remainingPropsWith = coerce (Arrow.remainingPropsWith @() @a)

-- | FIXME: document
children :: forall a. NodeListDecoder a -> BaseNodeDecoder a
children = coerce (Arrow.children @() @a)

{----- ValueDecoder -----}

type ValueDecoder a = Decoder Value a

-- | FIXME: document
value :: forall a. (Arrow.DecodeBaseValue a) => ValueDecoder a
value = coerce (Arrow.value @a)

-- | FIXME: document
valueWith :: forall a. (Typeable a) => [Text] -> BaseValueDecoder a -> ValueDecoder a
valueWith = coerce (Arrow.valueWith @() @a)

{----- BaseValueDecoder -----}

type BaseValueDecoder a = Decoder BaseValue a
