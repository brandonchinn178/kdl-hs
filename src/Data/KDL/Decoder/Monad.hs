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
  decodeDocWith,

  -- * Decoder
  Decoder,
  DecodeError (..),
  module Data.KDL.Decoder.DecodeM,
  fail,
  withDecoder,
  debug,

  -- * Decode type classes
  withoutSchema,
  Arrow.DecodeNode (..),
  Arrow.DecodeValue (..),

  -- * Document
  DocumentDecoder,
  document,
  documentSchema,

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

  -- ** Explicitly specify decoders and type anns
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

  -- ** Explicitly specify decoders and type anns
  argWith',
  propWith',
  remainingPropsWith',

  -- * ValueData
  ValueDecoder,
  any,
  text,
  number,
  bool,
  null,
  Arrow.oneOf,

  -- * Combinators
  Arrow.many,
  Arrow.optional,
  Arrow.option,
  Arrow.some,

  -- * Internal API
  Arrow.HasDecodeHistory (..),
  Arrow.DecodeState (..),
) where

import Control.Applicative (Alternative)
import Control.Arrow qualified as Arrow
import Data.Coerce (coerce)
import Data.KDL.Decoder.Arrow qualified as Arrow
import Data.KDL.Decoder.DecodeM
import Data.KDL.Decoder.Schema (
  Schema (..),
  SchemaOf,
 )
import Data.KDL.Types (
  Document,
  Node,
  NodeList, Value,
 )
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Prelude hiding (any, fail, null)
import Data.Scientific (Scientific)

decodeWith :: forall a. DocumentDecoder a -> Text -> Either DecodeError a
decodeWith = coerce (Arrow.decodeWith @a)

decodeFileWith :: forall a. DocumentDecoder a -> FilePath -> IO (Either DecodeError a)
decodeFileWith = coerce (Arrow.decodeFileWith @a)

decodeDocWith :: forall a. DocumentDecoder a -> Document -> Either DecodeError a
decodeDocWith = coerce (Arrow.decodeDocWith @a)

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
    Decoder . Arrow.Decoder SchemaUnknown $ \a -> do
      x <- run1 a
      let Decoder (Arrow.Decoder _ run2) = k x
      run2 a

-- | Drop the Monad Decoder back down to an Arrow Decoder.Content.
--
-- See 'DecodeNode' for more information.
withoutSchema :: Decoder o a -> Arrow.Decoder o () a
withoutSchema (Decoder decoder) = decoder

fail :: forall a o. Text -> Decoder o a
fail msg = coerce (Arrow.arr (\() -> msg) Arrow.>>> Arrow.fail @a)

withDecoder :: forall o a b. Decoder o a -> (a -> DecodeM b) -> Decoder o b
withDecoder = coerce (Arrow.withDecoder @o @() @a @b)

debug :: forall o. (Show o) => Decoder o ()
debug = coerce (Arrow.debug @o @())

{----- Decoding Document -----}

newtype DocumentDecoder a = DocumentDecoder (NodeListDecoder a)

document :: forall a. NodeListDecoder a -> DocumentDecoder a
document = coerce (Arrow.document @a)

documentSchema :: forall a. DocumentDecoder a -> SchemaOf NodeList
documentSchema = coerce (Arrow.documentSchema @a)

{----- Decoding NodeList -----}

type NodeListDecoder = Decoder NodeList

-- | Decode a node with the given name and decoder.
--
-- == __Example__
--
-- @
-- instance KDL.DecodeNode Person where
--   nodeTypeAnns _ = ["Person"]
--   nodeDecoder = KDL.withoutSchema $ do
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

-- | Same as 'node', except explicitly specify the 'NodeDecoder' instead of using 'DecodeNode'.
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
--   nodeDecoder = KDL.withoutSchema $ do
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

-- | Same as 'remainingNodes', except explicitly specify the 'NodeDecoder' instead of using 'DecodeNode'
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

-- | Same as 'argAt', except explicitly specify the 'ValueDecoder' instead of using 'DecodeValue'
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

-- | Same as 'argsAt', except explicitly specify the 'ValueDecoder' instead of using 'DecodeValue'
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

-- | Same as 'dashChildrenAt', except explicitly specify the 'ValueDecoder' instead of using 'DecodeValue'
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
--   nodeDecoder = KDL.withoutSchema $ do
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

-- | Same as 'dashChildrenAt', except explicitly specify the 'NodeDecoder' instead of using 'DecodeNode'
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

-- FIXME: document
arg :: forall a. (Arrow.DecodeValue a) => NodeDecoder a
arg = coerce (Arrow.arg @a)

-- FIXME: document
argWith :: forall a. (Typeable a) => ValueDecoder a -> NodeDecoder a
argWith = coerce (Arrow.argWith @() @a)

-- | Same as 'argWith', except allow specifying type annotations.
argWith' :: forall a. (Typeable a) => [Text] -> ValueDecoder a -> NodeDecoder a
argWith' = coerce (Arrow.argWith' @() @a)

-- | FIXME: document
prop :: forall a. (Arrow.DecodeValue a) => Text -> NodeDecoder a
prop = coerce (Arrow.prop @a)

-- | FIXME: document
propWith :: forall a. (Typeable a) => Text -> ValueDecoder a -> NodeDecoder a
propWith = coerce (Arrow.propWith @() @a)

-- | Same as 'propWith', except allow specifying type annotations.
propWith' :: forall a. (Typeable a) => Text -> [Text] -> ValueDecoder a -> NodeDecoder a
propWith' = coerce (Arrow.propWith' @() @a)

-- | FIXME: document
remainingProps :: forall a. (Arrow.DecodeValue a) => NodeDecoder (Map Text a)
remainingProps = coerce (Arrow.remainingProps @a)

-- | FIXME: document
remainingPropsWith :: forall a. (Typeable a) => ValueDecoder a -> NodeDecoder (Map Text a)
remainingPropsWith = coerce (Arrow.remainingPropsWith @() @a)

-- | Same as 'remainingPropsWith', except allow specifying type annotations.
remainingPropsWith' :: forall a. (Typeable a) => [Text] -> ValueDecoder a -> NodeDecoder (Map Text a)
remainingPropsWith' = coerce (Arrow.remainingPropsWith' @() @a)

-- | FIXME: document
children :: forall a. NodeListDecoder a -> NodeDecoder a
children = coerce (Arrow.children @() @a)

{----- Decoding Value -----}

type ValueDecoder a = Decoder Value a

any :: ValueDecoder Value
any = coerce (Arrow.any @())

text :: ValueDecoder Text
text = coerce (Arrow.text @())

number :: ValueDecoder Scientific
number = coerce (Arrow.number @())

bool :: ValueDecoder Bool
bool = coerce (Arrow.bool @())

null :: ValueDecoder ()
null = coerce (Arrow.null @())
