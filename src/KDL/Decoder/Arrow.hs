{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module KDL.Decoder.Arrow (
  decodeWith,
  decodeFileWith,
  decodeDocWith,

  -- * Decoder
  Decoder,
  module KDL.Decoder.Internal.DecodeM,
  fail,
  withDecoder,
  debug,

  -- * Document
  DocumentDecoder (..),
  document,
  documentSchema,

  -- * NodeList
  NodeListDecoder,
  NodeListDecodeArrow,
  node,
  remainingNodes,
  argAt,
  argsAt,
  dashChildrenAt,
  dashNodesAt,

  -- ** Explicitly specify decoders
  nodeWith,
  remainingNodesWith,
  argAtWith,
  argsAtWith,
  dashChildrenAtWith,
  dashNodesAtWith,

  -- ** Explicitly specify decoders and type annotations
  nodeWith',
  remainingNodesWith',
  argAtWith',
  argsAtWith',
  dashChildrenAtWith',

  -- * Node
  NodeDecoder,
  NodeDecodeArrow,
  DecodeNode (..),
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
  ValueDecodeArrow,
  DecodeValue (..),
  any,
  text,
  number,
  bool,
  null,

  -- * Combinators
  oneOf,
  many,
  optional,
  option,
  some,
) where

import Control.Applicative (
  Alternative (..),
  optional,
 )
import Control.Monad (unless, when)
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Bifunctor (first)
import Data.Bits (finiteBitSize)
import Data.Int (Int64)
import Data.List (partition)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Scientific qualified as Scientific
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Int (Int16, Int32, Int8)
import KDL.Decoder.Internal.DecodeM
import KDL.Decoder.Internal.Decoder
import KDL.Decoder.Schema (
  Schema (..),
  SchemaItem (..),
  SchemaOf,
  TypedNodeSchema (..),
  TypedValueSchema (..),
 )
import KDL.Parser (parse, parseFile)
import KDL.Types (
  Ann (..),
  Document,
  Entry (..),
  Identifier (..),
  Node (..),
  NodeList (..),
  Value (..),
  ValueData (..),
 )
import Numeric.Natural (Natural)
import Prelude hiding (any, fail, null)
import Prelude qualified

-- | Decode the given KDL configuration with the given decoder.
decodeWith :: DocumentDecoder a -> Text -> Either DecodeError a
decodeWith decoder = decodeFromParseResult decoder Nothing . parse

-- | Read KDL configuration from the given file path and decode it with the given decoder.
decodeFileWith :: DocumentDecoder a -> FilePath -> IO (Either DecodeError a)
decodeFileWith decoder fp = decodeFromParseResult decoder (Just fp) <$> parseFile fp

-- | Decode an already-parsed 'Document' with the given decoder.
decodeDocWith :: DocumentDecoder a -> Document -> Either DecodeError a
decodeDocWith (UnsafeDocumentDecoder decoder) doc =
  runDecodeM . runDecodeStateM doc emptyDecodeHistory $
    decoder.run ()

decodeFromParseResult ::
  DocumentDecoder a ->
  Maybe FilePath ->
  Either Text Document ->
  Either DecodeError a
decodeFromParseResult decoder mPath =
  first (\e -> e{filepath = mPath}) . \case
    Left e -> runDecodeM . decodeThrow $ DecodeError_ParseError e
    Right doc -> decodeDocWith decoder doc

{----- Decoder -----}

{----- Decoding Document -----}

newtype DocumentDecoder a = UnsafeDocumentDecoder (NodeListDecoder a)

-- | Finalize a 'NodeListDecoder' as a 'DocumentDecoder' to use with 'decodeWith'.
--
-- Ensures that all nodes have been decoded (e.g. error if the user specified
-- unrecognized nodes, or misspelled a node name). To allow unrecognized nodes,
-- use @remainingNodes \@Node@ and ignore the result.
document :: NodeListDecoder a -> DocumentDecoder a
document decoder =
  UnsafeDocumentDecoder
    decoder
      { run = \() -> do
          a <- decoder.run ()
          validateNodeList
          pure a
      }

-- | Get the schema of a 'DocumentDecoder'.
--
-- The schema is statically determined without running the decoder.
documentSchema :: DocumentDecoder a -> SchemaOf NodeList
documentSchema (UnsafeDocumentDecoder decoder) = decoder.schema

{----- Decoding NodeList -----}

type NodeListDecoder a = NodeListDecodeArrow () a
type NodeListDecodeArrow a b = DecodeArrow NodeList a b

getNodeIndex :: Text -> DecodeState NodeList -> Int
getNodeIndex name = Map.findWithDefault 0 name . (.history.nodesSeen)

validateNodeList :: StateT (DecodeState NodeList) DecodeM ()
validateNodeList = do
  nodeList <- StateT.gets (.object)
  case nodeList.nodes of
    [] -> pure ()
    node_ : _ -> do
      let identifier = node_.name
      index <- StateT.gets (getNodeIndex identifier.value)
      Trans.lift . decodeThrow $
        DecodeError_UnexpectedNode
          { identifier = identifier
          , index = index
          }

-- | Decode a node with the given name using a 'DecodeNode' instance.
--
-- Ensures that the node has been fully decoded (e.g. error if the user specified
-- extra args, misspelled a prop name, or provided extraneous children nodes).
-- To allow extra values, use the following functions to parse and ignore them:
--
-- @
-- KDL.many $ KDL.arg \@Value
-- KDL.remainingProps \@Value
-- KDL.children $ KDL.remainingNodes \@Node
-- @
--
-- === __Example__
--
-- @
-- instance KDL.DecodeNode Person where
--   nodeDecoder = proc () -> do
--     name <- KDL.arg -< ()
--     returnA -< Person{..}
--
-- let
--   config =
--     """
--     person \"Alice"
--     person \"Bob"
--     """
--   decoder = KDL.document $ proc () -> do
--     many $ KDL.node "person" -< ()
-- KDL.decodeWith decoder config == Right [\"Alice", \"Bob"]
-- @
node :: (DecodeNode a) => Text -> NodeListDecoder a
node name = withDecodeNode $ nodeWith' name

-- | Same as 'node', except explicitly specify the 'NodeDecoder' instead of using 'DecodeNode'.
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     person \"Alice"
--     person \"Bob"
--     """
--   decoder = KDL.document $ proc () -> do
--     many . KDL.nodeWith "person" $ KDL.arg -< ()
-- KDL.decodeWith decoder config == Right [\"Alice", \"Bob"]
-- @
nodeWith :: forall a b. (Typeable b) => Text -> NodeDecodeArrow a b -> NodeListDecodeArrow a b
nodeWith name = nodeWith' name []

-- | Same as 'nodeWith', except allow specifying type annotations.
nodeWith' :: forall a b. (Typeable b) => Text -> [Text] -> NodeDecodeArrow a b -> NodeListDecodeArrow a b
nodeWith' name =
  withTypedNodeDecoder $ \schema decodeNode ->
    DecodeArrow (SchemaOne $ NodeNamed name schema) $ \a -> do
      decodeFirstNodeWhere ((== name) . (.name.value)) (decodeNode a) >>= \case
        Just (_, b) -> pure b
        Nothing -> do
          index <- StateT.gets (getNodeIndex name)
          Trans.lift $ decodeThrow DecodeError_ExpectedNode{name = name, index = index}

decodeFirstNodeWhere ::
  (Node -> Bool) ->
  (Node -> DecodeM a) ->
  StateT (DecodeState NodeList) DecodeM (Maybe (Node, a))
decodeFirstNodeWhere matcher decodeNode = do
  nodes <- StateT.gets (.object.nodes)
  case extractFirst matcher nodes of
    Nothing -> do
      pure Nothing
    Just (node_, nodes') -> do
      let name = node_.name
      index <- StateT.gets (getNodeIndex name.value)
      StateT.modify $ \s -> s{object = s.object{nodes = nodes'}}
      b <-
        Trans.lift . makeFatal . addContext ContextNode{name = name, index = index} $
          decodeNode node_
      StateT.modify $ \s -> s{history = s.history{nodesSeen = inc name.value s.history.nodesSeen}}
      pure $ Just (node_, b)
 where
  inc k = Map.insertWith (+) k 1

-- | Decode all remaining nodes.
--
-- === __Example__
--
-- @
-- instance KDL.DecodeNode MyArg where
--   nodeDecoder = proc () -> do
--     name <- KDL.arg -< ()
--     returnA -< MyArg{..}
--
-- let
--   config =
--     """
--     build "pkg1"
--     build "pkg2"
--     lint "pkg1"
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.remainingNodes -< ()
-- KDL.decodeWith decoder config == (Right . Map.fromList) [("build", [MyArg "pkg1", MyArg "pkg2"]), ("lint", [MyArg "pkg1"])]
-- @
remainingNodes :: (DecodeNode a) => NodeListDecoder (Map Text [a])
remainingNodes = withDecodeNode remainingNodesWith'

-- | Same as 'remainingNodes', except explicitly specify the 'NodeDecoder' instead of using 'DecodeNode'
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     build "pkg1"
--     build "pkg2"
--     lint "pkg1"
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.remainingNodesWith $ KDL.arg -< ()
-- KDL.decodeWith decoder config == (Right . Map.fromList) [("build", ["pkg1", "pkg2"]), ("lint", ["pkg1"])]
-- @
remainingNodesWith :: forall a b. (Typeable b) => NodeDecodeArrow a b -> NodeListDecodeArrow a (Map Text [b])
-- TODO: Detect duplicate `remainingNodes` calls and fail to build a decoder
remainingNodesWith = remainingNodesWith' []

-- | Same as 'remainingNodesWith', except allow specifying type annotations.
remainingNodesWith' :: forall a b. (Typeable b) => [Text] -> NodeDecodeArrow a b -> NodeListDecodeArrow a (Map Text [b])
-- TODO: Detect duplicate `remainingNodes` calls and fail to build a decoder
remainingNodesWith' =
  withTypedNodeDecoder $ \schema decodeNode ->
    DecodeArrow (SchemaOne $ RemainingNodes schema) $ \a -> do
      Map.fromListWith (<>) <$> go (decodeNode a)
 where
  go decodeNode = do
    decodeFirstNodeWhere (const True) decodeNode >>= \case
      Nothing -> pure []
      Just (node_, b) -> do
        nodeMap <- go decodeNode
        pure $ (node_.name.value, [b]) : nodeMap

-- | A helper to decode the first argument of the first node with the given name.
-- A utility for nodes that are acting like a key-value store.
--
-- > KDL.argAt "my-node" === KDL.node "my-node" KDL.arg
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     verbose #true
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.argAt "verbose" -< ()
-- KDL.decodeWith decoder config == Right True
-- @
argAt :: (DecodeValue a) => Text -> NodeListDecoder a
argAt name = withDecodeValue $ argAtWith' name

-- | Same as 'argAt', except explicitly specify the 'ValueDecoder' instead of using 'DecodeValue'
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     verbose #true
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.argAtWith "verbose" KDL.bool -< ()
-- KDL.decodeWith decoder config == Right True
-- @
argAtWith :: forall a b. (Typeable b) => Text -> ValueDecodeArrow a b -> NodeListDecodeArrow a b
argAtWith name = argAtWith' name []

-- | Same as 'argAtWith', except allow specifying type annotations.
argAtWith' :: forall a b. (Typeable b) => Text -> [Text] -> ValueDecodeArrow a b -> NodeListDecodeArrow a b
argAtWith' name typeAnns decoder = nodeWith name $ argWith' typeAnns decoder

-- | A helper to decode all the arguments of the first node with the given name.
-- A utility for nodes that are acting like a key-value store with a list of values.
--
-- > KDL.argsAt "my-node" === KDL.node "my-node" $ KDL.many KDL.arg
--
-- This is different from @many (argAt "foo")@, as that would find multiple nodes
-- named @"foo"@ and get the first arg from each.
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     email "a@example.com" "b@example.com"
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.argsAt "email" -< ()
-- KDL.decodeWith decoder config == Right ["a@example.com", "b@example.com"]
-- @
argsAt :: (DecodeValue a) => Text -> NodeListDecoder [a]
argsAt name = withDecodeValue $ argsAtWith' name

-- | Same as 'argsAt', except explicitly specify the 'ValueDecoder' instead of using 'DecodeValue'
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     email "a@example.com" "b@example.com"
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.argsAtWith "email" KDL.text -< ()
-- KDL.decodeWith decoder config == Right ["a@example.com", "b@example.com"]
-- @
argsAtWith :: forall a b. (Typeable b) => Text -> ValueDecodeArrow a b -> NodeListDecodeArrow a [b]
argsAtWith name = argsAtWith' name []

-- | Same as 'argsAtWith', except allow specifying type annotations.
argsAtWith' :: forall a b. (Typeable b) => Text -> [Text] -> ValueDecodeArrow a b -> NodeListDecodeArrow a [b]
argsAtWith' name typeAnns decoder = option [] $ nodeWith name $ many $ argWith' typeAnns decoder

-- | A helper for decoding child values in a list following the KDL convention of being named @"-"@.
--
-- > KDL.dashChildrenAt "my-node" === KDL.nodeWith "my-node" $ KDL.children $ KDL.many $ KDL.argAt "-"
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     attendees {
--       - \"Alice"
--       - \"Bob"
--     }
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.dashChildrenAt "attendees" -< ()
-- KDL.decodeWith decoder config == Right [\"Alice", \"Bob"]
-- @
dashChildrenAt :: (DecodeValue a) => Text -> NodeListDecoder [a]
dashChildrenAt name = withDecodeValue $ dashChildrenAtWith' name

-- | Same as 'dashChildrenAt', except explicitly specify the 'ValueDecoder' instead of using 'DecodeValue'
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     attendees {
--       - \"Alice"
--       - \"Bob"
--     }
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.dashChildrenAtWith "attendees" $ KDL.text -< ()
-- KDL.decodeWith decoder config == Right [\"Alice", \"Bob"]
-- @
dashChildrenAtWith :: forall a b. (Typeable b) => Text -> ValueDecodeArrow a b -> NodeListDecodeArrow a [b]
dashChildrenAtWith name = dashChildrenAtWith' name []

-- | Same as 'dashChildrenAtWith', except allow specifying type annotations.
dashChildrenAtWith' :: forall a b. (Typeable b) => Text -> [Text] -> ValueDecodeArrow a b -> NodeListDecodeArrow a [b]
dashChildrenAtWith' name typeAnns decoder = dashNodesAtWith name $ argWith' typeAnns decoder

-- | A helper for decoding child nodes in a list following the KDL convention of being named @"-"@.
--
-- > KDL.dashNodesAt "my-node" === KDL.nodeWith "my-node" $ KDL.children $ KDL.many $ KDL.node "-"
--
-- === __Example__
--
-- @
-- instance KDL.DecodeNode Attendee where
--   nodeDecoder = proc () -> do
--     name <- KDL.arg -< ()
--     returnA -< Attendee{..}
--
-- let
--   config =
--     """
--     attendees {
--       - \"Alice"
--       - \"Bob"
--     }
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.dashNodesAt "attendees" -< ()
-- KDL.decodeWith decoder config == Right [Attendee \"Alice", Attendee \"Bob"]
-- @
dashNodesAt :: (DecodeNode a) => Text -> NodeListDecoder [a]
dashNodesAt name = withDecodeNode $ \_ decoder -> dashNodesAtWith name decoder

-- | Same as 'dashChildrenAt', except explicitly specify the 'NodeDecoder' instead of using 'DecodeNode'
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     attendees {
--       - \"Alice"
--       - \"Bob"
--     }
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.dashNodesAtWith "attendees" KDL.arg -< ()
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
dashNodesAtWith :: forall a b. (Typeable b) => Text -> NodeDecodeArrow a b -> NodeListDecodeArrow a [b]
dashNodesAtWith name decoder =
  option [] . nodeWith name $
    children $
      many (nodeWith "-" decoder)

{----- Decoding Ann -----}

validateAnn :: [Text] -> Maybe Ann -> DecodeM ()
validateAnn typeAnns mGivenAnn =
  case mGivenAnn of
    Nothing -> pure ()
    Just givenAnn -> do
      let isValidAnn = Prelude.null typeAnns || givenAnn.identifier.value `elem` typeAnns
      unless isValidAnn $ do
        decodeThrow DecodeError_MismatchedAnn{givenAnn = givenAnn.identifier, validAnns = typeAnns}

{----- Decoding Node -----}

withDecodeNode :: forall a r. (DecodeNode a) => ([Text] -> NodeDecoder a -> r) -> r
withDecodeNode k = k (validNodeTypeAnns (Proxy @a)) nodeDecoder

withTypedNodeDecoder ::
  forall a b r.
  (Typeable b) =>
  (TypedNodeSchema -> (a -> Node -> DecodeM b) -> r) ->
  ([Text] -> NodeDecodeArrow a b -> r)
withTypedNodeDecoder k typeAnns decoder = k schema decodeNode
 where
  schema =
    TypedNodeSchema
      { typeHint = typeRep (Proxy @b)
      , validTypeAnns = typeAnns
      , nodeSchema = decoder.schema
      }
  decodeNode a node_ = do
    validateAnn typeAnns node_.ann
    runDecodeStateM node_ emptyDecodeHistory $ do
      -- TODO: add typeHint to Context
      decoder.run a <* validateNode

validateNode :: StateT (DecodeState Node) DecodeM ()
validateNode = do
  node_ <- StateT.gets (.object)
  case node_.entries of
    [] -> pure ()
    Entry{name = Nothing, value} : _ -> do
      index <- StateT.gets getArgIndex
      Trans.lift . decodeThrow $
        DecodeError_UnexpectedArg
          { index = index
          , value = value
          }
    Entry{name = Just identifier, value} : _ -> do
      Trans.lift . decodeThrow $
        DecodeError_UnexpectedProp
          { identifier = identifier
          , value = value
          }
  case node_.children of
    Nothing -> pure ()
    Just children_ -> do
      childrenHistory <- StateT.gets (.history.childrenHistory)
      Trans.lift . runDecodeStateM children_ childrenHistory $ validateNodeList

type NodeDecoder a = NodeDecodeArrow () a
type NodeDecodeArrow a b = DecodeArrow Node a b

getArgIndex :: DecodeState Node -> Int
getArgIndex = (.history.argsSeen)

-- | The type class for specifying how a type should be decoded from a KDL node.
class (Typeable a) => DecodeNode a where
  -- | Allowed type annotations for a node of this type.
  --
  -- If specified, nodes with an explicit type annotation MUST match one of the
  -- annotations in this list. Nodes with no type annotations are not checked.
  -- Defaults to @[]@, which means type annotations are ignored.
  --
  -- === __Example__
  --
  -- @
  -- instance DecodeNode Person where
  --   validNodeTypeAnns _ = ["person"]
  -- @
  validNodeTypeAnns :: Proxy a -> [Text]
  validNodeTypeAnns _ = []

  -- | Decode a t'Node' to a value of type @a@
  nodeDecoder :: NodeDecoder a

instance DecodeNode Node where
  nodeDecoder =
    DecodeArrow SchemaUnknown $ \() -> do
      node_ <- StateT.gets (.object)
      StateT.modify $ \s -> s{object = emptyNode node_.name}
      pure node_
   where
    emptyNode name =
      Node
        { ann = Nothing
        , name = name
        , entries = []
        , children = Nothing
        , format = Nothing
        }

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
--   decoder = KDL.document $ proc () -> do
--     KDL.nodeWith "person" $ decodePerson -< ()
--   decodePerson = proc () -> do
--     name <- KDL.arg -< ()
--     vals <- KDL.many KDL.arg -< ()
--     returnA -< (name, vals)
-- KDL.decodeWith decoder config == Right (\"Alice", [1, 2, 3])
-- @
arg :: (DecodeValue a) => NodeDecoder a
arg = withDecodeValue argWith'

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
--   decoder = KDL.document $ proc () -> do
--     KDL.nodeWith "person" $ decodePerson -< ()
--   decodePerson = proc () -> do
--     name \<- KDL.argWith $ Text.toUpper \<$> KDL.text -< ()
--     vals \<- KDL.many $ KDL.argWith $ show \<$> KDL.valueDecoder @Int -< ()
--     returnA -< (name, vals)
-- KDL.decodeWith decoder config == Right (\"ALICE", ["1", "2", "3"])
-- @
argWith :: forall a b. (Typeable b) => ValueDecodeArrow a b -> NodeDecodeArrow a b
argWith = argWith' []

-- | Same as 'argWith', except allow specifying type annotations.
argWith' :: forall a b. (Typeable b) => [Text] -> ValueDecodeArrow a b -> NodeDecodeArrow a b
argWith' =
  withTypedValueDecoder $ \schema decodeValue ->
    DecodeArrow (SchemaOne $ NodeArg schema) $ \a -> do
      index <- StateT.gets getArgIndex

      entries <- StateT.gets (.object.entries)
      (entry, entries') <-
        maybe (Trans.lift $ decodeThrow DecodeError_ExpectedArg{index = index}) pure $
          extractFirst (isNothing . (.name)) entries
      StateT.modify $ \s -> s{object = s.object{entries = entries'}}

      b <-
        Trans.lift . makeFatal . addContext ContextArg{index = index} $
          decodeValue a entry.value
      StateT.modify $ \s -> s{history = s.history{argsSeen = s.history.argsSeen + 1}}
      pure b

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
--   decoder = KDL.document $ proc () -> do
--     KDL.nodeWith "my-node" $ KDL.prop @Int "a" -< ()
-- KDL.decodeWith decoder config == Right 3
-- @
prop :: (DecodeValue a) => Text -> NodeDecoder a
prop name = withDecodeValue $ propWith' name

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
--   decoder = KDL.document $ proc () -> do
--     KDL.nodeWith "my-node" $ KDL.propWith "a" $ show \<$> KDL.number -< ()
-- KDL.decodeWith decoder config == Right "3.0"
-- @
propWith :: forall a b. (Typeable b) => Text -> ValueDecodeArrow a b -> NodeDecodeArrow a b
propWith name = propWith' name []

-- | Same as 'propWith', except allow specifying type annotations.
propWith' :: forall a b. (Typeable b) => Text -> [Text] -> ValueDecodeArrow a b -> NodeDecodeArrow a b
propWith' name =
  withTypedValueDecoder $ \schema decodeValue ->
    DecodeArrow (SchemaOne $ NodeProp name schema) $ \a -> do
      decodeOnePropWhere (== name) (decodeValue a)
        >>= maybe (Trans.lift $ decodeThrow DecodeError_ExpectedProp{name = name}) (pure . snd)

decodeOnePropWhere ::
  (Text -> Bool) ->
  (Value -> DecodeM a) ->
  StateT (DecodeState Node) DecodeM (Maybe (Identifier, a))
decodeOnePropWhere matcher decodeValue = do
  entries <- StateT.gets (.object.entries)
  case findProp entries of
    Nothing -> pure Nothing
    Just (name, prop_, entries') -> do
      StateT.modify $ \s -> s{object = s.object{entries = entries'}}
      b <-
        Trans.lift . makeFatal . addContext ContextProp{name = name} $
          decodeValue prop_.value
      StateT.modify $ \s -> s{history = s.history{propsSeen = Set.insert name s.history.propsSeen}}
      pure $ Just (name, b)
 where
  isPropWhere f entry = (f . (.value) <$> entry.name) == Just True
  findProp entries =
    case break (isPropWhere matcher) entries of
      (entries1, prop0@Entry{name = Just name} : remainingEntries) ->
        -- Collect remaining props with the same name, latter props override earlier props
        let (props, entries2) = partition (isPropWhere (== name.value)) remainingEntries
         in Just (name, NonEmpty.last $ prop0 NonEmpty.:| props, entries1 <> entries2)
      _ ->
        Nothing

-- | Decode all remaining props.
--
-- === __Example__
--
-- @
-- let
--   config =
--     """
--     my-node a=1 b=2 a=3
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.nodeWith "my-node" $ KDL.remainingProps @Int -< ()
-- KDL.decodeWith decoder config == (Right . Map.fromList) [("a", 3), ("b", 2)]
-- @
remainingProps :: (DecodeValue a) => NodeDecoder (Map Text a)
remainingProps = withDecodeValue remainingPropsWith'

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
--   decoder = KDL.document $ proc () -> do
--     KDL.nodeWith "my-node" $ KDL.remainingPropsWith $ show \<$> KDL.number -< ()
-- KDL.decodeWith decoder config == (Right . Map.fromList) [("a", "3.0"), ("b", "2.0")]
-- @
remainingPropsWith :: forall a b. (Typeable b) => ValueDecodeArrow a b -> NodeDecodeArrow a (Map Text b)
remainingPropsWith = remainingPropsWith' []

-- | Same as 'remainingPropsWith', except allow specifying type annotations.
remainingPropsWith' :: forall a b. (Typeable b) => [Text] -> ValueDecodeArrow a b -> NodeDecodeArrow a (Map Text b)
remainingPropsWith' =
  withTypedValueDecoder $ \schema decodeValue ->
    DecodeArrow (SchemaOne $ NodeRemainingProps schema) $ \a -> do
      Map.fromList <$> go (decodeValue a)
 where
  go decodeValue =
    decodeOnePropWhere (const True) decodeValue >>= \case
      Nothing -> pure []
      Just (name, b) -> do
        propMap <- go decodeValue
        pure $ (name.value, b) : propMap

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
--   decoder = KDL.document $ proc () -> do
--     KDL.nodeWith "person" decodePerson -< ()
--   decodePerson = proc () -> do
--     name <- KDL.arg -< ()
--     email <- KDL.children $ KDL.argAt "email" -< ()
--     returnA -< Person{..}
-- KDL.decodeWith decoder config == Right Person{name = \"Alice", email = "alice\@example.com"}
-- @
children :: forall a b. NodeListDecodeArrow a b -> NodeDecodeArrow a b
children decoder =
  DecodeArrow (SchemaOne $ NodeChildren decoder.schema) $ \a -> do
    mChildren <- StateT.gets (.object.children)
    childrenHistory <- StateT.gets (.history.childrenHistory)
    (b, decodeState) <-
      Trans.lift $
        StateT.runStateT (decoder.run a) $
          DecodeState
            { object = fromMaybe emptyNodeList mChildren
            , history = childrenHistory
            }
    StateT.modify $ \s ->
      s
        { object = s.object{children = decodeState.object <$ mChildren}
        , history = s.history{childrenHistory = decodeState.history}
        }
    pure b
 where
  emptyNodeList = NodeList{nodes = [], format = Nothing}

{----- Decoding ValueData -----}

withDecodeValue :: forall a r. (DecodeValue a) => ([Text] -> ValueDecoder a -> r) -> r
withDecodeValue k = k (validValueTypeAnns (Proxy @a)) valueDecoder

withTypedValueDecoder ::
  forall a b r.
  (Typeable b) =>
  (TypedValueSchema -> (a -> Value -> DecodeM b) -> r) ->
  ([Text] -> ValueDecodeArrow a b -> r)
withTypedValueDecoder k typeAnns decoder = k schema decodeValue
 where
  schema =
    TypedValueSchema
      { typeHint = typeRep (Proxy @b)
      , validTypeAnns = typeAnns
      , dataSchema = decoder.schema
      }
  decodeValue a value = do
    validateAnn typeAnns value.ann
    runDecodeStateM value emptyDecodeHistory $ do
      -- TODO: add typeHint to Context
      decoder.run a

type ValueDecoder a = ValueDecodeArrow () a
type ValueDecodeArrow a b = DecodeArrow Value a b

-- | The type class for specifying how a type should be decoded from a KDL value.
class (Typeable a) => DecodeValue a where
  -- | Allowed type annotations for a value of this type.
  --
  -- If specified, values with an explicit type annotation MUST match one of the
  -- annotations in this list. Nodes with no type annotations are not checked.
  -- Defaults to @[]@, which means type annotations are ignored.
  --
  -- === __Example__
  --
  -- @
  -- instance DecodeValue Age where
  --   validValueTypeAnns _ = ["age"]
  -- @
  validValueTypeAnns :: Proxy a -> [Text]
  validValueTypeAnns _ = []

  -- | Decode a t'Value' to a value of type @a@
  --
  -- Helpers that may be useful:
  --
  --   * 'oneOf'
  --   * 'withDecoder'
  --   * 'failM'
  valueDecoder :: ValueDecoder a

instance DecodeValue Value where
  valueDecoder = any
instance DecodeValue ValueData where
  valueDecoder = (.data_) <$> any
instance DecodeValue Text where
  validValueTypeAnns _ = ["text"]
  valueDecoder = text
instance DecodeValue String where
  validValueTypeAnns _ = ["string"]
  valueDecoder = Text.unpack <$> text
instance DecodeValue Bool where
  validValueTypeAnns _ = ["bool", "boolean"]
  valueDecoder = bool
instance (DecodeValue a) => DecodeValue (Maybe a) where
  validValueTypeAnns _ = validValueTypeAnns (Proxy @a)
  valueDecoder = oneOf [Nothing <$ null, Just <$> valueDecoder]
instance (DecodeValue a, DecodeValue b) => DecodeValue (Either a b) where
  validValueTypeAnns _ = validValueTypeAnns (Proxy @a) <> validValueTypeAnns (Proxy @b)
  valueDecoder = oneOf [Left <$> valueDecoder, Right <$> valueDecoder]

decodeInt :: (Integral b, Bounded b) => DecodeArrow Value a b
decodeInt = withDecoder number $ \x -> do
  unless (Scientific.isInteger x) $ do
    failM $ "Expected integer, got: " <> (Text.pack . show) x
  maybe (failM $ "Number doesn't fit bounds: " <> (Text.pack . show) x) pure $
    Scientific.toBoundedInteger x
instance DecodeValue Integer where
  validValueTypeAnns _ =
    concat
      [ ["i8", "i16", "i32", "i64", "i128", "isize"]
      , ["u8", "u16", "u32", "u64", "u128", "usize"]
      ]
  valueDecoder = toInteger <$> decodeInt @Int64
instance DecodeValue Int where
  validValueTypeAnns _ =
    concat
      [ ["i8", "i16", "isize"]
      , if bits >= 32 then ["i32"] else []
      , if bits >= 64 then ["i64"] else []
      ]
   where
    bits = finiteBitSize (0 :: Int)
  valueDecoder = decodeInt
instance DecodeValue Int8 where
  validValueTypeAnns _ = ["i8"]
  valueDecoder = decodeInt
instance DecodeValue Int16 where
  validValueTypeAnns _ = ["i16"]
  valueDecoder = decodeInt
instance DecodeValue Int32 where
  validValueTypeAnns _ = ["i32"]
  valueDecoder = decodeInt
instance DecodeValue Int64 where
  validValueTypeAnns _ = ["i64"]
  valueDecoder = decodeInt
instance DecodeValue Word where
  validValueTypeAnns _ =
    concat
      [ ["u8", "u16", "usize"]
      , if bits >= 32 then ["u32"] else []
      , if bits >= 64 then ["u64"] else []
      ]
   where
    bits = finiteBitSize (0 :: Word)
  valueDecoder = decodeInt
instance DecodeValue Word8 where
  validValueTypeAnns _ = ["u8"]
  valueDecoder = decodeInt
instance DecodeValue Word16 where
  validValueTypeAnns _ = ["u16"]
  valueDecoder = decodeInt
instance DecodeValue Word32 where
  validValueTypeAnns _ = ["u32"]
  valueDecoder = decodeInt
instance DecodeValue Word64 where
  validValueTypeAnns _ = ["u64"]
  valueDecoder = decodeInt
instance DecodeValue Natural where
  validValueTypeAnns _ = ["u8", "u16", "u32", "u64", "usize"]
  valueDecoder = withDecoder (valueDecoder @Integer) $ \x -> do
    when (x < 0) $ do
      failM $ "Expected a non-negative number, got: " <> (Text.pack . show) x
    pure $ fromIntegral x

decodeRealFloat :: (RealFloat b) => DecodeArrow Value a b
decodeRealFloat = withDecoder number $ \x -> do
  either (\_ -> failM $ "Number is too small or too large: " <> (Text.pack . show) x) pure $
    Scientific.toBoundedRealFloat x
instance DecodeValue Scientific where
  validValueTypeAnns _ = ["f32", "f64", "decimal64", "decimal128"]
  valueDecoder = number
instance DecodeValue Float where
  validValueTypeAnns _ = ["f32"]
  valueDecoder = decodeRealFloat
instance DecodeValue Double where
  validValueTypeAnns _ = ["f64"]
  valueDecoder = decodeRealFloat
instance DecodeValue Rational where
  validValueTypeAnns _ = ["decimal64", "decimal128"]
  valueDecoder = withDecoder number $ \x -> do
    -- Use toBoundedRealFloat to guard against large values, but use
    -- toRational after checking to maintain precision
    case Scientific.toBoundedRealFloat @Double x of
      Right _ -> pure $ toRational x
      Left _ -> failM $ "Number is too small or too large: " <> (Text.pack . show) x

valueDataDecoderPrim :: SchemaOf Value -> (Value -> DecodeM b) -> DecodeArrow Value a b
valueDataDecoderPrim schema f = DecodeArrow schema $ \_ -> Trans.lift . f =<< StateT.gets (.object)

-- | Decode any value, without any possibility of failure.
any :: DecodeArrow Value a Value
any = valueDataDecoderPrim (SchemaOr $ map SchemaOne [minBound .. maxBound]) pure

-- | Decode a KDL text value.
text :: DecodeArrow Value a Text
text = valueDataDecoderPrim (SchemaOne TextSchema) $ \case
  Value{data_ = Text s} -> pure s
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "text", value = v}

-- | Decode a KDL number value.
number :: DecodeArrow Value a Scientific
number = valueDataDecoderPrim (SchemaOne NumberSchema) $ \case
  Value{data_ = Number x} -> pure x
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "number", value = v}

-- | Decode a KDL bool value.
bool :: DecodeArrow Value a Bool
bool = valueDataDecoderPrim (SchemaOne BoolSchema) $ \case
  Value{data_ = Bool x} -> pure x
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "bool", value = v}

-- | Decode a KDL null value.
null :: DecodeArrow Value a ()
null = valueDataDecoderPrim (SchemaOne NullSchema) $ \case
  Value{data_ = Null} -> pure ()
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "null", value = v}

{----- Utilities -----}

-- | Return the first result that succeeds.
--
-- > oneOf [a, b, c] === a <|> b <|> c <|> empty
oneOf :: (Alternative f) => [f a] -> f a
oneOf = foldr (<|>) empty

-- | Return the given default value if the given action fails.
--
-- > option a f === f <|> pure a
option :: (Alternative f) => a -> f a -> f a
option a f = f <|> pure a

extractFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
extractFirst f = go
 where
  go = \case
    [] -> Nothing
    x : xs ->
      if f x
        then Just (x, xs)
        else fmap (x :) <$> go xs
