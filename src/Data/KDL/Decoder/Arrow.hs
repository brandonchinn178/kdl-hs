{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-|
This module defines the Arrow interface for decoding a KDL document. Intended to
be imported qualified as:

> import Data.KDL.Decoder.Arrow qualified as KDL

For most use-cases, the Monad interface exported by Data.KDL is sufficient. You
may wish to use the Arrow interface if you would like to statically analyze a
decoder's schema, e.g. to generate documentation.

=== Quickstart

FIXME: quickstart
-}
module Data.KDL.Decoder.Arrow (
  decodeWith,
  decodeFileWith,
  decodeDocWith,

  -- * Decoder
  Decoder (..),
  module Data.KDL.Decoder.DecodeM,
  fail,
  withDecoder,
  debug,

  -- * Document
  DocumentDecoder (..),
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

  -- ** Explicitly specify NodeDecoder
  nodeWith,
  remainingNodesWith,
  dashChildrenAtWith,
  dashNodesAtWith,

  -- ** Explicitly specify ValueDecoder
  argAtWith,
  argsAtWith,

  -- * Node
  NodeDecoder,
  DecodeNode (..),
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

  -- * Internal API
  runDecodeStateM,
  HasDecodeHistory (..),
  DecodeState (..),
) where

import Control.Applicative (
  Alternative (..),
  optional,
 )
import Control.Arrow (Arrow (..), ArrowChoice (..), (>>>))
import Control.Category (Category)
import Control.Category qualified
import Control.Monad (unless, when, (>=>))
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Bits (finiteBitSize)
import Data.Int (Int64)
import Data.KDL.Decoder.DecodeM
import Data.KDL.Decoder.Schema (
  Schema (..),
  SchemaItem (..),
  SchemaOf,
  TypedNodeSchema (..),
  TypedValueSchema (..),
  schemaAlt,
  schemaJoin,
 )
import Data.KDL.Parser (parse, parseFile)
import Data.KDL.Types (
  Ann (..),
  Document,
  Entry (..),
  Identifier (..),
  Node (..),
  NodeList (..),
  Value (..),
  ValueData (..),
 )
import Data.List (partition)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Scientific qualified as Scientific
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable, typeRep)
import Data.Word (Word16, Word32, Word64, Word8)
import Debug.Trace (traceM)
import GHC.Int (Int16, Int32, Int8)
import Numeric.Natural (Natural)
import Prelude hiding (any, fail, null)
import Prelude qualified

-- | FIXME: document
decodeWith :: DocumentDecoder a -> Text -> Either DecodeError a
decodeWith decoder = decodeFromParseResult decoder . parse

-- | FIXME: document
decodeFileWith :: DocumentDecoder a -> FilePath -> IO (Either DecodeError a)
decodeFileWith decoder = fmap (decodeFromParseResult decoder) . parseFile

-- | FIXME: document
decodeDocWith :: DocumentDecoder a -> Document -> Either DecodeError a
decodeDocWith (UnsafeDocumentDecoder decoder) doc =
  runDecodeM . runDecodeStateM doc emptyDecodeHistory $
    decoder.run ()

decodeFromParseResult :: DocumentDecoder a -> Either Text Document -> Either DecodeError a
decodeFromParseResult decoder = \case
  Left e -> runDecodeM . decodeThrow $ DecodeError_ParseError e
  Right doc -> decodeDocWith decoder doc

{----- Decoder -----}

class HasDecodeHistory o where
  data DecodeHistory o
  emptyDecodeHistory :: DecodeHistory o

-- | The state to track when decoding an object of type @o@.
--
-- At each decode step, some value within @o@ is consumed and
-- the action is recorded in the history.
data DecodeState o = DecodeState
  { object :: !o
  , history :: DecodeHistory o
  -- ^ Not strict, since this only matters for reporting errors
  }

type DecodeStateM o a = StateT (DecodeState o) DecodeM a

-- | @Decoder o a b@ represents an arrow with input @a@ and output @b@, within
-- the context of decoding a KDL object of type @o@. It also knows the expected
-- schema of @o@.
--
-- We're using arrows here so that we can:
--   1. Get the schema without running the decoder, and also
--   2. Use previously decoded values to inform decoding other values
--
-- Using monads alone would lose (1), but applicatives can't do (2).
data Decoder o a b = Decoder
  { schema :: SchemaOf o
  , run :: a -> DecodeStateM o b
  }

instance Category (Decoder o) where
  id = liftDecodeM pure
  Decoder sch1 bc . Decoder sch2 ab = Decoder (sch1 `schemaJoin` sch2) $ ab >=> bc
instance Arrow (Decoder o) where
  arr f = liftDecodeM (pure . f)
  Decoder sch1 bc *** Decoder sch2 bc' =
    Decoder (sch1 `schemaJoin` sch2) $ \(b, b') -> (,) <$> bc b <*> bc' b'
instance ArrowChoice (Decoder o) where
  Decoder sch1 bc +++ Decoder sch2 bc' =
    Decoder (sch1 `schemaAlt` sch2) $ either (fmap Left . bc) (fmap Right . bc')

instance Functor (Decoder o a) where
  fmap f (Decoder schema run) = Decoder schema $ (fmap f . run)
instance Applicative (Decoder o a) where
  pure = arr . const
  Decoder sch1 kf <*> Decoder sch2 kx =
    Decoder (sch1 `schemaJoin` sch2) $ \a -> kf a <*> kx a
instance Alternative (Decoder o a) where
  -- Can't use StateT's Alternative instance: https://hub.darcs.net/ross/transformers/issue/78
  empty = Decoder (SchemaOr []) $ \_ -> Trans.lift empty
  Decoder sch1 run1 <|> Decoder sch2 run2 =
    Decoder (sch1 `schemaAlt` sch2) $ \a -> StateT.StateT $ \s -> do
      StateT.runStateT (run1 a) s <|> StateT.runStateT (run2 a) s
  some (Decoder sch run) =
    Decoder (SchemaSome sch) $ \a ->
      StateT.StateT $
        let go s0 = do
              (x, s1) <- StateT.runStateT (run a) s0
              (xs, s2) <- go s1 <|> pure ([], s1)
              pure (x : xs, s2)
         in go
  many (Decoder sch run) = some (Decoder sch run) <|> pure []

liftDecodeM :: (a -> DecodeM b) -> Decoder o a b
liftDecodeM f = Decoder (SchemaAnd []) (Trans.lift . f)

withDecoder :: forall o a b c. Decoder o a b -> (b -> DecodeM c) -> Decoder o a c
withDecoder decoder f = decoder >>> liftDecodeM f

runDecodeStateM :: o -> DecodeHistory o -> DecodeStateM o a -> DecodeM a
runDecodeStateM o hist m =
  StateT.evalStateT m $
    DecodeState
      { object = o
      , history = hist
      }

fail :: forall b o. Decoder o Text b
fail = liftDecodeM failM

debug :: forall o a. (Show o) => Decoder o a ()
debug =
  Decoder (SchemaAnd []) $ \_ -> do
    o <- StateT.gets (.object)
    traceM $ "[kdl-hs] DEBUG: " ++ show o

{----- Decoding Document -----}

newtype DocumentDecoder a = UnsafeDocumentDecoder (NodeListDecoder () a)

document :: NodeListDecoder () a -> DocumentDecoder a
document decoder =
  UnsafeDocumentDecoder
    decoder
      { run = \() -> do
          a <- decoder.run ()
          validateNodeList
          pure a
      }

documentSchema :: DocumentDecoder a -> SchemaOf NodeList
documentSchema (UnsafeDocumentDecoder decoder) = decoder.schema

{----- Decoding NodeList -----}

type NodeListDecoder = Decoder NodeList
instance HasDecodeHistory NodeList where
  data DecodeHistory NodeList = DecodeHistory_NodeList
    { nodesSeen :: Map Text Int
    }
  emptyDecodeHistory = DecodeHistory_NodeList{nodesSeen = Map.empty}

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

-- | Decode a node with the given name and decoder.
--
-- == __Example__
--
-- @
-- instance KDL.DecodeNode Person where
--   validNodeTypeAnns _ = ["Person"]
--   nodeDecoder = proc () -> do
--     name <- KDL.arg -< ()
--     returnA -< Person{..}
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
--   decoder = KDL.document $ proc () -> do
--     many $ KDL.node "person" -< ()
-- KDL.decodeWith decoder config == Right ["Alice", "Bob", "Charlie", "Danielle"]
-- @
node :: (DecodeNode a) => Text -> NodeListDecoder () a
node name = withDecodeNode $ nodeWith name

-- | Same as 'node', except explicitly specify the 'NodeDecoder' instead of using 'DecodeNode'.
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     person "Alice"
--     (Person)person "Bob"
--     """
--   decoder = KDL.document $ proc () -> do
--     many . KDL.nodeWith "person" ["Person"] $ KDL.arg -< ()
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
nodeWith :: forall a b. (Typeable b) => Text -> [Text] -> NodeDecoder a b -> NodeListDecoder a b
nodeWith name =
  withTypedNodeDecoder $ \schema decodeNode ->
    Decoder (SchemaOne $ NodeNamed name schema) $ \a -> do
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

-- | Decode all remaining nodes with the given decoder.
--
-- == __Example__
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
-- KDL.decodeWith decoder config == Right (Map.fromList [("build", [MyArg "pkg1", MyArg "pkg2"]), ("lint", [MyArg "pkg1"])])
-- @
remainingNodes :: (DecodeNode a) => NodeListDecoder () (Map Text [a])
remainingNodes = withDecodeNode remainingNodesWith

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
--   decoder = KDL.document $ proc () -> do
--     KDL.remainingNodesWith [] KDL.arg -< ()
-- KDL.decodeWith decoder config == Right (Map.fromList [("build", ["pkg1", "pkg2"]), ("lint", ["pkg1"])])
-- @
remainingNodesWith :: forall a b. (Typeable b) => [Text] -> NodeDecoder a b -> NodeListDecoder a (Map Text [b])
-- TODO: Detect duplicate `remainingNodes` calls and fail to build a decoder
remainingNodesWith =
  withTypedNodeDecoder $ \schema decodeNode ->
    Decoder (SchemaOne $ RemainingNodes schema) $ \a -> do
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
-- == __Example__
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
argAt :: (DecodeValue a) => Text -> NodeListDecoder () a
argAt name = withDecodeValue $ argAtWith name

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
--   decoder = KDL.document $ proc () -> do
--     KDL.argAtWith "verbose" [] KDL.bool -< ()
-- KDL.decodeWith decoder config == Right True
-- @
argAtWith :: forall a b. (Typeable b) => Text -> [Text] -> ValueDecoder a b -> NodeListDecoder a b
argAtWith name typeAnns decoder = nodeWith name [] $ argWith typeAnns decoder

-- | A helper to decode all the arguments of the first node with the given name.
-- A utility for nodes that are acting like a key-value store with a list of values.
--
-- This is different from @many (argAt "foo")@, as that would find multiple nodes
-- named "foo" and get the first arg from each.
--
-- == __Example__
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
argsAt :: (DecodeValue a) => Text -> NodeListDecoder () [a]
argsAt name = withDecodeValue $ argsAtWith name

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
--   decoder = KDL.document $ proc () -> do
--     KDL.argsAtWith "email" [] KDL.text -< ()
-- KDL.decodeWith decoder config == Right ["a@example.com", "b@example.com"]
-- @
argsAtWith :: forall a b. (Typeable b) => Text -> [Text] -> ValueDecoder a b -> NodeListDecoder a [b]
argsAtWith name typeAnns decoder = option [] $ nodeWith name [] $ many $ argWith typeAnns decoder

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
--   decoder = KDL.document $ proc () -> do
--     KDL.dashChildrenAt "attendees" -< ()
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
dashChildrenAt :: (DecodeValue a) => Text -> NodeListDecoder () [a]
dashChildrenAt name = withDecodeValue $ dashChildrenAtWith name

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
--   decoder = KDL.document $ proc () -> do
--     KDL.dashChildrenAtWith "attendees" [] KDL.text -< ()
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
dashChildrenAtWith :: forall a b. (Typeable b) => Text -> [Text] -> ValueDecoder a b -> NodeListDecoder a [b]
dashChildrenAtWith name typeAnns decoder = dashNodesAtWith name $ argWith typeAnns decoder

-- | A helper for decoding child values in a list following the KDL convention of being named "-".
--
-- == __Example__
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
--       - "Alice"
--       - "Bob"
--     }
--     """
--   decoder = KDL.document $ proc () -> do
--     KDL.dashNodesAt "attendees" -< ()
-- KDL.decodeWith decoder config == Right [Attendee "Alice", Attendee "Bob"]
-- @
dashNodesAt :: (DecodeNode a) => Text -> NodeListDecoder () [a]
dashNodesAt name = withDecodeNode $ \_ decoder -> dashNodesAtWith name decoder

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
--   decoder = KDL.document $ proc () -> do
--     KDL.dashNodesAtWith "attendees" [] KDL.arg -< ()
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
dashNodesAtWith :: forall a b. (Typeable b) => Text -> NodeDecoder a b -> NodeListDecoder a [b]
dashNodesAtWith name decoder =
  option [] . nodeWith name [] $
    children $
      many (nodeWith "-" [] decoder)

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

-- | FIXME: document
withDecodeNode :: forall a r. (DecodeNode a) => ([Text] -> NodeDecoder () a -> r) -> r
withDecodeNode k = k (validNodeTypeAnns (Proxy @a)) nodeDecoder

-- | FIXME: document
withTypedNodeDecoder ::
  forall a b r.
  (Typeable b) =>
  (TypedNodeSchema -> (a -> Node -> DecodeM b) -> r) ->
  ([Text] -> NodeDecoder a b -> r)
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

type NodeDecoder = Decoder Node
instance HasDecodeHistory Node where
  data DecodeHistory Node = DecodeHistory_Node
    { argsSeen :: Int
    , propsSeen :: Set Identifier
    , childrenHistory :: DecodeHistory NodeList
    }
  emptyDecodeHistory =
    DecodeHistory_Node
      { argsSeen = 0
      , propsSeen = Set.empty
      , childrenHistory = emptyDecodeHistory
      }

getArgIndex :: DecodeState Node -> Int
getArgIndex = (.history.argsSeen)

-- | FIXME: document
class (Typeable a) => DecodeNode a where
  validNodeTypeAnns :: Proxy a -> [Text]
  validNodeTypeAnns _ = []
  nodeDecoder :: NodeDecoder () a

instance DecodeNode Node where
  nodeDecoder =
    Decoder SchemaUnknown $ \() -> do
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

-- FIXME: document
arg :: (DecodeValue a) => NodeDecoder () a
arg = withDecodeValue argWith

-- FIXME: document
argWith :: forall a b. (Typeable b) => [Text] -> ValueDecoder a b -> NodeDecoder a b
argWith =
  withTypedValueDecoder $ \schema decodeValue ->
    Decoder (SchemaOne $ NodeArg schema) $ \a -> do
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

-- | FIXME: document
prop :: (DecodeValue a) => Text -> NodeDecoder () a
prop name = withDecodeValue $ propWith name

-- | FIXME: document
propWith :: forall a b. (Typeable b) => Text -> [Text] -> ValueDecoder a b -> NodeDecoder a b
propWith name =
  withTypedValueDecoder $ \schema decodeValue ->
    Decoder (SchemaOne $ NodeProp name schema) $ \a -> do
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

-- | FIXME: document
remainingProps :: (DecodeValue a) => NodeDecoder () (Map Text a)
remainingProps = withDecodeValue remainingPropsWith

-- | FIXME: document
remainingPropsWith :: forall a b. (Typeable b) => [Text] -> ValueDecoder a b -> NodeDecoder a (Map Text b)
remainingPropsWith =
  withTypedValueDecoder $ \schema decodeValue ->
    Decoder (SchemaOne $ NodeRemainingProps schema) $ \a -> do
      Map.fromList <$> go (decodeValue a)
 where
  go decodeValue =
    decodeOnePropWhere (const True) decodeValue >>= \case
      Nothing -> pure []
      Just (name, b) -> do
        propMap <- go decodeValue
        pure $ (name.value, b) : propMap

-- | FIXME: document
children :: forall a b. NodeListDecoder a b -> NodeDecoder a b
children decoder =
  Decoder (SchemaOne $ NodeChildren decoder.schema) $ \a -> do
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

withDecodeValue :: forall a r. (DecodeValue a) => ([Text] -> ValueDecoder () a -> r) -> r
withDecodeValue k = k (validValueTypeAnns (Proxy @a)) valueDecoder

withTypedValueDecoder ::
  forall a b r.
  (Typeable b) =>
  (TypedValueSchema -> (a -> Value -> DecodeM b) -> r) ->
  ([Text] -> ValueDecoder a b -> r)
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

type ValueDecoder = Decoder Value

instance HasDecodeHistory Value where
  data DecodeHistory Value = DecodeHistory_Value
  emptyDecodeHistory = DecodeHistory_Value

-- | FIXME: document
class (Typeable a) => DecodeValue a where
  validValueTypeAnns :: Proxy a -> [Text]
  validValueTypeAnns _ = []
  valueDecoder :: ValueDecoder () a

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

decodeInt :: (Integral b, Bounded b) => ValueDecoder a b
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

decodeRealFloat :: (RealFloat b) => ValueDecoder a b
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

valueDataDecoderPrim :: SchemaOf Value -> (Value -> DecodeM b) -> ValueDecoder a b
valueDataDecoderPrim schema f = Decoder schema $ \_ -> Trans.lift . f =<< StateT.gets (.object)

any :: ValueDecoder a Value
any = valueDataDecoderPrim (SchemaOr $ map SchemaOne [minBound .. maxBound]) pure

text :: ValueDecoder a Text
text = valueDataDecoderPrim (SchemaOne TextSchema) $ \case
  Value{data_ = Text s} -> pure s
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "text", value = v}

number :: ValueDecoder a Scientific
number = valueDataDecoderPrim (SchemaOne NumberSchema) $ \case
  Value{data_ = Number x} -> pure x
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "number", value = v}

bool :: ValueDecoder a Bool
bool = valueDataDecoderPrim (SchemaOne BoolSchema) $ \case
  Value{data_ = Bool x} -> pure x
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "bool", value = v}

null :: ValueDecoder a ()
null = valueDataDecoderPrim (SchemaOne NullSchema) $ \case
  Value{data_ = Null} -> pure ()
  v -> decodeThrow DecodeError_ValueDecodeFail{expectedType = "null", value = v}

{----- Utilities -----}

oneOf :: (Alternative f) => [f a] -> f a
oneOf = foldr (<|>) empty

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
