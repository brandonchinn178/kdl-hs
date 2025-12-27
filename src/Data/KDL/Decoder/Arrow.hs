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
  DecodeError (..),
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

  -- * BaseNode
  BaseNodeDecoder,
  DecodeBaseNode (..),
  arg,
  prop,
  remainingProps,
  children,

  -- ** Explicitly specify ValueDecoder
  argWith,
  propWith,
  remainingPropsWith,

  -- * BaseValue
  BaseValueDecoder,
  DecodeBaseValue (..),
  any,
  text,
  number,
  bool,
  null,
  oneOf,

  -- * Combinators
  many,
  optional,
  option,
  some,

  -- * Internal API
  runDecoder,
  HasDecodeHistory (..),
  DecodeState (..),
) where

import Control.Applicative (
  Alternative (..),
  optional,
 )
import Control.Arrow (Arrow (..), ArrowChoice (..), returnA, (>>>))
import Control.Category (Category)
import Control.Category qualified
import Control.Monad (unless, (>=>))
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Bits (finiteBitSize)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.KDL.Decoder.DecodeM
import Data.KDL.Decoder.Schema (
  Schema (..),
  SchemaItem (..),
  SchemaOf,
  schemaAlt,
  schemaJoin,
 )
import Data.KDL.Parser (parse, parseFile)
import Data.KDL.Types (
  Ann (..),
  BaseNode (..),
  BaseValue (..),
  Document,
  Entry (..),
  Identifier (..),
  Node,
  NodeList (..),
  Value,
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
import Data.Typeable (TypeRep, Typeable, typeRep)
import Debug.Trace (traceM)
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
decodeDocWith (UnsafeDocumentDecoder decoder) doc = runDecodeM $ snd <$> runDecoder decoder (doc, ())

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
  , run :: a -> StateT (DecodeState o) DecodeM b
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

withDecoder :: Decoder o a b -> (b -> DecodeM c) -> Decoder o a c
withDecoder decoder f = decoder >>> liftDecodeM f

runDecoder :: forall o a b. (HasDecodeHistory o) => Decoder o a b -> (o, a) -> DecodeM (DecodeState o, b)
runDecoder decoder (o, a) = swap <$> StateT.runStateT (decoder.run a) initialState
 where
  initialState =
    DecodeState
      { object = o
      , history = emptyDecodeHistory
      }
  swap ~(x, y) = (y, x)

fail :: forall b o. Decoder o Text b
fail = liftDecodeM failM

debug :: forall o a. (Show o) => Decoder o a ()
debug =
  Decoder (SchemaAnd []) $ \_ -> do
    o <- StateT.gets (.object)
    traceM $ "[kdl-hs] DEBUG: " ++ show o

{----- DocumentDecoder -----}

newtype DocumentDecoder a = UnsafeDocumentDecoder (NodeListDecoder () a)

document :: NodeListDecoder () a -> DocumentDecoder a
document decoder =
  UnsafeDocumentDecoder
    decoder
      { run = \() -> do
          a <- decoder.run ()
          Trans.lift . validateNodeList =<< StateT.gets (.object)
          pure a
      }

documentSchema :: DocumentDecoder a -> SchemaOf NodeList
documentSchema (UnsafeDocumentDecoder decoder) = decoder.schema

{----- NodeListDecoder -----}

type NodeListDecoder = Decoder NodeList
instance HasDecodeHistory NodeList where
  data DecodeHistory NodeList = DecodeHistory_NodeList
    { nodesSeen :: Map Text Int
    }
  emptyDecodeHistory = DecodeHistory_NodeList{nodesSeen = Map.empty}

mergeNodeListHistory :: DecodeHistory NodeList -> DecodeHistory NodeList -> DecodeHistory NodeList
mergeNodeListHistory h1 h2 =
  DecodeHistory_NodeList
    { nodesSeen = Map.unionWith (+) h1.nodesSeen h2.nodesSeen
    }

validateNodeList :: NodeList -> DecodeM ()
validateNodeList nodeList = do
  unless (Prelude.null nodeList.nodes) $
    decodeThrow (DecodeError_UnknownNodes nodeList.nodes)

-- | Decode a node with the given name and decoder.
--
-- == __Example__
--
-- @
-- instance KDL.DecodeBaseNode Person where
--   baseNodeTypeAnns _ = ["Person"]
--   baseNodeDecoder = proc () -> do
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
node :: (DecodeBaseNode a) => Text -> NodeListDecoder () a
node name = withDecodeBaseNode $ nodeWith name

-- | Same as 'node', except explicitly specify the 'NodeDecoder' instead of using 'DecodeBaseNode'.
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
--   decoder = KDL.document $ proc () -> do
--     many . KDL.nodeWith "person" . KDL.nodeWith ["Person"] $ KDL.arg -< ()
-- KDL.decodeWith decoder config == Right ["Alice", "Bob", "Charlie", "Danielle"]
-- @
nodeWith :: forall a b. (Typeable b) => Text -> [Text] -> BaseNodeDecoder a b -> NodeListDecoder a b
nodeWith name typeAnns decoder = proc a -> do
  mb <- nodeWithMaybe name typeAnns decoder -< a
  case mb of
    Just b -> returnA -< b
    Nothing -> errorMissing -< ()
 where
  errorMissing =
    Decoder (SchemaAnd []) $ \() -> do
      index <- getIndex
      Trans.lift $ decodeThrow DecodeError_ExpectedNode{name = name, index = index}

  getIndex = Map.findWithDefault 0 name <$> StateT.gets (.history.nodesSeen)

-- | Same as 'nodeWith', except returns Nothing if a node with the given name
-- can't be found, instead of erroring.
nodeWithMaybe :: forall a b. (Typeable b) => Text -> [Text] -> BaseNodeDecoder a b -> NodeListDecoder a (Maybe b)
-- TODO: Detect duplicate `node` calls with the same name and fail to build a decoder
nodeWithMaybe name =
  withNodeDecoder $ \decoder ->
    Decoder (SchemaOne $ NodeNamed name decoder.schema) $ \a -> do
      fmap snd <$> decodeFirstNodeWhere ((== name) . (.obj.name.value)) decoder a

decodeFirstNodeWhere ::
  (Node -> Bool) ->
  NodeDecoder a b ->
  a ->
  StateT (DecodeState NodeList) DecodeM (Maybe (Node, b))
decodeFirstNodeWhere matcher decoder a = do
  nodes <- StateT.gets (.object.nodes)
  case extractFirst matcher nodes of
    Nothing -> do
      pure Nothing
    Just (node_, nodes') -> do
      let name = node_.obj.name
      index <- getIndex name
      b <-
        Trans.lift . addContext ContextNode{name = name, index = index} $
          snd <$> runDecoder decoder (node_, a)
      StateT.modify $ \s ->
        s
          { object = s.object{nodes = nodes'}
          , history = s.history{nodesSeen = inc name.value s.history.nodesSeen}
          }
      pure $ Just (node_, b)
 where
  getIndex name = Map.findWithDefault 0 name.value <$> StateT.gets (.history.nodesSeen)
  inc k = Map.insertWith (+) k 1

-- | Decode all remaining nodes with the given decoder.
--
-- == __Example__
--
-- @
-- instance KDL.DecodeBaseNode MyArg where
--   baseNodeDecoder = proc () -> do
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
remainingNodes :: (DecodeBaseNode a) => NodeListDecoder () (Map Text [a])
remainingNodes = withDecodeBaseNode remainingNodesWith

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
--   decoder = KDL.document $ proc () -> do
--     KDL.remainingNodes . KDL.nodeWith [] $ KDL.arg -< ()
-- KDL.decodeWith decoder config == Right (Map.fromList [("build", ["pkg1", "pkg2"]), ("lint", ["pkg1"])])
-- @
remainingNodesWith :: forall a b. (Typeable b) => [Text] -> BaseNodeDecoder a b -> NodeListDecoder a (Map Text [b])
-- TODO: Detect duplicate `remainingNodes` calls and fail to build a decoder
remainingNodesWith =
  withNodeDecoder $ \decoder ->
    Decoder (SchemaOne $ RemainingNodes decoder.schema) $ \a -> do
      Map.fromListWith (<>) <$> go decoder a
 where
  go decoder a = do
    decodeFirstNodeWhere (const True) decoder a >>= \case
      Nothing -> pure []
      Just (node_, b) -> do
        nodeMap <- go decoder a
        pure $ (node_.obj.name.value, [b]) : nodeMap

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
argAt :: (DecodeBaseValue a) => Text -> NodeListDecoder () a
argAt name = nodeWith name [] arg

-- | A helper to decode all the arguments of the first node with the given name.
-- A utility for nodes that are acting like a key-value store with a list of values.
--
-- This is different from 'many (argAt "foo")', as that would find multiple nodes
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
argsAt :: (DecodeBaseValue a) => Text -> NodeListDecoder () [a]
argsAt name = fmap (fromMaybe []) $ nodeWithMaybe name [] $ many arg

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
dashChildrenAt :: (DecodeBaseValue a) => Text -> NodeListDecoder () [a]
dashChildrenAt name = withDecodeBaseValue $ dashChildrenAtWith name

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
--   decoder = KDL.document $ proc () -> do
--     KDL.dashChildrenAtWith "attendees" $ KDL.valueWith [] KDL.text -< ()
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
dashChildrenAtWith :: forall a b. (Typeable b) => Text -> [Text] -> BaseValueDecoder a b -> NodeListDecoder a [b]
dashChildrenAtWith name typeAnns decoder = dashNodesAtWith name [] $ argWith typeAnns decoder

-- | A helper for decoding child values in a list following the KDL convention of being named "-".
--
-- == __Example__
--
-- @
-- instance KDL.DecodeBaseNode Attendee where
--   baseNodeDecoder = proc () -> do
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
dashNodesAt :: (DecodeBaseNode a) => Text -> NodeListDecoder () [a]
dashNodesAt name = withDecodeBaseNode $ dashNodesAtWith name

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
--   decoder = KDL.document $ proc () -> do
--     KDL.dashNodesAtWith "attendees" $ KDL.nodeWith [] KDL.arg -< ()
-- KDL.decodeWith decoder config == Right ["Alice", "Bob"]
-- @
dashNodesAtWith :: forall a b. (Typeable b) => Text -> [Text] -> BaseNodeDecoder a b -> NodeListDecoder a [b]
dashNodesAtWith name typeAnns decoder =
  fmap (fromMaybe []) . nodeWithMaybe name [] $
    children $
      many (nodeWith "-" typeAnns decoder)

{----- AnnDecoder -----}

annDecoder ::
  forall a b o r.
  (HasDecodeHistory o, Typeable b) =>
  (TypeRep -> [Text] -> SchemaOf o -> SchemaItem (Ann o)) ->
  (Decoder (Ann o) a b -> r) ->
  [Text] ->
  Decoder o a b ->
  r
annDecoder mkSchema k typeAnns decoder =
  k . Decoder (SchemaOne schema) $ \a -> do
    x <- StateT.gets (.object)
    case x.ann of
      Just givenAnn -> do
        let isValidAnn = Prelude.null typeAnns || givenAnn.value `elem` typeAnns
        unless isValidAnn $ do
          Trans.lift $ decodeThrow DecodeError_MismatchedAnn{givenAnn = givenAnn, validAnns = typeAnns}
      _ -> pure ()
    -- TODO: add typeHint to Context
    (decodeState, b) <- Trans.lift $ runDecoder decoder (x.obj, a)
    StateT.modify $ \s -> s{object = s.object{obj = decodeState.object}}
    pure b
 where
  typeHint = typeRep (Proxy @b)
  schema = mkSchema typeHint typeAnns decoder.schema

{----- NodeDecoder -----}

type NodeDecoder = Decoder Node
instance HasDecodeHistory Node where
  data DecodeHistory Node = DecodeHistory_Node
  emptyDecodeHistory = DecodeHistory_Node

-- | FIXME: document
withDecodeBaseNode :: forall a r. (DecodeBaseNode a) => ([Text] -> BaseNodeDecoder () a -> r) -> r
withDecodeBaseNode k = k (baseNodeTypeAnns (Proxy @a)) baseNodeDecoder

-- | FIXME: document
withNodeDecoder :: forall a b r. (Typeable b) => (NodeDecoder a b -> r) -> [Text] -> BaseNodeDecoder a b -> r
withNodeDecoder k typeAnns = annDecoder NodeSchema k typeAnns . validate
 where
  validate decoder =
    decoder
      { run = \a -> do
          b <- decoder.run a
          Trans.lift . validateBaseNode =<< StateT.gets (.object)
          pure b
      }

validateBaseNode :: BaseNode -> DecodeM ()
validateBaseNode baseNode = do
  unless (Prelude.null baseNode.entries) $
    decodeThrow (DecodeError_UnknownEntries baseNode.entries)
  traverse_ validateNodeList baseNode.children

{----- BaseNodeDecoder -----}

type BaseNodeDecoder = Decoder BaseNode
instance HasDecodeHistory BaseNode where
  data DecodeHistory BaseNode = DecodeHistory_BaseNode
    { argsSeen :: Int
    , propsSeen :: Set Identifier
    , childrenHistory :: DecodeHistory NodeList
    }
  emptyDecodeHistory =
    DecodeHistory_BaseNode
      { argsSeen = 0
      , propsSeen = Set.empty
      , childrenHistory = emptyDecodeHistory
      }

-- | FIXME: document
class (Typeable a) => DecodeBaseNode a where
  baseNodeTypeAnns :: Proxy a -> [Text]
  baseNodeTypeAnns _ = []
  baseNodeDecoder :: BaseNodeDecoder () a

instance DecodeBaseNode BaseNode where
  baseNodeDecoder =
    Decoder SchemaUnknown $ \() -> do
      baseNode <- StateT.gets (.object)
      StateT.modify $ \s -> s{object = emptyBaseNode baseNode.name}
      pure baseNode
   where
    emptyBaseNode name =
      BaseNode
        { name = name
        , entries = []
        , children = Nothing
        , format = Nothing
        }

-- FIXME: document
arg :: (DecodeBaseValue a) => BaseNodeDecoder () a
arg = withDecodeBaseValue argWith

-- FIXME: document
argWith :: forall a b. (Typeable b) => [Text] -> BaseValueDecoder a b -> BaseNodeDecoder a b
argWith =
  withValueDecoder $ \decoder ->
    Decoder (SchemaOne $ NodeArg decoder.schema) $ \a -> do
      index <- getIndex

      entries <- StateT.gets (.object.entries)
      (entry, entries') <-
        maybe (Trans.lift $ decodeThrow DecodeError_ExpectedArg{index = index}) pure $
          extractFirst (isNothing . (.name)) entries

      b <-
        Trans.lift . addContext ContextArg{index = index} $
          snd <$> runDecoder decoder (entry.value, a)
      StateT.modify $ \s ->
        s
          { object = s.object{entries = entries'}
          , history = s.history{argsSeen = s.history.argsSeen + 1}
          }
      pure b
 where
  getIndex = StateT.gets (.history.argsSeen)

-- | FIXME: document
prop :: (DecodeBaseValue a) => Text -> BaseNodeDecoder () a
prop name = withDecodeBaseValue $ propWith name

-- | FIXME: document
propWith :: forall a b. (Typeable b) => Text -> [Text] -> BaseValueDecoder a b -> BaseNodeDecoder a b
propWith name =
  withValueDecoder $ \decoder ->
    Decoder (SchemaOne $ NodeProp name decoder.schema) $ \a -> do
      decodeOnePropWhere (== name) decoder a
        >>= maybe (Trans.lift $ decodeThrow DecodeError_ExpectedProp{name = name}) (pure . snd)

decodeOnePropWhere ::
  (Text -> Bool) ->
  ValueDecoder a b ->
  a ->
  StateT (DecodeState BaseNode) DecodeM (Maybe (Identifier, b))
decodeOnePropWhere matcher decoder a = do
  entries <- StateT.gets (.object.entries)
  case findProp entries of
    Nothing -> pure Nothing
    Just (name, prop_, entries') -> do
      b <-
        Trans.lift . addContext ContextProp{name = name} $
          snd <$> runDecoder decoder (prop_.value, a)
      StateT.modify $ \s ->
        s
          { object = s.object{entries = entries'}
          , history = s.history{propsSeen = Set.insert name s.history.propsSeen}
          }
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
remainingProps :: (DecodeBaseValue a) => BaseNodeDecoder () (Map Text a)
remainingProps = withDecodeBaseValue remainingPropsWith

-- | FIXME: document
remainingPropsWith :: forall a b. (Typeable b) => [Text] -> BaseValueDecoder a b -> BaseNodeDecoder a (Map Text b)
remainingPropsWith =
  withValueDecoder $ \decoder ->
    Decoder (SchemaOne $ NodeRemainingProps decoder.schema) $ \a -> do
      Map.fromList <$> go decoder a
 where
  go decoder a =
    decodeOnePropWhere (const True) decoder a >>= \case
      Nothing -> pure []
      Just (name, b) -> do
        propMap <- go decoder a
        pure $ (name.value, b) : propMap

-- | FIXME: document
children :: forall a b. NodeListDecoder a b -> BaseNodeDecoder a b
children decoder =
  Decoder (SchemaOne $ NodeChildren decoder.schema) $ \a -> do
    mChildren <- StateT.gets (.object.children)
    (decodeState, b) <- Trans.lift $ runDecoder decoder (fromMaybe emptyNodeList mChildren, a)
    StateT.modify $ \s ->
      s
        { object = s.object{children = decodeState.object <$ mChildren}
        , history = s.history{childrenHistory = mergeNodeListHistory s.history.childrenHistory decodeState.history}
        }
    pure b
 where
  emptyNodeList = NodeList{nodes = [], format = Nothing}

{----- ValueDecoder -----}

type ValueDecoder = Decoder Value
instance HasDecodeHistory Value where
  data DecodeHistory Value = DecodeHistory_Value
  emptyDecodeHistory = DecodeHistory_Value

withDecodeBaseValue :: forall a r. (DecodeBaseValue a) => ([Text] -> BaseValueDecoder () a -> r) -> r
withDecodeBaseValue k = k (baseValueTypeAnns (Proxy @a)) baseValueDecoder

withValueDecoder :: (Typeable b) => (ValueDecoder a b -> r) -> [Text] -> BaseValueDecoder a b -> r
withValueDecoder = annDecoder ValueSchema

{----- BaseValueDecoder -----}

type BaseValueDecoder = Decoder BaseValue
instance HasDecodeHistory BaseValue where
  data DecodeHistory BaseValue = DecodeHistory_BaseValue
  emptyDecodeHistory = DecodeHistory_BaseValue

-- | FIXME: document
class (Typeable a) => DecodeBaseValue a where
  baseValueTypeAnns :: Proxy a -> [Text]
  baseValueTypeAnns _ = []
  baseValueDecoder :: BaseValueDecoder () a

instance DecodeBaseValue BaseValue where
  baseValueDecoder = any
instance DecodeBaseValue Text where
  baseValueTypeAnns _ = ["string", "text"]
  baseValueDecoder = text
instance DecodeBaseValue Integer where -- FIXME: Add Word8, Int8, ...
  baseValueTypeAnns _ = ["i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "isize", "usize"]
  baseValueDecoder = toInteger <$> baseValueDecoder @Int64
instance DecodeBaseValue Int where
  baseValueTypeAnns _ =
    concat
      [ ["i8", "i16", "isize"]
      , if bits >= 32 then ["i32"] else []
      , if bits >= 64 then ["i64"] else []
      ]
   where
    bits = finiteBitSize (0 :: Int)
  baseValueDecoder = fromIntegral <$> baseValueDecoder @Int64
instance DecodeBaseValue Int64 where
  baseValueTypeAnns _ = ["i64"]
  baseValueDecoder = withDecoder number $ \x -> do
    unless (Scientific.isInteger x) $
      failM $
        "Expected integer, got: " <> (Text.pack . show) x
    maybe (failM $ "Number is too large: " <> (Text.pack . show) x) pure $
      Scientific.toBoundedInteger @Int64 x
instance DecodeBaseValue Scientific where -- FIXME: Add Double, Float, Rational
  baseValueTypeAnns _ = ["f32", "f64", "decimal64", "decimal128"]
  baseValueDecoder = number
instance DecodeBaseValue Bool where
  baseValueTypeAnns _ = ["bool", "boolean"]
  baseValueDecoder = bool
instance (DecodeBaseValue a) => DecodeBaseValue (Maybe a) where
  baseValueTypeAnns _ = baseValueTypeAnns (Proxy @a)
  baseValueDecoder = oneOf [Nothing <$ null, Just <$> baseValueDecoder]

baseValueDecoderPrim :: SchemaOf BaseValue -> (BaseValue -> DecodeM b) -> BaseValueDecoder a b
baseValueDecoderPrim schema f = Decoder schema $ \_ -> Trans.lift . f =<< StateT.gets (.object)

any :: BaseValueDecoder a BaseValue
any = baseValueDecoderPrim (SchemaOr $ map SchemaOne [minBound .. maxBound]) pure

text :: BaseValueDecoder a Text
text = baseValueDecoderPrim (SchemaOne TextSchema) $ \case
  Text s -> pure s
  v -> decodeThrow DecodeError_BaseValueDecodeFail{expectedType = "text", baseValue = v}

number :: BaseValueDecoder a Scientific
number = baseValueDecoderPrim (SchemaOne NumberSchema) $ \case
  Number x -> pure x
  v -> decodeThrow DecodeError_BaseValueDecodeFail{expectedType = "number", baseValue = v}

bool :: BaseValueDecoder a Bool
bool = baseValueDecoderPrim (SchemaOne BoolSchema) $ \case
  Bool x -> pure x
  v -> decodeThrow DecodeError_BaseValueDecodeFail{expectedType = "bool", baseValue = v}

null :: BaseValueDecoder a ()
null = baseValueDecoderPrim (SchemaOne NullSchema) $ \case
  Null -> pure ()
  v -> decodeThrow DecodeError_BaseValueDecodeFail{expectedType = "null", baseValue = v}

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
