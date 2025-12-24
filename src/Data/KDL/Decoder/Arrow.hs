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
) where

import Control.Applicative (
  Alternative (..),
  optional,
 )
import Control.Arrow (Arrow (..), ArrowChoice (..), returnA, (>>>))
import Control.Category (Category)
import Control.Category qualified
import Control.Monad (forM, unless, (>=>))
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.KDL.Decoder.DecodeM (
  BaseDecodeError (..),
  DecodeError (..),
  DecodeM,
  decodeThrow,
  failM,
  runDecodeM,
 )
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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Scientific qualified as Scientific
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (TypeRep, Typeable, typeRep)
import Debug.Trace (traceM)
import Prelude hiding (any, fail, null)
import Prelude qualified

decodeWith :: DocumentDecoder a -> Text -> Either DecodeError a
decodeWith decoder = decodeFromParseResult decoder . parse

decodeFileWith :: DocumentDecoder a -> FilePath -> IO (Either DecodeError a)
decodeFileWith decoder = fmap (decodeFromParseResult decoder) . parseFile

decodeFromParseResult :: DocumentDecoder a -> Either Text Document -> Either DecodeError a
decodeFromParseResult (UnsafeDocumentDecoder decoder) = \case
  Left e -> Left . DecodeError [] $ DecodeError_ParseError e
  Right doc -> runDecodeM $ snd <$> decoder.run (doc, ())

{----- Decoder -----}

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
  , run :: (o, a) -> DecodeM (o, b)
  }

instance Category (Decoder o) where
  id = liftDecodeM pure
  Decoder s1 bc . Decoder s2 ab = Decoder (s1 `schemaJoin` s2) $ ab >=> bc
instance Arrow (Decoder o) where
  arr f = liftDecodeM (pure . f)
  Decoder s1 bc *** Decoder s2 bc' =
    Decoder (s1 `schemaJoin` s2) $ \(o0, (b, b')) -> do
      (o1, c) <- bc (o0, b)
      (o2, c') <- bc' (o1, b')
      pure (o2, (c, c'))
instance ArrowChoice (Decoder o) where
  Decoder s1 bc +++ Decoder s2 bc' =
    Decoder (s1 `schemaAlt` s2) $ \case
      (o, Left b) -> (fmap . fmap) Left $ bc (o, b)
      (o, Right b') -> (fmap . fmap) Right $ bc' (o, b')

instance Functor (Decoder o a) where
  fmap f (Decoder schema run) = Decoder schema $ ((fmap . fmap) f . run)
instance Applicative (Decoder o a) where
  pure = arr . const
  Decoder s1 kf <*> Decoder s2 kx =
    Decoder (s1 `schemaJoin` s2) $ \(o0, a) -> do
      (o1, f) <- kf (o0, a)
      (o2, x) <- kx (o1, a)
      pure (o2, f x)
instance Alternative (Decoder o a) where
  empty = Decoder (SchemaOr []) (traverse $ \_ -> empty)
  Decoder s1 run1 <|> Decoder s2 run2 = Decoder (s1 `schemaAlt` s2) $ \x -> run1 x <|> run2 x
  some (Decoder s run) =
    Decoder (SchemaSome s) $
      let go (o0, a) = do
            (o1, x) <- run (o0, a)
            (o2, xs) <- go (o1, a) <|> pure (o1, [])
            pure (o2, x : xs)
       in go
  many (Decoder s run) = empty <|> some (Decoder s run)

liftDecodeM :: (a -> DecodeM b) -> Decoder o a b
liftDecodeM f = Decoder (SchemaAnd []) (traverse f)

withDecoder :: Decoder o a b -> (b -> DecodeM c) -> Decoder o a c
withDecoder decoder f = decoder >>> liftDecodeM f

fail :: forall b o. Decoder o Text b
fail = liftDecodeM failM

debug :: forall o a. (Show o) => Decoder o a ()
debug =
  Decoder (SchemaAnd []) $ \(o, _) -> do
    traceM $ "[kdl-hs] DEBUG: " ++ show o
    pure (o, ())

{----- DocumentDecoder -----}

newtype DocumentDecoder a = UnsafeDocumentDecoder (NodeListDecoder () a)

document :: NodeListDecoder () a -> DocumentDecoder a
document decoder =
  UnsafeDocumentDecoder
    decoder
      { run = \(nodeList, ()) -> do
          (nodeList', a) <- decoder.run (nodeList, ())
          validateNodeList nodeList'
          pure (nodeList', a)
      }

documentSchema :: DocumentDecoder a -> SchemaOf NodeList
documentSchema (UnsafeDocumentDecoder decoder) = decoder.schema

{----- NodeListDecoder -----}

type NodeListDecoder = Decoder NodeList

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
    Nothing -> liftDecodeM (\_ -> decodeThrow $ DecodeError_ExpectedNode name) -< ()

-- | Same as 'nodeWith', except returns Nothing if a node with the given name
-- can't be found, instead of erroring.
nodeWithMaybe :: forall a b. (Typeable b) => Text -> [Text] -> BaseNodeDecoder a b -> NodeListDecoder a (Maybe b)
-- TODO: Detect duplicate `node` calls with the same name and fail to build a decoder
nodeWithMaybe name =
  withNodeDecoder $ \decoder ->
    Decoder (SchemaOne $ NodeNamed name decoder.schema) $ \(nodeList, a) -> do
      case extractFirst ((== name) . (.obj.name.value)) nodeList.nodes of
        Nothing -> pure (nodeList, Nothing)
        Just (node_, nodes) -> do
          (_, b) <- decoder.run (node_, a)
          pure (nodeList{nodes = nodes}, Just b)

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
    Decoder (SchemaOne $ RemainingNodes decoder.schema) $ \(nodeList, a) -> do
      nodeMap <-
        fmap (Map.fromListWith (<>)) . forM nodeList.nodes $ \node_ -> do
          (_, b) <- decoder.run (node_, a)
          pure (node_.obj.name.value, [b])
      pure (nodeList{nodes = []}, nodeMap)

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
  (Typeable b) =>
  (TypeRep -> [Text] -> SchemaOf o -> SchemaItem (Ann o)) ->
  (TypeRep -> Ann o -> BaseDecodeError) ->
  (Decoder (Ann o) a b -> r) ->
  [Text] ->
  Decoder o a b ->
  r
annDecoder mkSchema mkError k typeAnns decoder =
  k . Decoder (SchemaOne schema) $ \(x, a) -> do
    case x.ann of
      Just givenAnn -> do
        let isValidAnn = Prelude.null typeAnns || givenAnn.value `elem` typeAnns
        unless isValidAnn $ do
          decodeThrow
            DecodeError_MismatchedAnn
              { givenAnn = givenAnn.value
              , validAnns = typeAnns
              }
      _ -> pure ()
    -- FIXME: this suppresses error messages; instead, add typeHint to Context
    (obj, b) <- decoder.run (x.obj, a) <|> decodeThrow (mkError typeHint x)
    pure (x{obj = obj}, b)
 where
  typeHint = typeRep (Proxy @b)
  schema = mkSchema typeHint typeAnns decoder.schema

{----- NodeDecoder -----}

type NodeDecoder = Decoder Node

-- | FIXME: document
withDecodeBaseNode :: forall a r. (DecodeBaseNode a) => ([Text] -> BaseNodeDecoder () a -> r) -> r
withDecodeBaseNode k = k (baseNodeTypeAnns (Proxy @a)) baseNodeDecoder

-- | FIXME: document
withNodeDecoder :: forall a b r. (Typeable b) => (NodeDecoder a b -> r) -> [Text] -> BaseNodeDecoder a b -> r
withNodeDecoder k typeAnns = annDecoder NodeSchema DecodeError_NodeDecodeFail k typeAnns . validate
 where
  validate decoder =
    decoder
      { run = \(baseNode, a) -> do
          (baseNode', b) <- decoder.run (baseNode, a)
          validateBaseNode baseNode'
          pure (baseNode', b)
      }

validateBaseNode :: BaseNode -> DecodeM ()
validateBaseNode baseNode = do
  unless (Prelude.null baseNode.entries) $
    decodeThrow (DecodeError_UnknownEntries baseNode.entries)
  traverse_ validateNodeList baseNode.children

{----- BaseNodeDecoder -----}

type BaseNodeDecoder = Decoder BaseNode

-- | FIXME: document
class (Typeable a) => DecodeBaseNode a where
  baseNodeTypeAnns :: Proxy a -> [Text]
  baseNodeTypeAnns _ = []
  baseNodeDecoder :: BaseNodeDecoder () a

-- FIXME: document
arg :: (DecodeBaseValue a) => BaseNodeDecoder () a
arg = withDecodeBaseValue argWith

-- FIXME: document
argWith :: forall a b. (Typeable b) => [Text] -> BaseValueDecoder a b -> BaseNodeDecoder a b
argWith =
  withValueDecoder $ \decoder ->
    Decoder (SchemaOne $ NodeArg decoder.schema) $ \(baseNode, a) -> do
      (entry, entries) <-
        maybe (decodeThrow missingError) pure $
          extractFirst (isNothing . (.name)) baseNode.entries
      (_, b) <- decoder.run (entry.value, a)
      pure (baseNode{entries = entries}, b)
 where
  missingError = DecodeError_ExpectedArg -- tODO: add arg index information

-- | FIXME: document
prop :: (DecodeBaseValue a) => Text -> BaseNodeDecoder () a
prop name = withDecodeBaseValue $ propWith name

-- | FIXME: document
propWith :: forall a b. (Typeable b) => Text -> [Text] -> BaseValueDecoder a b -> BaseNodeDecoder a b
propWith name =
  withValueDecoder $ \decoder ->
    Decoder (SchemaOne $ NodeProp name decoder.schema) $ \(baseNode, a) -> do
      let (props, entries) = partition (\entry -> ((.value) <$> entry.name) == Just name) baseNode.entries
      case NonEmpty.nonEmpty props of
        Nothing -> do
          decodeThrow $ DecodeError_ExpectedProp name
        Just propsNE -> do
          let prop_ = NonEmpty.last propsNE
          (_, b) <- decoder.run (prop_.value, a)
          pure (baseNode{entries = entries}, b)

-- | FIXME: document
remainingProps :: (DecodeBaseValue a) => BaseNodeDecoder () (Map Text a)
remainingProps = withDecodeBaseValue remainingPropsWith

-- | FIXME: document
remainingPropsWith :: forall a b. (Typeable b) => [Text] -> BaseValueDecoder a b -> BaseNodeDecoder a (Map Text b)
remainingPropsWith =
  withValueDecoder $ \decoder ->
    Decoder (SchemaOne $ NodeRemainingProps decoder.schema) $ \(baseNode, a) -> do
      let (props, entries) = partitionEithers $ map getProp baseNode.entries
      result <-
        fmap Map.fromList . forM props $ \(name, prop_) -> do
          (_, b) <- decoder.run (prop_, a)
          pure (name, b)
      pure (baseNode{entries = entries}, result)
 where
  getProp entry =
    case entry.name of
      Just name -> Left (name.value, entry.value)
      _ -> Right entry

-- | FIXME: document
children :: forall a b. NodeListDecoder a b -> BaseNodeDecoder a b
children decoder =
  Decoder (SchemaOne $ NodeChildren decoder.schema) $ \(baseNode, a) -> do
    (children', result) <- decoder.run (fromMaybe emptyNodeList baseNode.children, a)
    pure (baseNode{children = children' <$ baseNode.children}, result)
 where
  emptyNodeList = NodeList{nodes = [], format = Nothing}

{----- ValueDecoder -----}

type ValueDecoder = Decoder Value

withDecodeBaseValue :: forall a r. (DecodeBaseValue a) => ([Text] -> BaseValueDecoder () a -> r) -> r
withDecodeBaseValue k = k (baseValueTypeAnns (Proxy @a)) baseValueDecoder

withValueDecoder :: (Typeable b) => (ValueDecoder a b -> r) -> [Text] -> BaseValueDecoder a b -> r
withValueDecoder = annDecoder ValueSchema DecodeError_ValueDecodeFail

{----- BaseValueDecoder -----}

type BaseValueDecoder = Decoder BaseValue

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
baseValueDecoderPrim schema f = Decoder schema $ \(v, _) -> (v,) <$> f v

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
