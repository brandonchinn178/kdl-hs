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
  Decoder,
  decodeWith,
  decodeFileWith,
  DecodeError (..),
  module Data.KDL.Decoder.DecodeM,

  -- * Document
  DocumentDecoder (..),
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
  DecodeBaseNode (..),
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

  -- * Low-level API
  DecodeArrow (..),
) where

import Control.Applicative (
  Alternative (..),
  optional,
 )
import Control.Arrow (Arrow (..), ArrowChoice (..), (>>>))
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
  fail,
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
import Prelude hiding (any, fail, null)
import Prelude qualified

{----- Decoder entrypoint -----}

-- | @Decoder o a@ represents an action that decodes a KDL object of type @o@ and returns
-- a value of type @a@. It also knows the schema of the given object expected by the decoder.
type Decoder o a = DecodeArrow o () a

decodeWith :: DocumentDecoder a -> Text -> Either DecodeError a
decodeWith decoder = decodeFromParseResult decoder . parse

decodeFileWith :: DocumentDecoder a -> FilePath -> IO (Either DecodeError a)
decodeFileWith decoder = fmap (decodeFromParseResult decoder) . parseFile

decodeFromParseResult :: DocumentDecoder a -> Either Text Document -> Either DecodeError a
decodeFromParseResult (UnsafeDocumentDecoder decoder) = \case
  Left e -> Left . DecodeError [] $ DecodeError_ParseError e
  Right doc -> runDecoder decoder doc

runDecoder :: Decoder o a -> o -> Either DecodeError a
runDecoder decoder a = runDecodeM $ snd <$> decoder.run (a, ())

{----- DecodeArrow -----}

-- | The generic arrow for decoding a KDL document.
--
--   * o - The KDL object being decoded
--   * a - The input of the arrow (usually @()@)
--   * b - The output of the arrow
--
-- We're using arrows here so that we can:
--   1. Get the schema without running the decoder, and also
--   2. Use previously decoded values to inform decoding other values
--
-- Using monads alone would lose (1), but applicatives can't do (2).
data DecodeArrow o a b = DecodeArrow
  { schema :: SchemaOf o
  , run :: (o, a) -> DecodeM (o, b)
  }

instance Category (DecodeArrow o) where
  id = liftDecodeM pure
  DecodeArrow s1 bc . DecodeArrow s2 ab = DecodeArrow (s1 `schemaJoin` s2) $ ab >=> bc
instance Arrow (DecodeArrow o) where
  arr f = liftDecodeM (pure . f)
  DecodeArrow s1 bc *** DecodeArrow s2 bc' =
    DecodeArrow (s1 `schemaJoin` s2) $ \(o0, (b, b')) -> do
      (o1, c) <- bc (o0, b)
      (o2, c') <- bc' (o1, b')
      pure (o2, (c, c'))
instance ArrowChoice (DecodeArrow o) where
  DecodeArrow s1 bc +++ DecodeArrow s2 bc' =
    DecodeArrow (s1 `schemaAlt` s2) $ \case
      (o, Left b) -> (fmap . fmap) Left $ bc (o, b)
      (o, Right b') -> (fmap . fmap) Right $ bc' (o, b')

instance Functor (DecodeArrow o a) where
  fmap f (DecodeArrow schema run) = DecodeArrow schema $ ((fmap . fmap) f . run)
instance (Monoid a) => Applicative (DecodeArrow o a) where
  pure = arr . const
  DecodeArrow s1 kf <*> DecodeArrow s2 kx =
    DecodeArrow (s1 `schemaJoin` s2) $ \(o0, a) -> do
      (o1, f) <- kf (o0, a)
      (o2, x) <- kx (o1, a)
      pure (o2, f x)
instance (Monoid a) => Alternative (DecodeArrow o a) where
  empty = DecodeArrow (SchemaOr []) (traverse $ \_ -> empty)
  DecodeArrow s1 run1 <|> DecodeArrow s2 run2 = DecodeArrow (s1 `schemaAlt` s2) $ \x -> run1 x <|> run2 x
  some (DecodeArrow s run) =
    DecodeArrow (SchemaSome s) $
      let go (o0, a) = do
            (o1, x) <- run (o0, a)
            (o2, xs) <- go (o1, a) <|> pure (o1, [])
            pure (o2, x : xs)
       in go
  many (DecodeArrow s run) = empty <|> some (DecodeArrow s run)

liftDecodeM :: (a -> DecodeM b) -> DecodeArrow o a b
liftDecodeM f = DecodeArrow (SchemaAnd []) (traverse f)

withDecoder :: DecodeArrow o a b -> (b -> DecodeM c) -> DecodeArrow o a c
withDecoder decoder f = decoder >>> liftDecodeM f

{----- DocumentDecoder -----}

newtype DocumentDecoder a = UnsafeDocumentDecoder (NodeListDecoder a)

document :: NodeListDecoder a -> DocumentDecoder a
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

type NodeListDecoder a = Decoder NodeList a

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
--     many $ KDL.nodeAt "person" -< ()
-- KDL.decodeWith decoder config == Right ["Alice", "Bob", "Charlie", "Danielle"]
-- @
nodeAt :: (DecodeBaseNode a) => Text -> NodeListDecoder a
nodeAt name = nodeAtWith name node

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
--   decoder = KDL.document $ proc () -> do
--     many . KDL.nodeAtWith "person" . KDL.nodeWith ["Person"] $ KDL.arg -< ()
-- KDL.decodeWith decoder config == Right ["Alice", "Bob", "Charlie", "Danielle"]
-- @
nodeAtWith :: Text -> NodeDecoder a -> NodeListDecoder a
-- TODO: Detect duplicate `node` calls with the same name and fail to build a decoder
nodeAtWith name = wrap
 where
  wrap decoder =
    DecodeArrow (SchemaOne $ NodeNamed name decoder.schema) $ \(nodeList, ()) -> do
      (node_, nodes) <-
        maybe (decodeThrow $ DecodeError_ExpectedNode name) pure $
          extractFirst ((== name) . (.obj.name.value)) nodeList.nodes
      (_, a) <- decoder.run (node_, ())
      pure (nodeList{nodes = nodes}, a)

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
remainingNodes :: (DecodeBaseNode a) => NodeListDecoder (Map Text [a])
remainingNodes = remainingNodesWith node

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
remainingNodesWith :: NodeDecoder a -> NodeListDecoder (Map Text [a])
-- TODO: Detect duplicate `remainingNodes` calls and fail to build a decoder
remainingNodesWith decoder =
  DecodeArrow (SchemaOne $ RemainingNodes decoder.schema) $ \(nodeList, ()) -> do
    as <-
      fmap (Map.fromListWith (<>)) . forM nodeList.nodes $ \node_ -> do
        (_, a) <- decoder.run (node_, ())
        pure (node_.obj.name.value, [a])
    pure (nodeList{nodes = []}, as)

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
argAt :: (DecodeBaseValue a) => Text -> NodeListDecoder a
argAt name = nodeAtWith name $ nodeWith [] arg

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
--   decoder = KDL.document $ proc () -> do
--     KDL.argsAt "email" -< ()
-- KDL.decodeWith decoder config == Right ["a@example.com", "b@example.com"]
-- @
argsAt :: (DecodeBaseValue a) => Text -> NodeListDecoder [a]
argsAt name = nodeAtWith name $ nodeWith [] $ many arg

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
dashChildrenAt :: (DecodeBaseValue a) => Text -> NodeListDecoder [a]
dashChildrenAt name = dashChildrenAtWith name value

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
dashChildrenAtWith :: (Typeable a) => Text -> ValueDecoder a -> NodeListDecoder [a]
dashChildrenAtWith name decoder = dashNodesAtWith name $ nodeWith [] $ argWith decoder

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
dashNodesAt :: (DecodeBaseNode a) => Text -> NodeListDecoder [a]
dashNodesAt name = dashNodesAtWith name node

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
dashNodesAtWith :: (Typeable a) => Text -> NodeDecoder a -> NodeListDecoder [a]
dashNodesAtWith name decoder =
  option [] . nodeAtWith name . nodeWith [] $
    children $
      many (nodeAtWith "-" decoder)

{----- AnnDecoder -----}

annDecoder ::
  forall a o.
  (Typeable a) =>
  (TypeRep -> [Text] -> SchemaOf o -> SchemaItem (Ann o)) ->
  (TypeRep -> Ann o -> BaseDecodeError) ->
  [Text] ->
  Decoder o a ->
  Decoder (Ann o) a
annDecoder mkSchema mkError typeAnns decoder =
  DecodeArrow (SchemaOne schema) $ \(x, ()) -> do
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
    (obj, a) <- decoder.run (x.obj, ()) <|> decodeThrow (mkError typeHint x)
    pure (x{obj = obj}, a)
 where
  typeHint = typeRep (Proxy @a)
  schema = mkSchema typeHint typeAnns decoder.schema

{----- NodeDecoder -----}

type NodeDecoder a = Decoder Node a

-- | FIXME: document
node :: forall a. (DecodeBaseNode a) => NodeDecoder a
node = nodeWith (baseNodeTypeAnns (Proxy @a)) baseNodeDecoder

-- | FIXME: document
nodeWith :: forall a. (Typeable a) => [Text] -> BaseNodeDecoder a -> NodeDecoder a
nodeWith typeAnns = annDecoder NodeSchema DecodeError_NodeDecodeFail typeAnns . validate
 where
  validate decoder =
    decoder
      { run = \(baseNode, ()) -> do
          (baseNode', a) <- decoder.run (baseNode, ())
          validateBaseNode baseNode'
          pure (baseNode', a)
      }

validateBaseNode :: BaseNode -> DecodeM ()
validateBaseNode baseNode = do
  unless (Prelude.null baseNode.entries) $
    decodeThrow (DecodeError_UnknownEntries baseNode.entries)
  traverse_ validateNodeList baseNode.children

{----- BaseNodeDecoder -----}

type BaseNodeDecoder a = Decoder BaseNode a

-- | FIXME: document
class (Typeable a) => DecodeBaseNode a where
  baseNodeTypeAnns :: Proxy a -> [Text]
  baseNodeTypeAnns _ = []
  baseNodeDecoder :: BaseNodeDecoder a

-- FIXME: document
arg :: (DecodeBaseValue a) => BaseNodeDecoder a
arg = argWith value

-- FIXME: document
argWith :: ValueDecoder a -> BaseNodeDecoder a
argWith decoder =
  DecodeArrow (SchemaOne $ NodeArg decoder.schema) $ \(baseNode, ()) -> do
    (entry, entries) <-
      maybe (decodeThrow missingError) pure $
        extractFirst (isNothing . (.name)) baseNode.entries
    (_, a) <- decoder.run (entry.value, ())
    pure (baseNode{entries = entries}, a)
 where
  missingError = DecodeError_ExpectedArg -- tODO: add arg index information

-- | FIXME: document
prop :: (DecodeBaseValue a) => Text -> BaseNodeDecoder a
prop name = propWith name value

-- | FIXME: document
propWith :: Text -> ValueDecoder a -> BaseNodeDecoder a
propWith name decoder =
  DecodeArrow (SchemaOne $ NodeProp name decoder.schema) $ \(baseNode, ()) -> do
    let (props, entries) = partition (\entry -> ((.value) <$> entry.name) == Just name) baseNode.entries
    case NonEmpty.nonEmpty props of
      Nothing -> do
        decodeThrow $ DecodeError_ExpectedProp name
      Just propsNE -> do
        let prop_ = NonEmpty.last propsNE
        (_, a) <- decoder.run (prop_.value, ())
        pure (baseNode{entries = entries}, a)

-- | FIXME: document
remainingProps :: (DecodeBaseValue a) => BaseNodeDecoder (Map Text a)
remainingProps = remainingPropsWith value

-- | FIXME: document
remainingPropsWith :: ValueDecoder a -> BaseNodeDecoder (Map Text a)
remainingPropsWith decoder =
  DecodeArrow (SchemaOne $ NodeRemainingProps decoder.schema) $ \(baseNode, ()) -> do
    let (props, entries) = partitionEithers $ map getProp baseNode.entries
    as <-
      fmap Map.fromList . forM props $ \(name, prop_) -> do
        (_, a) <- decoder.run (prop_, ())
        pure (name, a)
    pure (baseNode{entries = entries}, as)
 where
  getProp entry =
    case entry.name of
      Just name -> Left (name.value, entry.value)
      _ -> Right entry

-- | FIXME: document
children :: NodeListDecoder a -> BaseNodeDecoder a
children decoder =
  DecodeArrow (SchemaOne $ NodeChildren decoder.schema) $ \(baseNode, ()) -> do
    (children', result) <- decoder.run (fromMaybe emptyNodeList baseNode.children, ())
    pure (baseNode{children = children' <$ baseNode.children}, result)
 where
  emptyNodeList = NodeList{nodes = [], format = Nothing}

{----- ValueDecoder -----}

type ValueDecoder a = Decoder Value a

-- | FIXME: document
value :: forall a. (DecodeBaseValue a) => ValueDecoder a
value = valueWith (baseValueTypeAnns (Proxy @a)) baseValueDecoder

-- | FIXME: document
valueWith :: forall a. (Typeable a) => [Text] -> BaseValueDecoder a -> ValueDecoder a
valueWith = annDecoder ValueSchema DecodeError_ValueDecodeFail

{----- BaseValueDecoder -----}

type BaseValueDecoder a = Decoder BaseValue a

-- | FIXME: document
class (Typeable a) => DecodeBaseValue a where
  baseValueTypeAnns :: Proxy a -> [Text]
  baseValueTypeAnns _ = []
  baseValueDecoder :: BaseValueDecoder a

instance DecodeBaseValue BaseValue where
  baseValueDecoder = any
instance DecodeBaseValue Integer where -- FIXME: Add Word8, Int8, ...
  baseValueTypeAnns _ = ["i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "isize", "usize"]
  baseValueDecoder = toInteger <$> baseValueDecoder @Int64
instance DecodeBaseValue Int64 where
  baseValueTypeAnns _ = ["i64"]
  baseValueDecoder = withDecoder number $ \x -> do
    unless (Scientific.isInteger x) $
      fail $
        "Expected integer, got: " <> (Text.pack . show) x
    maybe (fail $ "Number is too large: " <> (Text.pack . show) x) pure $
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

baseValueDecoderPrim :: SchemaOf BaseValue -> (BaseValue -> DecodeM a) -> BaseValueDecoder a
baseValueDecoderPrim schema f = DecodeArrow schema $ \(v, ()) -> (v,) <$> f v

any :: BaseValueDecoder BaseValue
any = baseValueDecoderPrim (SchemaOr $ map SchemaOne [minBound .. maxBound]) pure

text :: BaseValueDecoder Text
text = baseValueDecoderPrim (SchemaOne TextSchema) $ \case
  Text s -> pure s
  v -> decodeThrow DecodeError_BaseValueDecodeFail{expectedType = "text", baseValue = v}

number :: BaseValueDecoder Scientific
number = baseValueDecoderPrim (SchemaOne NumberSchema) $ \case
  Number x -> pure x
  v -> decodeThrow DecodeError_BaseValueDecodeFail{expectedType = "number", baseValue = v}

bool :: BaseValueDecoder Bool
bool = baseValueDecoderPrim (SchemaOne BoolSchema) $ \case
  Bool x -> pure x
  v -> decodeThrow DecodeError_BaseValueDecodeFail{expectedType = "bool", baseValue = v}

null :: BaseValueDecoder ()
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
