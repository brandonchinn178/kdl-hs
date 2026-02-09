{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

{-|
Defines the types that make up a KDL document.

This module enables @-XNoFieldSelectors@, so none of the fields create implicit
selector functions. Instead, use @-XOverloadedRecordDot@, or the functions
provided by this module.
-}
module KDL.Types (
  -- * Document
  Document,
  docNodes,

  -- * NodeList
  NodeList (..),
  NodeListFormat (..),
  fromNodeList,
  nodeListFormat,

  -- ** Helpers
  filterNodes,
  lookupNode,
  getArgAt,
  getArgsAt,
  getDashChildrenAt,
  getDashNodesAt,

  -- * Node
  Node (..),
  NodeFormat (..),
  nodeAnn,
  nodeName,
  nodeEntries,
  nodeChildren,
  nodeFormat,

  -- ** Helpers
  getArgs,
  getArg,
  getProps,
  getProp,

  -- * Entry
  Entry (..),
  EntryFormat (..),
  entryName,
  entryValue,
  entryFormat,

  -- * Value
  Value (..),
  ValueFormat (..),
  valueAnn,
  valueData,
  valueFormat,
  ValueData (..),

  -- * Ann
  Ann (..),
  AnnFormat (..),
  annIdentifier,
  annFormat,

  -- * Identifier
  Identifier (..),
  IdentifierFormat (..),
  fromIdentifier,
  identifierFormat,
  toIdentifier,
) where

import Control.Monad ((<=<))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)

{----- Document -----}

type Document = NodeList

docNodes :: Document -> [Node]
docNodes = fromNodeList

{----- NodeList -----}

data NodeList = NodeList
  { nodes :: [Node]
  , format :: Maybe NodeListFormat
  }
  deriving (Show, Eq)

data NodeListFormat = NodeListFormat
  { leading :: Text
  -- ^ Whitespace and comments preceding the first node.
  , trailing :: Text
  -- ^ Whitespace and comments following the last node.
  }
  deriving (Show, Eq)

fromNodeList :: NodeList -> [Node]
fromNodeList = (.nodes)

nodeListFormat :: NodeList -> Maybe NodeListFormat
nodeListFormat = (.format)

-- | A helper to get all nodes with the given name
filterNodes :: Text -> NodeList -> [Node]
filterNodes name = filter ((== name) . (.name.value)) . (.nodes)

-- | A helper to get the first node with the given name
lookupNode :: Text -> NodeList -> Maybe Node
lookupNode name = listToMaybe . filterNodes name

-- | A helper to get the first argument of the first node with the given name.
-- A utility for nodes that are acting like a key-value store.
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     foo 1
--     """
-- Right doc <- pure $ parse config
-- getArgAt "foo" doc == Just (Number 1)
-- @
getArgAt :: Text -> NodeList -> Maybe Value
getArgAt name = listToMaybe . getArgsAt name

-- | A helper to get all the arguments of the first node with the given name.
-- A utility for nodes that are acting like a key-value store with a list of values.
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     foo 1 2 "test"
--     """
-- Right doc <- pure $ parse config
-- getArgsAt "foo" doc == [Number 1, Number 2, Text "test"]
-- @
getArgsAt :: Text -> NodeList -> [Value]
getArgsAt name = maybe [] getArgs . lookupNode name

-- | A helper for getting child values following the KDL convention of being named "-".
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     foo {
--       - 1
--       - 2
--       - "test"
--     }
--     """
-- Right doc <- pure $ parse config
-- getDashChildrenAt "foo" doc == [Number 1, Number 2, Text "test"]
-- @
getDashChildrenAt :: Text -> NodeList -> [Value]
getDashChildrenAt name = mapMaybe getArg . getDashNodesAt name

-- | A helper for getting child nodes following the KDL convention of being named "-".
--
-- == __Example__
--
-- @
-- let
--   config =
--     """
--     foo {
--       - 1
--       - 2
--       - "test"
--     }
--     """
-- Right doc <- pure $ parse config
-- mapM getArg (getDashNodesAt "foo" doc) == Just [Number 1, Number 2, Text "test"]
-- @
getDashNodesAt :: Text -> NodeList -> [Node]
getDashNodesAt name = maybe [] (filterNodes "-") . (nodeChildren <=< lookupNode name)

{----- Ann -----}

data Ann = Ann
  { identifier :: Identifier
  , format :: Maybe AnnFormat
  }
  deriving (Show, Eq)

data AnnFormat = AnnFormat
  { leading :: Text
  -- ^ Whitespace and comments preceding the annotation itself.
  , beforeId :: Text
  -- ^ Whitespace and comments between the opening `(` and the identifier.
  , afterId :: Text
  -- ^ Whitespace and comments between the identifier and the closing `)`.
  , trailing :: Text
  -- ^ Whitespace and comments following the annotation itself.
  }
  deriving (Show, Eq)

annIdentifier :: Ann -> Identifier
annIdentifier = (.identifier)

annFormat :: Ann -> Maybe AnnFormat
annFormat = (.format)

{----- Node -----}

data Node = Node
  { ann :: Maybe Ann
  , name :: Identifier
  , entries :: [Entry]
  , children :: Maybe NodeList
  , format :: Maybe NodeFormat
  }
  deriving (Show, Eq)

data NodeFormat = NodeFormat
  { leading :: Text
  -- ^ Whitespace and comments preceding the node itself.
  , beforeChildren :: Text
  -- ^ Whitespace and comments preceding the node's children block.
  , beforeTerminator :: Text
  -- ^ Whitespace and comments right before the node's terminator.
  , terminator :: Text
  -- ^ The terminator for the node.
  , trailing :: Text
  -- ^ Whitespace and comments following the node, after the terminator.
  }
  deriving (Show, Eq)

nodeAnn :: Node -> Maybe Ann
nodeAnn = (.ann)

nodeName :: Node -> Identifier
nodeName = (.name)

nodeEntries :: Node -> [Entry]
nodeEntries = (.entries)

nodeChildren :: Node -> Maybe NodeList
nodeChildren = (.children)

nodeFormat :: Node -> Maybe NodeFormat
nodeFormat = (.format)

-- | Get all the positional arguments of the node.
getArgs :: Node -> [Value]
getArgs node =
  [ value
  | Entry{name = Nothing, value} <- node.entries
  ]

-- | Get the first argument of the node.
getArg :: Node -> Maybe Value
getArg = listToMaybe . getArgs

-- | Get the properties of the node.
getProps :: Node -> Map Text Value
getProps node =
  Map.fromList
    [ (name.value, value)
    | Entry{name = Just name, value} <- node.entries
    ]

-- | Get the property with the given name in the node.
getProp :: Text -> Node -> Maybe Value
getProp name = Map.lookup name . getProps

{----- Entry -----}

data Entry = Entry
  { name :: Maybe Identifier
  -- ^ The name of the entry, if it's a property, Nothing if it's a positional arg
  , value :: Value
  , format :: Maybe EntryFormat
  }
  deriving (Show, Eq)

data EntryFormat = EntryFormat
  { leading :: Text
  -- ^ Whitespace and comments preceding the entry itself.
  , afterKey :: Text
  -- ^ Whitespace and comments between an entry's key name and its equals sign.
  , afterEq :: Text
  -- ^ Whitespace and comments between an entry's equals sign and its value.
  , trailing :: Text
  -- ^ Whitespace and comments following the entry itself.
  }
  deriving (Show, Eq)

entryName :: Entry -> Maybe Identifier
entryName = (.name)

entryValue :: Entry -> Value
entryValue = (.value)

entryFormat :: Entry -> Maybe EntryFormat
entryFormat = (.format)

{----- Value -----}

data Value = Value
  { ann :: Maybe Ann
  , data_ :: ValueData
  , format :: Maybe ValueFormat
  }
  deriving (Show, Eq)

data ValueFormat = ValueFormat
  { repr :: Text
  -- ^ The actual text representation of the value.
  }
  deriving (Show, Eq)

valueAnn :: Value -> Maybe Ann
valueAnn = (.ann)

valueData :: Value -> ValueData
valueData = (.data_)

valueFormat :: Value -> Maybe ValueFormat
valueFormat = (.format)

data ValueData
  = String Text
  | Number Scientific
  | Bool Bool
  | Inf
  | NegInf
  | NaN
  | Null
  deriving (Show, Eq)

{----- Identifier -----}

data Identifier = Identifier
  { value :: Text
  , format :: Maybe IdentifierFormat
  }
  deriving (Show, Eq, Ord)

data IdentifierFormat = IdentifierFormat
  { repr :: Text
  }
  deriving (Show, Eq, Ord)

fromIdentifier :: Identifier -> Text
fromIdentifier = (.value)

identifierFormat :: Identifier -> Maybe IdentifierFormat
identifierFormat = (.format)

toIdentifier :: Text -> Identifier
toIdentifier value = Identifier{value = value, format = Nothing}
