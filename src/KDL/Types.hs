{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

{-|
Defines the types that make up a KDL document.

This module enables @-XNoFieldSelectors@, so none of the fields create implicit
selector functions. Instead, use @-XOverloadedRecordDot@,
@-XNamedFieldPuns@/@-XRecordWildCards@, or explicitly pattern-match.
-}
module KDL.Types (
  -- * Document
  Document,

  -- * NodeList
  NodeList (..),
  NodeListExtension (..),
  NodeListFormat (..),

  -- ** Helpers
  filterNodes,
  lookupNode,
  getArgAt,
  getArgsAt,
  getDashChildrenAt,
  getDashNodesAt,

  -- * Node
  Node (..),
  NodeExtension (..),
  NodeFormat (..),

  -- ** Helpers
  getArgs,
  getArg,
  getProps,
  getProp,

  -- * Entry
  Entry (..),
  EntryExtension (..),
  EntryFormat (..),

  -- * Value
  Value (..),
  ValueExtension (..),
  ValueFormat (..),
  ValueData (..),

  -- * Ann
  Ann (..),
  AnnExtension (..),
  AnnFormat (..),

  -- * Identifier
  Identifier (..),
  IdentifierExtension (..),
  IdentifierFormat (..),
  fromIdentifier,
  toIdentifier,

  -- * Span
  Span (..),

  -- * Re-exports
  def,
) where

import Control.Monad ((<=<))
import Data.Default (Default (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)

{----- Document -----}

type Document = NodeList

{----- NodeList -----}

data NodeList = NodeList
  { nodes :: [Node]
  , ext :: NodeListExtension
  }
  deriving (Show, Eq)

data NodeListExtension = NodeListExtension
  { format :: Maybe NodeListFormat
  , span :: Span
  }
  deriving (Show, Eq)

data NodeListFormat = NodeListFormat
  { leading :: Text
  -- ^ Whitespace and comments preceding the first node.
  , trailing :: Text
  -- ^ Whitespace and comments following the last node.
  }
  deriving (Show, Eq)

instance Default NodeListExtension where
  def =
    NodeListExtension
      { format = def
      , span = def
      }
instance Default NodeListFormat where
  def =
    NodeListFormat
      { leading = ""
      , trailing = ""
      }

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
getDashNodesAt name = maybe [] (filterNodes "-") . ((.children) <=< lookupNode name)

{----- Ann -----}

data Ann = Ann
  { identifier :: Identifier
  , ext :: AnnExtension
  }
  deriving (Show, Eq)

data AnnExtension = AnnExtension
  { format :: Maybe AnnFormat
  , span :: Span
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

instance Default AnnExtension where
  def =
    AnnExtension
      { format = def
      , span = def
      }
instance Default AnnFormat where
  def =
    AnnFormat
      { leading = ""
      , beforeId = ""
      , afterId = ""
      , trailing = ""
      }

{----- Node -----}

data Node = Node
  { ann :: Maybe Ann
  , name :: Identifier
  , entries :: [Entry]
  , children :: Maybe NodeList
  , ext :: NodeExtension
  }
  deriving (Show, Eq)

data NodeExtension = NodeExtension
  { format :: Maybe NodeFormat
  , span :: Span
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

instance Default NodeExtension where
  def =
    NodeExtension
      { format = def
      , span = def
      }
instance Default NodeFormat where
  def =
    NodeFormat
      { leading = ""
      , beforeChildren = ""
      , beforeTerminator = ""
      , terminator = ""
      , trailing = ""
      }

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
  , ext :: EntryExtension
  }
  deriving (Show, Eq)

data EntryExtension = EntryExtension
  { format :: Maybe EntryFormat
  , span :: Span
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

instance Default EntryExtension where
  def =
    EntryExtension
      { format = def
      , span = def
      }
instance Default EntryFormat where
  def =
    EntryFormat
      { leading = ""
      , afterKey = ""
      , afterEq = ""
      , trailing = ""
      }

{----- Value -----}

data Value = Value
  { ann :: Maybe Ann
  , data_ :: ValueData
  , ext :: ValueExtension
  }
  deriving (Show, Eq)

data ValueExtension = ValueExtension
  { format :: Maybe ValueFormat
  , span :: Span
  }
  deriving (Show, Eq)

data ValueFormat = ValueFormat
  { repr :: Maybe Text
  -- ^ The actual text representation of the value.
  }
  deriving (Show, Eq)

instance Default ValueExtension where
  def =
    ValueExtension
      { format = def
      , span = def
      }
instance Default ValueFormat where
  def =
    ValueFormat
      { repr = Nothing
      }

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
  , ext :: IdentifierExtension
  }
  deriving (Show, Eq, Ord)

data IdentifierExtension = IdentifierExtension
  { format :: Maybe IdentifierFormat
  , span :: Span
  }
  deriving (Show, Eq, Ord)

data IdentifierFormat = IdentifierFormat
  { repr :: Maybe Text
  }
  deriving (Show, Eq, Ord)

instance Default IdentifierExtension where
  def =
    IdentifierExtension
      { format = def
      , span = def
      }
instance Default IdentifierFormat where
  def =
    IdentifierFormat
      { repr = Nothing
      }

fromIdentifier :: Identifier -> Text
fromIdentifier = (.value)

toIdentifier :: Text -> Identifier
toIdentifier value = Identifier{value = value, ext = def}

{----- Span -----}

-- | The span of a KDL element, if parsed with 'includeSpans'. If 'includeSpans'
-- was not enabled, all fields are set to 0.
data Span = Span
  { startLine :: Int
  , startCol :: Int
  , endLine :: Int
  , endCol :: Int
  }
  deriving (Show, Eq, Ord)

instance Default Span where
  def =
    Span
      { startLine = 0
      , startCol = 0
      , endLine = 0
      , endCol = 0
      }
