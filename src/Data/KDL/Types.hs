{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.KDL.Types (
  -- * Document
  Document,
  docNodes,

  -- * NodeList
  NodeList (..),
  NodeListFormat (..),
  fromNodeList,
  nodeListFormat,

  -- * Node
  Node,
  BaseNode (..),
  NodeFormat (..),
  nodeName,
  nodeEntries,
  nodeChildren,
  nodeFormat,

  -- * Entry
  Entry (..),
  EntryFormat (..),
  entryName,
  entryValue,
  entryFormat,

  -- * Value
  Value,
  BaseValue (..),
  renderBaseValue,

  -- * Ann
  Ann (..),
  annAnn,
  annObj,
  annFormat,

  -- * Identifier
  Identifier (..),
  IdentifierFormat (..),
  fromIdentifier,
  identifierFormat,
) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text

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
  -- ^ Whitespace and comments preceding the document's first node.
  , trailing :: Text
  -- ^ Whitespace and comments following the document's last node.
  }
  deriving (Show, Eq)

fromNodeList :: NodeList -> [Node]
fromNodeList = (.nodes)

nodeListFormat :: NodeList -> Maybe NodeListFormat
nodeListFormat = (.format)

{----- Ann -----}

data Ann a = Ann
  { ann :: Maybe Identifier
  , obj :: a
  , format :: Maybe AnnFormat
  }
  deriving (Show, Eq)

data AnnFormat = AnnFormat
  { leading :: Text
  -- ^ Whitespace and comments preceding the annotation itself.
  , before_id :: Text
  -- ^ Whitespace and comments between the opening `(` and the identifier.
  , after_id :: Text
  -- ^ Whitespace and comments between the identifier and the closing `)`.
  , trailing :: Text
  -- ^ Whitespace and comments following the annotation itself.
  }
  deriving (Show, Eq)

annAnn :: Ann a -> Maybe Identifier
annAnn = (.ann)

annObj :: Ann a -> a
annObj = (.obj)

annFormat :: Ann a -> Maybe AnnFormat
annFormat = (.format)

{----- Node -----}

type Node = Ann BaseNode
data BaseNode = BaseNode
  { name :: Identifier
  , entries :: [Entry]
  , children :: Maybe NodeList
  , format :: Maybe NodeFormat
  }
  deriving (Show, Eq)

data NodeFormat = NodeFormat
  { leading :: Text
  -- ^ Whitespace and comments preceding the node itself.
  , before_children :: Text
  -- ^ Whitespace and comments preceding the node's children block.
  , before_terminator :: Text
  -- ^ Whitespace and comments right before the node's terminator.
  , terminator :: Text
  -- ^ The terminator for the node.
  , trailing :: Text
  -- ^ Whitespace and comments following the node, after the terminator.
  }
  deriving (Show, Eq)

nodeName :: BaseNode -> Identifier
nodeName = (.name)

nodeEntries :: BaseNode -> [Entry]
nodeEntries = (.entries)

nodeChildren :: BaseNode -> Maybe NodeList
nodeChildren = (.children)

nodeFormat :: BaseNode -> Maybe NodeFormat
nodeFormat = (.format)

{----- Entry -----}

data Entry = Entry
  { name :: Maybe Identifier
  -- ^ The name of the entry, if it's a property, Nothing if it's a positional arg
  , value :: Value
  , format :: Maybe EntryFormat
  }
  deriving (Show, Eq)

data EntryFormat = EntryFormat
  { leading :: String
  -- ^ Whitespace and comments preceding the entry itself.
  , after_key :: String
  -- ^ Whitespace and comments between an entry's key name and its equals sign.
  , after_eq :: String
  -- ^ Whitespace and comments between an entry's equals sign and its value.
  , value_repr :: String
  -- ^ The actual text representation of the entry's value.
  , trailing :: String
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

type Value = Ann BaseValue
data BaseValue
  = Text Text
  | Number Scientific
  | Bool Bool
  | Null
  deriving (Show, Eq)

renderBaseValue :: BaseValue -> Text
renderBaseValue = \case
  Text s -> "\"" <> s <> "\""
  Number x -> (Text.pack . show) x
  Bool b -> if b then "#true" else "#false"
  Null -> "#null"

{----- Identifier -----}

data Identifier = Identifier
  { value :: Text
  , format :: Maybe IdentifierFormat
  }
  deriving (Show, Eq)

data IdentifierFormat = IdentifierFormat
  { repr :: Text
  }
  deriving (Show, Eq)

fromIdentifier :: Identifier -> Text
fromIdentifier = (.value)

identifierFormat :: Identifier -> Maybe IdentifierFormat
identifierFormat = (.format)
