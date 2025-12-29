{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.KDL.Types (
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
  Node,
  BaseNode (..),
  NodeFormat (..),
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
  Value,
  BaseValue (..),
  renderValue,
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
  toIdentifier,
  renderIdentifier,
) where

import Control.Monad ((<=<))
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
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

-- | A helper to get all nodes with the given name
filterNodes :: Text -> NodeList -> [Node]
filterNodes name = filter ((== name) . (.obj.name.value)) . (.nodes)

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
--     foo 1 2 #false
--     """
-- Right doc <- pure $ parse config
-- getArgsAt "foo" doc == [Number 1, Number 2, Bool False]
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
--       - #false
--     }
--     """
-- Right doc <- pure $ parse config
-- getDashNodesAt "foo" doc == [Number 1, Number 2, Bool False]
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
--       - #false
--     }
--     """
-- Right doc <- pure $ parse config
-- map getArg (getDashNodesAt "foo" doc) == [Just (Number 1), Just (Number 2), Just (Bool False)]
-- @
getDashNodesAt :: Text -> NodeList -> [Node]
getDashNodesAt name = maybe [] (filterNodes "-") . (nodeChildren <=< lookupNode name)

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

renderAnnWith :: (a -> Text) -> Ann a -> Text
renderAnnWith renderObj Ann{..} = maybe "" (parens . renderIdentifier) ann <> renderObj obj
 where
  parens s = "(" <> s <> ")"

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

nodeName :: Node -> Identifier
nodeName = (.obj.name)

nodeEntries :: Node -> [Entry]
nodeEntries = (.obj.entries)

nodeChildren :: Node -> Maybe NodeList
nodeChildren = (.obj.children)

nodeFormat :: Node -> Maybe NodeFormat
nodeFormat = (.obj.format)

-- | Get all the positional arguments of the node.
getArgs :: Node -> [Value]
getArgs node =
  [ value
  | Entry{name = Nothing, value} <- node.obj.entries
  ]

-- | Get the first argument of the node.
getArg :: Node -> Maybe Value
getArg = listToMaybe . getArgs

-- | Get the properties of the node.
getProps :: Node -> Map Text Value
getProps node =
  Map.fromList
    [ (name.value, value)
    | Entry{name = Just name, value} <- node.obj.entries
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

renderValue :: Value -> Text
renderValue = renderAnnWith renderBaseValue

renderBaseValue :: BaseValue -> Text
renderBaseValue = \case
  Text s -> renderString s
  Number x -> (Text.pack . show) x
  Bool b -> if b then "#true" else "#false"
  Null -> "#null"
 where
  renderString s = if isPlainIdent s then s else "\"" <> Text.concatMap escapeChar s <> "\""
  isPlainIdent s =
    and . map not $
      [ Text.any isDisallowedChar s
      , case fmap Text.uncons <$> Text.uncons s of
          Just (c, _) | isDigit c -> True
          Just (c0, Just (c1, _))
            | c0 `elem` ['.', '-', '+']
            , isDigit c1 ->
                True
          _ -> False
      , s `elem` ["inf", "-inf", "nan", "true", "false", "null"]
      ]
  isDisallowedChar c =
    or
      [ c `elem` disallowedIdentChars
      , any (c `Text.elem`) newlines
      , c `elem` unicodeSpaces
      , isDisallowedUnicode c
      , c == '='
      ]
  disallowedIdentChars = ['\\', '/', '(', ')', '{', '}', '[', ']', ';', '"', '#']
  newlines =
    [ "\x000D\x000A"
    , "\x000D"
    , "\x000A"
    , "\x0085"
    , "\x000B"
    , "\x000C"
    , "\x2028"
    , "\x2029"
    ]
  unicodeSpaces =
    [ '\x0009'
    , '\x0020'
    , '\x00A0'
    , '\x1680'
    , '\x2000'
    , '\x2001'
    , '\x2002'
    , '\x2003'
    , '\x2004'
    , '\x2005'
    , '\x2006'
    , '\x2007'
    , '\x2008'
    , '\x2009'
    , '\x200A'
    , '\x202F'
    , '\x205F'
    , '\x3000'
    ]
  isDisallowedUnicode c =
    or
      [ '\x0000' < c && c <= '\x0008'
      , '\x000E' < c && c <= '\x001F'
      , '\x200E' < c && c <= '\x200F'
      , '\x202A' < c && c <= '\x202E'
      , '\x2066' < c && c <= '\x2069'
      , c == '\xFEFF'
      ]
  escapeChar = \case
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '\x08' -> "\\b"
    '\x0C' -> "\\f"
    c -> Text.singleton c

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

renderIdentifier :: Identifier -> Text
renderIdentifier ident = maybe ident.value (.repr) ident.format
