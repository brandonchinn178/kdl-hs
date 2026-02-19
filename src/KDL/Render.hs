{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KDL.Render (
  render,

  -- * Rendering components
  renderValue,
  renderValueData,
  renderIdentifier,
) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import KDL.Types (
  Ann (..),
  AnnExtension (..),
  AnnFormat (..),
  Document,
  Entry (..),
  EntryExtension (..),
  EntryFormat (..),
  Identifier (..),
  IdentifierExtension (..),
  IdentifierFormat (..),
  Node (..),
  NodeExtension (..),
  NodeFormat (..),
  NodeList (..),
  NodeListExtension (..),
  NodeListFormat (..),
  Value (..),
  ValueData (..),
  ValueExtension (..),
  ValueFormat (..),
 )

render :: Document -> Text
render = renderNodeList 0

type IndentLevel = Int

renderNodeList :: IndentLevel -> NodeList -> Text
renderNodeList lvl NodeList{..} =
  Text.concat
    [ maybe (if lvl > 0 then "\n" else "") (.leading) ext.format
    , foldMap (renderNode lvl) nodes
    , maybe (indent (lvl - 1)) (.trailing) ext.format
    ]

renderNode :: IndentLevel -> Node -> Text
renderNode lvl Node{..} =
  Text.concat
    [ maybe (indent lvl) (.leading) ext.format
    , maybe "" renderAnn ann
    , renderIdentifier name
    , foldMap renderEntry entries
    , let def_ = if children == Nothing then "" else " "
       in maybe def_ (.beforeChildren) ext.format
    , case children of
        Nothing -> ""
        Just nodes -> renderChildren lvl nodes
    , maybe "" (.beforeTerminator) ext.format
    , maybe "\n" (.terminator) ext.format
    , maybe "" (.trailing) ext.format
    ]

renderChildren :: IndentLevel -> NodeList -> Text
renderChildren lvl nodeList =
  case nodeList.ext.format of
    -- Special case empty node list to render as "{}"
    Nothing | null nodeList.nodes -> "{}"
    _ -> "{" <> renderNodeList (lvl + 1) nodeList <> "}"

indent :: IndentLevel -> Text
indent lvl = Text.replicate lvl "  "

renderEntry :: Entry -> Text
renderEntry Entry{..} =
  Text.concat
    [ maybe " " (.leading) ext.format
    , case name of
        Nothing -> renderValue value
        Just nameId ->
          Text.concat
            [ renderIdentifier nameId
            , maybe "" (.afterKey) ext.format
            , "="
            , maybe "" (.afterEq) ext.format
            , renderValue value
            ]
    , maybe "" (.trailing) ext.format
    ]

renderAnn :: Ann -> Text
renderAnn Ann{..} =
  Text.concat
    [ maybe "" (.leading) ext.format
    , "("
    , maybe "" (.beforeId) ext.format
    , renderIdentifier identifier
    , maybe "" (.afterId) ext.format
    , ")"
    , maybe "" (.trailing) ext.format
    ]

renderValue :: Value -> Text
renderValue Value{..} =
  Text.concat
    [ maybe "" renderAnn ann
    , fromMaybe (renderValueData data_) (ext.format >>= (.repr))
    ]

renderValueData :: ValueData -> Text
renderValueData = \case
  String s -> renderString s
  Number x -> (Text.pack . show) x
  Bool b -> if b then "#true" else "#false"
  Inf -> "#inf"
  NegInf -> "#-inf"
  NaN -> "#nan"
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

renderIdentifier :: Identifier -> Text
renderIdentifier ident = fromMaybe ident.value (ident.ext.format >>= (.repr))
