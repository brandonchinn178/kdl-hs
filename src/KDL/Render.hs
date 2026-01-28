{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KDL.Render (
  render,

  -- * Rendering components
  renderNodeList,
  renderNode,
  renderEntry,
  renderAnn,
  renderValue,
  renderValueData,
  renderIdentifier,
) where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import KDL.Types (
  Ann (..),
  AnnFormat (..),
  Document,
  Entry (..),
  EntryFormat (..),
  Identifier (..),
  IdentifierFormat (..),
  Node (..),
  NodeFormat (..),
  NodeList (..),
  NodeListFormat (..),
  Value (..),
  ValueData (..),
  ValueFormat (..),
 )

render :: Document -> Text
render = renderNodeList

renderNodeList :: NodeList -> Text
renderNodeList NodeList{..} =
  Text.concat
    [ maybe "" (.leading) format
    , foldMap renderNode nodes
    , maybe "" (.trailing) format
    ]

renderNode :: Node -> Text
renderNode Node{..} =
  Text.concat
    [ maybe "" (.leading) format
    , maybe "" renderAnn ann
    , renderIdentifier name
    , foldMap renderEntry entries
    , maybe "" (.before_children) format
    , case children of
        Nothing -> ""
        Just nodes -> "{" <> renderNodeList nodes <> "}"
    , maybe "" (.before_terminator) format
    , maybe "" (.terminator) format
    , maybe "" (.trailing) format
    ]

renderEntry :: Entry -> Text
renderEntry Entry{..} =
  Text.concat
    [ maybe "" (.leading) format
    , case name of
        Nothing -> renderValue value
        Just nameId ->
          Text.concat
            [ renderIdentifier nameId
            , maybe "" (.after_key) format
            , "="
            , maybe "" (.after_eq) format
            , renderValue value
            ]
    , maybe "" (.trailing) format
    ]

renderAnn :: Ann -> Text
renderAnn Ann{..} =
  Text.concat
    [ maybe "" (.leading) format
    , "("
    , maybe "" (.before_id) format
    , renderIdentifier identifier
    , maybe "" (.after_id) format
    , ")"
    , maybe "" (.trailing) format
    ]

renderValue :: Value -> Text
renderValue Value{..} =
  Text.concat
    [ maybe "" renderAnn ann
    , maybe (renderValueData data_) (.repr) format
    ]

renderValueData :: ValueData -> Text
renderValueData = \case
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

renderIdentifier :: Identifier -> Text
renderIdentifier ident = maybe ident.value (.repr) ident.format
