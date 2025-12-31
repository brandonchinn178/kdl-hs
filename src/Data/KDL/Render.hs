{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.KDL.Render (
  render,

  -- * Rendering components
  renderAnn,
  renderValue,
  renderValueData,
  renderIdentifier,
) where

import Data.Char (isDigit)
import Data.KDL.Types (
  Ann (..),
  AnnFormat (..),
  Document,
  Identifier (..),
  IdentifierFormat (..),
  Value (..),
  ValueData (..),
 )
import Data.Text (Text)
import Data.Text qualified as Text

-- TODO: Implement render after parsing Document with formatting information
render :: Document -> Text
render = error "render is not implemented yet"

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
    , renderValueData data_
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
