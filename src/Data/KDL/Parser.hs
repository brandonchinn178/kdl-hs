{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.KDL.Parser (
  parse,
  parseFile,
) where

import Data.KDL.Parser.Hustle qualified as Hustle
import Data.KDL.Types (
  Ann (..),
  Document,
  Entry (..),
  Identifier (..),
  Node (..),
  NodeList (..),
  Value (..),
  ValueData (..),
  ValueFormat (..),
 )
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

-- TODO: Implement our own parser that implements the v2.0.0 spec + preserves formatting and comments
parse :: Text -> Either Text Document
parse input =
  case Hustle.parse Hustle.document "" input of
    Left e -> Left . Text.pack . Hustle.errorBundlePretty $ e
    Right (Hustle.Document nodes) -> Right $ fromNodes nodes
 where
  fromNodes nodes =
    NodeList
      { nodes = map fromNode nodes
      , format = Nothing
      }

  fromAnn identifier =
    Ann
      { identifier = fromIdentifier identifier
      , format = Nothing
      }

  fromNode Hustle.Node{..} =
    Node
      { ann = fromAnn <$> nodeAnn
      , name = fromIdentifier nodeName
      , entries = map fromArgEntry nodeArgs <> map fromPropEntry (Map.toList nodeProps)
      , children = Just $ fromNodes nodeChildren
      , format = Nothing
      }

  fromArgEntry v =
    Entry
      { name = Nothing
      , value = fromValue v
      , format = Nothing
      }

  fromPropEntry (name, v) =
    Entry
      { name = Just $ fromIdentifier name
      , value = fromValue v
      , format = Nothing
      }

  fromValue Hustle.Value{..} =
    Value
      { ann = fromAnn <$> valueAnn
      , data_ =
          case valueExp of
            Hustle.StringValue s -> Text s
            Hustle.IntegerValue x -> Number (fromInteger x)
            Hustle.SciValue x -> Number x
            Hustle.BooleanValue x -> Bool x
            Hustle.NullValue -> Null
      , format =
          case valueExp of
            Hustle.IntegerValue x -> Just ValueFormat{repr = Text.pack $ show x}
            _ -> Nothing
      }

  fromIdentifier (Hustle.Identifier s) =
    Identifier
      { value = s
      , format = Nothing
      }

parseFile :: FilePath -> IO (Either Text Document)
parseFile = fmap parse . Text.readFile
