{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.KDL.Parser (
  parse,
  parseFile,
) where

import Data.KDL.Types (
  Ann (..),
  BaseNode (..),
  BaseValue (..),
  Document,
  Entry (..),
  Identifier (..),
  NodeList (..),
 )
import Data.KDL.Parser.Hustle qualified as Hustle
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

    fromNode Hustle.Node{..} =
      Ann
        { ann = fromIdentifier <$> nodeAnn
        , obj =
            BaseNode
              { name = fromIdentifier nodeName
              , entries = map fromArgEntry nodeArgs <> map fromPropEntry (Map.toList nodeProps)
              , children = Just $ fromNodes nodeChildren
              , format = Nothing
              }
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
      Ann
        { ann = fromIdentifier <$> valueAnn
        , obj =
            case valueExp of
              Hustle.StringValue s -> Text s
              Hustle.IntegerValue x -> Number (fromInteger x)
              Hustle.SciValue x -> Number x
              Hustle.BooleanValue x -> Bool x
              Hustle.NullValue -> Null
        , format = Nothing
        }

    fromIdentifier (Hustle.Identifier s) =
      Identifier
        { value = s
        , format = Nothing
        }

parseFile :: FilePath -> IO (Either Text Document)
parseFile = fmap parse . Text.readFile
