{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Implement the v2 parser specified at: https://kdl.dev/spec/#name-full-grammar
-}
module KDL.Parser (
  parse,
  parseFile,

  -- * Configurable parsing
  ParseConfig (..),
  parseWith,
  parseFileWith,
) where

import Data.Bifunctor (first)
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import KDL.Parser.Internal (
  ParseConfig (..),
  p_document,
  runParser,
 )
import KDL.Types (Document)
import Text.Megaparsec qualified as Megaparsec

parse :: Text -> Either Text Document
parse = parseWith def

parseFile :: FilePath -> IO (Either Text Document)
parseFile = parseFileWith def

parseWith :: ParseConfig -> Text -> Either Text Document
parseWith config input =
  first (Text.strip . Text.pack . Megaparsec.errorBundlePretty) $
    runParser config p_document input

parseFileWith :: ParseConfig -> FilePath -> IO (Either Text Document)
parseFileWith config0 fp = parseWith config <$> Text.readFile fp
 where
  config = config0{filepath = fp}
