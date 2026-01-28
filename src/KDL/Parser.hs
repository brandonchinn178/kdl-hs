{-|
Implement the v2 parser specified at: https://kdl.dev/spec/#name-full-grammar
-}
module KDL.Parser (
  parse,
  parseFile,
) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import KDL.Parser.Internal (p_document)
import KDL.Types (Document)
import Text.Megaparsec qualified as Megaparsec

parse :: Text -> Either Text Document
parse input =
  first (Text.strip . Text.pack . Megaparsec.errorBundlePretty) $
    Megaparsec.parse p_document "" input

parseFile :: FilePath -> IO (Either Text Document)
parseFile = fmap parse . Text.readFile
