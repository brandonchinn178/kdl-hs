{- FOURMOLU_DISABLE -}

{- | Vendered from https://github.com/fuzzypixelz/hustle -}
module KDL.Parser.Hustle
  ( Parser
  , Document(..)
  , Node(..)
  , Value(..)
  , ValueType(..)
  , Identifier(..)
  , pretty
  , document
  , parse
  , errorBundlePretty
  ) where

import           KDL.Parser.Hustle.Formatter ( Pretty(pretty) )
import           KDL.Parser.Hustle.Parser  ( document )
import           KDL.Parser.Hustle.Types   ( Document(..)
                                                , Identifier(..)
                                                , Node(..)
                                                , Parser
                                                , Value(..)
                                                , ValueType(..)
                                                )
import           Text.Megaparsec                ( errorBundlePretty
                                                , parse
                                                )
