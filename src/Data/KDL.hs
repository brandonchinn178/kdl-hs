{-|
This module is intended to be imported qualified as:

> import Data.KDL qualified as KDL

= Quickstart

Given a file @config.kdl@:

@
package {
  name my-pkg
  version "1.2.3"

  dependencies {
    aeson ">= 2.2.3.0" optional=#true
    text ">= 2"
  }
}
@

Parse it with:

@
import Data.KDL qualified as KDL

main :: IO ()
main = do
  config <- KDL.decodeFileWith decoder "config.kdl"
  print config

decoder :: KDL.Decoder Config
decoder = KDL.document $ do
  KDL.node "package"

data Config = Config
  { name :: Text
  , version :: Text
  , dependencies :: Map Text Dep
  }
  deriving (Show)

data Dep = Dep
  { version :: Text
  , optional :: Bool
  }
  deriving (Show)

instance KDL.DecodeNode Config where
  nodeDecoder = KDL.noSchema $ do
    name <- KDL.argAt "name"
    version <- KDL.argAt "version"
    dependencies <- KDL.nodeWith "dependencies" . KDL.children $ KDL.remainingNodes
    pure Config{..}

instance KDL.DecodeNode Dep where
  nodeDecoder = KDL.noSchema $ do
    version <- KDL.arg
    optional <- KDL.option False $ KDL.prop "optional"
    pure Dep{..}
@
-}
module Data.KDL (
  module X,
) where

import Data.KDL.Decoder.Monad as X
import Data.KDL.Parser as X
import Data.KDL.Render as X
import Data.KDL.Types as X
