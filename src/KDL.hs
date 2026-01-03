{-|
This module is intended to be imported qualified as:

> import KDL qualified

This provides a Monad interface for decoding KDL files, which is sufficient for
most cases. You may wish to use "KDL.Arrow" if you would like to
statically analyze a decoder's schema, e.g. to generate documentation.

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

Parse it with @kdl-hs@:

@
import KDL qualified

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
  nodeDecoder = do
    name <- KDL.argAt "name"
    version <- KDL.argAt "version"
    dependencies <- KDL.nodeWith "dependencies" . KDL.children $ KDL.remainingNodes
    pure Config{..}

instance KDL.DecodeNode Dep where
  nodeDecoder = do
    version <- KDL.arg
    optional <- KDL.option False $ KDL.prop "optional"
    pure Dep{..}
@
-}
module KDL (
  module X,
) where

import KDL.Decoder as X
import KDL.Parser as X
import KDL.Render as X
import KDL.Types as X
