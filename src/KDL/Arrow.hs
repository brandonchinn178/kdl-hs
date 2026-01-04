{-|
This module defines the Arrow interface for decoding a KDL document. Intended to
be imported qualified as:

> import KDL.Arrow qualified as KDL

For most use-cases, the Monad interface exported by "KDL" is sufficient. You
may wish to use the Arrow interface if you would like to statically analyze a
decoder's schema, e.g. to generate documentation.

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
{\-# LANGUAGE Arrows #-\}

import KDL.Arrow qualified as KDL

main :: IO ()
main = do
  config <- KDL.decodeFileWith decoder "config.kdl"
  print config

decoder :: KDL.DocumentDecoder Config
decoder = KDL.document $ proc () -> do
  KDL.node "package" -< ()

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
  nodeDecoder = proc () -> do
    name <- KDL.argAt "name" -< ()
    version <- KDL.argAt "version" -< ()
    dependencies <- KDL.nodeWith "dependencies" . KDL.children $ KDL.remainingNodes -< ()
    returnA -< Config{..}

instance KDL.DecodeNode Dep where
  nodeDecoder = proc () -> do
    version <- KDL.arg -< ()
    optional <- KDL.option False $ KDL.prop "optional" -< ()
    returnA -< Dep{..}
@
-}
module KDL.Arrow (
  -- * KDL re-exports
  module X,

  -- * base re-reexports
  module Control.Arrow,
  module Control.Category,
) where

import Control.Arrow
import Control.Category
import KDL.Decoder.Arrow as X
import KDL.Decoder.Schema as X
import KDL.Parser as X
import KDL.Render as X
import KDL.Types as X
