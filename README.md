# kdl-hs

`kdl-hs` can parse and manage [KDL configuration files](https://kdl.dev).

## Quickstart

Given a file `config.kdl`:

```kdl
package {
  name my-pkg
  version "1.2.3"

  dependencies {
    aeson ">= 2.2.3.0" optional=#true
    text ">= 2"
  }
}
```

Parse it with `kdl-hs`:

```hs
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
```

## Acknowledgements

* The KDL spec authors for devising a super cool configuration language
* [kdl-rs](https://github.com/kdl-org/kdl-rs) for providing a guide for implementation
* [hustle](https://github.com/fuzzypixelz/hustle) for the initial parser implementation that got me started.
