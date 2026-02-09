{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as Text
import Data.Typeable (Typeable)
import KDL qualified
import Skeletest.Main
import Text.Show.Pretty (ppShow)

snapshotRenderers :: [SnapshotRenderer]
snapshotRenderers =
  [ hsRender @KDL.NodeList
  , hsRender @KDL.Node
  , hsRender @KDL.Entry
  , hsRender @KDL.Value
  ]
 where
  hsRender :: forall a. (Show a, Typeable a) => SnapshotRenderer
  hsRender =
    SnapshotRenderer
      { render = Text.pack . ppShow @a
      , snapshotLang = Just "haskell"
      }
