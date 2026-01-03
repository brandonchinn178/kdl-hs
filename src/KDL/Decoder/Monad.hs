{-# LANGUAGE LambdaCase #-}

{-|
See "KDL" for more information.
-}
module KDL.Decoder.Monad (
  -- * Monad-specific definitions
  fail,

  -- * Re-exports from "KDL.Decoder.Arrow"
  module KDL.Decoder.Arrow,
) where

import Control.Arrow (arr, (>>>))
import Data.Text (Text)
import KDL.Decoder.Arrow hiding (
  fail,
 )
import KDL.Decoder.Arrow qualified as Arrow
import Prelude hiding (any, fail, null)

-- | Same as 'KDL.Decoder.Arrow.fail', except a more ergonomic signature
-- for use in do-notation.
fail :: forall a o. Text -> Decoder o () a
fail msg = arr (\() -> msg) >>> Arrow.fail
