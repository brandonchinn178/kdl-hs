{-# LANGUAGE LambdaCase #-}

{-|
See "Data.KDL" for more information.
-}
module Data.KDL.Decoder.Monad (
  -- * Monad-specific definitions
  fail,

  -- * Re-exports from "Data.KDL.Decoder.Arrow"
  module Data.KDL.Decoder.Arrow,
) where

import Control.Arrow (arr, (>>>))
import Data.KDL.Decoder.Arrow hiding (
  fail,
 )
import Data.KDL.Decoder.Arrow qualified as Arrow
import Data.Text (Text)
import Prelude hiding (any, fail, null)

-- | Same as 'Data.KDL.Decoder.Arrow.fail', except a more ergonomic signature
-- for use in do-notation.
fail :: forall a o. Text -> Decoder o () a
fail msg = arr (\() -> msg) >>> Arrow.fail
