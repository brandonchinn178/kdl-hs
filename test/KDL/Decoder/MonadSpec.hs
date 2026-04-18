module KDL.Decoder.MonadSpec (spec) where

import KDL.Decoder.SharedSpec.Monad
import Skeletest

spec :: Spec
spec = do
  apiSpec
  decodeNodeSpec
  decodeValueSpec
