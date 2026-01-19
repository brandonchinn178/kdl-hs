{-# LANGUAGE OverloadedStrings #-}

module KDL.TestUtils.Error (
  decodeErrorMsg,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import KDL qualified
import Skeletest
import Skeletest.Predicate qualified as P

decodeErrorMsg :: [Text] -> Predicate IO (Either KDL.DecodeError a)
decodeErrorMsg msgs = P.left (KDL.renderDecodeError P.>>> P.eq msg)
 where
  msg = Text.intercalate "\n" msgs
