{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.KDL.Decoder.DecodeM (
  DecodeM (..),
  DecodeError (..),
  BaseDecodeError (..),
  Context,
  ContextItem (..),
  decodeThrow,
  failM,
  addContext,
  runDecodeM,
  renderDecodeError,
) where

import Control.Applicative (Alternative (..))
import Data.KDL.Types (
  BaseValue,
  Entry,
  Node,
  Value,
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (
  TypeRep,
 )
import Numeric.Natural (Natural)

type Context = [ContextItem]

data ContextItem
  = ContextNode Text
  | ContextArgs
  | ContextIndex Natural
  | ContextProp Text
  deriving (Show, Eq)

data DecodeM a = DecodeM (forall r. (Context -> BaseDecodeError -> r) -> (a -> r) -> r)

data DecodeError = DecodeError Context BaseDecodeError
  deriving (Show, Eq)

data BaseDecodeError
  = DecodeError_Custom Text
  | DecodeError_ParseError Text
  | DecodeError_ExpectedNode Text
  | DecodeError_ExpectedArg
  | DecodeError_ExpectedProp Text
  | DecodeError_MismatchedAnn {givenAnn :: Text, validAnns :: [Text]}
  | DecodeError_NodeDecodeFail {typeHint :: TypeRep, node :: Node}
  | DecodeError_ValueDecodeFail {typeHint :: TypeRep, value :: Value}
  | DecodeError_BaseValueDecodeFail {expectedType :: Text, baseValue :: BaseValue}
  | DecodeError_UnknownNodes [Node]
  | DecodeError_UnknownEntries [Entry]
  deriving (Show, Eq)

instance Functor DecodeM where
  fmap f (DecodeM k) = DecodeM $ \onFail onSuccess -> k onFail (onSuccess . f)
instance Applicative DecodeM where
  pure x = DecodeM $ \_ onSuccess -> onSuccess x
  DecodeM kf <*> DecodeM ka = DecodeM $ \onFail onSuccess ->
    kf onFail $ \f -> ka onFail (onSuccess . f)
instance Monad DecodeM where
  DecodeM ka >>= k = DecodeM $ \onFail onSuccess ->
    ka onFail $ \a -> let DecodeM kb = k a in kb onFail onSuccess
instance Alternative DecodeM where
  empty = failM "<empty>"
  DecodeM k1 <|> DecodeM k2 = DecodeM $ \onFail onSuccess -> k1 (\_ _ -> k2 onFail onSuccess) onSuccess

decodeThrow :: BaseDecodeError -> DecodeM a
decodeThrow e = DecodeM $ \onFail _ -> onFail [] e

failM :: Text -> DecodeM a
failM = decodeThrow . DecodeError_Custom

addContext :: ContextItem -> DecodeM a -> DecodeM a
addContext ctxItem (DecodeM f) = DecodeM $ \onFail onSuccess -> f (onFail . (ctxItem :)) onSuccess

runDecodeM :: DecodeM a -> Either DecodeError a
runDecodeM (DecodeM f) = f (\ctx msg -> Left $ DecodeError (reverse ctx) msg) Right

renderDecodeError :: DecodeError -> Text
renderDecodeError (DecodeError ctx e) = "At: " <> (Text.pack . show) ctx <> "\n" <> (Text.pack . show) e -- TODO: render pretty
