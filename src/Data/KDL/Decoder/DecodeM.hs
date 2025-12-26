{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  Identifier,
  Node,
  renderIdentifier, renderBaseValue,
 )
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

type Context = [ContextItem]

data ContextItem
  = ContextNode
      { name :: Identifier
      , index :: Int
      }
  | ContextArg
      { index :: Int
      }
  | ContextProp
      { name :: Identifier
      }
  deriving (Show, Eq, Ord)

data DecodeM a = DecodeM (forall r. (DecodeError -> r) -> (a -> r) -> r)

data DecodeError = DecodeError [(Context, BaseDecodeError)]
  deriving (Show, Eq)
instance Semigroup DecodeError where
  DecodeError e1 <> DecodeError e2 = DecodeError (e1 <> e2)
instance Monoid DecodeError where
  mempty = DecodeError []

data BaseDecodeError
  = DecodeError_Custom Text
  | DecodeError_ParseError Text
  | DecodeError_ExpectedNode {name :: Text, index :: Int}
  | DecodeError_ExpectedArg {index :: Int}
  | DecodeError_ExpectedProp {name :: Text}
  | DecodeError_MismatchedAnn {givenAnn :: Identifier, validAnns :: [Text]}
  | DecodeError_BaseValueDecodeFail {expectedType :: Text, baseValue :: BaseValue}
  | DecodeError_UnknownNodes [Node]
  | DecodeError_UnknownEntries [Entry]
  deriving (Show, Eq)

instance Functor DecodeM where
  fmap f (DecodeM k) = DecodeM $ \onFail onSuccess -> k onFail (onSuccess . f)
instance Applicative DecodeM where
  pure x = DecodeM $ \_ onSuccess -> onSuccess x
  DecodeM kf <*> DecodeM ka = DecodeM $ \onFail onSuccess ->
    -- Collect all errors
    kf
      (\e1 -> ka (\e2 -> onFail $ e1 <> e2) (\_ -> onFail e1))
      (\f -> ka onFail (onSuccess . f))
instance Monad DecodeM where
  (>>) = (*>)
  DecodeM ka >>= k = DecodeM $ \onFail onSuccess ->
    ka onFail $ \a -> let DecodeM kb = k a in kb onFail onSuccess
instance Alternative DecodeM where
  empty = DecodeM $ \onFail _ -> onFail mempty
  DecodeM k1 <|> DecodeM k2 = DecodeM $ \onFail onSuccess ->
    k1
      (\e1 -> k2 (\e2 -> onFail $ e1 <> e2) onSuccess)
      onSuccess

decodeThrow :: BaseDecodeError -> DecodeM a
decodeThrow e = DecodeM $ \onFail _ -> onFail $ DecodeError [([], e)]

failM :: Text -> DecodeM a
failM = decodeThrow . DecodeError_Custom

addContext :: ContextItem -> DecodeM a -> DecodeM a
addContext ctxItem (DecodeM f) = DecodeM $ \onFail onSuccess -> f (onFail . addCtx) onSuccess
 where
  addCtx (DecodeError es) = DecodeError $ [(ctxItem : ctx, msg) | (ctx, msg) <- es]

runDecodeM :: DecodeM a -> Either DecodeError a
runDecodeM (DecodeM f) = f (Left . reverseCtxs) Right
 where
  reverseCtxs (DecodeError es) = DecodeError $ [(reverse ctx, msg) | (ctx, msg) <- es]

renderDecodeError :: DecodeError -> Text
renderDecodeError = Text.intercalate "\n" . map renderCtxErrors . groupCtxErrors
 where
  -- Group errors with the same contexts together
  groupCtxErrors (DecodeError es) = Map.toAscList $ Map.fromListWith (<>) $ map (fmap (: [])) es

  renderCtxErrors (ctx, errs) =
    Text.intercalate "\n" $ ("At: " <> renderCtxItems ctx) : renderErrors errs

  renderCtxItems items
    | null items = "<root>"
    | otherwise = Text.intercalate " > " . map renderCtxItem $ items
  renderCtxItem = \case
    ContextNode{..} -> renderIdentifier name <> " #" <> (Text.pack . show) index
    ContextArg{..} -> "arg #" <> (Text.pack . show) index
    ContextProp{..} -> "prop " <> renderIdentifier name

  renderErrors = map (("  " <>) . renderError)
  renderError = \case
    DecodeError_Custom msg -> msg
    DecodeError_ParseError msg -> msg
    DecodeError_ExpectedNode{..}
      | index == 0 -> "Expected node: " <> name
      | otherwise -> "Expected another node: " <> name
    DecodeError_ExpectedArg{..} -> "Expected arg #" <> (Text.pack . show) index
    DecodeError_ExpectedProp{..} -> "Expected prop: " <> name
    DecodeError_MismatchedAnn{..} -> "Expected one of " <> (Text.pack . show) validAnns <> ", got: " <> renderIdentifier givenAnn
    DecodeError_BaseValueDecodeFail{..} -> "Expected " <> expectedType <> ", got: " <> renderBaseValue baseValue
    DecodeError_UnknownNodes nodes -> "Unexpected nodes: " <> (Text.pack . show) nodes -- TODO: show better
    DecodeError_UnknownEntries entries -> "Unexpected entries: " <> (Text.pack . show) entries -- TODO: show better
