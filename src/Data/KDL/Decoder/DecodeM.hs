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
  makeFatal,
  makeNonFatal,
  addContext,
  runDecodeM,
  renderDecodeError,
) where

import Control.Applicative (Alternative (..))
import Data.KDL.Types (
  BaseValue,
  Identifier,
  Value,
  renderBaseValue,
  renderIdentifier,
  renderValue,
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

data DecodeM a
  = DecodeM
      ( forall r.
        (DecodeError -> r) -> -- fatal error, not handled by <|>
        (DecodeError -> r) -> -- non-fatal error, handled by <|>
        (a -> r) ->
        r
      )

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
  | DecodeError_UnexpectedNode {identifier :: Identifier, index :: Int}
  | DecodeError_UnexpectedArg {index :: Int, value :: Value}
  | DecodeError_UnexpectedProp {identifier :: Identifier, value :: Value}
  deriving (Show, Eq)

instance Functor DecodeM where
  fmap f (DecodeM k) = DecodeM $ \onFatal onFail onSuccess -> k onFatal onFail (onSuccess . f)
instance Applicative DecodeM where
  pure x = DecodeM $ \_ _ onSuccess -> onSuccess x
  DecodeM kf <*> DecodeM ka = DecodeM $ \onFatal onFail onSuccess ->
    -- Collect all errors
    kf
      (\e1 -> ka (\e2 -> onFatal $ e1 <> e2) (\e2 -> onFatal $ e1 <> e2) (\_ -> onFatal e1))
      (\e1 -> ka (\e2 -> onFatal $ e1 <> e2) (\e2 -> onFail $ e1 <> e2) (\_ -> onFail e1))
      (\f -> ka onFatal onFail (onSuccess . f))
instance Monad DecodeM where
  (>>) = (*>)
  DecodeM ka >>= k = DecodeM $ \onFatal onFail onSuccess ->
    ka onFatal onFail $ \a -> let DecodeM kb = k a in kb onFatal onFail onSuccess
instance Alternative DecodeM where
  empty = DecodeM $ \_ onFail _ -> onFail mempty
  DecodeM k1 <|> DecodeM k2 = DecodeM $ \onFatal onFail onSuccess ->
    k1
      onFatal
      (\e1 -> k2 onFatal (\e2 -> onFail $ e1 <> e2) onSuccess)
      onSuccess

-- | Throw an error.
--
-- This error is non-fatal and can be handled by '<|>'. See 'makeFatal'
-- for more information.
decodeThrow :: BaseDecodeError -> DecodeM a
decodeThrow e = DecodeM $ \_ onFail _ -> onFail $ DecodeError [([], e)]

failM :: Text -> DecodeM a
failM = decodeThrow . DecodeError_Custom

-- | Make all errors in the given action fatal errors.
--
-- Inspired by standard parsing libraries like megaparsec, errors should be
-- considered fatal when parsing has started consuming something.
makeFatal :: DecodeM a -> DecodeM a
makeFatal (DecodeM f) = DecodeM $ \onFatal _ onSuccess -> f onFatal onFatal onSuccess

-- | Make all errors non-fatal errors.
makeNonFatal :: DecodeM a -> DecodeM a
makeNonFatal (DecodeM f) = DecodeM $ \_ onFail onSuccess -> f onFail onFail onSuccess

addContext :: ContextItem -> DecodeM a -> DecodeM a
addContext ctxItem (DecodeM f) = DecodeM $ \onFatal onFail onSuccess -> f (onFatal . addCtx) (onFail . addCtx) onSuccess
 where
  addCtx (DecodeError es) = DecodeError $ [(ctxItem : ctx, msg) | (ctx, msg) <- es]

runDecodeM :: DecodeM a -> Either DecodeError a
runDecodeM (DecodeM f) = f Left Left Right

renderDecodeError :: DecodeError -> Text
renderDecodeError = Text.intercalate "\n" . map renderCtxErrors . groupCtxErrors
 where
  -- Group errors with the same contexts together
  groupCtxErrors (DecodeError es) = Map.toAscList $ Map.fromListWith (<>) [(ctx, [e]) | (ctx, e) <- es]

  renderCtxErrors (ctx, errs) =
    Text.intercalate "\n" $ ("At: " <> renderCtxItems ctx) : renderErrors errs

  renderCtxItems items
    | null items = "<root>"
    | otherwise = Text.intercalate " > " . map renderCtxItem $ items
  renderCtxItem = \case
    ContextNode{..} -> renderIdentifier name <> " #" <> showT index
    ContextArg{..} -> "arg #" <> showT index
    ContextProp{..} -> "prop " <> renderIdentifier name

  renderErrors = map (("  " <>) . renderError)
  renderError = \case
    DecodeError_Custom msg -> msg
    DecodeError_ParseError msg -> msg
    DecodeError_ExpectedNode{..}
      | index == 0 -> "Expected node: " <> name
      | otherwise -> "Expected another node: " <> name
    DecodeError_ExpectedArg{..} -> "Expected arg #" <> showT index
    DecodeError_ExpectedProp{..} -> "Expected prop: " <> name
    DecodeError_MismatchedAnn{..} -> "Expected one of " <> showT validAnns <> ", got: " <> renderIdentifier givenAnn
    DecodeError_BaseValueDecodeFail{..} -> "Expected " <> expectedType <> ", got: " <> renderBaseValue baseValue
    DecodeError_UnexpectedNode{..} -> "Unexpected node: " <> renderIdentifier identifier <> " #" <> showT index
    DecodeError_UnexpectedArg{..} -> "Unexpected arg #" <> showT index <> ": " <> renderValue value
    DecodeError_UnexpectedProp{..} -> "Unexpected prop " <> renderIdentifier identifier <> ": " <> renderValue value

  -- Replace with Text.show after requiring at least text-2.1.2
  showT :: (Show a) => a -> Text
  showT = Text.pack . show
