{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module KDL.Decoder.Internal.DecodeM (
  -- * Decoding errors
  module KDL.Decoder.Internal.Error,

  -- * DecodeM monad
  DecodeM (..),
  runDecodeM,
  decodeThrow,
  failM,
  addContext,
) where

import Control.Applicative (Alternative (..))
import Data.Text (Text)
import KDL.Decoder.Internal.Error

-- | The monad that returns either a 'DecodeError' or a result of type @a@.
--
-- To a first approximation, this monad is equivalent to the @Either DecodeError@
-- monad, with the following changes:
--
--   * Uses continuation-passing style for performance
--   * Collects as many errors as possible, within an Applicative context
data DecodeM a
  = DecodeM
      ( forall r.
        (DecodeError -> r) ->
        (a -> r) ->
        r
      )

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

-- | Run a 'DecodeM' action and return the result or the error.
runDecodeM :: DecodeM a -> Either DecodeError a
runDecodeM (DecodeM f) = f Left Right

-- | Throw an error.
decodeThrow :: DecodeErrorKind -> DecodeM a
decodeThrow e = DecodeM $ \onFail _ -> onFail $ DecodeError Nothing [([], e)]

-- | Throw a 'DecodeError_Custom' error.
failM :: Text -> DecodeM a
failM = decodeThrow . DecodeError_Custom

-- | Add context to all errors that occur in the given action.
addContext :: ContextItem -> DecodeM a -> DecodeM a
addContext ctxItem (DecodeM f) = DecodeM $ \onFail onSuccess -> f (onFail . addCtx) onSuccess
 where
  addCtx e = e{errors = [(ctxItem : ctx, msg) | (ctx, msg) <- e.errors]}
