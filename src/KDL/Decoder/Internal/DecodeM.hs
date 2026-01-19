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
  makeFatal,
  makeNonFatal,
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
--   * Has two error channels, one for fatal errors and one for non-fatal errors (see 'makeFatal')
--   * Collects as many errors as possible, within an Applicative context
data DecodeM a
  = DecodeM
      ( forall r.
        (DecodeError -> r) -> -- fatal error, not handled by <|>
        (DecodeError -> r) -> -- non-fatal error, handled by <|>
        (a -> r) ->
        r
      )

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

-- | Run a 'DecodeM' action and return the result or the error.
runDecodeM :: DecodeM a -> Either DecodeError a
runDecodeM (DecodeM f) = f Left Left Right

-- | Throw an error.
--
-- This error is non-fatal and can be handled by '<|>'. See 'makeFatal'
-- for more information.
decodeThrow :: BaseDecodeError -> DecodeM a
decodeThrow e = DecodeM $ \_ onFail _ -> onFail $ DecodeError Nothing [([], e)]

-- | Throw a 'DecodeError_Custom' error.
failM :: Text -> DecodeM a
failM = decodeThrow . DecodeError_Custom

-- | Make all errors in the given action fatal errors.
--
-- A la standard parsing libraries like megaparsec, errors should be
-- considered fatal when decoding has started consuming something.
makeFatal :: DecodeM a -> DecodeM a
makeFatal (DecodeM f) = DecodeM $ \onFatal _ onSuccess -> f onFatal onFatal onSuccess

-- | Make all errors non-fatal errors.
makeNonFatal :: DecodeM a -> DecodeM a
makeNonFatal (DecodeM f) = DecodeM $ \_ onFail onSuccess -> f onFail onFail onSuccess

-- | Add context to all errors that occur in the given action.
addContext :: ContextItem -> DecodeM a -> DecodeM a
addContext ctxItem (DecodeM f) = DecodeM $ \onFatal onFail onSuccess -> f (onFatal . addCtx) (onFail . addCtx) onSuccess
 where
  addCtx e = e{errors = [(ctxItem : ctx, msg) | (ctx, msg) <- e.errors]}
