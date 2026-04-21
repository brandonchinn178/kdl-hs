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
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import KDL.Decoder.Internal.Error

-- | The monad that returns either a 'DecodeError' or a result of type @a@.
--
-- The odd structure here is because of our backtracking semantics. We want to
-- collect all errors that may appear (even if a value is successfully parsed)
-- so that if we get a failure later on, we can return the deepest error, even
-- if it was in a successful branch.
--
-- Take this motivating example: a node takes an arbitrary number of string
-- args. If you pass some strings then a number, it'll successfully parse up to
-- the number and return success, only for the node to fail later with
-- "unexpected argument: 123". But the true error was
-- "unexpected number, expected string".
data DecodeM a
  = DecodeM_Found a [BaseDecodeError]
  | DecodeM_Fail (NonEmpty BaseDecodeError)

instance Functor DecodeM where
  fmap f = \case
    DecodeM_Found a es -> DecodeM_Found (f a) es
    DecodeM_Fail es -> DecodeM_Fail es
instance Applicative DecodeM where
  pure x = DecodeM_Found x []
  l <*> r =
    case (l, r) of
      (DecodeM_Found f es1, DecodeM_Found a es2) -> DecodeM_Found (f a) (mergeErrorsLR es1 es2)
      (DecodeM_Found _ es1, DecodeM_Fail es2) -> DecodeM_Fail (mergeErrorsL es1 es2)
      (DecodeM_Fail es1, DecodeM_Found _ es2) -> DecodeM_Fail (mergeErrorsR es1 es2)
      (DecodeM_Fail es1, DecodeM_Fail es2) -> DecodeM_Fail (mergeErrors es1 es2)
instance Monad DecodeM where
  (>>) = (*>)
  m >>= k =
    case m of
      DecodeM_Fail es1 -> DecodeM_Fail es1
      DecodeM_Found a es1 ->
        case k a of
          DecodeM_Found b es2 -> DecodeM_Found b (mergeErrorsLR es1 es2)
          DecodeM_Fail es2 -> DecodeM_Fail (mergeErrorsL es1 es2)
instance Alternative DecodeM where
  empty = failM "<empty>"
  l <|> r =
    case l of
      DecodeM_Found a es1 -> DecodeM_Found a es1
      DecodeM_Fail es1 ->
        case r of
          DecodeM_Found a es2 -> DecodeM_Found a (NonEmpty.toList $ mergeErrorsR es1 es2)
          DecodeM_Fail es2 -> DecodeM_Fail (mergeErrors es1 es2)

-- | Run a 'DecodeM' action and return the result or the deepest error found.
runDecodeM :: DecodeM a -> Either DecodeError a
runDecodeM = \case
  DecodeM_Found a _ -> Right a
  DecodeM_Fail errors -> Left DecodeError{filepath = Nothing, errors}

mergeErrors ::
  NonEmpty BaseDecodeError ->
  NonEmpty BaseDecodeError ->
  NonEmpty BaseDecodeError
mergeErrors es1 es2 =
  case compare (key es1) (key es2) of
    LT -> es2
    EQ -> es1 <> es2
    GT -> es1
 where
  key = length . fst . NonEmpty.head

mergeErrorsL ::
  [BaseDecodeError] ->
  NonEmpty BaseDecodeError ->
  NonEmpty BaseDecodeError
mergeErrorsL l r = maybe r (\l' -> mergeErrors l' r) (NonEmpty.nonEmpty l)

mergeErrorsR ::
  NonEmpty BaseDecodeError ->
  [BaseDecodeError] ->
  NonEmpty BaseDecodeError
mergeErrorsR l r = maybe l (\r' -> mergeErrors l r') (NonEmpty.nonEmpty r)

mergeErrorsLR ::
  [BaseDecodeError] ->
  [BaseDecodeError] ->
  [BaseDecodeError]
mergeErrorsLR l r =
  case (l, r) of
    ([], _) -> r
    (_, []) -> l
    (x : xs, y : ys) -> NonEmpty.toList $ mergeErrors (x :| xs) (y :| ys)

mapErrors :: (BaseDecodeError -> BaseDecodeError) -> DecodeM a -> DecodeM a
mapErrors f = \case
  DecodeM_Found a es -> DecodeM_Found a (fmap f es)
  DecodeM_Fail es -> DecodeM_Fail (fmap f es)

-- | Throw an error.
decodeThrow :: DecodeErrorKind -> DecodeM a
decodeThrow e = DecodeM_Fail . NonEmpty.singleton $ ([], e)

-- | Throw a 'DecodeError_Custom' error.
failM :: Text -> DecodeM a
failM = decodeThrow . DecodeError_Custom

-- | Add context to all errors that occur in the given action.
addContext :: ContextItem -> DecodeM a -> DecodeM a
addContext ctxItem = mapErrors (first (ctxItem :))
