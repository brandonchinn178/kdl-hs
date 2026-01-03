{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module KDL.Decoder.Internal.Decoder (
  -- * Decoder
  Decoder,

  -- * DecodeArrow
  DecodeArrow (..),
  liftDecodeM,
  withDecoder,
  fail,
  debug,

  -- * DecodeStateM
  DecodeStateM,
  runDecodeStateM,

  -- * DecodeState
  HasDecodeHistory (..),
  DecodeHistory (..),
  DecodeState (..),
) where

import Control.Applicative (
  Alternative (..),
 )
import Control.Arrow (Arrow (..), ArrowChoice (..), (>>>))
import Control.Category (Category)
import Control.Category qualified
import Control.Monad ((>=>))
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as StateT
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Debug.Trace (traceM)
import KDL.Decoder.Internal.DecodeM
import KDL.Decoder.Schema (
  Schema (..),
  SchemaOf,
  schemaAlt,
  schemaJoin,
 )
import KDL.Types (Identifier, Node, NodeList, Value)
import Prelude hiding (any, fail, null)

class HasDecodeHistory o where
  data DecodeHistory o
  emptyDecodeHistory :: DecodeHistory o

instance HasDecodeHistory NodeList where
  data DecodeHistory NodeList = DecodeHistory_NodeList
    { nodesSeen :: Map Text Int
    }
  emptyDecodeHistory = DecodeHistory_NodeList{nodesSeen = Map.empty}

instance HasDecodeHistory Node where
  data DecodeHistory Node = DecodeHistory_Node
    { argsSeen :: Int
    , propsSeen :: Set Identifier
    , childrenHistory :: DecodeHistory NodeList
    }
  emptyDecodeHistory =
    DecodeHistory_Node
      { argsSeen = 0
      , propsSeen = Set.empty
      , childrenHistory = emptyDecodeHistory
      }

instance HasDecodeHistory Value where
  data DecodeHistory Value = DecodeHistory_Value
  emptyDecodeHistory = DecodeHistory_Value

-- | The state to track when decoding an object of type @o@.
--
-- At each decode step, some value within @o@ is consumed and
-- the action is recorded in the history.
data DecodeState o = DecodeState
  { object :: !o
  , history :: DecodeHistory o
  -- ^ Not strict, since this only matters for reporting errors
  }

type DecodeStateM o a = StateT (DecodeState o) DecodeM a

runDecodeStateM :: o -> DecodeHistory o -> DecodeStateM o a -> DecodeM a
runDecodeStateM o hist m =
  StateT.evalStateT m $
    DecodeState
      { object = o
      , history = hist
      }

-- | @DecodeArrow o a b@ represents an arrow with input @a@ and output @b@, within
-- the context of decoding a KDL object of type @o@. It also knows the expected
-- schema of @o@. Most of the time, @a@ is @()@; it would only be different if
-- you're using Arrows notation.
--
-- We're using arrows here so that we can:
--
--   1. Get the schema without running the decoder, and also
--   2. Use previously decoded values to inform decoding other values
--
-- Using monads alone would lose (1), but applicatives can't do (2).
data DecodeArrow o a b = DecodeArrow
  { schema :: SchemaOf o
  , run :: a -> DecodeStateM o b
  }

instance Category (DecodeArrow o) where
  id = liftDecodeM pure
  DecodeArrow sch2 bc . DecodeArrow sch1 ab = DecodeArrow (sch1 `schemaJoin` sch2) $ ab >=> bc
instance Arrow (DecodeArrow o) where
  arr f = liftDecodeM (pure . f)
  DecodeArrow sch1 bc *** DecodeArrow sch2 bc' =
    DecodeArrow (sch1 `schemaJoin` sch2) $ \(b, b') -> (,) <$> bc b <*> bc' b'
instance ArrowChoice (DecodeArrow o) where
  DecodeArrow sch1 bc +++ DecodeArrow sch2 bc' =
    DecodeArrow (sch1 `schemaAlt` sch2) $ either (fmap Left . bc) (fmap Right . bc')

instance Functor (DecodeArrow o a) where
  fmap f (DecodeArrow schema run) = DecodeArrow schema $ (fmap f . run)
instance Applicative (DecodeArrow o a) where
  pure = arr . const
  DecodeArrow sch1 kf <*> DecodeArrow sch2 kx =
    DecodeArrow (sch1 `schemaJoin` sch2) $ \a -> kf a <*> kx a
instance Alternative (DecodeArrow o a) where
  -- Can't use StateT's Alternative instance: https://hub.darcs.net/ross/transformers/issue/78
  empty = DecodeArrow (SchemaOr []) $ \_ -> Trans.lift empty
  DecodeArrow sch1 run1 <|> DecodeArrow sch2 run2 =
    DecodeArrow (sch1 `schemaAlt` sch2) $ \a -> StateT.StateT $ \s -> do
      StateT.runStateT (run1 a) s <|> StateT.runStateT (run2 a) s
  some (DecodeArrow sch run) =
    DecodeArrow (SchemaSome sch) $ \a ->
      StateT.StateT $
        let go s0 = do
              (x, s1) <- StateT.runStateT (run a) s0
              (xs, s2) <- go s1 <|> pure ([], s1)
              pure (x : xs, s2)
         in go
  many (DecodeArrow sch run) = some (DecodeArrow sch run) <|> pure []

type Decoder o a = DecodeArrow o () a

-- | Eliminates all schema information; avoid whenever possible.
instance Monad (DecodeArrow o a) where
  DecodeArrow _ run1 >>= k =
    DecodeArrow SchemaUnknown $ \a -> do
      x <- run1 a
      let DecodeArrow _ run2 = k x
      run2 a

liftDecodeM :: (a -> DecodeM b) -> DecodeArrow o a b
liftDecodeM f = DecodeArrow (SchemaAnd []) (Trans.lift . f)

-- | Run actions within a t'DecodeArrow'. Useful for adding post-processing logic.
--
-- === __Example__
--
-- @
-- decoder = KDL.withDecoder KDL.number $ \\x -> do
--   when (x > 100)
--     KDL.failM $ "argument is too large: " <> (Text.pack . show) x
--   pure $ MyVal x
-- @
withDecoder :: forall o a b c. DecodeArrow o a b -> (b -> DecodeM c) -> DecodeArrow o a c
withDecoder decoder f = decoder >>> liftDecodeM f

-- | Unconditionally fail the decoder.
--
-- === __Example__
--
-- @
-- decoder = proc () -> do
--   x <- KDL.arg -< ()
--   if x > 100
--     then KDL.fail -\< "argument is too large: " <> (Text.pack . show) x
--     else returnA -< ()
--   returnA -< x
-- @
fail :: forall b o. DecodeArrow o Text b
fail = DecodeArrow (SchemaOr []) (Trans.lift . failM)

-- | Debug the current state of the object being decoded.
--
-- === __Example__
--
-- @
-- decoder = proc () -> do
--   KDL.debug -< ()    -- Node{entries = [Entry{}, Entry{}]}
--   x <- KDL.arg -< ()
--   KDL.debug -< ()    -- Node{entries = [Entry{}]}
--   y <- KDL.arg -< ()
--   KDL.debug -< ()    -- Node{entries = []}
--   returnA -< (x, y)
-- @
debug :: forall o a. (Show o) => DecodeArrow o a ()
debug =
  DecodeArrow (SchemaAnd []) $ \_ -> do
    o <- StateT.gets (.object)
    traceM $ "[kdl-hs] DEBUG: " ++ show o
