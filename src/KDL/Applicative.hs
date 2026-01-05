{-# LANGUAGE DataKinds #-}

{-|
This module is intended to be imported qualified as:

> import KDL.Applicative qualified as KDL

This module is equivalent to "KDL", except when @ApplicativeDo@ and
@QualifiedDo@ are enabled, @KDL.do@ will ensure you don't accidentally use
monadic operations.

The Applicative decoder can do most of the things the Arrow decoder can do,
except run different decoders based on a previously decoded result. If you
need to do that, either use "KDL.Arrow", or follow this example:

@
# Example KDL config

rules {
  - a
  - b {
    foo 123 # only allowed in b, not a
  }
}
@

@
{\-# LANGUAGE ApplicativeDo #-\}
{\-# LANGUAGE QualifiedDo #-\}

import KDL.Arrow ((>>>), (|||))
import KDL.Arrow qualified

decoder = KDL.do
  rules <-
    KDL.dashNodesWith "rules" $
      (toEither \<$> KDL.arg) >>> fromEither
  pure rules
 where
  toEither = \case
    "a" -> Left $ ()
    "b" -> Right . Left $ ()
    name -> Right . Right $ "Invalid rule: " <> name
  fromEither =
    -- "a"
    pure RuleA |||
    -- "b"
    (RuleB \<$> KDL.children (KDL.argAt "foo")) |||
    -- else
    KDL.Arrow.fail
@
-}
module KDL.Applicative (
  -- * QualifiedDo
  Prelude.fmap,
  Prelude.pure,
  Prelude.return,
  (Prelude.<*>),
  (Prelude.>>),
  (>>=),

  -- * Re-exports
  module X,
) where

import GHC.TypeError qualified as GHC
import KDL.Decoder as X
import KDL.Parser as X
import KDL.Render as X
import KDL.Types as X
import Prelude hiding ((>>=))

class NoBind a where
  -- | Gives a compile-time error if used.
  --
  -- It seems like QualifiedDo still needs a definition for this, even with
  -- ApplicativeDo enabled.
  -- https://gitlab.haskell.org/ghc/ghc/-/issues/26723
  (>>=) :: a
instance (GHC.Unsatisfiable (GHC.Text ">>= is not allowed in a KDL.do block")) => NoBind a where
  (>>=) = GHC.unsatisfiable
