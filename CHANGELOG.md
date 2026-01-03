## Unreleased

## v0.2.0

* Remove monad `Decoder` newtype, just give the `Monad` instance to the canonical `Decoder` type and use the same `Decoder` type everywhere
* Reorganize module structure
* Schema improvements
* Rename `Decoder` => `DecodeArrow`, set `Decoder o a` as alias for `DecodeArrow o () a`
* Use v2 syntax for `#true`/`#false`/`#null`

## v0.1.0

Initial release, with:

* Support for decoding v1 KDL syntax (and a subset of v2 KDL syntax)
* Decoding via Arrows or Monads
* Statically determine decoder schema
