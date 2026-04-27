## v1.1.1

* Fix running tests with sdist bundle

## v1.1.0

* Rename `KDL.text` to `KDL.string`
  * Matches the rename of the `Text` constructor to `String`
* Rename `BaseDecodeError` to `DecodeErrorKind`
* Create new `BaseDecodeError` alias
* Prune internal re-exports of `KDL.Decoder.Internal.DecodeM`
* Improve decoding error reporting ([#28](https://github.com/brandonchinn178/kdl-hs/issues/28))
  * Might cause slight performance regressions - please create a ticket if you notice any!

## v1.0.1

* Skip `kdl-test` tests if `dotslash` isn't installed

## v1.0.0

* Implement KDL v2 parser
* Implement `KDL.render`, which is format-preserving
* Improve rendering parse errors
* Include filepath in error messages when `decodeFileWith` fails
* Support adding span information

API changes:
* Rename `Text` constructor to `String` in `ValueData`
* Overhauled `*Format` data types
* Removed selector functions (e.g. `nodeName` => `(.name)` or `Node{name}`)

## v0.2.1

* Add `KDL.Applicative`
* Re-export `Control.Arrow` and `Control.Category` from `KDL.Arrow`, for convenience

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
