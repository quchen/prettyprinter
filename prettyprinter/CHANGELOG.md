# [1.7.1]

- [Deprecate the `Data.Text.Prettyprint.*` modules](https://github.com/quchen/prettyprinter/pull/203)
  * Users should migrate to the new `Prettyprinter` module hierarchy.
  * The old modules will be removed no sooner than September 2022.
- [Make `text` an optional dependency:](https://github.com/quchen/prettyprinter/pull/202)
  * When built with `-f-text`, any `text`-based APIs will operate on `String`s instead.
- Documentation improvements:
  * [#194](https://github.com/quchen/prettyprinter/pull/194)
  * [`1f0bffe`](https://github.com/quchen/prettyprinter/commit/1f0bffe5eb53874d1ba46b0a80bda67c02365f1b)

[1.7.1]: https://github.com/quchen/prettyprinter/compare/v1.7.0...v1.7.1

# [1.7.0]

## Breaking changes

- [Fix `layoutPretty` and `layoutSmart` so they don't produce trailing whitespace as a result of indenting empty lines.](https://github.com/quchen/prettyprinter/pull/139)
  * Users of `removeTrailingWhitespace` should check whether it is still needed.
- [Use `floor` instead of `round` to compute ribbon width.](https://github.com/quchen/prettyprinter/pull/160)
- [Remove deprecated `Data.Text.Prettyprint.Doc.Render.ShowS` module.](https://github.com/quchen/prettyprinter/pull/173)
- [Add optimized implementation of `stimes` for `Doc`.](https://github.com/quchen/prettyprinter/pull/135)
- [Generalize the type of `layoutCompact` to clarify that it doesn't preserve annotations.](https://github.com/quchen/prettyprinter/pull/183)
- [Add strictness annotations in `SimpleDocStream` and `PageWidth`.](https://github.com/quchen/prettyprinter/pull/129)

## Non-breaking changes

- [Add shallower `Prettyprinter` module hierarchy exposing the same API.](https://github.com/quchen/prettyprinter/pull/174)
  * The current plan for the existing `Data.Text.Prettyprint.Doc*` modules is:
    * Start deprecation in early 2021.
    * Remove the modules after a deprecation period of at least one year.
- [Fix build with GHC 7.4.](https://github.com/quchen/prettyprinter/pull/187)
- Various documentation improvements.

[1.7.0]: https://github.com/quchen/prettyprinter/compare/v1.6.2...v1.7.0

# 1.6.2

- Speed up rendering to lazy and strict `Text`.
- Documentation improvements for `group` and `flatAlt`.
- Internal refactoring of the `layoutWadlerLeijen`-based layouters.

# 1.6.1

- Slightly reduce the scope of the fitting predicates for some edge cases.
- Use an export list in `Data.Text.Prettyprint.Doc.Internal`.
- Improve `group` for `Union` and `FlatAlt`.
- Speed up `removeTrailingWhitespace`.
- Improve generating spaces for indentation and `spaces`.
- Simplify some `Doc` constants by defining them as `Doc` literals.
- Enable `-O2`.
- Various documentation fixes and improvements.

# 1.6.0

## Breaking changes

- Fix `fuse`'s handling of annotated documents:
  - Don't remove annotations on empty documents.
  - Apply fusion within annotations.
- Fix layouting of hard linebreaks with `Unbounded` page widths.

## Non-breaking changes

- Speed up `group` for documents containing linebreaks and previously
  `group`ed documents.
- Add debugging helpers in `Data.Text.Prettyprint.Doc.Internal.Debug`
- Documentation improvements and fixes

# 1.5.1

- Removing trailing whitespace sometimes restored necessary whitespace in the
  wrong spot

# 1.5

- Fix inconsistent formatting within align and wide sub-docs on narrow layouts

# 1.4

- Add fixity declaration to `<+>`, matching `<>`
- Fix removal of trailing whitespace

# 1.3.0.1

- Support Stack 2

# 1.3.0

- Add alignment to Pretty [a] instance
- Fix removal of blank lines in `removeTrailingWhitespace`
- Widened support for GHC versions 7.4–8.8

# 1.2.1.1

- Fix dependency of doctest suite

# 1.2.1

- Add function to trim trailing space in layouted `SimpleDocStream`,
  `removeTrailingWhitespace`
- Add `Pretty` instances for `Identity` and `Const`

# 1.2.0.1

- Fix `alterAnnotationsS` (and thus `unAnnotateS`), which removed pushing, but
  not popping, style frames. This led to them throwing errors in pretty much all
  use cases.

# 1.2

- `encloseSep` does no longer include an `align` wrapper; in other words,

    ```haskell
    encloseSep_old … = align (encloseSep_new …)
    ```
- Change the default ribbon fraction to 1 (was 0.4)
- Expose `viaShow` and `unsafeViaShow` from the public module
- Fix `layoutSmart` behaving as if there was no space left for unbounded pages

# 1.1.1

- Add `panicPeekedEmpty` and `panicPoppedEmpty` to the panic module

# 1.1.0.1

- Rendering directly to a handle is now more efficient in the `Text` renderer,
  since no intermediate `Text` is generated anymore.
- Remove upper version bounds from `.cabal` files

# 1.1

- Allow `alterAnnotations` to convert one annotation to multiple ones, to
  support e.g. `Keyword ---> Green+Bold`
- Remove `Pretty` instance for `Doc`: the implicit un-annotation done by it did
  more harm than good.

# 1.0.1

- Add `alterAnnotations`, which allows changing or removing annotations.
  `reAnnotate` and `unAnnotate` are now special cases of this.
- Fix »group« potentially taking exponential time, by making the (internal)
  `flatten` function detect whether it is going to have any effect inside
  `group`.
- Add proper version bounds for all dependencies and backport them to version 1
- Haddock: example for `Pretty Void`

# 1

- Add Foldable/Traversable instances for `SimpleDocTree`, `SimpleDocStream`
- Add Functor instances for `Doc`, `SimpleDocTree`, `SimpleDocStream`
- Add the simplified renderers `renderSimplyDecorated` and
  `renderSimplyDecoratedA` to the tree and stack renderer modules
- Lots of typo fixes and doc tweaks
- Add a changelog :-)

# 0.1

Initial release.
