# 1.2.0.0 (Next release)

- Add collapsible newlines via `hardlineCollapse`. This changes the `Doc` type,
  because the `Line` constructor now takes an argument.

# 1.1.0.1 (Next release)

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
