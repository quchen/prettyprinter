# Next version: 1.0.*

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
