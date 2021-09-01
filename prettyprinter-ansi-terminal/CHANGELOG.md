# [1.1.3]

- [Deprecate the `Data.Text.Prettyprint.*` modules](https://github.com/quchen/prettyprinter/pull/203)
  * Users should migrate to the new `Prettyprinter` module hierarchy.
  * The old modules will be removed no sooner than September 2022.

[1.1.3]: https://github.com/quchen/prettyprinter/compare/ansi-terminal-v1.1.2...ansi-terminal-v1.1.3

# [1.1.2]

- [Add shallower `Prettyprinter` module hierarchy exposing the same API.](https://github.com/quchen/prettyprinter/pull/174)
  * The current plan for the existing `Data.Text.Prettyprint.Doc*` modules is:
    * Start deprecation in early 2021.
    * Remove the modules after a deprecation period of at least one year.
- [Make `renderLazy` lazy, and speed it up.](https://github.com/quchen/prettyprinter/pull/176)
- [Add export list for Prettyprinter.Render.Terminal.Internal.](https://github.com/quchen/prettyprinter/pull/148)
- [Optimize generating spaces for indentation.](https://github.com/quchen/prettyprinter/pull/132)
- [Enable `-O2`.](https://github.com/quchen/prettyprinter/pull/144)
- [Extend GHC support to 7.6 and 7.4.](https://github.com/quchen/prettyprinter/pull/74)

[1.1.2]: https://github.com/quchen/prettyprinter/compare/ansi-terminal-v1.1.1.2...ansi-terminal-v1.1.2

# 1.1.1.2

- Fix documentation claiming there would be a trailing newline in `renderIO`
  when there is none

# 1.1.1.1

- `renderIO` now renders directly to STDOUT, instead of first building a textual
  rendering and then printing that to STDOUT.

# 1.1.1

- Expose `AnsiStyle`’s constructors for adaptability

# 1.1

- Overhauled the API significantly – Styles are now combined using the
  `Semigroup` instance from a number of readable primitives.

# 1.0.1

Fix version shenanigans, since the prerelease was released to Hackage as version
1 already, so uploading the »new« version 1 did not work

# 1

Initial release
