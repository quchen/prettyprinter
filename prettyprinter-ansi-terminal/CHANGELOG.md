# 1.2.0

- Make `renderLazy` lazy, and speed it up.

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
