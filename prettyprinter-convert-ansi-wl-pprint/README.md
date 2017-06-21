ansi-wl-pprint conversion package
=================================

This package defines a converter from the old `ansi-wl-pprint` document type to
the new `prettyprinter` one. Its purpose is making packages that only generate
`ansi-wl-pprint` data available to the `prettyprinter` ecosystem.

Note the difference to the `prettyprinter-compat-ansi-wl-pprint` module, which
does *not* convert any data, and instead provides an API that mimicks
`ansi-wl-pprint`, while secretly being `prettyprinter`-based behind the
curtains. This package on the other hand does a proper conversion.
