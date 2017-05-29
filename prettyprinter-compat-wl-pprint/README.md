wl-pprint compatibility package
===============================

This package defines a compatibility layer between the old `wl-pprint` package,
and the new `prettyprinter` package.

This allows easily transitioning dependent packages from the old to the new
package, by simply replacing `wl-pprint` with `prettyprinter-compat-wl-pprint`
in the `.cabal` file.

Note that this package is **only for transitional purposes**, and therefore
deprecated and wholly undocumented. For new development, use the current version
of `prettyprinter`.
