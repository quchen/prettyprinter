wl-pprint-ansi compatibility module
===================================

This module defines a compatibility layer between the old `wl-pprint-ansi`
module, and the new one (version 1+).

This allows easily transitioning dependent packages from the old to the new
package, by simply replacing `wl-pprint-ansi` with `wl-pprint-compat-old-ansi`
in the `.cabal` file.

Note that this module is **only for transitional purposes**, and therefore
deprecated and wholly undocumented. For new development, use the current version
of `wl-pprint`, and the ANSI terminal backend provided in `wl-pprint-ansi`.
