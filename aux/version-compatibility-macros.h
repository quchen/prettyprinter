-- These macros allow writing CPP compatibility hacks in a way that makes their
-- purpose much clearer than just demanding a specific version of a library.

#define APPLICATIVE_MONAD               MIN_VERSION_base(4,8,0)
#define FOLDABLE_TRAVERSABLE_IN_PRELUDE MIN_VERSION_base(4,8,0)
#define MONOID_IN_PRELUDE               MIN_VERSION_base(4,8,0)
#define NATURAL_IN_BASE                 MIN_VERSION_base(4,8,0)
#define SEMIGROUP_IN_BASE               MIN_VERSION_base(4,8,0)

#define MONAD_FAIL                      MIN_VERSION_base(4,9,0)
