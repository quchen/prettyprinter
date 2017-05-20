module MultilineTh (multiline) where



import qualified Data.Text                  as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Prelude



multiline :: QuasiQuoter
multiline = QuasiQuoter
    { quoteExp = quoteUnlines
    , quotePat = const badUse
    , quoteType = const badUse
    , quoteDec = const badUse
    }
  where
    badUse = fail "multiline quasiquoter can only be used as an expression"

quoteUnlines :: String -> Q Exp
quoteUnlines =
      liftString
    . T.unpack
    . T.unwords
    . filter (not . T.null)
    . T.words
    . T.pack
