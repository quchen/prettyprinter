{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render an unannotated 'SimpleDoc' as plain 'Text'.
module Data.Text.Prettyprint.Doc.Render.Text (
    -- * Tags for clarity

    -- | Since both lazy and strict text render simply as »Text« in Haddock,
    -- these simple tags make it obvious which one of the two is being used.
    Lazy, Strict,

    -- * Conversion to plain 'Text'
    renderLazy, renderStrict,

    -- * Render to a 'Handle'
    renderIO,

    -- ** Convenience functions
    putDoc, hPutDoc
) where



import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO      as TL
import           System.IO

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Util.Panic

#if !MIN_VERSION_base(4,8,0)
import Data.Semigroup
#endif

-- $setup
--
-- (Definitions for the doctests)
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.IO as T
-- >>> import qualified Data.Text.Lazy.IO as TL



type Lazy a = a
type Strict a = a



-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering function
-- and transforms it to lazy text.
--
-- >>> let render = TL.putStrLn . renderLazy . layoutPretty defaultLayoutOptions
-- >>> let doc = "lorem" <+> align (vsep ["ipsum dolor", parens "foo bar", "sit amet"])
-- >>> render doc
-- lorem ipsum dolor
--       (foo bar)
--       sit amet
renderLazy :: SimpleDoc () -> Lazy TL.Text
renderLazy = TLB.toLazyText . build
  where
    build = \case
        SFail          -> panicUncaughtFail
        SEmpty         -> mempty
        SChar c x      -> TLB.singleton c <> build x
        SText _l t x   -> TLB.fromText t <> build x
        SLine i x      -> TLB.singleton '\n' <> TLB.fromText (T.replicate i " ") <> build x
        SAnnPush _ x   -> build x
        SAnnPop x      -> build x

-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
renderStrict :: SimpleDoc () -> Strict Text
renderStrict = TL.toStrict . renderLazy



-- | @('renderIO' h sdoc)@ writes @sdoc@ to the file @h@.
--
-- >>> renderIO System.IO.stdout (layoutPretty defaultLayoutOptions "hello\nworld")
-- hello
-- world
renderIO :: Handle -> SimpleDoc () -> IO ()
renderIO h sdoc = TL.hPutStrLn h (renderLazy sdoc)

-- | @('putDoc' doc)@ prettyprints document @doc@ to standard output. Uses the
-- 'defaultLayoutOptions'.
--
-- >>> putDoc ("hello" <+> "world")
-- hello world
--
-- @
-- 'putDoc' = 'hPutDoc' 'stdout'
-- @
putDoc :: Doc () -> IO ()
putDoc = hPutDoc stdout

-- | Like 'putDoc', but instead of using 'stdout', print to a user-provided
-- handle, e.g. a file or a socket. Uses the 'defaultLayoutOptions'.
--
-- @
-- main = 'withFile' filename (\h -> 'hPutDoc' h doc)
--   where
--     doc = 'vcat' ["vertical", "text"]
--     filename = "someFile.txt"
-- @
--
-- @
-- 'hPutDoc' h doc = 'renderIO' h ('layoutPretty' 'defaultLayoutOptions' doc)
-- @
hPutDoc :: Handle -> Doc () -> IO ()
hPutDoc h doc = renderIO h (layoutPretty defaultLayoutOptions doc)
