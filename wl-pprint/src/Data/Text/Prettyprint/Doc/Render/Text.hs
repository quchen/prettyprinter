{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render an unannotated 'SimpleDoc' as plain 'Text'.
module Data.Text.Prettyprint.Doc.Render.Text (
    -- * Conversion to plain 'Text'
    renderLazy, renderStrict,

    -- * Render directly to 'stdout'
    renderIO,

    -- ** Convenience functions
    putDoc, hPutDoc
) where



import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO      as LT
import           System.IO

import Data.Text.Prettyprint.Doc



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XLambdaCase
-- >>> import qualified Data.Text.IO as T
-- >>> import qualified Data.Text.Lazy.IO as LT



-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering function
-- and transforms it to lazy text.
--
-- >>> let render = LT.putStrLn . renderLazy . layoutPretty defaultLayoutOptions
-- >>> let doc = "lorem" <+> align (vsep ["ipsum dolor", parens "foo bar", "sit amet"])
-- >>> render doc
-- lorem ipsum dolor
--       (foo bar)
--       sit amet
renderLazy :: SimpleDoc () -> LT.Text
renderLazy = TLB.toLazyText . build
  where
    build = \case
        SFail          -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
        SEmpty         -> mempty
        SChar c x      -> TLB.singleton c <> build x
        SText _l t x   -> TLB.fromText t <> build x
        SLine i x      -> TLB.singleton '\n' <> TLB.fromText (T.replicate i " ") <> build x
        SAnnPush _ x   -> build x
        SAnnPop x      -> build x

-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
renderStrict :: SimpleDoc () -> Text
renderStrict = LT.toStrict . renderLazy



-- | @('renderIO' h sdoc)@ writes @sdoc@ to the file @h@.
--
-- >>> renderIO System.IO.stdout (layoutPretty defaultLayoutOptions "hello\nworld")
-- hello
-- world
renderIO :: Handle -> SimpleDoc () -> IO ()
renderIO h sdoc = LT.hPutStrLn h (renderLazy sdoc)

-- | @('putDoc' doc)@ prettyprints document @doc@ to standard output, with a page
-- width of 80 characters and a ribbon width of 32 characters.
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
-- handle, e.g. a file or a socket. Uses a line length of 80, and a ribbon width
-- of 32 characters.
--
-- @
-- main = 'withFile' "someFile.txt" (\h -> 'hPutDoc' h ('vcat' ["vertical", "text"]))
-- @
--
-- @
-- 'hPutDoc' h doc = 'renderIO' h ('layoutPretty' 'defaultLayoutOptions' doc)
-- @
hPutDoc :: Handle -> Doc () -> IO ()
hPutDoc h doc = renderIO h (layoutPretty defaultLayoutOptions doc)
