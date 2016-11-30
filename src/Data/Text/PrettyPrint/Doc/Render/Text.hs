{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render 'SimpleDoc' as plain 'Text'.
module Data.Text.PrettyPrint.Doc.Render.Text (
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

import Data.Text.PrettyPrint.Doc



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XLambdaCase
-- >>> import qualified Data.Text.IO as T
-- >>> import qualified Data.Text.Lazy.IO as LT



-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering function
-- and transforms it to lazy text.
--
-- All styling information is discarded. If this is undesirable, maybe the
-- functions in "Data.Text.PrettyPrint.Doc.Render.Terminal" are closer to what
-- you are looking for.
--
-- >>> let render = LT.putStrLn . renderLazy . layoutPretty 0.4 80
-- >>> let doc = "lorem" <+> align (vsep ["ipsum dolor", parens (red "styles are ignored"), "sit amet"])
-- >>> render doc
-- lorem ipsum dolor
--       (styles are ignored)
--       sit amet
renderLazy :: SimpleDoc -> LT.Text
renderLazy = TLB.toLazyText . build
  where
    build = \case
        SFail          -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
        SEmpty         -> mempty
        SChar c x      -> TLB.singleton c <> build x
        SText t x      -> TLB.fromText t <> build x
        SLine i x      -> TLB.singleton '\n' <> TLB.fromText (T.replicate i " ") <> build x
        SStylePush _ x -> build x
        SStylePop x    -> build x

-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
renderStrict :: SimpleDoc -> Text
renderStrict = LT.toStrict . renderLazy



-- | @('renderIO' h sdoc)@ writes @sdoc@ to the file @h@.
renderIO :: Handle -> SimpleDoc -> IO ()
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
putDoc :: Doc -> IO ()
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
-- 'hPutDoc' h doc = 'renderIO' h ('layoutPretty' 0.4 80 doc)
-- @
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc h doc = renderIO h (layoutPretty 0.4 80 doc)
