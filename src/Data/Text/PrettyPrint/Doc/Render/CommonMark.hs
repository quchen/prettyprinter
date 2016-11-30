{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render 'SimpleDoc' as common markdown AKA CommonMark in 'Text' format.
module Data.Text.PrettyPrint.Doc.Render.CommonMark (
    -- * Conversion to CommonMark-infused 'Text'
    renderLazy,
    renderStrict,

    -- * Render directly to 'stdout'
    renderIO,

    -- ** Convenience functions
    putDoc, hPutDoc,
) where



import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO      as LT
import           System.IO

import Data.Text.PrettyPrint.Doc
import Data.Text.PrettyPrint.Doc.Render.RenderM



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XLambdaCase
-- >>> import qualified Data.Text.IO as T
-- >>> import qualified Data.Text.Lazy.IO as LT



-- | Add Markdown-style markers for emphasis and strong emphasis. Other styles
-- are ignored.
--
-- >>> let doc = "This text" <+> italics ("is emphasized" <+> bold "even stronger" <> "!")
-- >>> let pprint = LT.putStrLn . renderLazy . layoutPretty 0.4 40
-- >>> pprint doc
-- This text *is emphasized **even stronger**!*
-- >>> pprint (red doc)
-- This text *is emphasized **even stronger**!*
renderLazy :: SimpleDoc -> LT.Text
renderLazy doc
  = let (resultBuilder, remainingStyles) = execRenderM [] (build doc)
    in if null remainingStyles
        then TLB.toLazyText resultBuilder
        else error ("There are "
                    <> show (length remainingStyles)
                    <> " unpaired styles! Please report this as a bug.")

build :: SimpleDoc -> RenderM TLB.Builder Style ()
build = \case
    SFail -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
    SEmpty -> pure ()
    SChar c x -> do writeResult (TLB.singleton c)
                    build x
    SText t x -> do writeResult (TLB.fromText t)
                    build x
    SLine i x -> do writeResult (TLB.singleton '\n' )
                    writeResult (TLB.fromText (T.replicate i " "))
                    build x
    SStylePush s x -> do
        pushStyle s
        writeResult (styleToMarker s)
        build x
    SStylePop x -> do
        s <- unsafePopStyle
        writeResult (styleToMarker s)
        build x

styleToMarker :: Style -> TLB.Builder
styleToMarker = \case
    SItalicized -> TLB.fromString "*"
    SBold       -> TLB.fromString "**"
    _other      -> mempty

-- | Strict version of 'renderLazy'.
renderStrict :: SimpleDoc -> Text
renderStrict = LT.toStrict . renderLazy



-- | @('renderIO' h sdoc)@ writes @sdoc@ to the file @h@.
renderIO :: Handle -> SimpleDoc -> IO ()
renderIO h sdoc = LT.hPutStrLn h (renderLazy sdoc)

-- | @('putDoc' doc)@ prettyprints document @doc@ to standard output, with a
-- page width of 80 characters and a ribbon width of 32 characters.
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
-- > main = withFile "someFile.txt" (\h -> hPutDoc h (vcat ["vertical", "text"]))
--
-- @
-- 'hPutDoc' h doc = 'renderIO' h ('layoutPretty' 0.4 80 doc)
-- @
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc h doc = renderIO h (layoutPretty 0.4 80 doc)
