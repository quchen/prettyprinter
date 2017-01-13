{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render 'SimpleDoc' as 'HTML' encoded as 'Text'.
module Data.Text.Prettyprint.Doc.Render.Html (
    -- * Conversion to HTML-infused 'Text'
    renderLazy,
    renderStrict,

    -- * Styling options
    HtmlColors(..),
    defaultHtmlColors,
    solarizedLightColors,
    solarizedDarkColors,

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
import           System.IO              (Handle, stdout)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.RenderM

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XLambdaCase



-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering function
-- and transforms it to lazy text with HTML tags added. The output contains
-- significant whitespace, which HTML rendering swallows. This can be avoided by
-- putting the result in a @<pre>@ environment.
--
-- >>> let doc = "some" <+> align (vsep ([bold "text", "to", italics ("nicely" <+> bold "lay"), colorDull SRed "out"]))
-- >>> putDoc (plain doc)
-- some text
--      to
--      nicely lay
--      out
-- >>> putDoc doc
-- some <span style="font-weight: bold;">text</span>
--      to
--      <span style="font-style: italic;">nicely <span style="font-weight: bold;">lay</span></span>
--      <span style="color: rgb(205, 0, 0)">out</span>
renderLazy :: HtmlColors -> SimpleDoc -> LT.Text
renderLazy colors doc
  = let (resultBuilder, remainingStyles) = execRenderM [] (build colors doc)
    in if null remainingStyles
        then TLB.toLazyText resultBuilder
        else error ("There are "
                    <> show (length remainingStyles)
                    <> " unpaired styles! Please report this as a bug.")

build :: HtmlColors -> SimpleDoc -> RenderM TLB.Builder Style ()
build colors = go
  where
    go = \case
        SFail -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
        SEmpty -> pure ()
        SChar c x -> do
            writeResult (TLB.singleton c)
            go x
        SText _l t x -> do
            writeResult (TLB.fromText t)
            go x
        SLine i x -> do
            writeResult (TLB.singleton '\n' )
            writeResult (TLB.fromText (T.replicate i " "))
            go x
        SStylePush style x -> do
            pushStyle style
            writeResult (htmlTagFor colors style Opening)
            go x
        SStylePop x -> do
            style <- unsafePopStyle
            writeResult (htmlTagFor colors style Closing)
            go x

-- | Strict 'Text' version of 'renderLazy'.
renderStrict :: HtmlColors -> SimpleDoc -> Text
renderStrict colors = LT.toStrict . renderLazy colors

-- | Opening or closing HTML tag part?
data OpenClose = Opening | Closing

htmlTagFor :: HtmlColors -> Style -> OpenClose -> TLB.Builder
htmlTagFor colors = \case
    SItalicized             -> htmlTag "span" (Just "style=\"font-style: italic;\"")
    SBold                   -> htmlTag "span" (Just "style=\"font-weight: bold;\"")
    SUnderlined             -> htmlTag "span" (Just "style=\"text-decoration: underline;\"")
    SColor SForeground dv c -> htmlTag "span" (Just ("style=\"color: " <> cssColor colors dv c <> "\""))
    SColor SBackground dv c -> htmlTag "span" (Just ("style=\"background-color: " <> cssColor colors dv c <> "\""))

cssColor :: HtmlColors -> SIntensity -> SColor -> TLB.Builder
cssColor colors SDull = \case
    SBlack   -> _dullblack colors
    SRed     -> _dullred colors
    SGreen   -> _dullgreen colors
    SYellow  -> _dullyellow colors
    SBlue    -> _dullblue colors
    SMagenta -> _dullmagenta colors
    SCyan    -> _dullcyan colors
    SWhite   -> _dullwhite colors
cssColor colors SVivid = \case
    SBlack   -> _black colors
    SRed     -> _red colors
    SGreen   -> _green colors
    SYellow  -> _yellow colors
    SBlue    -> _blue colors
    SMagenta -> _magenta colors
    SCyan    -> _cyan colors
    SWhite   -> _white colors

-- | Enclose a document in an HTML tag
htmlTag
    :: TLB.Builder
        -- ^ Tag name, e.g. @span@; attributes,
    -> Maybe TLB.Builder
        -- ^ HTML attributes, e.g. @style="text-decoration: underline"@
    -> OpenClose
    -> TLB.Builder
htmlTag tag attrs openClose = case openClose of
    Opening -> "<" <> tag <> maybe mempty (" " <>) attrs <> ">"
    Closing -> "</" <> tag <> ">"



-- | @('renderIO' h sdoc)@ writes @sdoc@ to the file @h@.
--
-- >>> renderIO System.IO.stdout defaultHtmlColors (layoutPretty (RibbonFraction 1) (PageWidth 80) "hello\nworld")
-- hello
-- world
renderIO :: Handle -> HtmlColors -> SimpleDoc -> IO ()
renderIO h colors sdoc = LT.hPutStrLn h (renderLazy colors sdoc)

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
hPutDoc h doc = renderIO h defaultHtmlColors (layoutPretty (RibbonFraction 0.4) (PageWidth 80) doc)



-- | CSS color values for each of the styles.
--
-- Entries should be valid CSS colors, e.g. @"rgb(205, 0, 0)"@ or @"#d01b24"@.
data HtmlColors = HtmlColors
    { _black       :: TLB.Builder
    , _red         :: TLB.Builder
    , _green       :: TLB.Builder
    , _yellow      :: TLB.Builder
    , _blue        :: TLB.Builder
    , _magenta     :: TLB.Builder
    , _cyan        :: TLB.Builder
    , _white       :: TLB.Builder

    , _dullblack   :: TLB.Builder
    , _dullred     :: TLB.Builder
    , _dullgreen   :: TLB.Builder
    , _dullyellow  :: TLB.Builder
    , _dullblue    :: TLB.Builder
    , _dullmagenta :: TLB.Builder
    , _dullcyan    :: TLB.Builder
    , _dullwhite   :: TLB.Builder
    }

defaultHtmlColors :: HtmlColors
defaultHtmlColors = HtmlColors
    { _black       = "rgb(127, 127, 127)"
    , _red         = "rgb(255, 0, 0)"
    , _green       = "rgb(0, 255, 0)"
    , _yellow      = "rgb(255, 255, 0)"
    , _blue        = "rgb(92, 92, 255)"
    , _magenta     = "rgb(255, 0, 255)"
    , _cyan        = "rgb(0, 255, 255)"
    , _white       = "rgb(255, 255, 255)"

    , _dullblack   = "rgb(0, 0, 0)"
    , _dullred     = "rgb(205, 0, 0)"
    , _dullgreen   = "rgb(0, 205, 0)"
    , _dullyellow  = "rgb(205, 205, 0)"
    , _dullblue    = "rgb(0, 0, 238)"
    , _dullmagenta = "rgb(205, 0, 205)"
    , _dullcyan    = "rgb(0, 205, 205)"
    , _dullwhite   = "rgb(229, 229, 229)"
    }

-- | <http://ethanschoonover.com/solarized Solarized light> color scheme,
-- augmented with dull versions of the colors.
solarizedLightColors :: HtmlColors
solarizedLightColors = HtmlColors
    { _black       = "#002731"
    , _red         = "#d01b24"
    , _green       = "#728905"
    , _yellow      = "#a57705"
    , _blue        = "#2075c7"
    , _magenta     = "#c61b6e"
    , _cyan        = "#259185"
    , _white       = "#e9e2cb"

    , _dullblack   = "#005064"
    , _dullred     = "#e53941"
    , _dullgreen   = "#9bba07"
    , _dullyellow  = "#d79b07"
    , _dullblue    = "#3b8edf"
    , _dullmagenta = "#e33187"
    , _dullcyan    = "#2fbaaa"
    , _dullwhite   = "#f8f6ef"
    }

-- | <http://ethanschoonover.com/solarized Solarized dark> color scheme,
-- augmented with dull versions of the colors.
solarizedDarkColors :: HtmlColors
solarizedDarkColors = HtmlColors
    { _black       = "#005064"
    , _red         = "#d01b24"
    , _green       = "#728905"
    , _yellow      = "#a57705"
    , _blue        = "#2075c7"
    , _magenta     = "#c61b6e"
    , _cyan        = "#259185"
    , _white       = "#e9e2cb"

    , _dullblack   = "#002731"
    , _dullred     = "#a3151c"
    , _dullgreen   = "#495803"
    , _dullyellow  = "#745304"
    , _dullblue    = "#195b9b"
    , _dullmagenta = "#991555"
    , _dullcyan    = "#1b6860"
    , _dullwhite   = "#dacea7"
    }
