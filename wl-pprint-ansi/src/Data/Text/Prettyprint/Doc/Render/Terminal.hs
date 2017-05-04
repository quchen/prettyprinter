{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render 'SimpleDoc' in a terminal.
module Data.Text.Prettyprint.Doc.Render.Terminal (
    -- * Styling
    AnsiTerminal, Color(..),

    -- ** Font color
    color, colorDull,
    --
    -- ** Background color
    bgColor, bgColorDull,

    -- ** Font style
    bold, italics, underline,

    -- * Conversion to ANSI-infused 'Text'
    renderLazy, renderStrict,

    -- * Render directly to 'stdout'
    renderIO,

    -- ** Convenience functions
    putDoc, hPutDoc,
) where



import           Control.Monad
import           Data.Functor
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO      as LT
import qualified System.Console.ANSI    as ANSI
import           System.IO              (Handle, stdout)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.RenderM

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.Lazy.IO as LT
-- >>> import qualified Data.Text.Lazy as LT
-- >>> import Data.Text.Prettyprint.Doc.Render.Terminal



-- | A general ANSI style. Use e.g. 'color' or 'bold' to apply a style to a
-- 'Doc'ument.
data AnsiTerminal =
      Italicized
    | Bold
    | Underlined
    | Color Layer Intensity Color
    deriving (Eq, Ord, Show)

-- | 8 different colors, so that all can be displayed in an ANSI terminal.
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Eq, Ord, Show)

-- | Dull or vivid coloring, as supported by ANSI terminals.
data Intensity = Vivid | Dull
    deriving (Eq, Ord, Show)

-- | Foreground (text) or background (paper) color
data Layer = Foreground | Background
    deriving (Eq, Ord, Show)

-- | Style the foreground with a vivid color.
color :: Color -> Doc AnsiTerminal -> Doc AnsiTerminal
color c = annotate (Color Foreground Vivid c)

-- | Style the background with a vivid color.
bgColor :: Color -> Doc AnsiTerminal -> Doc AnsiTerminal
bgColor c = annotate (Color Background Vivid c)

-- | Style the foreground with a dull color.
colorDull :: Color -> Doc AnsiTerminal -> Doc AnsiTerminal
colorDull c = annotate (Color Foreground Dull c)

-- | Style the background with a dull color.
bgColorDull :: Color -> Doc AnsiTerminal -> Doc AnsiTerminal
bgColorDull c = annotate (Color Background Dull c)

-- | Render the enclosed document in __bold__.
bold :: Doc AnsiTerminal -> Doc AnsiTerminal
bold = annotate Bold

-- | Render the enclosed document in /italics/.
italics :: Doc AnsiTerminal -> Doc AnsiTerminal
italics = annotate Italicized

-- | Render the enclosed document underlined.
underline :: Doc AnsiTerminal -> Doc AnsiTerminal
underline = annotate Underlined


-- | @('renderLazy' doc)@ takes the output @doc@ from a rendering function
-- and transforms it to lazy text, including ANSI styling directives for things
-- like colorization.
--
-- ANSI color information will be discarded by this function unless you are
-- running on a Unix-like operating system. This is due to a technical
-- limitation in Windows ANSI support.
--
-- With a bit of trickery to make the ANSI codes printable, here is an example
-- that would render colored in an ANSI terminal:
--
-- >>> let render = LT.putStrLn . LT.replace "\ESC" "\\e" . renderLazy . layoutPretty defaultLayoutOptions
-- >>> let doc = color Red ("red" <+> align (vsep [color Blue ("blue" <+> bold "bold" <+> "blue"), "red"]))
-- >>> render (unAnnotate doc)
-- red blue bold blue
--     red
-- >>> render doc
-- \e[0;91mred \e[0;94mblue \e[0;94;1mbold\e[0;94m blue\e[0;91m
--     red\e[0m
--
-- Run the above via @echo -e '...'@ in your terminal to see the coloring.
renderLazy :: SimpleDoc AnsiTerminal -> LT.Text
renderLazy doc
  = let (resultBuilder, remainingStyles) = execRenderM [emptyStyle] (build doc)
    in case remainingStyles of
        [] -> error ("There is no empty style left at the end of rendering" ++
                     " (but there should be). Please report this as a bug.")
        [_] -> TLB.toLazyText resultBuilder
        xs -> error ("There are " <> show (length xs) <> " styles left at the" ++
                     "end of rendering (there should be only 1). Please report" ++
                     " this as a bug.")

build :: SimpleDoc AnsiTerminal -> RenderM TLB.Builder CombinedStyle ()
build = \case
    SFail -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
    SEmpty -> pure ()
    SChar c x -> do
        writeOutput (TLB.singleton c)
        build x
    SText _l t x -> do
        writeOutput (TLB.fromText t)
        build x
    SLine i x -> do
        writeOutput (TLB.singleton '\n')
        writeOutput (TLB.fromText (T.replicate i " "))
        build x
    SAnnPush s x -> do
        currentStyle <- unsafePeekStyle
        let newStyle = currentStyle `addStyle` s
        writeOutput (styleToBuilder newStyle)
        pushStyle newStyle
        build x
    SAnnPop x -> do
        _currentStyle <- unsafePopStyle
        newStyle <- unsafePeekStyle
        writeOutput (styleToBuilder newStyle)
        build x

styleToBuilder :: CombinedStyle -> TLB.Builder
styleToBuilder = TLB.fromString . ANSI.setSGRCode . stylesToSgrs

data CombinedStyle = CombinedStyle
    (Maybe (Intensity, Color)) -- Foreground
    (Maybe (Intensity, Color)) -- Background
    Bool                       -- Bold
    Bool                       -- Italics
    Bool                       -- Underlining

addStyle :: CombinedStyle -> AnsiTerminal -> CombinedStyle
addStyle (CombinedStyle m'fg m'bg b i u) = \case
    Italicized              -> CombinedStyle m'fg m'bg b True u
    Bold                    -> CombinedStyle m'fg m'bg True i u
    Underlined              -> CombinedStyle m'fg m'bg b i True
    Color Foreground dv col -> CombinedStyle (Just (dv, col)) m'bg b i u
    Color Background dv col -> CombinedStyle m'fg (Just (dv, col)) b i u

emptyStyle :: CombinedStyle
emptyStyle = CombinedStyle Nothing Nothing False False False

stylesToSgrs :: CombinedStyle -> [ANSI.SGR]
stylesToSgrs (CombinedStyle m'fg m'bg b i u) = catMaybes
    [ Just ANSI.Reset
    , fmap (\(intensity, c) -> ANSI.SetColor ANSI.Foreground (convertIntensity intensity) (convertColor c)) m'fg
    , fmap (\(intensity, c) -> ANSI.SetColor ANSI.Background (convertIntensity intensity) (convertColor c)) m'bg
    , guard b $> ANSI.SetConsoleIntensity ANSI.BoldIntensity
    , guard i $> ANSI.SetItalicized True
    , guard u $> ANSI.SetUnderlining ANSI.SingleUnderline
    ]
  where
    convertIntensity :: Intensity -> ANSI.ColorIntensity
    convertIntensity = \case
        Vivid -> ANSI.Vivid
        Dull  -> ANSI.Dull

    convertColor :: Color -> ANSI.Color
    convertColor = \case
        Black   -> ANSI.Black
        Red     -> ANSI.Red
        Green   -> ANSI.Green
        Yellow  -> ANSI.Yellow
        Blue    -> ANSI.Blue
        Magenta -> ANSI.Magenta
        Cyan    -> ANSI.Cyan
        White   -> ANSI.White



-- | @('renderStrict' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
renderStrict :: SimpleDoc AnsiTerminal -> Text
renderStrict = LT.toStrict . renderLazy



-- | @('renderIO' h sdoc)@ writes @sdoc@ to the file @h@.
--
-- >>> renderIO System.IO.stdout (layoutPretty defaultLayoutOptions "hello\nworld")
-- hello
-- world
renderIO :: Handle -> SimpleDoc AnsiTerminal -> IO ()
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
putDoc :: Doc AnsiTerminal -> IO ()
putDoc = hPutDoc stdout

-- | Like 'putDoc', but instead of using 'stdout', print to a user-provided
-- handle, e.g. a file or a socket. Uses a line length of 80, and a ribbon width
-- of 32 characters.
--
-- > main = withFile "someFile.txt" (\h -> hPutDoc h (vcat ["vertical", "text"]))
--
-- @
-- 'hPutDoc' h doc = 'renderIO' h ('layoutPretty' 'defaultLayoutOptions' doc)
-- @
hPutDoc :: Handle -> Doc AnsiTerminal -> IO ()
hPutDoc h doc = renderIO h (layoutPretty defaultLayoutOptions doc)
