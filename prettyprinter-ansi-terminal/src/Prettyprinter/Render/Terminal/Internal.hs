{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK not-home #-}

#include "version-compatibility-macros.h"

-- | __Warning:__ Internal module. May change arbitrarily between versions.
module Prettyprinter.Render.Terminal.Internal (
    -- * Styling
    AnsiStyle(..),
    AnsiColor(..),
    Color(..),

    -- ** Font color
    color, colorDull, colorPaletted, colorRGB,

    -- ** Background color
    bgColor, bgColorDull, bgColorPaletted, bgColorRGB,

    -- ** Font style
    bold, italicized, underlined, inverted,

    -- ** Internal markers
    Intensity(..),
    Bold(..),
    Underlined(..),
    Italicized(..),
    Inverted(..),

    -- * Conversion to ANSI-infused 'Text'
    renderLazy, renderStrict,

    -- * Render directly to 'stdout'
    renderIO,

    -- ** Convenience functions
    putDoc, hPutDoc,
) where



import           Control.Applicative
import qualified Data.Colour.RGBSpace   as RGB
import           Data.IORef
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import           Data.Word              (Word8)
import qualified System.Console.ANSI    as ANSI
import           System.IO              (Handle, hPutChar, stdout)

import Prettyprinter
import Prettyprinter.Render.Util.Panic

#if !(SEMIGROUP_MONOID_SUPERCLASS)
import Data.Semigroup
#endif

#if !(MIN_VERSION_base(4,6,0))
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'
#endif

-- $setup
--
-- (Definitions for the doctests)
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.Lazy.IO as TL
-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Prettyprinter.Render.Terminal



-- | The 8 ANSI terminal colors.
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Eq, Ord, Show)

-- | Dull or vivid coloring, as supported by ANSI terminals.
data Intensity = Vivid | Dull
    deriving (Eq, Ord, Show)

-- | Foreground (text) or background (paper) color
data Layer = Foreground | Background
    deriving (Eq, Ord, Show)

-- FaintIntensity is not widely supported: sometimes treated as concealing text. Not supported natively on Windows 10
data Bold       = Bold       | Faint            deriving (Eq, Ord, Show)
-- DoubleUnderline is not widely supported. Not supported natively on Windows 10
data Underlined = Underlined | DoubleUnderlined deriving (Eq, Ord, Show)
data Italicized = Italicized                    deriving (Eq, Ord, Show)
-- Swap the foreground and background colors. Supported natively on Windows 10
data Inverted   = Inverted                      deriving (Eq, Ord, Show)

-- | Style the foreground with a vivid color.
color :: Color -> AnsiStyle
color c = mempty { ansiForeground = Just (Color16 Vivid c) }

-- | Style the background with a vivid color.
bgColor :: Color -> AnsiStyle
bgColor c =  mempty { ansiBackground = Just (Color16 Vivid c) }

-- | Style the foreground with a dull color.
colorDull :: Color -> AnsiStyle
colorDull c =  mempty { ansiForeground = Just (Color16 Dull c) }

-- | Style the background with a dull color.
bgColorDull :: Color -> AnsiStyle
bgColorDull c =  mempty { ansiBackground = Just (Color16 Dull c) }

-- | Style the foreground with one of a palette of 256 colors. See 'ColorPalette' for more info
colorPaletted :: Word8 -> AnsiStyle
colorPaletted w = mempty { ansiForeground = Just (ColorPalette w) }

-- | Style the background with one of a palette of 256 colors. See 'ColorPalette' for more info
bgColorPaletted :: Word8 -> AnsiStyle
bgColorPaletted w = mempty { ansiBackground = Just (ColorPalette w) }

-- | Style the foreground with any RGB color
colorRGB :: RGB.Colour Float -> AnsiStyle
colorRGB c = mempty { ansiForeground = Just (ColorRGB c) }

-- | Style the background with any RGB color
bgColorRGB :: RGB.Colour Float -> AnsiStyle
bgColorRGB c = mempty { ansiBackground = Just (ColorRGB c) }

-- | Render in __bold__.
bold :: AnsiStyle
bold = mempty { ansiBold = Just Bold }

-- | Render in /italics/.
italicized :: AnsiStyle
italicized = mempty { ansiItalics = Just Italicized }

-- | Render underlined.
underlined :: AnsiStyle
underlined = mempty { ansiUnderlining = Just Underlined }

-- | Swap the foreground and background colors
inverted :: AnsiStyle
inverted = mempty { ansiInverted = Just Inverted }

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
-- >>> let render = TL.putStrLn . TL.replace "\ESC" "\\e" . renderLazy . layoutPretty defaultLayoutOptions
-- >>> let doc = annotate (color Red) ("red" <+> align (vsep [annotate (color Blue <> underlined) ("blue+u" <+> annotate bold "bold" <+> "blue+u"), "red"]))
-- >>> render (unAnnotate doc)
-- red blue+u bold blue+u
--     red
-- >>> render doc
-- \e[0;91mred \e[0;94;4mblue+u \e[0;94;1;4mbold\e[0;94;4m blue+u\e[0;91m
--     red\e[0m
--
-- Run the above via @echo -e '...'@ in your terminal to see the coloring.
renderLazy :: SimpleDocStream AnsiStyle -> TL.Text
renderLazy =
    let push x = (x :)

        unsafePeek []    = panicPeekedEmpty
        unsafePeek (x:_) = x

        unsafePop []     = panicPoppedEmpty
        unsafePop (x:xs) = (x, xs)

        go :: [AnsiStyle] -> SimpleDocStream AnsiStyle -> TLB.Builder
        go s sds = case sds of
            SFail -> panicUncaughtFail
            SEmpty -> mempty
            SChar c rest -> TLB.singleton c <> go s rest
            SText _ t rest -> TLB.fromText t <> go s rest
            SLine i rest -> TLB.singleton '\n' <> TLB.fromText (T.replicate i " ") <> go s rest
            SAnnPush style rest ->
                let currentStyle = unsafePeek s
                    newStyle = style <> currentStyle
                in  TLB.fromText (styleToRawText newStyle) <> go (push newStyle s) rest
            SAnnPop rest ->
                let (_currentStyle, s') = unsafePop s
                    newStyle = unsafePeek s'
                in  TLB.fromText (styleToRawText newStyle) <> go s' rest

    in  TLB.toLazyText . go [mempty]


-- | @('renderIO' h sdoc)@ writes @sdoc@ to the handle @h@.
--
-- >>> let render = renderIO System.IO.stdout . layoutPretty defaultLayoutOptions
-- >>> let doc = annotate (color Red) ("red" <+> align (vsep [annotate (color Blue <> underlined) ("blue+u" <+> annotate bold "bold" <+> "blue+u"), "red"]))
--
-- We render the 'unAnnotate'd version here, since the ANSI codes don’t display
-- well in Haddock,
--
-- >>> render (unAnnotate doc)
-- red blue+u bold blue+u
--     red
--
-- This function behaves just like
--
-- @
-- 'renderIO' h sdoc = 'TL.hPutStr' h ('renderLazy' sdoc)
-- @
--
-- but will not generate any intermediate text, rendering directly to the
-- handle.
renderIO :: Handle -> SimpleDocStream AnsiStyle -> IO ()
renderIO h sdoc = do
    styleStackRef <- newIORef [mempty]

    let push x = modifyIORef' styleStackRef (x :)
        unsafePeek = readIORef styleStackRef >>= \tok -> case tok of
            [] -> panicPeekedEmpty
            x:_ -> pure x
        unsafePop = readIORef styleStackRef >>= \tok -> case tok of
            [] -> panicPoppedEmpty
            x:xs -> writeIORef styleStackRef xs >> pure x

    let go = \sds -> case sds of
            SFail -> panicUncaughtFail
            SEmpty -> pure ()
            SChar c rest -> do
                hPutChar h c
                go rest
            SText _ t rest -> do
                T.hPutStr h t
                go rest
            SLine i rest -> do
                hPutChar h '\n'
                T.hPutStr h (T.replicate i (T.singleton ' '))
                go rest
            SAnnPush style rest -> do
                currentStyle <- unsafePeek
                let newStyle = style <> currentStyle
                push newStyle
                T.hPutStr h (styleToRawText newStyle)
                go rest
            SAnnPop rest -> do
                _currentStyle <- unsafePop
                newStyle <- unsafePeek
                T.hPutStr h (styleToRawText newStyle)
                go rest
    go sdoc
    readIORef styleStackRef >>= \stack -> case stack of
        []  -> panicStyleStackFullyConsumed
        [_] -> pure ()
        xs  -> panicStyleStackNotFullyConsumed (length xs)

panicStyleStackFullyConsumed :: void
panicStyleStackFullyConsumed
  = error ("There is no empty style left at the end of rendering" ++
           " (but there should be). Please report this as a bug.")

panicStyleStackNotFullyConsumed :: Int -> void
panicStyleStackNotFullyConsumed len
  = error ("There are " <> show len <> " styles left at the" ++
           "end of rendering (there should be only 1). Please report" ++
           " this as a bug.")

-- | Various kinds of colors that can be used in a terminal
data AnsiColor
  -- | A color from the standard palette of 16 colors (8 colors by 2 color intensities). Many terminals allow the palette colors to be customised
  = Color16 Intensity Color
  -- | A color from a palette of 256 colors using a numerical index (0-based). 
  --   Supported natively on Windows 10 from the Creators Update (April 2017) but not on legacy Windows native terminals.
  --   See xtermSystem, xterm6LevelRGB and xterm24LevelGray from 'System.Console.ANSI.Types' to construct indices based on xterm's standard protocol for a 256-color palette.
  | ColorPalette Word8
  -- | Full 24-bit true colors
  | ColorRGB (RGB.Colour Float)
  deriving (Show, Eq)

-- $
-- >>> let render = renderIO System.IO.stdout . layoutPretty defaultLayoutOptions
-- >>> let doc = annotate (color Red) ("red" <+> align (vsep [annotate (color Blue <> underlined) ("blue+u" <+> annotate bold "bold" <+> "blue+u"), "red"]))
-- >>> render (unAnnotate doc)
-- red blue+u bold blue+u
--     red
--
-- This test won’t work since I don’t know how to type \ESC for doctest :-/
-- -- >>> render doc
-- -- \ESC[0;91mred \ESC[0;94;4mblue+u \ESC[0;94;1;4mbold\ESC[0;94;4m blue+u\ESC[0;91m
-- --     red\ESC[0m

-- | Render the annotated document in a certain style. Styles not set in the
-- annotation will use the style of the surrounding document, or the terminal’s
-- default if none has been set yet.
--
-- @
-- style = 'color' 'Green' '<>' 'bold'
-- styledDoc = 'annotate' style "hello world"
-- @
data AnsiStyle = SetAnsiStyle
    { ansiForeground  :: Maybe AnsiColor          -- ^ Set the foreground color, or keep the old one.
    , ansiBackground  :: Maybe AnsiColor          -- ^ Set the background color, or keep the old one.
    , ansiBold        :: Maybe Bold               -- ^ Switch on boldness, or don’t do anything.
    , ansiItalics     :: Maybe Italicized         -- ^ Switch on italics, or don’t do anything.
    , ansiUnderlining :: Maybe Underlined         -- ^ Switch on underlining, or don’t do anything.
    , ansiInverted    :: Maybe Inverted           -- ^ Swap the foreground and background color, or don't do anything
    } deriving (Eq, Show)

-- | Keep the first decision for each of foreground color, background color,
-- boldness, italication, and underlining. If a certain style is not set, the
-- terminal’s default will be used.
--
-- Example:
--
-- @
-- 'color' 'Red' '<>' 'color' 'Green'
-- @
--
-- is red because the first color wins, and not bold because (or if) that’s the
-- terminal’s default.
instance Semigroup AnsiStyle where
    cs1 <> cs2 = SetAnsiStyle
        { ansiForeground  = ansiForeground  cs1 <|> ansiForeground  cs2
        , ansiBackground  = ansiBackground  cs1 <|> ansiBackground  cs2
        , ansiBold        = ansiBold        cs1 <|> ansiBold        cs2
        , ansiItalics     = ansiItalics     cs1 <|> ansiItalics     cs2
        , ansiUnderlining = ansiUnderlining cs1 <|> ansiUnderlining cs2
        , ansiInverted    = ansiInverted    cs1 <|> ansiInverted    cs2 }

-- | 'mempty' does nothing, which is equivalent to inheriting the style of the
-- surrounding doc, or the terminal’s default if no style has been set yet.
instance Monoid AnsiStyle where
    mempty = SetAnsiStyle Nothing Nothing Nothing Nothing Nothing Nothing
    mappend = (<>)

styleToRawText :: AnsiStyle -> Text
styleToRawText = T.pack . ANSI.setSGRCode . stylesToSgrs
  where
    stylesToSgrs :: AnsiStyle -> [ANSI.SGR]
    stylesToSgrs (SetAnsiStyle fg bg b i u inv) = catMaybes
        [ Just ANSI.Reset
        , fmap (\c -> case c of
                Color16 intensity c' -> ANSI.SetColor        ANSI.Foreground (convertIntensity intensity) (convertColor c')
                ColorPalette c'      -> ANSI.SetPaletteColor ANSI.Foreground c'
                ColorRGB c'          -> ANSI.SetRGBColor     ANSI.Foreground c'
            ) fg
        , fmap (\c -> case c of
                Color16 intensity c' -> ANSI.SetColor        ANSI.Background (convertIntensity intensity) (convertColor c')
                ColorPalette c'      -> ANSI.SetPaletteColor ANSI.Background c'
                ColorRGB c'          -> ANSI.SetRGBColor     ANSI.Background c'
            ) bg
        , fmap (\b'                  -> ANSI.SetConsoleIntensity (convertBoldness b')) b
        , fmap (\_                   -> ANSI.SetItalicized True) i
        , fmap (\u'                  -> ANSI.SetUnderlining (convertUnderline u')) u
        , fmap (\_                   -> ANSI.SetSwapForegroundBackground True) inv
        ]

    convertIntensity :: Intensity -> ANSI.ColorIntensity
    convertIntensity = \i -> case i of
        Vivid -> ANSI.Vivid
        Dull  -> ANSI.Dull

    convertColor :: Color -> ANSI.Color
    convertColor = \c -> case c of
        Black   -> ANSI.Black
        Red     -> ANSI.Red
        Green   -> ANSI.Green
        Yellow  -> ANSI.Yellow
        Blue    -> ANSI.Blue
        Magenta -> ANSI.Magenta
        Cyan    -> ANSI.Cyan
        White   -> ANSI.White

    convertBoldness :: Bold -> ANSI.ConsoleIntensity
    convertBoldness Bold  = ANSI.BoldIntensity 
    convertBoldness Faint = ANSI.FaintIntensity 

    convertUnderline :: Underlined -> ANSI.Underlining
    convertUnderline Underlined       = ANSI.SingleUnderline 
    convertUnderline DoubleUnderlined = ANSI.DoubleUnderline 

-- | @('renderStrict' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
renderStrict :: SimpleDocStream AnsiStyle -> Text
renderStrict = TL.toStrict . renderLazy

-- | @('putDoc' doc)@ prettyprints document @doc@ to standard output using
-- 'defaultLayoutOptions'.
--
-- >>> putDoc ("hello" <+> "world")
-- hello world
--
-- @
-- 'putDoc' = 'hPutDoc' 'stdout'
-- @
putDoc :: Doc AnsiStyle -> IO ()
putDoc = hPutDoc stdout

-- | Like 'putDoc', but instead of using 'stdout', print to a user-provided
-- handle, e.g. a file or a socket using 'defaultLayoutOptions'.
--
-- > main = withFile "someFile.txt" (\h -> hPutDoc h (vcat ["vertical", "text"]))
--
-- @
-- 'hPutDoc' h doc = 'renderIO' h ('layoutPretty' 'defaultLayoutOptions' doc)
-- @
hPutDoc :: Handle -> Doc AnsiStyle -> IO ()
hPutDoc h doc = renderIO h (layoutPretty defaultLayoutOptions doc)
