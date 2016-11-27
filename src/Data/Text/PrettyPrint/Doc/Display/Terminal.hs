{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Display 'SimpleDoc' in a terminal.
module Data.Text.PrettyPrint.Doc.Display.Terminal (
    -- * Conversion to ANSI-infused 'Text'
    displayLazy, displayStrict,

    -- * Display directly to the terminal
    displayIO,

    -- ** Convenience
    --
    -- | These functions use default parameters, and handle the most common use
    -- cases.
    putDoc, hPutDoc, putDocW, hPutDocW,
) where



import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Functor
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as LTB
import qualified Data.Text.Lazy.IO          as LT
import           System.Console.ANSI
import           System.IO                  (Handle, stdout)

import Data.Text.PrettyPrint.Doc



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.Lazy.IO as TL
-- >>> import qualified Data.Text.Lazy as TL
-- >>> import Data.Text.PrettyPrint.Doc.Display.Terminal



-- | @('displayLazy' doc)@ takes the output @doc@ from a rendering function
-- and transforms it to lazy text, including ANSI styling directives for things
-- like colorization.
--
-- ANSI color information will be discarded by this function unless you are
-- running on a Unix-like operating system. This is due to a technical
-- limitation in Windows ANSI support.
--
-- With a bit of trickery to make the ANSI codes printable, here is an example
-- that would render coloured in an ANSI terminal:
--
-- >>> let display = TL.putStrLn . TL.replace "\ESC" "\\ESC" . displayLazy . renderPretty 0.4 80
-- >>> let doc = "Wo" <> bold "ah" <+> align (vsep [red "coloured", green "text"])
-- >>> display (plain doc)
-- Woah coloured
--      text
-- >>> display doc
-- Wo\ESC[0;1mah\ESC[0m \ESC[0;91mcoloured\ESC[0m
--      \ESC[0;92mtext\ESC[0m
displayLazy :: SimpleDoc -> LT.Text
displayLazy doc
  = let (resultBuilder, remainingStyles) = runState (execWriterT (build doc)) [emptyStyle]
    in case remainingStyles of
        [] -> error "There is no empty style left at the end of rendering\
                    \ (but there should be). Please report this as a bug."
        [_] -> LTB.toLazyText resultBuilder
        xs -> error ("There are " <> show (length xs) <> " styles left at the\
                     \end of rendering (there should be only 1). Please report\
                     \ this as a bug.")

build :: SimpleDoc -> WriterT LTB.Builder (State [CombinedStyle]) ()
build = \case
    SFail -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
    SEmpty -> pure ()
    SChar c x -> do tell (LTB.singleton c)
                    build x
    SText t x -> do tell (LTB.fromText t)
                    build x
    SLine i x -> do tell (LTB.singleton '\n')
                    tell (LTB.fromText (T.replicate i " "))
                    build x
    SStylePush s x -> do
        (currentStyle, _pastStyles) <- peekStyleStack
        let newStyle = currentStyle `addStyle` s
        lift (modify (currentStyle:))
        tell (styleToBuilder newStyle)
        build x
    SStylePop x -> do
        (lastStyle, lastStyles) <- peekStyleStack
        lift (put lastStyles)
        tell (styleToBuilder lastStyle)
        build x

styleToBuilder :: CombinedStyle -> LTB.Builder
styleToBuilder = LTB.fromString . setSGRCode . stylesToSgrs

peekStyleStack :: Monoid w => WriterT w (State [style]) (style, [style])
peekStyleStack = lift get >>= \case
    [] -> error "Attempted to pop a style restoration off an empty\
                \ stack. Please report this as a bug."
    s:ss -> pure (s,ss)

data CombinedStyle = CombinedStyle
    (Maybe (SIntensity, SColor)) -- Foreground
    (Maybe (SIntensity, SColor)) -- Background
    Bool                         -- Bold
    Bool                         -- Italics
    Bool                         -- Underlining

addStyle :: CombinedStyle -> Style -> CombinedStyle
addStyle (CombinedStyle m'fg m'bg b i u) = \case
    SItalicized               -> CombinedStyle m'fg m'bg b True u
    SBold                     -> CombinedStyle m'fg m'bg True i u
    SUnderlined               -> CombinedStyle m'fg m'bg b i True
    SColor SForeground dv col -> CombinedStyle (Just (dv, col)) m'bg b i u
    SColor SBackground dv col -> CombinedStyle m'fg (Just (dv, col)) b i u

emptyStyle :: CombinedStyle
emptyStyle = CombinedStyle Nothing Nothing False False False

stylesToSgrs :: CombinedStyle -> [SGR]
stylesToSgrs (CombinedStyle m'fg m'bg b i u) = catMaybes
    [ Just Reset
    , fmap (\(intensity, color) -> SetColor Foreground (convertIntensity intensity) (convertColor color)) m'fg
    , fmap (\(intensity, color) -> SetColor Background (convertIntensity intensity) (convertColor color)) m'bg
    , guard b $> SetConsoleIntensity BoldIntensity
    , guard i $> SetItalicized True
    , guard u $> SetUnderlining SingleUnderline
    ]
  where
    convertIntensity :: SIntensity -> ColorIntensity
    convertIntensity = \case
        SVivid -> Vivid
        SDull  -> Dull

    convertColor :: SColor -> Color
    convertColor = \case
        SBlack   -> Black
        SRed     -> Red
        SGreen   -> Green
        SYellow  -> Yellow
        SBlue    -> Blue
        SMagenta -> Magenta
        SCyan    -> Cyan
        SWhite   -> White



-- | @('displayStrict' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
displayStrict :: SimpleDoc -> Text
displayStrict = LT.toStrict . displayLazy



-- | @('displayIO' h sdoc)@ writes @sdoc@ to the file @h@.
displayIO :: Handle -> SimpleDoc -> IO ()
displayIO h sdoc = LT.hPutStrLn h (displayLazy sdoc)

-- | @putDoc doc@ prettyprints document @doc@ to standard output, with a page
-- width of 80 characters and a ribbon width of 32 characters (see
-- 'renderPretty' for documentation of those values.)
--
-- >>> putDoc ("hello" <+> "world")
-- hello world
--
-- @
-- 'putDoc' = 'hPutDoc' 'stdout'
-- @
putDoc :: Doc -> IO ()
putDoc = hPutDoc stdout

-- | 'putDoc', but with a page width parameter.
putDocW :: Int -> Doc -> IO ()
putDocW = hPutDocW stdout

-- | Like 'putDoc', but instead of using 'stdout', print to a user-provided
-- handle, e.g. a file or a socket. Uses a line length of 80, and a ribbon width
-- of 32 characters (see 'renderPretty' for documentation of those values).
--
-- > main = withFile "someFile.txt" (\h -> hPutDoc h (vcat ["vertical", "text"]))
--
-- @
-- 'hPutDoc' h doc = 'displayIO' h ('renderPretty' 0.4 80 doc)
-- @
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc h doc = displayIO h (renderPretty 0.4 80 doc)

-- | 'hPutDocW', but with a page width parameter.
hPutDocW :: Handle -> Int -> Doc -> IO ()
hPutDocW h w doc = displayIO h (renderPretty 0.4 w doc)
