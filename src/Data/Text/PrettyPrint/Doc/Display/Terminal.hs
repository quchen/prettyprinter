{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Display 'SimpleDoc' in a terminal.
module Data.Text.PrettyPrint.Doc.Display.Terminal (
    displayLazyText, displayStrictText, displayIO,

    putDoc, hPutDoc, putDocW, hPutDocW,
) where



import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as LTB
import           System.Console.ANSI    (hSetSGR)
import           System.Console.ANSI    (setSGRCode)
import           System.IO              (Handle, hPutChar, stdout)

import Data.Text.PrettyPrint.Doc



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text.PrettyPrint.Doc.Display.Terminal



-- | @('displayLazyText' sdoc)@ takes the output @sdoc@ from a rendering
-- function and transforms it to lazy text, including ANSI styling directives
-- for things like colorization.
--
-- ANSI color information will be discarded by this function unless you are
-- running on a Unix-like operating system. This is due to a technical
-- limitation in Windows ANSI support.
displayLazyText :: SimpleDoc -> LT.Text
displayLazyText = LTB.toLazyText . build
  where
    build = \case
        SFail     -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
        SEmpty    -> mempty
        SChar c x -> LTB.singleton c <> build x
        SText t x -> LTB.fromText t <> build x
        SLine i x -> LTB.singleton '\n' <> LTB.fromText (T.replicate i " ") <> build x
        SSGR s x  -> LTB.fromString (setSGRCode s) <> build x

-- renderWithColourWIP :: SimpleDoc -> STVar [SGR] -> ST s _
renderWithColourWIP = \case
    SFail     -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
    SEmpty    -> pure mempty
    SChar c x -> do
        let x = LTB.singleton c
        rest <- build x
        pure (x <> rest)
    SText t x -> do
        let x = LTB.fromText t
        rest <- build x
        pure (x <> rest)
    SLine i x -> do
        let x = LTB.singleton '\n' <> LTB.fromText (T.replicate i " ")
        rest <- build x
        pure (x <> rest)
    Style styleAdditions x -> do
        currentStyle <- peek
        let newStyle = currentStyle `addStyles` styleAdditions
        let newStyleAnsi = LTB.fromString (setSGRCode (fromStyle newStyle))
        let currentStyleAnsi = LTB.fromString (setSGRCode (fromStyle currentStyle))
        rest <- build x
        pure (newStyleAnsi <> rest <> currentStyleAnsi)

-- | @('displayLazyText' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
displayStrictText :: SimpleDoc -> Text
displayStrictText = LT.toStrict . displayLazyText


-- | @(displayIO handle simpleDoc)@ writes @simpleDoc@ to the file handle
-- @handle@. This function is used for example by 'hPutDoc':
--
-- > hPutDoc handle doc = displayIO handle (renderPretty 0.4 80 doc)
--
-- Any ANSI colorisation in @simpleDoc@ will be output.
displayIO :: Handle -> SimpleDoc -> IO ()
displayIO h = display
  where
    display = \case
        SFail     -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
        SEmpty    -> pure ()
        SChar c x -> hPutChar h c *> display x
        SText t x -> T.hPutStr h t *> display x
        SLine i x -> hPutChar h '\n' *> T.hPutStr h (T.replicate i " ") *> display x
        SSGR s x  -> hSetSGR h s *> display x

-- | @putDoc doc@ prettyprints document @doc@ to standard output, with a page
-- width of 80 characters and a ribbon width of 32 characters (see
-- 'renderPretty' for documentation of those values.)
--
-- >>> putDoc ("hello" <+> "world")
-- hello world
--
-- Any ANSI colorisation in @doc@ will be output.
putDoc :: Doc -> IO ()
putDoc = hPutDoc stdout

-- | 'putDoc', but with a page width parameter.
putDocW :: Int -> Doc -> IO ()
putDocW = hPutDocW stdout

-- | 'putDoc', but instead of using 'stdout', print to a user-provided handle,
-- e.g. a file or a socket. Uses a line length of 80, and a ribbon width of 32
-- characters (see 'renderPretty' for documentation of those values).
--
-- > main = withFile "someFile.txt" (\h -> hPutDoc h (vcat ["vertical", "text"]))
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc h doc = displayIO h (renderPretty 0.4 80 doc)

-- | 'hPutDocW', but with a page width parameter.
hPutDocW :: Handle -> Int -> Doc -> IO ()
hPutDocW h w doc = displayIO h (renderPretty 0.4 w doc)
