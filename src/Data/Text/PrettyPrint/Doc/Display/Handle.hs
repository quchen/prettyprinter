{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.PrettyPrint.Doc.Display.Handle (
    displayIO,

    putDoc, hPutDoc, putDocW, hPutDocW,
) where



import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           System.Console.ANSI (hSetSGR)
import           System.IO           (Handle, hPutChar, stdout)

import Data.Text.PrettyPrint.Doc



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text.PrettyPrint.Doc.Display.Handle



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
