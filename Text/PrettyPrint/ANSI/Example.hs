module Main (main) where

import Text.PrettyPrint.ANSI.Leijen

import System.IO


main :: IO ()
main = do
    -- Going directly to the console is portable across Unix and Windows...
    putDoc $ red (text "Red") <> text "," <+> white (text "white") <+> text "and" <+> blue (text "blue") <> char '!' <> linebreak
    putDoc $ blue (text "Nested" <+> dullyellow (text "colors") <+> text "example") <> linebreak
    hPutDoc stdout $ onred (text "Red") <> text "," <+> onwhite (text "white") <+> text "and" <+> onblue (text "blue") <> char '!' <> linebreak
    hPutDoc stdout $ onblue (text "Nested" <+> ondullyellow (text "colors") <+> text "example") <> linebreak
    
    -- ...but going via a string will only preserve color information on Unix
    putStr $ show $ green (text "I will be green on Unix but uncolored on Windows") <> linebreak