module Main (main) where

import Text.PrettyPrint.ANSI.Leijen

import System.IO


main :: IO ()
main = do
    -- Going directly to the console is portable across Unix and Windows...
    putDoc $ red (text "Red") <> text "," <+> white (text "white") <+> text "and" <+> blue (text "blue") <> char '!' <> linebreak
    putDoc $ blue (text "Nested" <+> yellow (text "colors") <+> text "example") <> linebreak
    hPutDoc stdout $ redb (text "Red") <> text "," <+> whiteb (text "white") <+> text "and" <+> blueb (text "blue") <> char '!' <> linebreak
    hPutDoc stdout $ blueb (text "Nested" <+> yellowb (text "colors") <+> text "example") <> linebreak
    
    -- ...but going via a string will only preserve color information on Unix
    putStr $ show $ green (text "I will be green on Unix but uncolored on Windows") <> linebreak