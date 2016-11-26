{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.PrettyPrint.ANSI.Leijen

import System.IO


main :: IO ()
main = do
    -- Going directly to the console is portable across Unix and Windows...
    putDoc $ red (text "Red") <> comma <+> white (text "white") <+> text "and" <+> blue (text "blue") <> char '!' <> line'
    putDoc $ blue (text "Nested" <+> dullyellow (text "colors") <+> text "example") <> line'
    hPutDoc stdout $ onred (text "Red") <> comma <+> onwhite (text "white") <+> text "and" <+> onblue (text "blue") <> char '!' <> line'
    hPutDoc stdout $ onblue (text "Nested" <+> ondullyellow (text "colors") <+> text "example") <> line'

    -- ...but going via a string will only preserve formatting information information on Unix
    putStr $ show $ green (text "I will be green on Unix but uncolored on Windows") <> line'

    -- Let's see some non-color formatting:
    putDoc $ text "We can do" <+> bold (text "boldness") <> text ", if your terminal supports it, and even perhaps" <+> underline (text "underlining") <> line'

    -- Just a little test of the formatting removal:
    putDoc $ text "There is a handy utility called 'plain' to" <+> plain (bold $ text "remove formatting") <+>
              plain (text "if you need to e.g. support" <+> red (text "non-ANSI") <+> text "terminals")
