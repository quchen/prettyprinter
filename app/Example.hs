{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.PrettyPrint.Doc
import Data.Text.PrettyPrint.Doc.Display.Terminal

import System.IO


main :: IO ()
main = do
    -- Going directly to the console is portable across Unix and Windows...
    putDoc (red "Red" <> comma <+> white "white" <+> "and" <+> blue "blue" <> char '!' <> line')
    putDoc (blue ("Nested" <+> dullyellow "colors" <+> "example") <> line')
    hPutDoc stdout (onred "Red" <> comma <+> onwhite "white" <+> "and" <+> onblue "blue" <> char '!' <> line')
    hPutDoc stdout (onblue ("Nested" <+> ondullyellow "colors" <+> "example") <> line')

    -- ...but going via a string will only preserve formatting information information on Unix
    (putStr . show) (green "I will be green on Unix but uncolored on Windows" <> line')

    -- Let's see some non-color formatting:
    putDoc ("We can do" <+> bold "boldness" <> ", if your terminal supports it, and even perhaps" <+> underline "underlining" <> line')

    -- Just a little test of the formatting removal:
    putDoc ("There is a handy utility called 'plain' to" <+> plain (bold "remove formatting") <+>
              plain ("if you need to e.g. support" <+> red "non-ANSI" <+> "terminals"))
