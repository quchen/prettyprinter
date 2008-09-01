module Main (main) where

import Text.PrettyPrint.ANSI.Leijen

main :: IO ()
main = do
    putDoc $ red (text "Red") <> text "," <+> white (text "white") <+> text "and" <+> blue (text "blue") <> char '!' <> linebreak
    putDoc $ blue (text "Nested" <+> yellow (text "colors") <+> text "example") <> linebreak
    putDoc $ redb (text "Red") <> text "," <+> whiteb (text "white") <+> text "and" <+> blueb (text "blue") <> char '!' <> linebreak
    putDoc $ blueb (text "Nested" <+> yellowb (text "colors") <+> text "example") <> linebreak