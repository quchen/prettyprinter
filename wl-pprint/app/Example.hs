{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Text.PrettyPrint.Doc
import           Data.Text.PrettyPrint.Doc.Render.Terminal
import           System.IO



ul :: [Doc] -> Doc
ul = align . vsep . map (\x -> "-" <+> align x)

ol :: [Doc] -> Doc
ol = align . vsep . zipWith (\i x -> align (pretty i <> dot <+> x)) [1::Int ..]

p :: Text -> Doc
p = fillSep . map pretty . T.words

h1 :: Doc -> Doc
h1 = bold . black . underlineWith "="

h2 :: Doc -> Doc
h2 = black . underlineWith "-"

underlineWith :: Text -> Doc -> Doc
underlineWith symbol x = cat
    [ hardline
    , align (width x (\w ->
        hardline <> pretty (T.take w (T.replicate w symbol))))
    , hardline ]



main :: IO ()
main = (renderIO stdout . layoutPretty 0.8 80) (vsep
    [ h1 "HTML-inspired demonstration"
    , p "The layout algorithm tries to introduce line breaks when necessary, in order to make the text nice and compact."
    , p "The default settings restrict the ribbon, i.e. the non-indentation part of the text, to 32 characters, and the rightmost column of 80 characters is respected."
    , p "That's nice for structured output, but if our goal is to print lots of text, not restricting the ribbon is easier on the eyes, as lines are filled more homogeneously, as you can see in this document."
    , h2 "Let's try some nested lists"
    , ul [ p "Here we go!"
         , ol [ "We"
              , "can"
              , ul ["nest", "lists", "and", "they", "align", "nicely"]
              , p "And when entries are too long, the prettyprinter tries to introduce line breaks so everything stays nice and compact"
              , p "Entries that are close to each other are formatted in a similar way, because they have to fit similar constraints." ]
         , red (p "Entries can be colored")
         , italics (p "Or italicized, if the terminal supports it")
         , p "We can even start lists" <+> ol ["in", "the", "middle", "of", "some", "other", "text"]
         , hang 1 (align (fillSep (["We", underline "cannot"] <> map pretty (T.words "do that in HTML, but work with me here :-]"))))
         ]
    ])
