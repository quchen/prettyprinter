{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module shows how to write custom prettyprinter backends.
--
-- The module is written to be readable top-to-bottom in both Haddock and raw
-- source form.
module Data.Text.Prettyprint.Doc.Render.TutorialExample where



import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.RenderM

-- $setup
--
-- (Definitions for the doctests)
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.Lazy.IO as TL



-- $standalone-text
--
-- = The type of available markup
--
-- First, we define a set of valid annotations must be defined, with the goal of
-- defining a @'Doc' 'SimpleHtml'@. We will later define how to convert this to
-- the output format ('TL.Text').

data SimpleHtml = Bold | Italics | Color Color | Paragraph | Headline
data Color = Red | Green | Blue



-- $standalone-text
--
-- == Conveinence definitions

bold, italics, paragraph, headline :: Doc SimpleHtml -> Doc SimpleHtml
bold = annotate Bold
italics = annotate Italics
paragraph = annotate Paragraph
headline = annotate Headline

color :: Color -> Doc SimpleHtml -> Doc SimpleHtml
color c = annotate (Color c)





-- $standalone-text
--
-- = The rendering algorithm
--
-- With the annotation definitions out of the way, we can now define a
-- conversion function from 'SimpleDoc' annotated with our 'SimpleHtml' to the
-- final 'TL.Text' representation.

-- | The 'RenderM' type defines a stack machine suitable for many rendering
-- needs. It has two auxiliary parameters: the type of the end result, and the
-- type of the document’s annotations.
--
-- Most 'RenderM' creations will look like this definition: a recursive walk
-- through the 'SimpleDoc', pushing styles on the stack and popping them off
-- again, and writing raw output.
renderStackMachine :: SimpleDoc SimpleHtml -> RenderM TLB.Builder SimpleHtml ()
renderStackMachine = \case
    SFail -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@. This is a bug in the layout algorithm!"
    SEmpty -> pure ()
    SChar c x -> do
        writeOutput (TLB.singleton c)
        renderStackMachine x
    SText _l t x -> do
        writeOutput (TLB.fromText t)
        renderStackMachine x
    SLine i x -> do
        writeOutput (TLB.singleton '\n' )
        writeOutput (TLB.fromText (T.replicate i " "))
        renderStackMachine x
    SAnnPush s x -> do
        pushStyle s
        writeOutput (fst (htmlTag s))
        renderStackMachine x
    SAnnPop x -> do
        s <- unsafePopStyle
        writeOutput (snd (htmlTag s))
        renderStackMachine x

-- | Convert a 'SimpleHtml' annotation to a pair of opening and closing tags.
-- This is where the translation of style to raw output happens.
htmlTag :: SimpleHtml -> (TLB.Builder, TLB.Builder)
htmlTag = \case
    Bold -> ("<strong>", "</strong>")
    Italics -> ("<em>", "</em>")
    Color c -> ("<span style=\"color: " <> hexCode c <> "\">", "</span>")
    Paragraph -> ("<p>", "</p>")
    Headline -> ("<h1>", "</h1>")
  where
    hexCode :: Color -> TLB.Builder
    hexCode = \case
        Red   -> "#f00"
        Green -> "#0f0"
        Blue  -> "#00f"

-- | We can now wrap our stack machine definition from 'renderStackMachine' in a
-- nicer interface; on successful conversion, we run the builder to give us the
-- final 'TL.Text', and before we do that we check that the style stack is empty
-- (i.e. there are no unmatched style applications) after the machine is run.
render :: SimpleDoc SimpleHtml -> TL.Text
render doc
  = let (resultBuilder, remainingStyles) = execRenderM [] (renderStackMachine doc)
    in if null remainingStyles
        then TLB.toLazyText resultBuilder
        else error ("There are "
                    <> show (length remainingStyles)
                    <> " unpaired styles! Please report this as a bug.")

-- $standalone-text
--
-- = Example invocation
--
-- We can now render an example document using our definitions:
--
-- >>> :{
-- >>> let go = TL.putStrLn . render . layoutPretty defaultLayoutOptions
-- >>> in go (vsep
-- >>>     [ headline "Example document"
-- >>>     , paragraph ("This is a" <+> color Red "paragraph" <> comma)
-- >>>     , paragraph ("and" <+> bold "this text is bold.")
-- >>>     ])
-- >>> :}
-- <h1>Example document</h1>
-- <p>This is a <span style="color: #f00">paragraph</span>,</p>
-- <p>and <strong>this text is bold.</strong></p>
