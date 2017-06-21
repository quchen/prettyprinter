{-# LANGUAGE LambdaCase #-}

module Data.Text.Prettyprint.Convert.AnsiWlPprint (convertFromAnsiWlPprint) where



import qualified Data.Text as T

import qualified Data.Text.Prettyprint.Doc.Internal        as New
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as NewTerm
import qualified System.Console.ANSI                       as Ansi
import qualified Text.PrettyPrint.ANSI.Leijen.Internal     as Old



-- | Convert from an @ansi-wl-pprint@ @'Old.Doc'@ to the @'New.Doc'@ format.
-- Useful in order to use the @prettyprinter@ library together with another
-- library that produces @ansi-wl-pprint@ output.
convertFromAnsiWlPprint :: Old.Doc -> New.Doc NewTerm.AnsiStyle
convertFromAnsiWlPprint = \case
    Old.Fail     -> New.Fail
    Old.Empty    -> New.Empty
    Old.Char c   -> New.Char c
    Old.Text l t -> New.Text l (T.pack t)
    Old.Line     -> New.Line

    Old.FlatAlt x y -> New.FlatAlt (go x) (go y)
    Old.Cat x y     -> New.Cat (go x) (go y)
    Old.Nest i x    -> New.Nest i (go x)
    Old.Union x y   -> New.Union (go x) (go y)

    Old.Column f -> New.Column (convertFromAnsiWlPprint . f)
    Old.Columns f -> New.WithPageWidth (\pw -> convertFromAnsiWlPprint (case pw of
        New.AvailablePerLine width _ribbon -> f (Just width)
        New.Unbounded -> f Nothing ))
    Old.Nesting f -> New.Nesting (convertFromAnsiWlPprint . f)

    Old.Color layer intensity color x ->
        let convertLayerIntensity :: Ansi.ConsoleLayer -> Ansi.ColorIntensity -> NewTerm.Color -> NewTerm.AnsiStyle
            convertLayerIntensity Ansi.Foreground Ansi.Dull = NewTerm.colorDull
            convertLayerIntensity Ansi.Background Ansi.Dull = NewTerm.bgColorDull
            convertLayerIntensity Ansi.Foreground Ansi.Vivid = NewTerm.color
            convertLayerIntensity Ansi.Background Ansi.Vivid = NewTerm.bgColor

            convertColor :: Ansi.Color -> NewTerm.AnsiStyle
            convertColor c = convertLayerIntensity layer intensity (case c of
                Ansi.Black   -> NewTerm.Black
                Ansi.Red     -> NewTerm.Red
                Ansi.Green   -> NewTerm.Green
                Ansi.Yellow  -> NewTerm.Yellow
                Ansi.Blue    -> NewTerm.Blue
                Ansi.Magenta -> NewTerm.Magenta
                Ansi.Cyan    -> NewTerm.Cyan
                Ansi.White   -> NewTerm.White )

        in New.annotate (convertColor color) (go x)
    Old.Intensify intensity x -> case intensity of
        Ansi.BoldIntensity   -> New.annotate NewTerm.bold (go x)
        Ansi.FaintIntensity  -> go x
        Ansi.NormalIntensity -> go x
    Old.Italicize i x -> case i of
        False -> go x
        True  -> New.annotate NewTerm.italicized (go x)
    Old.Underline _ x -> New.annotate NewTerm.underlined (go x)
    Old.RestoreFormat{} -> error "Malformed input: RestoreFormat mayb only be used during rendering. Please report this as a bug."
  where
    go = convertFromAnsiWlPprint
