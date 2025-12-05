-- | Convert back and forth between the 'Old.Doc' type of the @ansi-wl-pprint@
-- and the 'New.Doc' of the prettyprinter package. Useful in order to use the
-- @prettyprinter@ library together with another library that produces
-- @ansi-wl-pprint@ output, and vice versa.
--
-- @
-- ╭────────────────────╮    'fromAnsiWlPprint'    ╭────────────────────╮
-- │        'Old.Doc'         ├───────────────────────▷│  'New.Doc' 'NewTerm.AnsiStyle'     │
-- │  (ansi-wl-pprint)  │◁───────────────────────┤  (prettyprinter)   │
-- ╰────────────────────╯     'toAnsiWlPprint'     ╰────────────────────╯
-- @
--
-- These conversion functions work well, but strictly speaking they are __not__
-- inverses of each other. @ansi-wl-pprint@ supports slightly less features than
-- @prettyprinter@ – the latter has italics, and allows reacting on the
-- configured ribbon width via 'New.withPageWidth'.
module Prettyprinter.Convert.AnsiWlPprint (
    fromAnsiWlPprint,
    toAnsiWlPprint,
) where



import qualified Data.Text as T

import qualified Prettyprinter.Internal                 as New
import qualified Prettyprinter.Render.Terminal.Internal as NewTerm
import qualified System.Console.ANSI                    as Ansi
import qualified Text.PrettyPrint.ANSI.Leijen.Internal  as Old



-- | @ansi-wl-pprint ───▷ prettyprinter@
fromAnsiWlPprint :: Old.Doc -> New.Doc NewTerm.AnsiStyle
fromAnsiWlPprint = \doc -> case doc of
    Old.Fail     -> New.Fail
    Old.Empty    -> New.Empty
    Old.Char c   -> New.Char c
    Old.Text l t -> New.Text l (T.pack t)
    Old.Line     -> New.Line

    Old.FlatAlt x y -> New.FlatAlt (go x) (go y)
    Old.Cat x y     -> New.Cat (go x) (go y)
    Old.Nest i x    -> New.Nest i (go x)
    Old.Union x y   -> New.Union (go x) (go y)

    Old.Column f -> New.Column (go . f)
    Old.Columns f -> New.WithPageWidth (go . f . convert)
      where
        convert :: New.PageWidth -> Maybe Int
        convert (New.AvailablePerLine width _ribbon) = Just width
        convert New.Unbounded                        = Nothing
    Old.Nesting f -> New.Nesting (go . f)

    Old.Color layer intensity color x ->
        let convertLayerIntensity :: Ansi.ConsoleLayer -> Ansi.ColorIntensity -> NewTerm.Color -> NewTerm.AnsiStyle
            convertLayerIntensity Ansi.Foreground Ansi.Dull  = NewTerm.colorDull
            convertLayerIntensity Ansi.Background Ansi.Dull  = NewTerm.bgColorDull
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
    go = fromAnsiWlPprint

-- | @prettyprinter ───▷ ansi-wl-pprint@
toAnsiWlPprint :: New.Doc NewTerm.AnsiStyle -> Old.Doc
toAnsiWlPprint = \doc -> case doc of
    New.Fail     -> Old.Fail
    New.Empty    -> Old.Empty
    New.Char c   -> Old.Char c
    New.Text l t -> Old.Text l (T.unpack t)
    New.Line     -> Old.Line

    New.FlatAlt x y -> Old.FlatAlt (go x) (go y)
    New.Cat x y     -> Old.Cat (go x) (go y)
    New.Nest i x    -> Old.Nest i (go x)
    New.Union x y   -> Old.Union (go x) (go y)

    New.Column f -> Old.Column (go . f)
    New.WithPageWidth f -> Old.Columns (go . f . convert)
      where
        convert :: Maybe Int -> New.PageWidth
        convert Nothing = New.Unbounded
        convert (Just width) = New.AvailablePerLine width 1.0
    New.Nesting f -> Old.Nesting (go . f)

    New.Annotated style x -> (convertFg . convertBg . convertBold . convertUnderlining) (go x)
                               -- Italics are unsupported by ansi-wl-pprint so we skip them
      where
        convertFg, convertBg, convertBold, convertUnderlining :: Old.Doc -> Old.Doc
        convertFg = case NewTerm.ansiForeground style of
            Just (NewTerm.Color16 intensity color) -> convertColor True intensity color
            _ -> id
        convertBg = case NewTerm.ansiBackground style of
            Just (NewTerm.Color16 intensity color) -> convertColor False intensity color
            _ -> id
        convertBold = case NewTerm.ansiBold style of
            Just NewTerm.Bold -> Old.bold
            _ -> id
        convertUnderlining = case NewTerm.ansiUnderlining style of
            Just NewTerm.Underlined -> Old.underline
            _ -> id

        convertColor
            :: Bool -- True = foreground, False = background
            -> NewTerm.Intensity
            -> NewTerm.Color
            -> Old.Doc
            -> Old.Doc
        convertColor True  NewTerm.Vivid NewTerm.Black   = Old.black
        convertColor True  NewTerm.Vivid NewTerm.Red     = Old.red
        convertColor True  NewTerm.Vivid NewTerm.Green   = Old.green
        convertColor True  NewTerm.Vivid NewTerm.Yellow  = Old.yellow
        convertColor True  NewTerm.Vivid NewTerm.Blue    = Old.blue
        convertColor True  NewTerm.Vivid NewTerm.Magenta = Old.magenta
        convertColor True  NewTerm.Vivid NewTerm.Cyan    = Old.cyan
        convertColor True  NewTerm.Vivid NewTerm.White   = Old.white

        convertColor True  NewTerm.Dull  NewTerm.Black   = Old.dullblack
        convertColor True  NewTerm.Dull  NewTerm.Red     = Old.dullred
        convertColor True  NewTerm.Dull  NewTerm.Green   = Old.dullgreen
        convertColor True  NewTerm.Dull  NewTerm.Yellow  = Old.dullyellow
        convertColor True  NewTerm.Dull  NewTerm.Blue    = Old.dullblue
        convertColor True  NewTerm.Dull  NewTerm.Magenta = Old.dullmagenta
        convertColor True  NewTerm.Dull  NewTerm.Cyan    = Old.dullcyan
        convertColor True  NewTerm.Dull  NewTerm.White   = Old.dullwhite

        convertColor False NewTerm.Vivid NewTerm.Black   = Old.onblack
        convertColor False NewTerm.Vivid NewTerm.Red     = Old.onred
        convertColor False NewTerm.Vivid NewTerm.Green   = Old.ongreen
        convertColor False NewTerm.Vivid NewTerm.Yellow  = Old.onyellow
        convertColor False NewTerm.Vivid NewTerm.Blue    = Old.onblue
        convertColor False NewTerm.Vivid NewTerm.Magenta = Old.onmagenta
        convertColor False NewTerm.Vivid NewTerm.Cyan    = Old.oncyan
        convertColor False NewTerm.Vivid NewTerm.White   = Old.onwhite

        convertColor False NewTerm.Dull  NewTerm.Black   = Old.ondullblack
        convertColor False NewTerm.Dull  NewTerm.Red     = Old.ondullred
        convertColor False NewTerm.Dull  NewTerm.Green   = Old.ondullgreen
        convertColor False NewTerm.Dull  NewTerm.Yellow  = Old.ondullyellow
        convertColor False NewTerm.Dull  NewTerm.Blue    = Old.ondullblue
        convertColor False NewTerm.Dull  NewTerm.Magenta = Old.ondullmagenta
        convertColor False NewTerm.Dull  NewTerm.Cyan    = Old.ondullcyan
        convertColor False NewTerm.Dull  NewTerm.White   = Old.ondullwhite

  where
    go = toAnsiWlPprint
