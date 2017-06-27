{-# LANGUAGE LambdaCase #-}

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
module Data.Text.Prettyprint.Convert.AnsiWlPprint (
    fromAnsiWlPprint,
    toAnsiWlPprint,
) where



import qualified Data.Text as T

import qualified Data.Text.Prettyprint.Doc.Internal        as New
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as NewTerm
import qualified System.Console.ANSI                       as Ansi
import qualified Text.PrettyPrint.ANSI.Leijen.Internal     as Old



-- | @ansi-wl-pprint ───▷ prettyprinter@
fromAnsiWlPprint :: Old.Doc -> New.Doc NewTerm.AnsiStyle
fromAnsiWlPprint = \case
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
toAnsiWlPprint = \case
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

    New.Annotated NewTerm.SetAnsiStyle
        { NewTerm.ansiForeground  = ann_ansiForeground
        , NewTerm.ansiBackground  = ann_ansiBackground
        , NewTerm.ansiBold        = ann_ansiBold
        , NewTerm.ansiItalics     = _ann_ansiItalics -- Unsupported by ansi-wl-pprint
        , NewTerm.ansiUnderlining = ann_ansiUnderlining }
        x -> (convertFg . convertBg . convertBold . convertUnderlining) (go x)
      where
        convertFg :: Old.Doc -> Old.Doc
        convertFg = case ann_ansiForeground of
            Nothing -> id
            Just (NewTerm.Vivid, NewTerm.Black)   -> Old.black
            Just (NewTerm.Vivid, NewTerm.Red)     -> Old.red
            Just (NewTerm.Vivid, NewTerm.Green)   -> Old.green
            Just (NewTerm.Vivid, NewTerm.Yellow)  -> Old.yellow
            Just (NewTerm.Vivid, NewTerm.Blue)    -> Old.blue
            Just (NewTerm.Vivid, NewTerm.Magenta) -> Old.magenta
            Just (NewTerm.Vivid, NewTerm.Cyan)    -> Old.cyan
            Just (NewTerm.Vivid, NewTerm.White)   -> Old.white
            Just (NewTerm.Dull,  NewTerm.Black)   -> Old.dullblack
            Just (NewTerm.Dull,  NewTerm.Red)     -> Old.dullred
            Just (NewTerm.Dull,  NewTerm.Green)   -> Old.dullgreen
            Just (NewTerm.Dull,  NewTerm.Yellow)  -> Old.dullyellow
            Just (NewTerm.Dull,  NewTerm.Blue)    -> Old.dullblue
            Just (NewTerm.Dull,  NewTerm.Magenta) -> Old.dullmagenta
            Just (NewTerm.Dull,  NewTerm.Cyan)    -> Old.dullcyan
            Just (NewTerm.Dull,  NewTerm.White)   -> Old.dullwhite

        convertBg :: Old.Doc -> Old.Doc
        convertBg = case ann_ansiBackground of
            Nothing -> id
            Just (NewTerm.Vivid, NewTerm.Black)   -> Old.onblack
            Just (NewTerm.Vivid, NewTerm.Red)     -> Old.onred
            Just (NewTerm.Vivid, NewTerm.Green)   -> Old.ongreen
            Just (NewTerm.Vivid, NewTerm.Yellow)  -> Old.onyellow
            Just (NewTerm.Vivid, NewTerm.Blue)    -> Old.onblue
            Just (NewTerm.Vivid, NewTerm.Magenta) -> Old.onmagenta
            Just (NewTerm.Vivid, NewTerm.Cyan)    -> Old.oncyan
            Just (NewTerm.Vivid, NewTerm.White)   -> Old.onwhite
            Just (NewTerm.Dull,  NewTerm.Black)   -> Old.ondullblack
            Just (NewTerm.Dull,  NewTerm.Red)     -> Old.ondullred
            Just (NewTerm.Dull,  NewTerm.Green)   -> Old.ondullgreen
            Just (NewTerm.Dull,  NewTerm.Yellow)  -> Old.ondullyellow
            Just (NewTerm.Dull,  NewTerm.Blue)    -> Old.ondullblue
            Just (NewTerm.Dull,  NewTerm.Magenta) -> Old.ondullmagenta
            Just (NewTerm.Dull,  NewTerm.Cyan)    -> Old.ondullcyan
            Just (NewTerm.Dull,  NewTerm.White)   -> Old.ondullwhite

        convertBold :: Old.Doc -> Old.Doc
        convertBold = case ann_ansiBold of
            Nothing           -> id
            Just NewTerm.Bold -> Old.bold

        convertUnderlining :: Old.Doc -> Old.Doc
        convertUnderlining = case ann_ansiUnderlining of
            Nothing                 -> id
            Just NewTerm.Underlined -> Old.underline

  where
    go = toAnsiWlPprint
