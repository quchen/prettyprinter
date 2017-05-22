module Data.Text.Prettyprint.Doc.Render.Util.Panic (
    panicUncaughtFail,
    panicUnpairedPop,
    panicSimpleDocTreeConversionFailed,
    panicInputNotFullyConsumed,
) where

-- | Raise a hard 'error' if there is a 'Data.Text.Prettyprint.Doc.SFail' in a
-- 'Data.Text.Prettyprint.Doc.SimpleDocStream'.
panicUncaughtFail :: a
panicUncaughtFail = error ("»SFail« must not appear in a rendered »SimpleDocStream«. This is a bug in the layout algorithm! " ++ report)

-- | Raise a hard 'error' when an annotation terminator is encountered in an
-- unannotated region.
panicUnpairedPop :: a
panicUnpairedPop = error ("An unpaired style terminator was encountered. This is a bug in the layout algorithm! " ++ report)

-- | Raise a hard generic 'error' when the
-- 'Data.Text.Prettyprint.Doc.SimpleDocStream' to
-- 'Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree.SimpleDocTree' conversion fails.
panicSimpleDocTreeConversionFailed :: a
panicSimpleDocTreeConversionFailed = error ("Conversion from SimpleDocStream to SimpleDocTree failed! " ++ report)

-- | Raise a hard 'error' when the »to
-- 'Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree.SimpleDocTree'« parser finishes
-- without consuming the full input.
panicInputNotFullyConsumed :: a
panicInputNotFullyConsumed = error ("Conversion from SimpleDocStream to SimpleDocTree left unconsumed input! " ++ report)

report :: String
report = "Please report this as a bug"
