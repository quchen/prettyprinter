module Data.Text.Prettyprint.Doc.Render.Util.Panic (
    panicUncaughtFail,
    panicUnpairedPop,
    panicSimpleDocTreeConversionFailed,
    panicInputNotFullyConsumed,
) where

-- | Raise a hard 'error' if there is a 'Data.Text.Prettyprinter.Doc.SFail' in a
-- 'Data.Text.Prettyprinter.Doc.SimpleDoc'.
panicUncaughtFail :: a
panicUncaughtFail = error ("»SFail« must not appear in a rendered »SimpleDoc«. This is a bug in the layout algorithm! " ++ report)

panicUnpairedPop :: a
panicUnpairedPop = error ("An unpaired style terminator was encountered. This is a bug in the layout algorithm! " ++ report)

panicSimpleDocTreeConversionFailed :: a
panicSimpleDocTreeConversionFailed = error ("Conversion from SimpleDoc to SimpleDocTree failed! " ++ report)

panicInputNotFullyConsumed :: a
panicInputNotFullyConsumed = error ("Conversion from SimpleDoc to SimpleDocTree left unconsumed input! " ++ report)

report :: String
report = "Please report this as a bug"
