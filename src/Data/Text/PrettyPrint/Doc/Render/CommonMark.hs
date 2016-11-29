{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render 'SimpleDoc' as common markdown AKA CommonMark in 'Text' format.
module Data.Text.PrettyPrint.Doc.Render.CommonMark (
    renderLazy,
    renderStrict,
) where



import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as LTB

import Data.Text.PrettyPrint.Doc
import Data.Text.PrettyPrint.Doc.Render.RenderM



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XLambdaCase
-- >>> import qualified Data.Text.IO as T
-- >>> import qualified Data.Text.Lazy.IO as LT



-- | Add Markdown-style markers for emphasis and strong emphasis. Other styles
-- are ignored.
--
-- >>> let doc = "This text" <+> italics ("is emphasized" <+> bold "even stronger" <> "!")
-- >>> let pprint = LT.putStrLn . renderLazy . layoutPretty 0.4 40
-- >>> pprint doc
-- This text *is emphasized **even stronger**!*
-- >>> pprint (red doc)
-- This text *is emphasized **even stronger**!*
renderLazy :: SimpleDoc -> LT.Text
renderLazy doc
  = let (resultBuilder, remainingStyles) = execRenderM [] (build doc)
    in if null remainingStyles
        then LTB.toLazyText resultBuilder
        else error ("There are "
                    <> show (length remainingStyles)
                    <> " unpaired styles! Please report this as a bug.")

build :: SimpleDoc -> RenderM LTB.Builder Style ()
build = \case
    SFail -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
    SEmpty -> pure ()
    SChar c x -> do writeResult (LTB.singleton c)
                    build x
    SText t x -> do writeResult (LTB.fromText t)
                    build x
    SLine i x -> do writeResult (LTB.singleton '\n' )
                    writeResult (LTB.fromText (T.replicate i " "))
                    build x
    SStylePush s x -> do
        pushStyle s
        writeResult (styleToMarker s)
        build x
    SStylePop x -> do
        s <- unsafePopStyle
        writeResult (styleToMarker s)
        build x

styleToMarker :: Style -> LTB.Builder
styleToMarker = \case
    SItalicized -> LTB.fromString "*"
    SBold       -> LTB.fromString "**"
    _other      -> mempty

-- | Strict version of 'renderLazy'.
renderStrict :: SimpleDoc -> Text
renderStrict = LT.toStrict . renderLazy
