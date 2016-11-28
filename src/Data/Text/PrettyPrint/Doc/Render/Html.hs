{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render 'SimpleDoc' as 'HTML' encoded as 'Text'.
--
-- Since the 'Doc' language talks about 'bold'ening and not emphasis for
-- example, we do not have a correct corresponding HTML tag to render this.
-- Therefoew, we choose semantic tags like @<strong>@ instead, which are similar
-- in their default renderings in most browsers.
module Data.Text.PrettyPrint.Doc.Render.Html (
    renderLazy,
    renderStrict,
) where



import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as LTB

import Data.Text.PrettyPrint.Doc



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XLambdaCase
-- >>> import qualified Data.Text.IO as T
-- >>> import qualified Data.Text.Lazy.IO as LT



-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering function
-- and transforms it to lazy text with HTML tags added. The output contains
-- significant whitespace, which HTML rendering swallows. This can be avoided by
-- putting the result in a @<pre>@ environment.
--
-- >>> let pprint = LT.putStrLn . renderLazy . layoutPretty 0.4 40
-- >>> let doc = "some" <+> align (vsep ([bold "text", "to", italics ("nicely" <+> bold "lay"), dullred "out"]))
-- >>> pprint (plain doc)
-- some text
--      to
--      nicely lay
--      out
-- >>> pprint doc
-- some <strong>text</strong>
--      to
--      <em>nicely <strong>lay</strong></em>
--      <span style="color: rgb(205, 0, 0)">out</span>
renderLazy :: SimpleDoc -> LT.Text
renderLazy doc
  = let (resultBuilder, remainingStyles) = runState (execWriterT (build doc)) []
    in if null remainingStyles
        then LTB.toLazyText resultBuilder
        else error ("There are "
                    <> show (length remainingStyles)
                    <> " unpaired styles! Please report this as a bug.")

build :: SimpleDoc -> WriterT LTB.Builder (State [Style]) ()
build = \case
    SFail -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
    SEmpty -> pure ()
    SChar c x -> do tell (LTB.singleton c)
                    build x
    SText t x -> do tell (LTB.fromText t)
                    build x
    SLine i x -> do tell (LTB.singleton '\n' )
                    tell (LTB.fromText (T.replicate i " "))
                    build x
    SStylePush s x -> do
        lift (modify (s:))
        tell (htmlTagFor s Opening)
        build x
    SStylePop x -> do
        s <- lift get >>= \case
            [] -> error "Attempted to pop a style off an empty stack.\
                        \ Please report this as a bug."
            s':ss -> lift (put ss) *> pure s'
        tell (htmlTagFor s Closing)
        build x

-- | Strict 'Text' version of 'renderLazy'.
renderStrict :: SimpleDoc -> Text
renderStrict = LT.toStrict . renderLazy

-- | Opening or closing HTML tag part?
data OpenClose = Opening | Closing

htmlTagFor :: Style -> OpenClose -> LTB.Builder
htmlTagFor = \case
    SItalicized -> htmlTag "em" Nothing
    SBold -> htmlTag "strong" Nothing
    SUnderlined -> htmlTag "span" (Just "style=\"text-decoration: underline\"")
    SColor SForeground dv c -> htmlTag "span" (Just ("style=\"color: " <> cssColor dv c <> "\""))
    SColor SBackground dv c -> htmlTag "span" (Just ("style=\"background: " <> cssColor dv c <> "\""))

cssColor :: SIntensity -> SColor -> LTB.Builder
cssColor SDull = \case
    SBlack   -> "rgb(0, 0, 0)"
    SRed     -> "rgb(205, 0, 0)"
    SGreen   -> "rgb(0, 205, 0)"
    SYellow  -> "rgb(205, 205, 0)"
    SBlue    -> "rgb(0, 0, 238)"
    SMagenta -> "rgb(205, 0, 205)"
    SCyan    -> "rgb(0, 205, 205)"
    SWhite   -> "rgb(229, 229, 229)"
cssColor SVivid = \case
    SBlack   -> "rgb(127, 127, 127)"
    SRed     -> "rgb(255, 0, 0)"
    SGreen   -> "rgb(0, 255, 0)"
    SYellow  -> "rgb(255, 255, 0)"
    SBlue    -> "rgb(92, 92, 255)"
    SMagenta -> "rgb(255, 0, 255)"
    SCyan    -> "rgb(0, 255, 255)"
    SWhite   -> "rgb(255, 255, 255)"

-- | Enclose a document in an HTML tag
htmlTag
    :: LTB.Builder -- ^ Tag name, e.g. @span@
    -> Maybe LTB.Builder -- ^ Tag attributes, e.g. @style="text-decoration: underline"@
    -> OpenClose
    -> LTB.Builder
htmlTag tag attrs openClose = case openClose of
    Opening -> "<" <> tag <> maybe mempty (" " <>) attrs <> ">"
    Closing -> "</" <> tag <> ">"
