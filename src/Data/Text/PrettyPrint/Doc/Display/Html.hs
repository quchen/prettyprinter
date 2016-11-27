{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Display 'SimpleDoc' as 'HTML' encoded as 'Text'.
--
-- Since the 'Doc' language talks about 'bold'ening and not emphasis for
-- example, we do not have a correct corresponding HTML tag to display this.
-- Therefoew, we choose semantic tags like @<strong>@ instead, which are similar
-- in their default renderings in most browsers.
module Data.Text.PrettyPrint.Doc.Display.Html (
    displayLazy,
    displayStrict,
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



-- | @('displayLazy' sdoc)@ takes the output @sdoc@ from a rendering function
-- and transforms it to lazy text with HTML tags added. The output contains
-- significant whitespace, which HTML rendering swallows. This can be avoided by
-- putting the result in a @<pre>@ environment.
--
-- >>> let doc = "This text" <+> bold ("is strong" <+> italics "with emphasis")
-- >>> let pprint = LT.putStrLn . displayLazy . renderPretty 0.4 40
-- >>> pprint (plain doc)
-- This text is strong with emphasis
-- >>> pprint doc
-- This text <strong>is strong <em>with emphasis</em></strong>
--
-- >>> displayLazy (renderPretty 0.4 80 (red "TODO"))
displayLazy :: SimpleDoc -> LT.Text
displayLazy doc
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

-- | Strict 'Text' version of 'displayLazy'.
--
-- >>> let doc = "some" <+> align (vsep ([bold "text", "to", italics ("nicely" <+> bold "lay"), "out"]))
-- >>> let pprint = T.putStrLn . displayStrict . renderPretty 0.4 40
-- >>> pprint (plain doc)
-- some text
--      to
--      nicely lay
--      out
-- >>> pprint doc
-- some <strong>text</strong>
--      to
--      <em>nicely <strong>lay</strong></em>
--      out
displayStrict :: SimpleDoc -> Text
displayStrict = LT.toStrict . displayLazy

-- | Opening or closing HTML tag part?
data OpenClose = Opening | Closing

htmlTagFor :: Style -> OpenClose -> LTB.Builder
htmlTagFor = \case
    SItalicized -> htmlTag "em" Nothing
    SBold -> htmlTag "strong" Nothing
    SUnderlined -> htmlTag "span" (Just "style=\"text-decoration: underline\"")
    SColor _ _ _ -> error "TODO: HTML colour rendering"

-- | Enclose a document in an HTML tag
htmlTag
    :: LTB.Builder -- ^ Tag name, e.g. @span@
    -> Maybe LTB.Builder -- ^ Tag attributes, e.g. @style="text-decoration: underline"@
    -> OpenClose
    -> LTB.Builder
htmlTag tag attrs openClose = case openClose of
    Opening -> "<" <> tag <> maybe mempty (" " <>) attrs <> ">"
    Closing -> "</" <> tag <> ">"
