{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Display 'SimpleDoc' as 'HTML' encoded as 'Text'.
--
-- Since the 'Doc' language talks about 'bold'ening and not emphasis for
-- example, we do not have a correct corresponding HTML tag to display this.
-- Therefoew, we choose semantic tags like @<strong>@ instead, which are similar
-- in their default renderings in most browsers.
module Data.Text.PrettyPrint.Doc.Display.Html (
    displayLazyHtmlText,
    displayStrictHtmlText,
) where

import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as LTB

import Data.Text.PrettyPrint.Doc



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XLambdaCase
-- >>> import qualified Data.Text.IO as T
-- >>> import qualified Data.Text.Lazy.IO as LT



-- | @('displayLazyHtmlText' sdoc)@ takes the output @sdoc@ from a rendering
-- function and transforms it to lazy text with HTML tags added. The output
-- contains significant whitespace, which HTML rendering swallows. This can be
-- avoided by putting the result in a @<pre>@ environment.
--
-- >>> let doc = "This text" <+> bold ("is strong" <+> italics "with emphasis")
-- >>> let pprint = LT.putStrLn . displayLazyHtmlText . renderPretty 0.4 40
-- >>> pprint (plain doc)
-- This text is strong with emphasis
-- >>> pprint doc
-- This text <strong>is strong <em>with emphasis</em></strong>
displayLazyHtmlText :: SimpleDoc -> LT.Text
displayLazyHtmlText = LTB.toLazyText . build
  where
    build = \case
        SFail        -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
        SEmpty       -> mempty
        SChar c x    -> LTB.singleton c <> build x
        SText t x    -> LTB.fromText t <> build x
        SLine i x    -> LTB.singleton '\n' <> LTB.fromText (T.replicate i " ") <> build x
        SStyle s x y -> applyStyle s x <> build y

    applyStyle :: Style -> SimpleDoc -> LTB.Builder
    applyStyle s d = case s of
        SItalicized  -> htmlItalicize (build d)
        SBold        -> htmlBolden (build d)
        SUnderlined  -> htmlUnderline (build d)
        SColor l i c -> htmlColorize l i c (build d)

-- | Strict 'Text' version of 'displayLazyHtmlText'.
--
-- >>> let doc = "some" <+> align (vsep ([bold "text", "to", italics ("nicely" <+> bold "lay"), "out"]))
-- >>> let pprint = T.putStrLn . displayStrictHtmlText . renderPretty 0.4 40
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
displayStrictHtmlText :: SimpleDoc -> Text
displayStrictHtmlText = LT.toStrict . displayLazyHtmlText

htmlBolden, htmlItalicize, htmlUnderline :: LTB.Builder -> LTB.Builder
htmlBolden    = encloseInTag "strong" Nothing
htmlItalicize = encloseInTag "em" Nothing
htmlUnderline = encloseInTag "span" (Just "style=\"text-decoration: underline\"")

htmlColorize :: SLayer -> SIntensity -> SColor -> LTB.Builder -> LTB.Builder
htmlColorize _l _s _c x = x

-- | Enclose a document in an HTML tag
encloseInTag
    :: LTB.Builder -- ^ Tag name, e.g. @span@
    -> Maybe LTB.Builder -- ^ Tag attributes, e.g. @style="text-decoration: underline"@
    -> LTB.Builder -- ^ Document to enclose
    -> LTB.Builder
encloseInTag tag attrs doc = mconcat
    [ "<", tag
    , case attrs of Nothing -> mempty
                    Just attrs' -> " " <> attrs'
    , ">"
    , doc
    , "</", tag, ">" ]
