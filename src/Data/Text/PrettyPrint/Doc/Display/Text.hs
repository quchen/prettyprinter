{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Display 'SimpleDoc' as 'Text'.
module Data.Text.PrettyPrint.Doc.Display.Text (
    displayLazyText,
    displayStrictText,

    displayMarkedLazyText,
    displayMarkedStrictText,
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



-- | @('displayLazyText' sdoc)@ takes the output @sdoc@ from a rendering
-- function and transforms it to lazy text.
--
-- All styling information is discarded. If this is undesirable, maybe the
-- functions in "Data.Text.PrettyPrint.Doc.Display.Terminal" are closer to what
-- you are looking for.
displayLazyText :: SimpleDoc -> LT.Text
displayLazyText = LTB.toLazyText . build
  where
    build = \case
        SFail        -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
        SEmpty       -> mempty
        SChar c x    -> LTB.singleton c <> build x
        SText t x    -> LTB.fromText t <> build x
        SLine i x    -> LTB.singleton '\n' <> LTB.fromText (T.replicate i " ") <> build x
        SStyle _ x y -> build x <> build y

-- | @('displayLazyText' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
displayStrictText :: SimpleDoc -> Text
displayStrictText = LT.toStrict . displayLazyText

-- | Add subtle markers for emphasis, strong emphasis and underlining. Note that
-- this happens after the rendering algorithm is done, so the added characters
-- may result in lines slighly longer than the page width.
--
-- >>> let doc = "This text" <+> bold ("is bold" <+> italics "with emphasis" <+> "!")
-- >>> let pprint = LT.putStrLn . displayMarkedLazyText . renderPretty 0.4 40
-- >>> pprint doc
-- This text **is bold *with emphasis* !**
displayMarkedLazyText :: SimpleDoc -> LT.Text
displayMarkedLazyText = LTB.toLazyText . build
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
        SItalicized  -> encloseIn "*"  (build d)
        SBold        -> encloseIn "**" (build d)
        SUnderlined  -> encloseIn "_"  (build d)
        SColor _ _ _ -> build d -- We cannot colour text :-(

encloseIn :: Monoid m => m -> m -> m
encloseIn e x = e <> x <> e

-- | Strict version of 'displayMarkedLazyText'.
displayMarkedStrictText :: SimpleDoc -> Text
displayMarkedStrictText = LT.toStrict . displayLazyText
