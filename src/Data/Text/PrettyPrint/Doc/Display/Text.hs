{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Display 'SimpleDoc' as 'Text'.
module Data.Text.PrettyPrint.Doc.Display.Text (
    displayLazyText,
    displayStrictText,
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
        SFail      -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
        SEmpty     -> mempty
        SChar c x  -> LTB.singleton c <> build x
        SText t x  -> LTB.fromText t <> build x
        SLine i x  -> LTB.singleton '\n' <> LTB.fromText (T.replicate i " ") <> build x
        SStyle _ x -> build x
        SUnStyle x -> build x

-- | @('displayLazyText' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
displayStrictText :: SimpleDoc -> Text
displayStrictText = LT.toStrict . displayLazyText
