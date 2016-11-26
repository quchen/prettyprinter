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
-- ANSI color information will be discarded by this function unless you are
-- running on a Unix-like operating system. This is due to a technical
-- limitation in Windows ANSI support.
displayLazyText :: SimpleDoc -> LT.Text
displayLazyText = LTB.toLazyText . build
  where
    build = \case
        SFail     -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
        SEmpty    -> mempty
        SChar c x -> LTB.singleton c <> build x
        SText t x -> LTB.fromText t <> build x
        SLine i x -> LTB.singleton '\n' <> LTB.fromText (T.replicate i " ") <> build x
        SSGR _ x  -> "TODO text style stuff" <> build x

-- | @('displayLazyText' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
displayStrictText :: SimpleDoc -> Text
displayStrictText = LT.toStrict . displayLazyText
