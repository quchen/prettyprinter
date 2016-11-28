{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render 'SimpleDoc' as 'Text'.
module Data.Text.PrettyPrint.Doc.Render.Text (
    renderLazy,
    renderStrict,
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



-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering function
-- and transforms it to lazy text.
--
-- All styling information is discarded. If this is undesirable, maybe the
-- functions in "Data.Text.PrettyPrint.Doc.Render.Terminal" are closer to what
-- you are looking for.
renderLazy :: SimpleDoc -> LT.Text
renderLazy = LTB.toLazyText . build
  where
    build = \case
        SFail          -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
        SEmpty         -> mempty
        SChar c x      -> LTB.singleton c <> build x
        SText t x      -> LTB.fromText t <> build x
        SLine i x      -> LTB.singleton '\n' <> LTB.fromText (T.replicate i " ") <> build x
        SStylePush _ x -> build x
        SStylePop x    -> build x

-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
renderStrict :: SimpleDoc -> Text
renderStrict = LT.toStrict . renderLazy
