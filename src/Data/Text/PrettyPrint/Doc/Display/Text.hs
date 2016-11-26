{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.ANSI.Leijen
-- Copyright   :  Daan Leijen (c) 2000, http://www.cs.uu.nl/~daan
--                Max Bolingbroke (c) 2008, http://blog.omega-prime.co.uk
--                David Luposchainsky (c) 2016, http://github.io/quchen
-- License     :  BSD-style (see the file LICENSE.md)
--
-- Maintainer  :  David Luposchainsky <dluposchainsky at google>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines a prettyprinter to format text in a flexible and
-- convenient way.
--
-- It is based on previous work by Daan Leijen and Max Bolingbroke, who
-- implemented and significantly extended the prettyprinter given by a paper by
-- Phil Wadler in his 1997 paper "A Prettier Printer", by adding lots of
-- convenience functions, styling, and new functionality. Their package,
-- <http:/hackage.haskell.org/package/ansi-wl-pprint ansi-wl-pprint>/ is widely
-- used in the Haskell ecosystem.
--
-- However, ansi-wl-pprint is showing its age, resulting in several
-- shortcomings:
--
--   - Definitions clashing with others that are now standard Haskell, such as
--     @\<$>@
--   - Hard to read operators, such as @\<//>@
--   - Some undocumented definitions, not many examples
--   - Based on 'String' instead of 'Text'

module Data.Text.PrettyPrint.Doc.Display.Text (
    displayLazyText,
    displayStrictText,
) where

import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as LTB
import           System.Console.ANSI    (setSGRCode)

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
        SSGR s x  -> LTB.fromString (setSGRCode s) <> build x

-- | @('displayLazyText' sdoc)@ takes the output @sdoc@ from a rendering and
-- transforms it to strict text.
displayStrictText :: SimpleDoc -> Text
displayStrictText = LT.toStrict . displayLazyText
