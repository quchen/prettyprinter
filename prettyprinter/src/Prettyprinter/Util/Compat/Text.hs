-- | This module is for use by packages that need to be able to use the prettyprinter package
-- without incurring a dependency on the text package.
--
-- Legitimate examples of packages that must have text as an optional dependency include text (and
-- bytestring).
module Prettyprinter.Util.Compat.Text
  ( module Data.Text
  ) where

import Data.Text (Text, cons, dropWhileEnd, head, intercalate, length, lines, map, null, pack, replicate,
                 singleton, snoc, stripEnd, unlines, unpack, words, uncons, splitOn)
