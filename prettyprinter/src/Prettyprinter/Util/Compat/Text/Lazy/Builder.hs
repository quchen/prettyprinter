-- | This module is for use by packages that need to be able to use the prettyprinter package
-- without incurring a dependency on the text package.
--
-- Legitimate examples of packages that must have text as an optional dependency include text (and
-- bytestring).
module Prettyprinter.Util.Compat.Text.Lazy.Builder
  ( module Data.Text.Lazy.Builder
  ) where

import Data.Text.Lazy.Builder (Builder (), fromText, singleton, toLazyText)
