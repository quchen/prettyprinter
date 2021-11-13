-- | This module is for use by packages that need to be able to use the prettyprinter package
-- without incurring a dependency on the text package.
--
-- Legitimate examples of packages that must have text as an optional dependency include text (and
-- bytestring).
module Prettyprinter.Util.Compat.Text.IO
  ( module Data.Text.IO
  ) where

import Data.Text.IO (hPutStr, putStrLn)