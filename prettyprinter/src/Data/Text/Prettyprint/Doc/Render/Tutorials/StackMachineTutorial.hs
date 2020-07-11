-- | This module is part of the old @Data.Text.Prettyprint.Doc@ module hierarchy
-- which is being replaced by a shallower @Prettyprinter@ module hierarchy
-- offering the same API.
--
-- This module will be deprecated and eventually removed.
--
-- Use "Prettyprinter.Render.Tutorials.StackMachineTutorial" instead.
module Data.Text.Prettyprint.Doc.Render.Tutorials.StackMachineTutorial (
    module Prettyprinter.Render.Tutorials.StackMachineTutorial
) where

-- Yeah, this produces a deprecation warning. It's hard to disable it while
-- staying compatible with GHC < 8.0 though. Don't waste your time.
import Prettyprinter.Render.Tutorials.StackMachineTutorial
