-- Provide a fake API, mimicking Data.Text.Lazy from text package,
-- but actually backed by type Text = String. It is used only in rare
-- circumstances, when prettyprinter is built with -text flag.
--

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Text.Lazy where

import Data.Text as T

type Text = T.Text
length = T.length
lines = T.lines
toStrict = id
pack = T.pack
unpack = T.unpack
