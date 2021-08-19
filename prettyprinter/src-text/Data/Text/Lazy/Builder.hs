-- Provide a fake API, mimicking Data.Text.Lazy.Builder from text package,
-- but actually backed by type Builder = String. It is used only in rare
-- circumstances, when prettyprinter is built with -text flag.
--

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Text.Lazy.Builder where

type Builder = String
fromText = id
singleton = (:[])
toLazyText = id
