-- Provide a fake API, mimicking Data.Text.Lazy.Builder from text package,
-- but actually backed by type Builder = String. It is used only in rare
-- circumstances, when prettyprinter is built with -text flag.
--

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Text.Lazy.Builder where

import Data.String (IsString (..))
import Data.Semigroup

newtype Builder =  Builder (String -> String)

instance IsString Builder where
  fromString s = Builder (s ++)

instance Semigroup Builder where
  Builder a <> Builder b = Builder (a . b)

instance Monoid Builder where
  mempty = Builder id

fromText :: String -> Builder
fromText t = Builder (t ++)

singleton :: Char -> Builder
singleton c = Builder ([c] ++)

toLazyText :: Builder -> String
toLazyText (Builder b) = b ""
