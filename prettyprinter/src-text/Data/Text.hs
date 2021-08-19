-- Provide a fake API, mimicking Data.Text from text package,
-- but actually backed by type Text = String. It is used only in rare
-- circumstances, when prettyprinter is built with -text flag.
--

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Text where

import Prelude hiding (head, length, null, replicate)
import qualified Data.Char
import qualified Data.List

type Text = String
cons = (:)
dropWhileEnd = Data.List.dropWhileEnd
head = Data.List.head
intercalate = Data.List.intercalate
length = Data.List.length :: [Char] -> Int
lines = Data.List.lines
map = Data.List.map
null = Data.List.null :: [Char] -> Bool
pack = id
replicate = (Data.List.concat .) . Data.List.replicate
singleton = (:[])
snoc xs x = xs ++ [x]
stripEnd = dropWhileEnd Data.Char.isSpace
unlines = Data.List.unlines
unpack = id
words = Data.List.words

uncons :: Text -> Maybe (Char, Text)
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

splitOn :: Text -> Text -> [Text]
splitOn pat src
  | null pat = error "splitOn: empty pattern"
  | otherwise = go [] src
  where
    go acc [] = [reverse acc]
    go acc xs@(y : ys)
      | pat `Data.List.isPrefixOf` xs
      = reverse acc : go [] (drop (length pat) xs)
      | otherwise
      = go (y : acc) ys
