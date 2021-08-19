{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Gauge.Main
import           Data.Char
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Prettyprinter.Internal



-- The old implementation. Performance isn’t much worse to be honest, mostly
-- well within a σ.
alternative :: Text -> Doc ann
alternative t = case T.length t of
    0 -> Empty
    1 -> Char (T.head t)
    n -> Text n t

current :: Text -> Doc ann
current = unsafeTextWithoutNewlines

main :: IO ()
main = defaultMain [ benchText (letters n) | n <- [0,1,2,3,5,10,50,100] ]

letters :: Int -> Text
letters n = T.pack (take n (filter isAlpha [minBound ..]))

benchText :: Text -> Benchmark
benchText input = bgroup (show (pretty (T.length input) <+> plural "letter" "letters" (T.length input)))
    [ bench "alternative" (whnf alternative input)
    , bench "current" (whnf current input) ]
