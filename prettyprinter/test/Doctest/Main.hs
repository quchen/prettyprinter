module Main (main) where

import Test.DocTest

-- Silence -Wunused-packages:
import Prettyprinter   ()
import Test.QuickCheck ()

main :: IO ()
main = doctest [ "src" , "-Imisc"]
