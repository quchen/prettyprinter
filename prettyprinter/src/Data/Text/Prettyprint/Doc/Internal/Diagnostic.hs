module Data.Text.Prettyprint.Doc.Internal.Diagnostic where

import Data.Text (Text)
import {-# SOURCE #-} Data.Text.Prettyprint.Doc.Internal (PageWidth)

-- | A variant of 'Doc' for debugging.
data Diag ann =
    Fail
    | Empty
    | Char !Char
    | Text !Int !Text
    | Line
    | FlatAlt (Diag ann) (Diag ann)
    | Cat (Diag ann) (Diag ann)
    | Nest !Int (Diag ann)
    | Union (Diag ann) (Diag ann)
    | Column [(Int, Diag ann)]
      -- ^ 'Doc': @(Int -> Diag ann)@
    | WithPageWidth [(PageWidth, Diag ann)]
      -- ^ 'Doc': @(PageWidth -> Diag ann)@
    | Nesting [(Int, Diag ann)]
      -- ^ 'Doc': @(Int -> Diag ann)@
    | Annotated ann (Diag ann)
    deriving Show
