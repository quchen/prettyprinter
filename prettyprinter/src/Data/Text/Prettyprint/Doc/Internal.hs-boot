module Data.Text.Prettyprint.Doc.Internal where

import Data.Text (Text)

data Doc ann =
    Fail
    | Empty
    | Char !Char
    | Text !Int !Text
    | Line
    | FlatAlt (Doc ann) (Doc ann)
    | Cat (Doc ann) (Doc ann)
    | Nest !Int (Doc ann)
    | Union (Doc ann) (Doc ann)
    | Column (Int -> Doc ann)
    | WithPageWidth (PageWidth -> Doc ann)
    | Nesting (Int -> Doc ann)
    | Annotated ann (Doc ann)

data PageWidth
