{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Text.Prettyprint.Doc.Internal.Diagnostic
    ( diag
    , diagS
    ) where

import Data.Text (Text)
import {-# SOURCE #-} Data.Text.Prettyprint.Doc.Internal (Doc, PageWidth)

import {-# SOURCE #-} qualified Data.Text.Prettyprint.Doc.Internal as Doc

-- | A copy of the 'Doc' type, used to provide a derived 'Show' instance.
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
    | Column (Int -> Diag ann)
    | WithPageWidth (PageWidth -> Diag ann)
    | Nesting (Int -> Diag ann)
    | Annotated ann (Diag ann)

deriving instance (Show ann, Show (Int -> Diag ann), Show (PageWidth -> Diag ann)) => Show (Diag ann)

docToDiag :: Doc ann -> Diag ann
docToDiag doc = case doc of
    Doc.Fail -> Fail
    Doc.Empty -> Empty
    Doc.Char c -> Char c
    Doc.Text l t -> Text l t
    Doc.Line -> Line
    Doc.FlatAlt a b -> FlatAlt (docToDiag a) (docToDiag b)
    Doc.Cat a b -> Cat (docToDiag a) (docToDiag b)
    Doc.Nest i d -> Nest i (docToDiag d)
    Doc.Union a b -> Union (docToDiag a) (docToDiag b)
    Doc.Column f -> Column (docToDiag . f)
    Doc.WithPageWidth f -> WithPageWidth (docToDiag . f)
    Doc.Nesting f -> Nesting (docToDiag . f)
    Doc.Annotated ann d -> Annotated ann (docToDiag d)

diagS :: (Show ann, Show (Int -> Diag ann), Show (PageWidth -> Diag ann)) => Doc ann -> String
diagS = show . docToDiag

diag :: (Show ann, Show (Int -> Diag ann), Show (PageWidth -> Diag ann)) => Doc ann -> IO ()
diag = putStrLn . diagS
