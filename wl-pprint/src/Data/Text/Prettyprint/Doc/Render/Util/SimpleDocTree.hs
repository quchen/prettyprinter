{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

#include "version-compatibility-macros.h"

-- | Conversion of the linked-list-like 'SimpleDoc' to a tree-like
-- 'SimpleDocTree'.
module Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree (
    SimpleDocTree(..),
    treeForm,
) where



import Control.Applicative
import Data.Text           (Text)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Util.Panic

#if MONAD_FAIL
import Control.Monad.Fail
#endif



-- | A type for parsers of unique results. Token stream »s«, results »a«.
--
-- Hand-written to avoid a dependency on a parser lib.
newtype UniqueParser s a = UniqueParser { runParser :: s -> Maybe (a, s) }

instance Functor (UniqueParser s) where
    fmap f (UniqueParser mx) = UniqueParser (\s ->
        fmap (\(x,s') -> (f x, s')) (mx s))

instance Applicative (UniqueParser s) where
    pure x = UniqueParser (\rest -> Just (x, rest))
    UniqueParser mf <*> UniqueParser mx = UniqueParser (\s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        pure (f x, s'') )

instance Monad (UniqueParser s) where
#if !(APPLICATIVE_MONAD)
    return = pure
#endif
    UniqueParser p >>= f = UniqueParser (\s -> do
        (a', s') <- p s
        (a'', s'') <- runParser (f a') s'
        pure (a'', s'') )

    fail _err = empty

#if MONAD_FAIL
instance MonadFail (UniqueParser s) where
    fail _err = empty
#endif

instance Alternative (UniqueParser s) where
    empty = UniqueParser (const empty)
    UniqueParser p <|> UniqueParser q = UniqueParser (\s -> p s <|> q s)

data SimpleDocTok ann
    = TokEmpty
    | TokChar Char
    | TokText !Int Text
    | TokLine Int
    | TokAnnPush ann
    | TokAnnPop
    deriving (Eq, Ord, Show)

-- | A 'SimpleDoc' is a linked list of different annotated cons cells ('SText'
-- and then some further 'SimpleDoc', 'SLine' and then some further 'SimpleDoc', …).
-- This format is very suitable as a target for a layout engine, but not very
-- useful for rendering to a structured format such as HTML, where we don’t want
-- to do a lookahead until the end of some markup. These formats benefit from a
-- tree-like structure that explicitly marks its contents as annotated.
-- 'SimpleDocTree' is that format.
data SimpleDocTree ann
    = STEmpty
    | STChar Char
    | STText !Int Text
    | STLine !Int
    | STAnn ann (SimpleDocTree ann)
    | STConcat [SimpleDocTree ann]
    deriving (Eq, Ord, Show)

-- | Get the next token, consuming it in the process.
nextToken :: UniqueParser (SimpleDoc ann) (SimpleDocTok ann)
nextToken = UniqueParser (\case
    SFail             -> panicUncaughtFail
    SEmpty            -> empty
    SChar c rest      -> Just (TokChar c      , rest)
    SText l t rest    -> Just (TokText l t    , rest)
    SLine i rest      -> Just (TokLine i      , rest)
    SAnnPush ann rest -> Just (TokAnnPush ann , rest)
    SAnnPop rest      -> Just (TokAnnPop      , rest) )

sdocToTreeParser :: UniqueParser (SimpleDoc ann) (SimpleDocTree ann)
sdocToTreeParser = fmap wrap (many contentPiece)

  where

    wrap :: [SimpleDocTree ann] -> SimpleDocTree ann
    wrap = \case
        []  -> STEmpty
        [x] -> x
        xs  -> STConcat xs

    contentPiece = nextToken >>= \case
        TokEmpty       -> pure STEmpty
        TokChar c      -> pure (STChar c)
        TokText l t    -> pure (STText l t)
        TokLine i      -> pure (STLine i)
        TokAnnPop      -> empty
        TokAnnPush ann -> do annotatedContents <- sdocToTreeParser
                             TokAnnPop <- nextToken
                             pure (STAnn ann annotatedContents)

-- | Convert a 'SimpleDoc' to its 'SimpleDocTree' representation.
treeForm :: SimpleDoc ann -> SimpleDocTree ann
treeForm sdoc = case runParser sdocToTreeParser sdoc of
    Nothing               -> panicSimpleDocTreeConversionFailed
    Just (sdoct, SEmpty)  -> sdoct
    Just (_, _unconsumed) -> panicInputNotFullyConsumed

-- $
--
-- >>> :set -XOverloadedStrings
-- >>> treeForm (layoutPretty defaultLayoutOptions ("lorem" <+> "ipsum" <+> annotate True ("TRUE" <+> annotate False "FALSE") <+> "dolor"))
-- STConcat [STText 5 "lorem",STChar ' ',STText 5 "ipsum",STChar ' ',STAnn True (STConcat [STText 4 "TRUE",STChar ' ',STAnn False (STText 5 "FALSE")]),STChar ' ',STText 5 "dolor"]
