{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

#include "version-compatibility-macros.h"

-- | A strict State/Writer monad to implement a stack machine for rendering
-- 'SimpleDoc's.
module Data.Text.Prettyprint.Doc.Render.Util.StackMachine (
    StackMachine,
    execStackMachine,

    pushStyle,
    unsafePopStyle,
    unsafePeekStyle,
    writeOutput,
) where



import Data.Monoid

#if !(APPLICATIVE_MONAD)
import Control.Applicative
#endif



-- | @WriterT output StateT [style] a@, but with a strict Writer value.
--
-- The @output@ type is used to append data chunks to, the @style@ is the member
-- of a stack of styles to model nested styles with.
newtype StackMachine output style a = StackMachine ([style] -> (a, output, [style]))

instance Functor (StackMachine output style) where
    fmap f (StackMachine r) = StackMachine (\s ->
        let (x1, w1, s1) = r s
        in (f x1, w1, s1))

instance Monoid output => Applicative (StackMachine output style) where
    pure x = StackMachine (\s -> (x, mempty, s))
    StackMachine f <*> StackMachine x = StackMachine (\s ->
        let (f1, w1, s1) = f s
            (x2, w2, s2) = x s1
            !w12 = w1 <> w2
        in (f1 x2, w12, s2))

instance Monoid output => Monad (StackMachine output style) where
#if !(APPLICATIVE_MONAD)
    return = pure
#endif
    StackMachine r >>= f = StackMachine (\s ->
        let (x1, w1, s1) = r s
            StackMachine r1 = f x1
            (x2, w2, s2) = r1 s1
            !w12 = w1 <> w2
        in (x2, w12, s2))

-- | Add a new style to the style stack.
pushStyle :: Monoid output => style -> StackMachine output style ()
pushStyle style = StackMachine (\styles -> ((), mempty, style : styles))

-- | Get the topmost style.
--
-- If the stack is empty, this raises an 'error'.
unsafePopStyle :: Monoid output => StackMachine output style style
unsafePopStyle = StackMachine (\case
    x:xs -> (x, mempty, xs)
    [] -> error "Popped an empty style stack! Please report this as a bug.")

-- | View the topmost style, but do not modify the stack.
--
-- If the stack is empty, this raises an 'error'.
unsafePeekStyle :: Monoid output => StackMachine output style style
unsafePeekStyle = StackMachine (\styles -> case styles of
    x:_ -> (x, mempty, styles)
    [] -> error "Peeked an empty style stack! Please report this as a bug.")

-- | Append a value to the output end.
writeOutput :: output -> StackMachine output style ()
writeOutput w = StackMachine (\styles -> ((), w, styles))

-- | Run the renderer and retrive the writing end
execStackMachine :: [styles] -> StackMachine write styles a -> (write, [styles])
execStackMachine styles (StackMachine r) = let (_, w, s) = r styles in (w, s)
