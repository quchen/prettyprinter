{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A strict State/Writer monad used for rendering 'SimpleDoc's.
module Data.Text.Prettyprint.Doc.Render.RenderM (
    RenderM,
    execRenderM,

    pushStyle,
    unsafePopStyle,
    unsafePeekStyle,
    writeOutput,
) where



import Data.Monoid

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif



-- | @WriterT output StateT [style] a@, but with a strict Writer value.
--
-- The @output@ type is used to append data chunks to, the @style@ is the member
-- of a stack of styles to model nested styles with.
newtype RenderM output style a = RenderM ([style] -> (a, output, [style]))

instance Functor (RenderM output style) where
    fmap f (RenderM r) = RenderM (\s ->
        let (x1, w1, s1) = r s
        in (f x1, w1, s1))

instance Monoid output => Applicative (RenderM output style) where
    pure x = RenderM (\s -> (x, mempty, s))
    RenderM f <*> RenderM x = RenderM (\s ->
        let (f1, w1, s1) = f s
            (x2, w2, s2) = x s1
            !w12 = w1 <> w2
        in (f1 x2, w12, s2))

instance Monoid output => Monad (RenderM output style) where
#if __GLASGOW_HASKELL__ < 710
    return = pure
#endif
    RenderM r >>= f = RenderM (\s ->
        let (x1, w1, s1) = r s
            RenderM r1 = f x1
            (x2, w2, s2) = r1 s1
            !w12 = w1 <> w2
        in (x2, w12, s2))

-- | Add a new style to the style stack.
pushStyle :: Monoid output => style -> RenderM output style ()
pushStyle style = RenderM (\styles -> ((), mempty, style : styles))

-- | Get the topmost style.
--
-- If the stack is empty, this raises an 'error'.
unsafePopStyle :: Monoid output => RenderM output style style
unsafePopStyle = RenderM (\case
    x:xs -> (x, mempty, xs)
    [] -> error "Popped an empty style stack! Please report this as a bug.")

-- | View the topmost style, but do not modify the stack.
--
-- If the stack is empty, this raises an 'error'.
unsafePeekStyle :: Monoid output => RenderM output style style
unsafePeekStyle = RenderM (\styles -> case styles of
    x:_ -> (x, mempty, styles)
    [] -> error "Peeked an empty style stack! Please report this as a bug.")

-- | Append a value to the output end.
writeOutput :: output -> RenderM output style ()
writeOutput w = RenderM (\styles -> ((), w, styles))

-- | Run the renderer and retrive the writing end
execRenderM :: [styles] -> RenderM write styles a -> (write, [styles])
execRenderM styles (RenderM r) = let (_, w, s) = r styles in (w, s)
