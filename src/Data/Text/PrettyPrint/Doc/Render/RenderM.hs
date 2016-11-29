{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A strict State/Writer monad used for rendering 'SimpleDoc's.
module Data.Text.PrettyPrint.Doc.Render.RenderM (
    RenderM,
    execRenderM,

    pushStyle,
    unsafePopStyle,
    unsafePeekStyle,
    writeResult,
) where



import Data.Monoid




-- | WriterT StateT, but with a strict Writer value.
newtype RenderM write style a = RenderM ([style] -> (a, write, [style]))

instance Functor (RenderM write style) where
    fmap f (RenderM r) = RenderM (\s ->
        let (x1, w1, s1) = r s
        in (f x1, w1, s1))

instance Monoid write => Applicative (RenderM write style) where
    pure x = RenderM (\s -> (x, mempty, s))
    RenderM f <*> RenderM x = RenderM (\s ->
        let (f1, w1, s1) = f s
            (x2, w2, s2) = x s1
            !w12 = w1 <> w2
        in (f1 x2, w12, s2))

instance Monoid write => Monad (RenderM write style) where
    RenderM r >>= f = RenderM (\s ->
        let (x1, w1, s1) = r s
            RenderM r1 = f x1
            (x2, w2, s2) = r1 s1
            !w12 = w1 <> w2
        in (x2, w12, s2))

pushStyle :: Monoid write => style -> RenderM write style ()
pushStyle style = RenderM (\styles -> ((), mempty, style : styles))

-- | Get the topmost style. 'error's if there is none.
unsafePopStyle :: Monoid write => RenderM write style style
unsafePopStyle = RenderM (\case
    x:xs -> (x, mempty, xs)
    [] -> error "Popped an empty style stack! Please report this as a bug.")

unsafePeekStyle :: Monoid write => RenderM write style style
unsafePeekStyle = RenderM (\styles -> case styles of
    x:_ -> (x, mempty, styles)
    [] -> error "Peeked an empty style stack! Please report this as a bug.")

writeResult :: write -> RenderM write style ()
writeResult w = RenderM (\styles -> ((), w, styles))

-- | Run the renderer and retrive the writing end
execRenderM :: [styles] -> RenderM write styles a -> (write, [styles])
execRenderM styles (RenderM r) = let (_, w, s) = r styles in (w, s)
