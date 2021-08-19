{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

#include "version-compatibility-macros.h"

module Main (main) where



import           Control.Monad
import           Control.Monad.State
import           Gauge.Main
import           Data.Text           (Text)
import qualified Data.Text           as T
import           System.Random

import           Prettyprinter
import           Prettyprinter.Render.Text
import qualified Text.PrettyPrint.ANSI.Leijen as WL

#if !(APPLICATIVE_MONAD)
import Control.Applicative
#endif



main :: IO ()
main = defaultMain
    [ benchOptimize
    , benchWLComparison
    ]

benchOptimize :: Benchmark
benchOptimize = env randomShortWords benchmark_
  where
    benchmark_ = \shortWords ->
        let doc = hsep (map pretty shortWords)
        in bgroup "Many small words"
            [ bench "Unoptimized"     (nf renderLazy (layoutPretty defaultLayoutOptions               doc))
            , bench "Shallowly fused" (nf renderLazy (layoutPretty defaultLayoutOptions (fuse Shallow doc)))
            , bench "Deeply fused"    (nf renderLazy (layoutPretty defaultLayoutOptions (fuse Deep    doc)))
            ]

    randomShortWords :: Applicative m => m [Text]
    randomShortWords = pure (evalState (randomShortWords' 100) (mkStdGen 0))

    randomShortWords' :: Int -> State StdGen [Text]
    randomShortWords' n = replicateM n randomShortWord

    randomShortWord :: State StdGen Text
    randomShortWord = do
        g <- get
        let (l, g') = randomR (0, 5) g
            (gNew, gFree) = split g'
            xs = take l (randoms gFree)
        put gNew
        pure (T.pack xs)

benchWLComparison :: Benchmark
benchWLComparison = bgroup "vs. other libs"
    [ bgroup "renderPretty"
        [ bench "this, unoptimized"     (nf (renderLazy . layoutPretty defaultLayoutOptions)               doc)
        , bench "this, shallowly fused" (nf (renderLazy . layoutPretty defaultLayoutOptions) (fuse Shallow doc))
        , bench "this, deeply fused"    (nf (renderLazy . layoutPretty defaultLayoutOptions) (fuse Deep    doc))
        , bench "ansi-wl-pprint"        (nf (\d -> WL.displayS (WL.renderPretty 0.4 80 d) "") wlDoc)
        ]
    , bgroup "renderSmart"
        [ bench "this, unoptimized"     (nf (renderLazy . layoutSmart defaultLayoutOptions)               doc)
        , bench "this, shallowly fused" (nf (renderLazy . layoutSmart defaultLayoutOptions) (fuse Shallow doc))
        , bench "this, deeply fused"    (nf (renderLazy . layoutSmart defaultLayoutOptions) (fuse Deep    doc))
        , bench "ansi-wl-pprint"        (nf (\d -> WL.displayS (WL.renderSmart 0.4 80 d) "") wlDoc)
        ]
    , bgroup "renderCompact"
        [ bench "this, unoptimized"     (nf (renderLazy . layoutCompact)               doc)
        , bench "this, shallowly fused" (nf (renderLazy . layoutCompact) (fuse Shallow doc))
        , bench "this, deeply fused"    (nf (renderLazy . layoutCompact) (fuse Deep    doc))
        , bench "ansi-wl-pprint"        (nf (\d -> WL.displayS (WL.renderCompact d) "") wlDoc)
        ]
    ]
  where
    doc :: Doc ann
    doc = let fun x = "fun" <> parens (softline <> x)
              funnn = chain 10 fun
          in funnn (sep (take 48 (cycle ["hello", "world"])))

    wlDoc :: WL.Doc
    wlDoc = let fun x = "fun" WL.<> WL.parens (WL.softline WL.<> x)
                funnn = chain 10 fun
            in funnn (WL.sep (take 48 (cycle ["hello", "world"])))

    chain n f = foldr (.) id (replicate n f)
