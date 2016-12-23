{-# LANGUAGE CPP               #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Control.Monad
import           Criterion.Main
import           Data.Text      (Text)
import qualified Data.Text      as T
import           System.Random

import           Data.Text.PrettyPrint.Doc
import           Data.Text.PrettyPrint.Doc.Render.Text
import qualified Text.PrettyPrint.ANSI.Leijen          as WL

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif


main :: IO ()
main = defaultMain
    [ benchOptimize
    , benchWLComparison
    ]

benchOptimize :: Benchmark
benchOptimize = env randomShortWords benchmark
  where
    benchmark = \shortWords ->
        let doc = hsep (map pretty shortWords)
        in bgroup "Many small words"
            [ bench "Unoptimized"  (nf renderLazy (layoutPretty 0.4 80           doc))
            , bench "Fused"        (nf renderLazy (layoutPretty 0.4 80 (fuse     doc)))
            , bench "Deeply fused" (nf renderLazy (layoutPretty 0.4 80 (deepFuse doc)))
            ]

    randomShortWords :: IO [Text]
    randomShortWords = replicateM 1e2 randomWord

    randomWord :: IO Text
    randomWord = do
        g <- newStdGen
        let (l, g') = randomR (0, 5) g
            xs = take l (randoms g')
        pure (T.pack (take l xs))

benchWLComparison :: Benchmark
benchWLComparison = bgroup "vs. other libs"
    [ bgroup "renderPretty"
        [ bench "this, unoptimized" (nf (renderLazy . layoutPretty 0.4 80) doc)
        , bench "this, fused" (nf (renderLazy . layoutPretty 0.4 80) (fuse doc))
        , bench "this, deeply fused" (nf (renderLazy . layoutPretty 0.4 80) (deepFuse doc))
        , bench "ansi-wl-pprint" (nf (\d -> WL.displayS (WL.renderPretty 0.4 80 d) "") wlDoc)
        ]
    , bgroup "renderSmart"
        [ bench "this, unoptimized" (nf (renderLazy . layoutSmart 0.4 80) doc)
        , bench "this, fused" (nf (renderLazy . layoutSmart 0.4 80) (fuse doc))
        , bench "this, deeply fused" (nf (renderLazy . layoutSmart 0.4 80) (deepFuse doc))
        , bench "ansi-wl-pprint" (nf (\d -> WL.displayS (WL.renderSmart 0.4 80 d) "") wlDoc)
        ]
    , bgroup "renderCompact"
        [ bench "this, unoptimized" (nf (renderLazy . layoutCompact) doc)
        , bench "this, fused" (nf (renderLazy . layoutCompact) (fuse doc))
        , bench "this, deeply fused" (nf (renderLazy . layoutCompact) (deepFuse doc))
        , bench "ansi-wl-pprint" (nf (\d -> WL.displayS (WL.renderCompact d) "") wlDoc)
        ]
    ]
  where
    doc :: Doc
    doc = let fun x = "fun" <> parens (softline <> x)
              funnn = chain 10 fun
          in funnn (sep (take 48 (cycle ["hello", "world"])))

    wlDoc :: WL.Doc
    wlDoc = let fun x = "fun" <> WL.parens (WL.softline <> x)
                funnn = chain 10 fun
            in funnn (WL.sep (take 48 (cycle ["hello", "world"])))

    chain n f = foldr (.) id (replicate n f)
