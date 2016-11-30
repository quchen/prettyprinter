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




main :: IO ()
main = defaultMain
    [ benchOptimize
    , benchWLComparison
    ]

benchOptimize :: Benchmark
benchOptimize = env randomShortWords benchmark
  where
    benchmark = \shortWords ->
        bgroup "Many small words"
            [ let doc' = hsep (map text shortWords)
              in bench "Optimizer OFF"
                    (nf renderLazy (layoutPretty 0.4 80 doc'))
            , let doc' = optimize (hsep (map text shortWords))
              in bench "Optimizer ON"
                    (nf renderLazy (layoutPretty 0.4 80 doc'))
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
        , bench "this, optimized" (nf (renderLazy . layoutPretty 0.4 80) (optimize doc))
        , bench "ansi-wl-pprint" (nf (\d -> (WL.displayS (WL.renderPretty 0.4 80 d) "")) wlDoc)
        ]
    , bgroup "renderSmart"
        [ bench "this, unoptimized" (nf (renderLazy . layoutSmart 0.4 80) doc)
        , bench "this, optimized" (nf (renderLazy . layoutSmart 0.4 80) (optimize doc))
        , bench "ansi-wl-pprint" (nf (\d -> (WL.displayS (WL.renderSmart 0.4 80 d) "")) wlDoc)
        ]
    , bgroup "renderCompact"
        [ bench "this, unoptimized" (nf (renderLazy . layoutCompact) doc)
        , bench "this, optimized" (nf (renderLazy . layoutCompact) (optimize doc))
        , bench "ansi-wl-pprint" (nf (\d -> (WL.displayS (WL.renderCompact d) "")) wlDoc)
        ]
    ]
  where
    doc :: Doc
    doc = let fun x = "fun" <> parens (softline <> x)
              funnn = fun.fun.fun.fun.fun.fun.fun.fun.fun
          in funnn (sep (take 48 (cycle ["hello", "world"])))

    wlDoc :: WL.Doc
    wlDoc = let fun x = "fun" <> WL.parens (WL.softline <> x)
                funnn = fun.fun.fun.fun.fun.fun.fun.fun.fun
            in funnn (WL.sep (take 48 (cycle ["hello", "world"])))
