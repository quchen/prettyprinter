{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

#include "version-compatibility-macros.h"

module Main (main) where



import           Control.Exception     (evaluate)
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Text             as T
import           Data.Text.PgpWordlist
import           Data.Word
import           System.Timeout        (timeout)

import           Prettyprinter
import           Prettyprinter.Internal.Debug
import           Prettyprinter.Render.Text
import           Prettyprinter.Render.Util.StackMachine (renderSimplyDecorated)

import Test.QuickCheck.Instances.Text ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import StripTrailingSpace

#if !(APPLICATIVE_MONAD)
import Control.Applicative
#endif
#if !(MONOID_IN_PRELUDE)
import Data.Monoid (mconcat)
#endif



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "Fusion"
        [ testProperty "Shallow fusion does not change rendering"
                       (fusionDoesNotChangeRendering Shallow)
        , testProperty "Deep fusion does not change rendering"
                       (fusionDoesNotChangeRendering Deep)
        ]
    , testStripTrailingSpace
    , testGroup "Performance tests"
        [ testCase "Grouping performance"
                   groupingPerformance
        , testCase "fillSep performance"
                   fillSepPerformance
        ]
    , testGroup "Regression tests"
        [ testCase "layoutSmart: softline behaves like a newline (#49)"
                   regressionLayoutSmartSoftline
        , testCase "alterAnnotationsS causes panic when removing annotations (#50)"
                   regressionAlterAnnotationsS
        , testCase "Bad fallback handling with align (#83)" badFallbackAlign
        , testGroup "removeTrailingWhitespace removes leading whitespace (#84)"
            [ testCase "Text node"
                       doNotRemoveLeadingWhitespaceText
            , testCase "Char node"
                       doNotRemoveLeadingWhitespaceChar
            , testCase "Text+Char nodes"
                       doNotRemoveLeadingWhitespaceTextChar
            ]
        , testGroup "removeTrailingWhitespace removes trailing line breaks (#86)"
            [ testCase "Keep lonely single trailing newline"
                       removeTrailingWhitespaceKeepLonelyTrailingNewline
            , testCase "Trailing newline with spaces"
                       removeTrailingNewlineWithSpaces
            , testCase "Keep single trailing newline"
                       removeTrailingWhitespaceKeepTrailingNewline
            , testCase "Reduce to single trailing newline"
                       removeTrailingWhitespaceInTrailingNewlines
            ]
        , testGroup "removeTrailingWhitespace restores indentation in the wrong spot (#93)"
            [ testCase "Don't restore indentation in the wrong spot"
                       removeTrailingWhitespaceDontRestoreIndentationInTheWrongSpot
            , testCase "Preserve leading indentation"
                       removeTrailingWhitespacePreserveIndentation
            ]
        , testGroup "Unbounded layout of hard linebreak within `group` fails (#91)"
            [ testCase "Line" regressionUnboundedGroupedLine
            , testCase "Line within align" regressionUnboundedGroupedLineWithinAlign
            ]
        , testCase "Indentation on otherwise empty lines results in trailing whitespace (#139)"
                   indentationShouldntCauseTrailingWhitespaceOnOtherwiseEmptyLines
        , testCase "Ribbon width should be computed with `floor` instead of `round` (#157)"
                   computeRibbonWidthWithFloor
        ]
    ]

fusionDoesNotChangeRendering :: FusionDepth -> Property
fusionDoesNotChangeRendering depth
  = forAllShow (arbitrary :: Gen (Doc Int)) (show . diag) (\doc ->
    forAll arbitrary (\layouter ->
        let tShow = T.pack . show
            render = renderSimplyDecorated id tShow tShow . layout layouter
            rendered = render doc
            renderedFused = render (fuse depth doc)
        in counterexample (mkCounterexample rendered renderedFused)
                          (render doc == render (fuse depth doc)) ))
  where
    mkCounterexample rendered renderedFused
      = (T.unpack . renderStrict . layoutPretty defaultLayoutOptions . vsep)
            [ "Unfused and fused documents render differently!"
            , "Unfused:"
            , indent 4 (pretty rendered)
            , "Fused:"
            , indent 4 (pretty renderedFused) ]

instance Arbitrary ann => Arbitrary (Doc ann) where
    arbitrary = document
    shrink = genericShrink -- Possibly not a good idea, may break invariants

document :: Arbitrary ann => Gen (Doc ann)
document = (dampen . frequency)
    [ (20, content)
    , (1, newlines)
    , (1, nestingAndAlignment)
    , (1, grouping)
    , (20, concatenationOfTwo)
    , (5, concatenationOfMany)
    , (1, enclosingOfOne)
    , (1, enclosingOfMany)
    , (1, annotated) ]

annotated :: Arbitrary ann => Gen (Doc ann)
annotated = annotate <$> arbitrary <*> document

content :: Gen (Doc ann)
content = frequency
    [ (1, pure emptyDoc)
    , (10, do word <- choose (minBound, maxBound :: Word8)
              let pgp8Word = toText (BSL.singleton word)
              pure (pretty pgp8Word) )
    , (1, (fmap pretty . elements . mconcat)
              [ ['a'..'z']
              , ['A'..'Z']
              , ['0'..'9']
              , "…_[]^!<>=&@:-()?*}{/\\#$|~`+%\"';" ] )
    ]

newlines :: Gen (Doc ann)
newlines = frequency
    [ (1, pure line)
    , (1, pure line')
    , (1, pure softline)
    , (1, pure softline')
    , (1, pure hardline) ]

nestingAndAlignment :: Arbitrary ann => Gen (Doc ann)
nestingAndAlignment = frequency
    [ (1, nest   <$> arbitrary <*> concatenationOfMany)
    , (1, group  <$> document)
    , (1, hang   <$> arbitrary <*> concatenationOfMany)
    , (1, indent <$> arbitrary <*> concatenationOfMany) ]

grouping :: Arbitrary ann => Gen (Doc ann)
grouping = frequency
    [ (1, align  <$> document)
    , (1, flatAlt <$> document <*> document) ]

concatenationOfTwo :: Arbitrary ann => Gen (Doc ann)
concatenationOfTwo = frequency
    [ (1, (<>) <$> document <*> document)
    , (1, (<+>) <$> document <*> document) ]

concatenationOfMany :: Arbitrary ann => Gen (Doc ann)
concatenationOfMany = frequency
    [ (1, hsep    <$> listOf document)
    , (1, vsep    <$> listOf document)
    , (1, fillSep <$> listOf document)
    , (1, sep     <$> listOf document)
    , (1, hcat    <$> listOf document)
    , (1, vcat    <$> listOf document)
    , (1, fillCat <$> listOf document)
    , (1, cat     <$> listOf document) ]

enclosingOfOne :: Arbitrary ann => Gen (Doc ann)
enclosingOfOne = frequency
    [ (1, squotes  <$> document)
    , (1, dquotes  <$> document)
    , (1, parens   <$> document)
    , (1, angles   <$> document)
    , (1, brackets <$> document)
    , (1, braces   <$> document) ]

enclosingOfMany :: Arbitrary ann => Gen (Doc ann)
enclosingOfMany = frequency
    [ (1, encloseSep <$> document <*> document <*> pure ", " <*> listOf document)
    , (1, list       <$> listOf document)
    , (1, tupled     <$> listOf document) ]

-- A 'show'able type representing a layout algorithm.
data Layouter ann
    = LayoutPretty LayoutOptions
    | LayoutSmart LayoutOptions
    | LayoutCompact
    -- LayoutWadlerLeijen (FittingPredicate ann) LayoutOptions
    deriving Show

instance Arbitrary (Layouter ann) where
    arbitrary = oneof
        [ LayoutPretty <$> arbitrary
        , LayoutSmart <$> arbitrary
        , pure LayoutCompact
        -- This produces inconsistent layouts that break the fusionDoesNotChangeRendering test
        -- , LayoutWadlerLeijen <$> arbitrary <*> arbitrary
        ]

{-
instance Show (FittingPredicate ann) where
    show _ = "<fitting predicate>"

instance Arbitrary (FittingPredicate ann) where
    arbitrary = FittingPredicate <$> arbitrary
-}

layout :: Layouter ann -> Doc ann -> SimpleDocStream ann
layout (LayoutPretty opts) = layoutPretty opts
layout (LayoutSmart opts) = layoutSmart opts
layout LayoutCompact = layoutCompact
-- layout (LayoutWadlerLeijen fp opts) = layoutWadlerLeijen fp opts

instance Arbitrary LayoutOptions where
    arbitrary = LayoutOptions <$> oneof
        [ AvailablePerLine <$> arbitrary <*> arbitrary
        , pure Unbounded
        ]

instance CoArbitrary (SimpleDocStream ann) where
    coarbitrary s0 = case s0 of
        SFail         -> variant' 0
        SEmpty        -> variant' 1
        SChar _c s    -> variant' 2 . coarbitrary s
        SText l _t s  -> variant' 3 . coarbitrary (l, s)
        SLine i s     -> variant' 4 . coarbitrary (i, s)
        SAnnPush _a s -> variant' 5 . coarbitrary s
        SAnnPop s     -> variant' 6 . coarbitrary s

instance CoArbitrary PageWidth where
    coarbitrary (AvailablePerLine a b) = variant' 0 . coarbitrary (a, b)
    coarbitrary Unbounded              = variant' 1

-- | Silences type defaulting warnings for 'variant'
variant' :: Int -> Gen a -> Gen a
variant' = variant

-- QuickCheck 2.8 does not have 'scale' yet, so for compatibility with older
-- releases we hand-code it here
dampen :: Gen a -> Gen a
dampen gen = sized (\n -> resize ((n*2) `quot` 3) gen)

docPerformanceTest :: Doc ann -> Assertion
docPerformanceTest doc
  = timeout 10000000 (forceDoc doc) >>= \doc' -> case doc' of
    Nothing -> assertFailure "Timeout!"
    Just _success -> pure ()
  where
    forceDoc :: Doc ann -> IO ()
    forceDoc = evaluate . foldr seq () . show

-- Deeply nested group/flatten calls can result in exponential performance.
--
-- See https://github.com/quchen/prettyprinter/issues/22
groupingPerformance :: Assertion
groupingPerformance = docPerformanceTest (pathological 1000)
  where
    pathological :: Int -> Doc ann
    pathological n = iterate (\x ->  hsep [x, sep []] ) "foobar" !! n

-- This test case was written because the `pretty` package had an issue with
-- this specific example.
--
-- See https://github.com/haskell/pretty/issues/32
fillSepPerformance :: Assertion
fillSepPerformance = docPerformanceTest (pathological 1000)
  where
    pathological :: Int -> Doc ann
    pathological n = iterate (\x -> fillSep ["a", x <+> "b"] ) "foobar" !! n

regressionLayoutSmartSoftline :: Assertion
regressionLayoutSmartSoftline
  = let doc = "a" <> softline <> "b"
        layouted :: SimpleDocStream ()
        layouted = layoutSmart (defaultLayoutOptions { layoutPageWidth = Unbounded }) doc
    in assertEqual "softline should be rendered as space page width is unbounded"
                   (SChar 'a' (SChar ' ' (SChar 'b' SEmpty)))
                   layouted

-- Removing annotations with alterAnnotationsS used to remove pushes, but not
-- pops, leading to imbalanced SimpleDocStreams.
regressionAlterAnnotationsS :: Assertion
regressionAlterAnnotationsS
  = let sdoc, sdoc' :: SimpleDocStream Int
        sdoc = layoutSmart defaultLayoutOptions (annotate 1 (annotate 2 (annotate 3 "a")))
        sdoc' = alterAnnotationsS (\ann -> case ann of 2 -> Just 2; _ -> Nothing) sdoc
    in assertEqual "" (SAnnPush 2 (SChar 'a' (SAnnPop SEmpty))) sdoc'

doNotRemoveLeadingWhitespaceText :: Assertion
doNotRemoveLeadingWhitespaceText
  = let sdoc :: SimpleDocStream ()
        sdoc = SLine 0 (SText 2 "  " (SChar 'x' SEmpty))
        sdoc' = SLine 2 (SChar 'x' SEmpty)
    in assertEqual "" sdoc' (removeTrailingWhitespace sdoc)

doNotRemoveLeadingWhitespaceChar :: Assertion
doNotRemoveLeadingWhitespaceChar
  = let sdoc :: SimpleDocStream ()
        sdoc = SLine 0 (SChar ' ' (SChar 'x' SEmpty))
        sdoc' = SLine 1 (SChar 'x' SEmpty)
    in assertEqual "" sdoc' (removeTrailingWhitespace sdoc)

doNotRemoveLeadingWhitespaceTextChar :: Assertion
doNotRemoveLeadingWhitespaceTextChar
  = let sdoc :: SimpleDocStream ()
        sdoc = SLine 0 (SChar ' ' (SText 2 "  " (SChar 'x' SEmpty)))
        sdoc' = SLine 3 (SChar 'x' SEmpty)
    in assertEqual "" sdoc' (removeTrailingWhitespace sdoc)

removeTrailingWhitespaceKeepTrailingNewline :: Assertion
removeTrailingWhitespaceKeepTrailingNewline
  = let sdoc :: SimpleDocStream ()
        sdoc = SLine 0 SEmpty
    in assertEqual "" sdoc (removeTrailingWhitespace sdoc)

removeTrailingNewlineWithSpaces :: Assertion
removeTrailingNewlineWithSpaces
  = let sdoc :: SimpleDocStream ()
        sdoc = SChar 'x' (SLine 2 (SText 2 "  " SEmpty))
        sdoc' = SChar 'x' (SLine 0 SEmpty)
    in assertEqual "" sdoc' (removeTrailingWhitespace sdoc)

removeTrailingWhitespaceKeepLonelyTrailingNewline :: Assertion
removeTrailingWhitespaceKeepLonelyTrailingNewline
  = let sdoc :: SimpleDocStream ()
        sdoc = SChar 'x' (SLine 0 SEmpty)
    in assertEqual "" sdoc (removeTrailingWhitespace sdoc)

removeTrailingWhitespaceInTrailingNewlines :: Assertion
removeTrailingWhitespaceInTrailingNewlines
  = let sdoc :: SimpleDocStream ()
        sdoc = SChar 'x' (SLine 2 (SLine 2 SEmpty))
        sdoc' = SChar 'x' (SLine 0 (SLine 0 SEmpty))
    in assertEqual "" sdoc' (removeTrailingWhitespace sdoc)

badFallbackAlign :: Assertion
badFallbackAlign
  = let x = group (flatAlt "Default" "Fallback")
        doc = "/" <> align (cat [x, x, "Too wide!!!!!"])
        actual = renderStrict (layoutSmart (LayoutOptions (AvailablePerLine 12 1)) doc)
        expected = "/Fallback\n Fallback\n Too wide!!!!!"
    in assertEqual "" expected actual

removeTrailingWhitespaceDontRestoreIndentationInTheWrongSpot :: Assertion
removeTrailingWhitespaceDontRestoreIndentationInTheWrongSpot
  = let sdoc :: SimpleDocStream ()
        sdoc = SLine 2 (SLine 0 (SChar 'x' SEmpty))
        sdoc' = SLine 0 (SLine 0 (SChar 'x' SEmpty))
    in assertEqual "" sdoc' (removeTrailingWhitespace sdoc)

removeTrailingWhitespacePreserveIndentation :: Assertion
removeTrailingWhitespacePreserveIndentation
  = let sdoc :: SimpleDocStream ()
        sdoc = SLine 2 (SChar 'x' SEmpty)
    in assertEqual "" sdoc (removeTrailingWhitespace sdoc)

regressionUnboundedGroupedLine :: Assertion
regressionUnboundedGroupedLine
  = let sdoc :: SimpleDocStream ()
        sdoc = layoutPretty (LayoutOptions Unbounded) (group hardline)
    in assertEqual "" (SLine 0 SEmpty) sdoc

regressionUnboundedGroupedLineWithinAlign :: Assertion
regressionUnboundedGroupedLineWithinAlign
  = let doc :: Doc ()
        doc = group (align ("x" <> hardline <> "y"))
        sdoc = layoutPretty (LayoutOptions Unbounded) doc
        expected = SChar 'x' (SLine 0 (SChar 'y' SEmpty))
    in assertEqual "" expected sdoc

indentationShouldntCauseTrailingWhitespaceOnOtherwiseEmptyLines :: Assertion
indentationShouldntCauseTrailingWhitespaceOnOtherwiseEmptyLines
  = let doc :: Doc ()
        doc = indent 1 ("x" <> hardline <> hardline <> "y" <> hardline)
        sdoc = layoutPretty (LayoutOptions Unbounded) doc
        expected = SChar ' ' (SChar 'x' (SLine 0 (SLine 1 (SChar 'y' (SLine 0 SEmpty)))))
    in assertEqual "" expected sdoc

computeRibbonWidthWithFloor :: Assertion
computeRibbonWidthWithFloor
  = let doc :: Doc ()
        doc = "a" <> softline' <> "b"
        sdoc = layoutPretty (LayoutOptions (AvailablePerLine 3 0.5)) doc
        expected = SChar 'a' (SLine 0 (SChar 'b' SEmpty))
    in assertEqual "" expected sdoc
