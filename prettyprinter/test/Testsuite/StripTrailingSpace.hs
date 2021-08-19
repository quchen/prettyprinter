{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

#include "version-compatibility-macros.h"

module StripTrailingSpace (testStripTrailingSpace) where



import           Data.Text (Text)
import qualified Data.Text as T

import Prettyprinter
import Prettyprinter.Render.Util.StackMachine

import Test.Tasty
import Test.Tasty.HUnit

#if !(APPLICATIVE_MONAD)
import Control.Applicative
#endif



box :: Text -> Text
box singleLine = unlines'
    [ "┌─" <> T.replicate (T.length singleLine) "─" <> "─┐"
    , "│ " <> singleLine <> " │"
    , "└─" <> T.replicate (T.length singleLine) "─" <> "─┘"
    ]

bbox :: Text -> Text
bbox singleLine = unlines'
    [ "╔═" <> T.replicate (T.length singleLine) "═" <> "═╗"
    , "║ " <> singleLine <> " ║"
    , "╚═" <> T.replicate (T.length singleLine) "═" <> "═╝"
    ]

testStripTrailingSpace :: TestTree
testStripTrailingSpace = testGroup "Stripping trailing space"
    [ testCase "No trailing space"
               (testStripping "No trailing space at all")
    , testCase "Single trailing space character"
               (testStripping ("Single trailing character" <> " "))
    , testCase "Space character inside"
               (testStripping ("Space character" <> " " <> "inside"))
    , testCase "Obvious trailing spaces"
               (testStripping ("Obvious trailing space" <> "   "))
    , testCase "Multiple spaces inside"
               (testStripping ("Multiple spaces" <> "    " <> "inside"))
    , testCase "Whitespace inside text"
               (testStripping "Whitespace inside text   ")
    , testCase "Indented blank line"
               (testStripping (nest 4 (vcat ["Indented blank line", "", "<end>"])))
    , testCase "Multiple indented blank lines"
               (testStripping (nest 4 (vcat ["Indented blank lines", "", "", "", "<end>"])))
    , testCase "Annotation"
               (testStripping (annotate () "Annotation with trailing space   "))
    , testCase "Document with annotation"
               (testStripping ("Here comes an" <> annotate () "annotation   " <> "and some trailing space again  " <> "  "))
    , testCase "Nested annotations"
               (testStripping ("A " <> annotate () ("nested   " <> annotate () "annotation ") <> "and some trailing space again  " <> "  "))
    , testCase "Stress test"
               (testStripping (nest 4 (vcat ["Stress test", "", "" <> annotate () "hello ", "", "world " <> "   ", annotate () "", "", "end"])))
    ]

testStripping :: Doc ann -> Assertion
testStripping doc = case hasTrailingWhitespace (render removeTrailingWhitespace doc) of
    False -> pure ()
    True  -> (assertFailure . T.unpack . T.unlines)
        [ bbox "Input is not stripped correctly!"
        , ""
        , box "Rendered/stripped:"
        , (revealSpaces . render removeTrailingWhitespace) doc
        , ""
        , box "Rendered/unstripped:"
        , (revealSpaces . render id) doc
        , ""
        , box "Rendered/unstripped, later stripped via Text API:"
        , (revealSpaces . removeTrailingSpaceText . render id) doc ]
  where

    render :: (SimpleDocStream ann -> SimpleDocStream ann) -> Doc ann -> Text
    render f = renderSimplyDecorated id (const "<ann>") (const "</ann>") . f . layoutPretty defaultLayoutOptions

    removeTrailingSpaceText :: Text -> Text
    removeTrailingSpaceText = unlines' . map T.stripEnd . T.lines

    hasTrailingWhitespace :: Text -> Bool
    hasTrailingWhitespace x = removeTrailingSpaceText x /= x

    revealSpaces :: Text -> Text
    revealSpaces = T.map (\x -> if x == ' ' then '␣' else x)

-- Text.unlines appends a trailing whitespace, so T.unlines . T.lines /= id
unlines' :: [Text] -> Text
unlines' = T.intercalate (T.singleton '\n')
