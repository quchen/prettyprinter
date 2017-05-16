module Text.PrettyPrint.Annotated.Leijen {-# DEPRECATED "Compatibility module for users of annotated-wl-pprint - use Data.Text.Prettyprint.Doc instead" #-} (

    Doc, SimpleDoc(..), SpanList, putDoc, hPutDoc, empty, char, text, (<>),
    nest, line, linebreak, group, softline, softbreak, align, hang, indent,
    encloseSep, list, tupled, semiBraces, (<+>), (<$>), (</>), (<$$>), (<//>),
    hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate, fill,
    fillBreak, enclose, squotes, dquotes, parens, angles, braces, brackets,
    lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket, squote,
    dquote, semi, colon, comma, space, dot, backslash, equals, pipe, string,
    int, integer, float, double, rational, bool, annotate, noAnnotate,
    renderPretty, renderCompact, displayDecorated, displayDecoratedA, display,
    displayS, displayIO, displaySpans, column, nesting, width

) where



import Prelude hiding ((<$>))

import           Control.Applicative (liftA2)
import           Data.Semigroup
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           System.IO

import           Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.ShowS      as New
import qualified Data.Text.Prettyprint.Doc.Render.Text       as New
import           Data.Text.Prettyprint.Doc.Render.Util.Panic



putDoc :: Doc () -> IO ()
putDoc = New.putDoc

hPutDoc :: Handle -> Doc () -> IO ()
hPutDoc = New.hPutDoc

displayS :: SimpleDoc ann -> ShowS
displayS = New.renderShowS

renderPretty :: LayoutOptions -> Doc ann -> SimpleDoc ann
renderPretty = layoutPretty

renderCompact :: Doc ann -> SimpleDoc ann
renderCompact = layoutCompact

display :: SimpleDoc ann -> String
display = flip displayS ""

noAnnotate :: Doc ann -> Doc xxx
noAnnotate = unAnnotate

linebreak :: Doc ann
linebreak = line'

softbreak :: Doc ann
softbreak = softline'

semiBraces :: [Doc ann] -> Doc ann
semiBraces = encloseSep lbrace rbrace semi

(<$>), (</>), (<$$>), (<//>) :: Doc ann -> Doc ann -> Doc ann
(<$>) = \x y -> x <> line <> y
(</>) = \x y -> x <> softline <> y
(<$$>) = \x y -> x <> line' <> y
(<//>) = \x y -> x <> softline' <> y

empty :: Doc ann
empty = emptyDoc

char :: Char -> Doc ann
char = pretty

bool :: Bool -> Doc ann
bool = pretty

text, string :: String -> Doc ann
text = pretty
string = pretty

int :: Int -> Doc ann
int = pretty

integer :: Integer -> Doc ann
integer = pretty

float :: Float -> Doc ann
float = pretty

double :: Double -> Doc ann
double = pretty

rational :: Rational -> Doc ann
rational = pretty . show

displayDecorated :: (a -> String -> String) -> SimpleDoc a -> String
displayDecorated decor sd = go id id [] sd ""
  where
    go s d []              SEmpty           = d . s
    go s d stk             (SChar c x)      = go (s . showChar c) d stk x
    go s d stk             (SText _ str x)  = go (s . showString (T.unpack str)) d stk x
    go s d stk             (SLine ind x)    = go (s . showString ('\n':replicate ind ' ')) d stk x
    go s d stk             (SAnnPush ann x) = go id (decor ann) ((s, d):stk) x
    go s d ((sf', d'):stk) (SAnnPop x)      = let formatted = d (s "")
                                              in go (sf' . showString formatted) d' stk x
    go _ _ [] (SAnnPop _) = error "stack underflow"
    go _ _ _ SEmpty       = error "stack not consumed by rendering"
    go _ _ _ SFail        = panicUncaughtFail

displayDecoratedA :: (Applicative f, Monoid b)
                  => (String -> f b) -> (a -> f b) -> (a -> f b)
                  -> SimpleDoc a -> f b
displayDecoratedA str start end sd = go [] sd
  where
    go []        SEmpty           = pure mempty
    go stk       (SChar c x)      = str [c] <++> go stk x
    go stk       (SText _ s x)    = str (T.unpack s) <++> go stk x
    go stk       (SLine ind x)    = str ('\n' : replicate ind ' ') <++> go stk x
    go stk       (SAnnPush ann x) = start ann <++> go (ann:stk) x
    go (ann:stk) (SAnnPop x)      = end ann <++> go stk x

    -- malformed documents
    go [] (SAnnPop _) = error "stack underflow"
    go _ SEmpty       = error "stack not consumed by rendering"
    go _ SFail        = panicUncaughtFail

    (<++>) = liftA2 mappend

type SpanList a = [(Int, Int, a)]

displaySpans :: SimpleDoc a -> (String, SpanList a)
displaySpans sd = go 0 [] sd
  where
    go :: Int -> [(Int, a)] -> SimpleDoc a -> (String, SpanList a)
    go _ []                 SEmpty           = ("", [])
    go i stk                (SChar c x)      = let (str, spans) = go (i+1) stk x
                                               in (c:str, spans)
    go i stk                (SText l s x)    = mapFst (T.unpack s ++) (go (i + l) stk x)
    go i stk                (SLine ind x)    = mapFst (('\n':replicate ind ' ') ++) (go (1+i+ind) stk x)
    go i stk                (SAnnPush ann x) = go i ((i, ann):stk) x
    go i ((start, ann):stk) (SAnnPop x)      = mapSnd ((start, i-start, ann) :) (go i stk x)

    -- malformed documents
    go _ []  (SAnnPop _) = error "stack underflow"
    go _ _ SEmpty        = error "Stack not consumed by rendering"
    go _ _ SFail         = panicUncaughtFail

    mapFst :: (a -> b) -> (a, c) -> (b, c)
    mapFst f (x, y) = (f x, y)

    mapSnd :: (a -> b) -> (c, a) -> (c, b)
    mapSnd f (x, y) = (x, f y)

displayIO :: Handle -> SimpleDoc a -> IO ()
displayIO h simpleDoc = go simpleDoc
   where
     go SFail          = panicUncaughtFail
     go SEmpty         = pure ()
     go (SChar c x)    = hPutChar h c >> go x
     go (SText _ s x)  = T.hPutStr h s >> go x
     go (SLine i x)    = hPutStr h ('\n':replicate i ' ') >> go x
     go (SAnnPush _ x) = go x
     go (SAnnPop x)    = go x
