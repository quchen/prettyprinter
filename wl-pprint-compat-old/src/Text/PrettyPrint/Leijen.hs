module Text.PrettyPrint.Leijen {-# DEPRECATED "Compatibility module for users of wl-pprint - use Data.Text.Prettyprint.Doc instead" #-} (

    Doc, putDoc, hPutDoc, empty, char, text, (<>), nest, line, linebreak, group,
    softline, softbreak, align, hang, indent, encloseSep, list, tupled,
    semiBraces, (<+>), (<$>), (</>), (<$$>), (<//>), hsep, vsep, fillSep, sep,
    hcat, vcat, fillCat, cat, punctuate, fill, fillBreak, enclose, squotes,
    dquotes, parens, angles, braces, brackets, lparen, rparen, langle, rangle,
    lbrace, rbrace, lbracket, rbracket, squote, dquote, semi, colon, comma,
    space, dot, backslash, equals, string, int, integer, float, double,
    rational, Pretty(..), SimpleDoc(..), renderPretty, renderCompact, displayS,
    displayIO , bool , column, nesting, width

) where



import Prelude hiding ((<$>))

import           Data.Monoid
import qualified Data.Text.Lazy as TL
import           System.IO

import           Data.Text.Prettyprint.Doc
    (Doc, Pretty (..), SimpleDoc (..))
import qualified Data.Text.Prettyprint.Doc                 as New
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as NewT



putDoc :: Doc -> IO ()
putDoc = NewT.putDoc

hPutDoc :: Handle -> Doc -> IO ()
hPutDoc = NewT.hPutDoc

empty :: Doc
empty = New.emptyDoc

char :: Char -> Doc
char = New.pretty

text :: String -> Doc
text = New.pretty

nest :: Int -> Doc -> Doc
nest  = New.nest

line :: Doc
line = New.line

linebreak :: Doc
linebreak = New.flatAlt New.line mempty

group :: Doc -> Doc
group = New.group

softline :: Doc
softline = New.softline

softbreak :: Doc
softbreak = New.group linebreak

align :: Doc -> Doc
align = New.align

hang :: Int -> Doc -> Doc
hang = New.hang

indent :: Int -> Doc -> Doc
indent = New.indent

encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep = New.encloseSep

list :: [Doc] -> Doc
list = New.list

tupled :: [Doc] -> Doc
tupled = New.tupled

semiBraces :: [Doc] -> Doc
semiBraces = New.encloseSep New.lbrace New.rbrace New.semi

(<+>), (<$>), (</>), (<$$>), (<//>) :: Doc -> Doc -> Doc
(<+>) = (New.<+>)
(<$>) = \x y -> x <> New.line <> y
(</>) = \x y -> x <> softline <> y
(<$$>) = \x y -> x <> linebreak <> y
(<//>) = \x y -> x <> softbreak <> y

hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat :: [Doc] -> Doc
hsep = New.hsep
vsep = New.vsep
fillSep = New.fillSep
sep = New.sep
hcat = New.hcat
vcat = New.vcat
fillCat = New.fillCat
cat = New.cat

punctuate :: Doc -> [Doc] -> [Doc]
punctuate = New.punctuate

fill :: Int -> Doc -> Doc
fill = New.fill

fillBreak :: Int -> Doc -> Doc
fillBreak = New.fillBreak

enclose :: Doc -> Doc -> Doc -> Doc
enclose = New.enclose

squotes, dquotes, parens, angles, braces, brackets :: Doc -> Doc
squotes = New.squotes
dquotes = New.dquotes
parens = New.parens
angles = New.angles
braces = New.braces
brackets = New.brackets

lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket, squote,
    dquote, semi, colon, comma, space, dot, backslash, equals :: Doc
lparen = New.lparen
rparen = New.rparen
langle = New.langle
rangle = New.rangle
lbrace = New.lbrace
rbrace = New.rbrace
lbracket = New.lbracket
rbracket = New.rbracket
squote = New.squote
dquote = New.dquote
semi = New.semi
colon = New.colon
comma = New.comma
space = New.space
dot = New.dot
backslash = New.backslash
equals = New.equals

string :: String -> Doc
string = New.pretty

int :: Int -> Doc
int = New.pretty

integer :: Integer -> Doc
integer = New.pretty

float :: Float -> Doc
float = New.pretty

double :: Double -> Doc
double = New.pretty

rational :: Rational -> Doc
rational = New.pretty . show

renderPretty :: Float -> Int -> Doc -> SimpleDoc
renderPretty ribbonFraction pageWidth
    = New.layoutPretty New.LayoutOptions
        { New.layoutRibbonFraction = realToFrac ribbonFraction
        , New.layoutPageWidth = pageWidth }

renderCompact :: Doc -> SimpleDoc
renderCompact = New.layoutCompact

displayS :: SimpleDoc -> ShowS
displayS sdoc =
    let rendered = NewT.renderLazy sdoc
    in (TL.unpack rendered ++)

displayIO :: Handle -> SimpleDoc -> IO ()
displayIO = NewT.renderIO

bool :: Bool -> Doc
bool = New.pretty

column :: (Int -> Doc) -> Doc
column = New.column

nesting :: (Int -> Doc) -> Doc
nesting = New.nesting

width :: Doc -> (Int -> Doc) -> Doc
width = New.width
