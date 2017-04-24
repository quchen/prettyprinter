module Text.PrettyPrint.ANSI.Leijen {-# DEPRECATED "Compatibility module for users of ansi-wl-pprint - use Data.Text.Prettyprint.Doc instead" #-} (

    Doc, putDoc, hPutDoc, empty, char, text, (<>), nest, line, linebreak, group,
    softline, softbreak, hardline, flatAlt, renderSmart, align, hang, indent,
    encloseSep, list, tupled, semiBraces, (<+>), (<$>), (</>), (<$$>), (<//>),
    hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate, fill,
    fillBreak, enclose, squotes, dquotes, parens, angles, braces, brackets,
    lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket, squote,
    dquote, semi, colon, comma, space, dot, backslash, equals, black, red,
    green, yellow, blue, magenta, cyan, white, dullblack, dullred, dullgreen,
    dullyellow, dullblue, dullmagenta, dullcyan, dullwhite, onblack, onred,
    ongreen, onyellow, onblue, onmagenta, oncyan, onwhite, ondullblack,
    ondullred, ondullgreen, ondullyellow, ondullblue, ondullmagenta, ondullcyan,
    ondullwhite, bold, debold, underline, deunderline, plain, string, int,
    integer, float, double, rational, Pretty(..), SimpleDoc(..), renderPretty,
    renderCompact, displayS, displayIO, bool, column, columns, nesting, width

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

hardline :: Doc
hardline = New.hardline

flatAlt :: Doc -> Doc -> Doc
flatAlt = New.flatAlt

renderSmart :: Float -> Int -> Doc -> SimpleDoc
renderSmart ribbonFraction pageWidth
    = New.layoutSmart New.LayoutOptions
        { New.layoutRibbonFraction = realToFrac ribbonFraction
        , New.layoutPageWidth = New.CharsPerLine pageWidth }

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

black, red, green, yellow, blue, magenta, cyan, white, dullblack, dullred,
    dullgreen, dullyellow, dullblue, dullmagenta, dullcyan, dullwhite, onblack,
    onred, ongreen, onyellow, onblue, onmagenta, oncyan, onwhite, ondullblack,
    ondullred, ondullgreen, ondullyellow, ondullblue, ondullmagenta, ondullcyan,
    ondullwhite, bold, debold, underline, deunderline :: Doc -> Doc
black         = New.color       New.SBlack
red           = New.color       New.SRed
green         = New.color       New.SGreen
yellow        = New.color       New.SYellow
blue          = New.color       New.SBlue
magenta       = New.color       New.SMagenta
cyan          = New.color       New.SCyan
white         = New.color       New.SWhite
dullblack     = New.colorDull   New.SBlack
dullred       = New.colorDull   New.SRed
dullgreen     = New.colorDull   New.SGreen
dullyellow    = New.colorDull   New.SYellow
dullblue      = New.colorDull   New.SBlue
dullmagenta   = New.colorDull   New.SMagenta
dullcyan      = New.colorDull   New.SCyan
dullwhite     = New.colorDull   New.SWhite
onblack       = New.bgColor     New.SBlack
onred         = New.bgColor     New.SRed
ongreen       = New.bgColor     New.SGreen
onyellow      = New.bgColor     New.SYellow
onblue        = New.bgColor     New.SBlue
onmagenta     = New.bgColor     New.SMagenta
oncyan        = New.bgColor     New.SCyan
onwhite       = New.bgColor     New.SWhite
ondullblack   = New.bgColorDull New.SBlack
ondullred     = New.bgColorDull New.SRed
ondullgreen   = New.bgColorDull New.SGreen
ondullyellow  = New.bgColorDull New.SYellow
ondullblue    = New.bgColorDull New.SBlue
ondullmagenta = New.bgColorDull New.SMagenta
ondullcyan    = New.bgColorDull New.SCyan
ondullwhite   = New.bgColorDull New.SWhite
bold = New.bold
debold = id
{-# WARNING debold "Debold does not do anything" #-}
underline = New.underline
deunderline = id
{-# WARNING deunderline "Debold does not do anything" #-}

plain :: Doc -> Doc
plain = New.plain

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
    = New.layoutSmart New.LayoutOptions
        { New.layoutRibbonFraction = realToFrac ribbonFraction
        , New.layoutPageWidth = New.CharsPerLine pageWidth }

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

columns :: (Maybe Int -> Doc) -> Doc
columns f = New.pageWidth (f . toMaybeInt)
  where
    toMaybeInt :: New.PageWidth -> Maybe Int
    toMaybeInt (New.CharsPerLine cpl) = Just cpl
    toMaybeInt New.Unbounded = Nothing

nesting :: (Int -> Doc) -> Doc
nesting = New.nesting

width :: Doc -> (Int -> Doc) -> Doc
width = New.width
