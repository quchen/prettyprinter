{-# LANGUAGE CPP #-}

module Text.PrettyPrint.ANSI.Leijen {-# DEPRECATED "Compatibility module for users of ansi-wl-pprint - use \"Prettyprinter\" instead" #-} (

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
    integer, float, double, rational, Pretty(..), SimpleDoc, renderPretty,
    renderCompact, displayS, displayIO, bool, column, columns, nesting, width

) where

#if MIN_VERSION_base(4,8,0)
import Prelude hiding ((<$>))
#else
import Prelude
#endif

import qualified Data.Text.Lazy as TL
import           System.IO

import           Prettyprinter                 (Pretty (..))
import qualified Prettyprinter                 as New
import qualified Prettyprinter.Render.Terminal as NewT

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif

type Doc = New.Doc NewT.AnsiStyle
type SimpleDoc = New.SimpleDocStream NewT.AnsiStyle



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
        { New.layoutPageWidth = New.AvailablePerLine pageWidth (realToFrac ribbonFraction) }

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
black         = New.annotate (NewT.color       NewT.Black)
red           = New.annotate (NewT.color       NewT.Red)
green         = New.annotate (NewT.color       NewT.Green)
yellow        = New.annotate (NewT.color       NewT.Yellow)
blue          = New.annotate (NewT.color       NewT.Blue)
magenta       = New.annotate (NewT.color       NewT.Magenta)
cyan          = New.annotate (NewT.color       NewT.Cyan)
white         = New.annotate (NewT.color       NewT.White)
dullblack     = New.annotate (NewT.colorDull   NewT.Black)
dullred       = New.annotate (NewT.colorDull   NewT.Red)
dullgreen     = New.annotate (NewT.colorDull   NewT.Green)
dullyellow    = New.annotate (NewT.colorDull   NewT.Yellow)
dullblue      = New.annotate (NewT.colorDull   NewT.Blue)
dullmagenta   = New.annotate (NewT.colorDull   NewT.Magenta)
dullcyan      = New.annotate (NewT.colorDull   NewT.Cyan)
dullwhite     = New.annotate (NewT.colorDull   NewT.White)
onblack       = New.annotate (NewT.bgColor     NewT.Black)
onred         = New.annotate (NewT.bgColor     NewT.Red)
ongreen       = New.annotate (NewT.bgColor     NewT.Green)
onyellow      = New.annotate (NewT.bgColor     NewT.Yellow)
onblue        = New.annotate (NewT.bgColor     NewT.Blue)
onmagenta     = New.annotate (NewT.bgColor     NewT.Magenta)
oncyan        = New.annotate (NewT.bgColor     NewT.Cyan)
onwhite       = New.annotate (NewT.bgColor     NewT.White)
ondullblack   = New.annotate (NewT.bgColorDull NewT.Black)
ondullred     = New.annotate (NewT.bgColorDull NewT.Red)
ondullgreen   = New.annotate (NewT.bgColorDull NewT.Green)
ondullyellow  = New.annotate (NewT.bgColorDull NewT.Yellow)
ondullblue    = New.annotate (NewT.bgColorDull NewT.Blue)
ondullmagenta = New.annotate (NewT.bgColorDull NewT.Magenta)
ondullcyan    = New.annotate (NewT.bgColorDull NewT.Cyan)
ondullwhite   = New.annotate (NewT.bgColorDull NewT.White)
bold = New.annotate NewT.bold
debold = id
{-# WARNING debold "Debold does not do anything" #-}
underline = New.annotate NewT.underlined
deunderline = id
{-# WARNING deunderline "Debold does not do anything" #-}

plain :: Doc -> Doc
plain = New.unAnnotate

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
        { New.layoutPageWidth = New.AvailablePerLine pageWidth (realToFrac ribbonFraction) }


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
    toMaybeInt (New.AvailablePerLine cpl _) = Just cpl
    toMaybeInt New.Unbounded = Nothing

nesting :: (Int -> Doc) -> Doc
nesting = New.nesting

width :: Doc -> (Int -> Doc) -> Doc
width = New.width
