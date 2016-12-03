{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Data.Text.PrettyPrint.Doc
-- Copyright   :  Daan Leijen (c) 2000, http://www.cs.uu.nl/~daan
--                Max Bolingbroke (c) 2008, http://blog.omega-prime.co.uk
--                David Luposchainsky (c) 2016, http://github.com/quchen
-- License     :  BSD-style (see the file LICENSE.md)
-- Maintainer  :  David Luposchainsky <dluposchainsky (λ) google>
-- Stability   :  experimental
-- Portability :  portable
--
-- = Overview
--
-- This module defines a prettyprinter to format text in a flexible and
-- convenient way. The idea is to combine a 'Doc'ument out of many small
-- components, then using a layouter to convert it to an easily renderable
-- 'SimpleDoc', which can then be rendered to a variety of formats, for example
-- plain 'Text'.
--
-- The documentation consists of several parts:
--
--   1. Just below is some general textual information about the library.
--   2. The actual library with extensive documentation and examples
--   3. Migration guide for users familiar with (ansi-)wl-pprint
--   4. Historical notes about previous libraries
--   5. Algebraic properties
--
-- = How the layout works
--
-- There are two key concepts to layouting: the available width, and 'group'ing.
--
-- == Available width
--
-- The page has a certain maximum width, which the layouter tries to not exceed,
-- by inserting line breaks where possible. The functions given in this module
-- make it fairly straightforward to specify where, and under what
-- circumstances, such a line break may be inserted by the layouter, for example
-- via the 'sep' function.
--
-- There is also the concept of /ribbon width/. The ribbon is the part of a line
-- that is printed, i.e. the line length without the leading indentation. The
-- layouters take a ribbon fraction argument, which specifies how much of a line
-- should be filled before trying to break it up. A ribbon width of 0.5 in a
-- document of width 80 will result in the layouter to try to not exceed @0.5*80 =
-- 40@ (ignoring current indentation depth).
--
-- == Grouping
--
-- A document can be 'group'ed, which tells the layouter that it should attempt
-- to collapse it to a single line. If the result does not fit within the
-- constraints (given by page and ribbon widths), the document is rendered
-- unaltered. This allows fallback definitions, so that we get nice results even
-- when the original document would exceed the layout constraints.
--
-- == Starting out
--
-- As a reading list for starters, some of the most commonly used functions in
-- this module include '<>', 'hsep', '<+>', 'vsep', 'align', 'hang'. These cover
-- many use cases already, and many other functions are variations or
-- combinations of these.
--
-- = Example
--
-- The layout of the document can adapt to the available space. For example, we
-- can define a prettyprinter for simple type declaration that aligns over
-- multiple lines nicely if space does not permit it to fit in a single line.
-- Let's look at some code for doing this:
--
-- >>> let prettyType = align . sep . zipWith (<+>) ("::" : repeat "->")
-- >>> let prettyDecl n tys = pretty n <+> prettyType tys
-- >>> let doc = prettyDecl "example" ["Int", "Bool", "Char", "IO ()"]
--
-- If the page is wide enough (80 characters in this case), the definitions are
-- space-separated,
--
-- >>> putDocW 80 doc
-- example :: Int -> Bool -> Char -> IO ()
--
-- If we narrow the page width to only 20 characters, the /same document/
-- renders vertically aligned:
--
-- >>> putDocW 20 doc
-- example :: Int
--         -> Bool
--         -> Char
--         -> IO ()
module Data.Text.PrettyPrint.Doc (
    -- * Documents
    Doc,

    -- * Basic functionality
    Pretty(..),
    emptyDoc, nest, line, line', softline, softline', hardline, group, flatAlt,

    -- * Alignment functions
    --
    -- | The functions in this section cannot be described by Wadler's original
    -- functions. They align their output relative to the current output
    -- position - in contrast to @'nest'@ which always aligns to the current
    -- nesting level. This deprives these functions from being \'optimal\'. In
    -- practice however they prove to be very useful. The functions in this
    -- section should be used with care, since they are more expensive than the
    -- other functions. For example, @'align'@ shouldn't be used to pretty print
    -- all top-level declarations of a language, but using @'hang'@ for let
    -- expressions is fine.
    align, hang, indent, encloseSep, list, tupled,

    -- * Binary functions
    (<>), (<+>),

    -- * List functions

    -- | The 'sep' and 'cat' functions differ in one detail: when 'group'ed, the
    -- 'sep's replace newlines wich 'space's, while the 'cat's simply remove
    -- them. If you're not sure what you want, start with the 'sep's.

    concatWith,

    -- ** 'sep' family
    --
    -- | When 'group'ed, these will replace newlines with spaces.
    hsep, vsep, fillSep, sep,
    -- ** 'cat' family
    --
    -- | When 'group'ed, these will remove newlines.
    hcat, vcat, fillCat, cat,
    -- ** Others
    punctuate,

    -- * Reactive/conditional layouting
    --
    -- | Layout documents differently based on the current conditions.
    column, nesting, width, pageWidth,

    -- * Filler functions
    --
    -- | Fill up available space
    fill, fillBreak,

    -- * Bracketing functions
    --
    -- | Enclose documents in common ways.
    enclose, squotes, dquotes, parens, angles, brackets, braces,

    -- * Named character functions
    --
    -- | Convenience definitions for common characters
    lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket, squote,
    dquote, semi, colon, comma, space, dot, slash, backslash, equals, pipe,

    -- * Styling

    Style(..), SColor(..), SIntensity(..), SLayer(..),

    -- ** Font color
    black, red, green, yellow, blue, magenta, cyan, white, dullblack, dullred,
    dullgreen, dullyellow, dullblue, dullmagenta, dullcyan, dullwhite,

    -- ** Background color
    onblack, onred, ongreen, onyellow, onblue, onmagenta, oncyan, onwhite,
    ondullblack, ondullred, ondullgreen, ondullyellow, ondullblue,
    ondullmagenta, ondullcyan, ondullwhite,

    -- ** Font style
    bold, italics, underline,

    -- ** Remove formatting
    plain,

    -- * Optimization
    optimize,

    -- * Layout
    --
    -- | Layouting produces a straightforward 'SimpleDoc' based on parameters
    -- such as page width and ribbon size, by evaluating how a 'Doc' fits these
    -- constraints the best.
    SimpleDoc(..),
    layoutPretty, layoutCompact, layoutSmart,

    -- * Notes

    -- ** Migration guide
    --
    -- $migration

    -- ** Historical
    --
    -- $history

    -- ** Algebraic properties
    --
    -- $algebra

) where



import           Data.Maybe
import           Data.Monoid
import qualified Data.Semigroup as Semi (Semigroup ((<>)))
import           Data.String    (IsString (..))
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Void



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.IO as T
-- >>> import Data.Text.PrettyPrint.Doc.Render.Text
-- >>> let putDocW w doc = renderIO System.IO.stdout (layoutPretty 1.0 w doc)



-- | The abstract data type @'Doc'@ represents pretty documents.
--
-- More specifically, a value of type @Doc@ represents a non-empty set of
-- possible layoutings of a document. The layouting functions select one of
-- these possibilities.
--
-- The simplest layouter is via the 'Show' class. @('show' doc)@ pretty prints
-- document @doc@ with a page width of 80 characters and a ribbon width of 32
-- characters.
--
-- >>> putStrLn (show (vsep ["hello", "world"]))
-- hello
-- world
data Doc =

    -- | Occurs when flattening a line. The layouter will reject this document,
    -- choosing a more suitable rendering.
    Fail

    -- | The empty document; unit of Cat (observationally)
    | Empty

    -- | invariant: char is not '\n'
    | Char Char

    -- | invariant: text doesn't contain '\n'
    | Text Text

    -- | Line break
    | Line

    -- | Layout the first doc, but when flattened (via group), layout the second.
    | FlatAlt Doc Doc

    -- | Concatenation of two documents
    | Cat Doc Doc

    -- | Document indented by a number of columns
    | Nest !Int Doc

    -- | invariant: first lines of first doc longer than the first lines of the second doc
    | Union Doc Doc

    -- | React on the current cursor position, see 'column'
    | Column (Int -> Doc)

    -- | React on the document's width, see 'pageWidth'
    | PageWidth (Maybe Int -> Doc)

    -- | React on the current nesting level, see 'nesting'
    | Nesting (Int -> Doc)

    -- | Add 'Style' information to the enclosed 'Doc'
    | StylePush Style Doc

    -- | Remove one previously pushed style. Used only during layouting.
    | StylePop

-- |
-- @
-- 'mempty' = 'emptyDoc'
-- 'mconcat' = 'hcat'
-- @
instance Monoid Doc where
    mempty = emptyDoc
    mappend = (Semi.<>)
    mconcat = hcat

instance Semi.Semigroup Doc where
    (<>) = Cat

-- | >>> putDoc "hello\nworld"
-- hello
-- world
instance IsString Doc where
    fromString = pretty . T.pack

-- | Overloaded conversion to 'Doc'.
class Pretty a where

    -- | >>> putDoc (pretty 1 <+> pretty "hello" <+> pretty 1.234)
    -- 1 hello 1.234
    pretty :: a -> Doc

    -- | @'prettyList'@ is only used to define the @instance
    -- 'Pretty' a => 'Pretty' [a]@. In normal circumstances only the @'pretty'@
    -- function is used.
    --
    -- >>> putDoc (prettyList [1, 23, 456])
    -- [1,23,456]
    prettyList :: [a] -> Doc
    prettyList = list . map pretty

-- | >>> putDoc (pretty [1,2,3])
-- [1,2,3]
instance Pretty a => Pretty [a] where
    pretty = prettyList

-- | Identity transformation.
--
-- >>> putDoc (pretty 123)
-- 123
-- >>> putDoc (pretty (pretty 123))
-- 123
instance Pretty Doc where
    pretty = id

-- | >>> putDoc (pretty ())
-- ()
--
-- The argument is not used,
--
-- >>> putDoc (pretty (error "Strict?" :: ()))
-- ()
instance Pretty () where
    pretty _ = "()"

-- | >>> putDoc (pretty True)
-- True
instance Pretty Bool where
    pretty = unsafeText . T.pack . show

-- | Instead of @('pretty' '\n')@, consider using @'line'@ as a more readable
-- alternative.
--
-- >>> putDoc (pretty 'f' <> pretty 'o' <> pretty 'o')
-- foo
-- >>> putDoc (pretty ("string" :: String))
-- string
instance Pretty Char where
    pretty '\n' = line
    pretty c = Char c

    prettyList = (pretty :: Text -> Doc) . fromString

-- | >>> putDoc (pretty (123 :: Int))
-- 123
instance Pretty Int where
    pretty = unsafeText . T.pack . show

-- | >>> putDoc (pretty (2^123 :: Integer))
-- 10633823966279326983230456482242756608
instance Pretty Integer where
    pretty = unsafeText . T.pack . show

-- | >>> putDoc (pretty (pi :: Float))
-- 3.1415927
instance Pretty Float where
    pretty = unsafeText . T.pack . show

-- | >>> putDoc (pretty (exp 1 :: Double))
-- 2.718281828459045
instance Pretty Double where
    pretty = unsafeText . T.pack . show

-- | >>> putDoc (pretty (123, "hello"))
-- (123,hello)
instance (Pretty a, Pretty b) => Pretty (a,b) where
            pretty (x,y) = tupled [pretty x, pretty y]

-- | >>> putDoc (pretty (123, "hello", False))
-- (123,hello,False)
instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
                pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

-- | Ignore 'Nothing's, print 'Just' contents.
--
-- >>> putDoc (pretty (Just True))
-- True
-- >>> putDoc (braces (pretty (Nothing :: Maybe Bool)))
-- {}
--
-- >>> putDoc (pretty [Just 1, Nothing, Just 3, Nothing])
-- [1,3]
instance Pretty a => Pretty (Maybe a) where
    pretty Nothing = mempty
    pretty (Just x) = pretty x

    prettyList = prettyList . catMaybes

-- | Automatically converts all newlines to @'line'@.
--
-- >>> putDoc (pretty ("hello\nworld" :: Text))
-- hello
-- world
--
-- Note that  @'line'@ can be undone by @'group'@:
--
-- >>> putDoc (group (pretty ("hello\nworld" :: Text)))
-- hello world
--
-- Manually use @'hardline'@ if you /definitely/ want newlines.
instance Pretty Text where
    pretty = vsep . map unsafeText . T.splitOn "\n"

instance Pretty Void where
    pretty = absurd



-- | A general style to be applied to some text. How these turn out in the final
-- text depends on the layouter, for example a terminal backend might create
-- ANSI escape codes for each color.
data Style =
      SItalicized
    | SBold
    | SUnderlined
    | SColor SLayer SIntensity SColor
    deriving (Eq, Ord, Show)

-- | 8 different colors, so that all can be displayed in an ANSI terminal.
data SColor = SBlack | SRed | SGreen | SYellow | SBlue | SMagenta | SCyan | SWhite
    deriving (Eq, Ord, Show)

-- | Dull or vivid coloring, as supported by ANSI terminals.
data SIntensity = SVivid | SDull
    deriving (Eq, Ord, Show)

-- | Foreground (text) or background (paper) color
data SLayer = SForeground | SBackground
    deriving (Eq, Ord, Show)

-- | The data type @SimpleDoc@ represents layouted documents and is used by the
-- display functions.
--
-- A simplified view is that @'Doc' = ['SimpleDoc']@, and the layouting
-- functions pick one of the 'SimpleDoc's. This means that 'SimpleDoc' has all
-- complexity contained in 'Doc' resolved, making it very easy to convert it to
-- other formats, such as plain text or terminal output.
--
-- To write your own Doc to X converter, it is therefore sufficient to convert
-- from 'SimpleDoc'.
data SimpleDoc =
      SFail
    | SEmpty
    | SChar Char SimpleDoc
    | SText Text SimpleDoc

    -- | @Int@ = indentation level for the line
    | SLine !Int SimpleDoc

    -- | Apply a style to the remaining document. The display function should do
    -- this until it hits a 'SStylePop' entry.
    | SStylePush Style SimpleDoc

    -- | Undo one previously set 'SStylePush'.
    | SStylePop SimpleDoc



-- | @(unsafeText s)@ contains the literal string @s@.
--
-- The string must not contain any newline characters, since this is an
-- invariant of the 'Text' constructor. If you're not sure, use the safer
-- 'text'.
unsafeText :: Text -> Doc
unsafeText t = case T.compareLength t 1 of
    LT -> Empty
    EQ -> pretty (T.head t)
    GT -> Text t

-- | The empty docdument behaves like @('pretty' "")@, so it has a height of 1.
-- This may lead to surprising behaviour if we expect it to bear no weight
-- inside e.g. vcat, where we get an empty line of output from it ('angles' for
-- visibility only):
--
-- >>> putDoc (vsep ["hello", angles emptyDoc, "world"])
-- hello
-- <>
-- world
--
-- Together with '<>', 'emptyDoc' forms the 'Monoid' 'Doc'.
emptyDoc :: Doc
emptyDoc = Empty

-- | @('nest' i x)@ layouts document @x@ with the current indentation level
-- increased by @i@. See also 'hang', 'align' and 'indent'.
--
-- >>> putDoc (vsep [nest 4 (vsep ["lorem", "ipsum", "dolor"]), "sit", "amet"])
-- lorem
--     ipsum
--     dolor
-- sit
-- amet
nest :: Int -> Doc -> Doc
nest 0 = id
nest i = Nest i

-- | The @'line'@ document advances to the next line and indents to the current
-- nesting level.
--
-- >>> let doc = "lorem ipsum" <> line <> "dolor sit amet"
-- >>> putDoc doc
-- lorem ipsum
-- dolor sit amet
--
-- @'line'@ behaves like @'space'@ if the line break is undone by 'group':
--
-- >>> putDoc (group doc)
-- lorem ipsum dolor sit amet
line :: Doc
line = FlatAlt Line space

-- | @'line''@ is like @'line'@, but behaves like @'mempty'@ if the line break
-- is undone by 'group' (instead of @'space'@).
--
-- >>> let doc = "lorem ipsum" <> line' <> "dolor sit amet"
-- >>> putDoc doc
-- lorem ipsum
-- dolor sit amet
-- >>> putDoc (group doc)
-- lorem ipsumdolor sit amet
line' :: Doc
line' = FlatAlt Line mempty

-- | @softline@ behaves like @'space'@ if the resulting output fits the page,
-- otherwise like @'line'@.
--
-- Here, we have enough space to put everything in one line:
--
-- >>> let doc = "lorem ipsum" <> softline <> "dolor sit amet"
-- >>> putDocW 80 doc
-- lorem ipsum dolor sit amet
--
-- If we narrow the page to width 10, the layouter produces a line break:
--
-- >>> putDocW 10 doc
-- lorem ipsum
-- dolor sit amet
--
-- @
-- 'softline' = 'group' 'line'
-- @
softline :: Doc
softline = group line

-- | @'softline''@ is like @'softline'@, but behaves like @'mempty'@ if the
-- resulting output does not fit on the page (instead of @'space'@). In other
-- words, @'line'@ is to @'line''@ how @'softline'@ is to @'softline''@.
--
-- With enough space, we get direct concatenation:
--
-- >>> let doc = "lorem ipsum" <> softline' <> "dolor sit amet"
-- >>> putDocW 80 doc
-- lorem ipsumdolor sit amet
--
-- If we narrow the page to width 10, the layouter produces a line break:
--
-- >>> putDocW 10 doc
-- lorem ipsum
-- dolor sit amet
--
-- @
-- 'softline'' = 'group' 'line''
-- @
softline' :: Doc
softline' = group line'

-- | A @'hardline'@ is /always/ layouted as a line break, even when 'group'ed or
-- when there is plenty of space. Note that it might still be simply discarded
-- if it is part of a 'flatAlt' inside a 'group'.
--
-- >>> let doc = "lorem ipsum" <> hardline <> "dolor sit amet"
-- >>> putDocW 1000 doc
-- lorem ipsum
-- dolor sit amet
--
-- >>> putDoc (group doc)
-- lorem ipsum
-- dolor sit amet
hardline :: Doc
hardline = Line

-- | @('group' x)@ tries layouting @x@ into a single line (by removing the
-- contained line breaks); if this does not fit the page, @x@ is layouted
-- without any changes.
--
-- See 'vcat', 'line', or 'flatAlt' for examples.
group :: Doc -> Doc
group x = Union (flatten x) x

-- Choose the first element of each @Union@, and discard the first field of all
-- @FlatAlt@s. Note that this is not an idempotent operation, since the other
-- @FlatAlt@ field is not flattened recursively.
flatten :: Doc -> Doc
flatten = \case
    FlatAlt _ y   -> y
    Cat x y       -> Cat (flatten x) (flatten y)
    Nest i x      -> Nest i (flatten x)
    Line          -> Fail
    Union x _     -> flatten x
    Column f      -> Column (flatten . f)
    PageWidth f   -> PageWidth (flatten . f)
    Nesting f     -> Nesting (flatten . f)
    StylePush s x -> StylePush s (flatten x)

    x@Fail       -> x
    x@Empty      -> x
    x@Char{}     -> x
    x@Text{}     -> x

    StylePop -> error "StylePop should only appear during layouting, never during flattening"

-- | @('flatAlt' x fallback)@ renders as @x@ by default, but falls back to
-- @fallback@ when 'group'ed.
--
-- Note that 'group' undoes only a single layer of 'flatAlt's, and does not
-- recursively flatten all of them. This allows more fine-grained control over
-- the 'group'ing process. Also, the fallback should be narrower than the
-- original document, or falling back to it might not produce a very appealing
-- layout.
--
-- 'flatAlt' is particularly useful for defining conditional separators such as
-- in the above example. Some others that might be useful include
--
-- @
-- softHyphen = 'flatAlt' 'mempty' "-"
-- softline   = 'flatAlt' 'space' 'line'
-- @
--
-- We can use this to render Haskell's do-notation nicely:
--
-- >>> let open        = flatAlt "" "{ "
-- >>> let close       = flatAlt "" " }"
-- >>> let separator   = flatAlt "" "; "
-- >>> let prettyDo xs = group ("do" <+> encloseSep open close separator xs)
-- >>> let statements  = ["name:_ <- getArgs", "let greet = \"Hello, \" <> name", "putStrLn greet"]
--
-- This is put into a single line with @{;}@ style if it fits,
--
-- >>> putDocW 80 (prettyDo statements)
-- do { name:_ <- getArgs; let greet = "Hello, " <> name; putStrLn greet }
--
-- When there is not enough space the statements are broken up into lines
-- nicely,
--
-- >>> putDocW 10 (prettyDo statements)
-- do name:_ <- getArgs
--    let greet = "Hello, " <> name
--    putStrLn greet
flatAlt :: Doc -> Doc -> Doc
flatAlt = FlatAlt



-- | @('align' x)@ layouts document @x@ with the nesting level set to the
-- current column. It is used for example to implement 'hang'.
--
-- As an example, we will put a document right above another one, regardless of
-- the current nesting level. Without 'align'ment, the second line is put simply
-- below everything we've had so far,
--
-- >>> putDoc ("lorem" <+> vsep ["ipsum", "dolor"])
-- lorem ipsum
-- dolor
--
-- If we add an 'align' to the mix, the @'vsep'@'s contents all start in the
-- same column,
--
-- >>> putDoc ("lorem" <+> align (vsep ["ipsum", "dolor"]))
-- lorem ipsum
--       dolor
align :: Doc -> Doc
align d = column (\k -> nesting (\i -> nest (k - i) d)) -- nesting might be negative!

-- | @('hang' i x)@ layouts document @x@ with a nesting level set to the
-- /current column/ plus @i@.
--
-- >>> let doc = fillSep (map pretty (T.words "Indenting these words with hang"))
-- >>> putDocW 24 ("prefix" <+> hang 4 doc)
-- prefix Indenting these
--            words with
--            hang
--
-- This differs from 'nest', which is based on the /current nesting level/ plus
-- @i@. When you're not sure, try the more efficient 'nest' first. In our
-- example, this would yield
--
-- >>> let doc = fillSep (map pretty (T.words "Indenting these words with nest"))
-- >>> putDocW 24 ("prefix" <+> nest 4 doc)
-- prefix Indenting these
--     words with nest
--
-- @
-- 'hang' i doc = 'align' ('nest' i doc)
-- @
hang :: Int -> Doc -> Doc
hang i d = align (nest i d)

-- | @('indent' i x)@ indents document @x@ with @i@ spaces, starting from the
-- current cursor position.
--
-- >>> let doc = fillSep (map pretty (T.words "The indent function indents these words!"))
-- >>> putDocW 24 ("prefix" <> indent 4 doc)
-- prefix    The indent
--           function
--           indents these
--           words!
--
-- @
-- 'indent' i d = 'hang' i ({i spaces} <> d)
-- @
indent :: Int -> Doc -> Doc
indent i d = hang i (spaces i <> d)

-- | @('encloseSep' l r sep xs)@ concatenates the documents @xs@ separated by
-- @sep@, and encloses the resulting document by @l@ and @r@.
--
-- The documents are layouted horizontally if that fits the page,
--
-- >>> let doc = "list" <+> encloseSep lbracket rbracket comma (map pretty [1,20,300,4000])
-- >>> putDocW 80 doc
-- list [1,20,300,4000]
--
-- If there is not enough space, then the input is split into lines entry-wise
-- therwise they are aligned vertically, with separators put in the front:
--
-- >>> putDocW 10 doc
-- list [1
--      ,20
--      ,300
--      ,4000]
--
-- For putting separators at the end of entries instead, have a look at
-- 'punctuate'.
encloseSep
    :: Doc   -- ^ left delimiter
    -> Doc   -- ^ right delimiter
    -> Doc   -- ^ separator
    -> [Doc] -- ^ input documents
    -> Doc
encloseSep l r s ds = case ds of
    []  -> l <> r
    [d] -> l <> d <> r
    _   -> align (cat (zipWith (<>) (l : repeat s) ds) <> r)

-- | Haskell-inspired special case of 'encloseSep' with braces and 'comma' as
-- separator.
--
-- >>> putDocW 80 (list (map pretty [1,20,300,4000]))
-- [1,20,300,4000]
list :: [Doc] -> Doc
list = encloseSep lbracket rbracket comma

-- | Haskell-inspired special case of 'encloseSep' with parentheses and 'comma'
-- as separator.
--
-- >>> putDocW 80 (tupled (map pretty [1,20,300,4000]))
-- (1,20,300,4000)
tupled :: [Doc] -> Doc
tupled = encloseSep lparen rparen comma



-- | @(x '<+>' y)@ concatenates document @x@ and @y@ with a @'space'@ in
-- between.
--
-- >>> putDoc ("hello" <+> "world")
-- hello world
--
-- @
-- x '<+>' y = x '<>' 'space' '<>' y
-- @
(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> space <> y



-- | 'foldr', with an empty special case for empty arguments. This is useful to
-- collapse collections of documents element-wise.
--
-- Multiple convenience definitions based on 'concatWith' are alredy predefined,
-- for example
--
-- @
-- 'hsep'    = 'concatWith' ('<+>')
-- 'fillSep' = 'concatWith' (\\x y -> x '<>' 'softline' '<>' y)
-- @
concatWith :: Foldable t => (Doc -> Doc -> Doc) -> t Doc -> Doc
concatWith f ds
    | null ds = mempty
    | otherwise = foldr1 f ds

-- | @('hsep' xs)@ concatenates all documents @xs@ horizontally with @'<+>'@,
-- i.e. it puts a space between all entries.
--
-- >>> let docs = map pretty (T.words "lorem ipsum dolor sit amet")
-- >>> putDoc (hsep docs)
-- lorem ipsum dolor sit amet
hsep :: [Doc] -> Doc
hsep = concatWith (<+>)

-- | @('vsep' xs)@ concatenates all documents @xs@ above each other. If a
-- 'group' undoes the line breaks inserted by @vsep@, the documents are
-- separated with a 'space' instead.
--
-- Using 'vsep' alone yields
--
-- >>> putDoc ("prefix" <+> vsep ["text", "to", "lay", "out"])
-- prefix text
-- to
-- lay
-- out
--
-- 'group'ing a 'vsep' separates the documents with a 'space' if it fits the
-- page (and does nothing otherwise). See the @'sep'@ convenience function for
-- this use case.
--
-- The 'align' function can be used to align the documents under their first
-- element:
--
-- >>> putDoc ("prefix" <+> align (vsep ["text", "to", "lay", "out"]))
-- prefix text
--        to
--        lay
--        out
--
-- Since 'group'ing a 'vsep' is rather common, 'sep' is a built-in for doing
-- that.
vsep :: [Doc] -> Doc
vsep = concatWith (\x y -> x <> line <> y)

-- | @('fillSep' xs)@ concatenates the documents @xs@ horizontally with @'<+>'@
-- as long as it fits the page, then inserts a @'line'@ and continues doing that
-- for all documents in @xs@. (@'line'@ means that if 'group'ed, the documents
-- are separated with a 'space' instead of newlines. Use 'fillCat' if you do not
-- want a 'space'.)
--
-- Let's print some words to fill the line:
--
-- >>> let docs = take 20 (cycle ["lorem", "ipsum", "dolor", "sit", "amet"])
-- >>> putDocW 80 ("Docs:" <+> fillSep docs)
-- Docs: lorem ipsum dolor sit amet lorem ipsum dolor sit amet lorem ipsum dolor
-- sit amet lorem ipsum dolor sit amet
--
-- The same document, printed at a width of only 40, yields
--
-- >>> putDocW 40 ("Docs:" <+> fillSep docs)
-- Docs: lorem ipsum dolor sit amet lorem
-- ipsum dolor sit amet lorem ipsum dolor
-- sit amet lorem ipsum dolor sit amet
fillSep :: [Doc] -> Doc
fillSep = concatWith (\x y -> x <> softline <> y)

-- | @('sep' xs)@ tries layouting the documents @xs@ separated with 'space's,
-- and if this does not fit the page, separates them with newlines. This is what
-- differentiates it from 'vsep', which always layouts its contents beneath each
-- other.
--
-- >>> let doc = "prefix" <+> sep ["text", "to", "lay", "out"]
-- >>> putDocW 80 doc
-- prefix text to lay out
--
-- With a narrower layout, the entries are separated by newlines:
--
-- >>> putDocW 20 doc
-- prefix text
-- to
-- lay
-- out
--
-- @
-- 'sep' = 'group' . 'vsep'
-- @
sep :: [Doc] -> Doc
sep = group . vsep



-- | @('hcat' xs)@ concatenates all documents @xs@ horizontally with @'<>'@
-- (i.e. without any spacing).
--
-- It is provided only for consistency, since it is identical to 'mconcat'.
--
-- >>> let docs = map pretty (T.words "lorem ipsum dolor")
-- >>> putDoc (hcat docs)
-- loremipsumdolor
hcat :: [Doc] -> Doc
hcat = concatWith (<>)

-- | @('vcat' xs)@ vertically concatenates the documents @xs@. If it is
-- 'group'ed, the line breaks are removed.
--
-- In other words @'vcat'@ is like @'vsep'@, with newlines removed instead of
-- replaced by 'space's.
--
-- >>> let docs = map pretty (T.words "lorem ipsum dolor")
-- >>> putDoc (vcat docs)
-- lorem
-- ipsum
-- dolor
--
-- Since 'group'ing a 'vcat' is rather common, 'cat' is a built-in shortcut for
-- it.
vcat :: [Doc] -> Doc
vcat = concatWith (\x y -> x <> line' <> y)

-- | @('fillCat' xs)@ concatenates documents @xs@ horizontally with @'<>'@ as
-- long as it fits the page, then inserts a @'line''@ and continues doing that
-- for all documents in @xs@. This is similar to how an ordinary word processor
-- lays out the text if you just keep typing after you hit the maximum line
-- length.
--
-- (@'line''@ means that if 'group'ed, the documents are separated with nothing
-- instead of newlines. See 'fillSep' if you want a 'space' instead.)
--
-- Observe the difference between 'fillSep' and 'fillCat'. 'fillSep'
-- concatenates the entries 'space'd when 'group'ed,
--
-- >>> let docs = take 20 (cycle (["lorem", "ipsum", "dolor", "sit", "amet"]))
-- >>> putDocW 40 ("Grouped:" <+> group (fillSep docs))
-- Grouped: lorem ipsum dolor sit amet
-- lorem ipsum dolor sit amet lorem ipsum
-- dolor sit amet lorem ipsum dolor sit
-- amet
--
-- On the other hand, 'fillCat' concatenates the entries directly when
-- 'group'ed,
--
-- >>> putDocW 40 ("Grouped:" <+> group (fillCat docs))
-- Grouped: loremipsumdolorsitametlorem
-- ipsumdolorsitametloremipsumdolorsitamet
-- loremipsumdolorsitamet
fillCat :: [Doc] -> Doc
fillCat = concatWith (\x y -> x <> softline' <> y)

-- | @('cat' xs)@ tries layouting the documents @xs@ separated with nothing, and
-- if this does not fit the page, separates them with newlines. This is what
-- differentiates it from 'vcat', which always layouts its contents beneath each
-- other.
--
-- >>> let docs = map pretty (T.words "lorem ipsum dolor")
-- >>> putDocW 80 ("Docs:" <+> cat docs)
-- Docs: loremipsumdolor
--
-- When there is enough space, the documents are put above one another,
--
-- >>> putDocW 10 ("Docs:" <+> cat docs)
-- Docs: lorem
-- ipsum
-- dolor
--
-- @
-- 'cat' = 'group' . 'vcat'
-- @
cat :: [Doc] -> Doc
cat = group . vcat



-- | @('punctuate' p xs)@ appends @p@ to all but the last document in @xs@.
--
-- >>> let docs = punctuate comma (map pretty (T.words "lorem ipsum dolor sit amet"))
-- >>> putDocW 80 (hsep docs)
-- lorem, ipsum, dolor, sit, amet
--
-- The separators are put at the end of the entries, which we can see if we
-- position the result vertically:
--
-- >>> putDocW 20 (vsep docs)
-- lorem,
-- ipsum,
-- dolor,
-- sit,
-- amet
--
-- If you want put the commas in front of their elements instead of at the end,
-- you should use 'tupled' or, in general, 'encloseSep'.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds



-- | Layout a document depending on which column it starts at. 'align' is
-- implemented in terms of 'column'.
--
-- >>> putDoc (column (\l -> "Columns are" <+> pretty l <> "-based."))
-- Columns are 0-based.
--
-- >>> let doc = "prefix" <+> column (\l -> "| <- column" <+> pretty l)
-- >>> putDoc (vsep [indent n doc | n <- [0,4,8]])
-- prefix | <- column 7
--     prefix | <- column 11
--         prefix | <- column 15
column :: (Int -> Doc) -> Doc
column = Column

-- | Layout a document depending on the current 'nest'ing level. 'align' is
-- implemented in terms of 'nesting'.
--
-- >>> let doc = "prefix" <+> nesting (\l -> brackets ("Nested:" <+> pretty l))
-- >>> putDoc (vsep [indent n doc | n <- [0,4,8]])
-- prefix [Nested: 0]
--     prefix [Nested: 4]
--         prefix [Nested: 8]
nesting :: (Int -> Doc) -> Doc
nesting = Nesting

-- | @('width' doc f)@ layouts the document 'doc', and makes the column width of
-- it available to a function.
--
-- >>> let annotate doc = width (brackets doc) (\w -> " <- width:" <+> pretty w)
-- >>> putDoc (align (vsep (map annotate ["---", "------", indent 3 "---", vsep ["---", indent 4 "---"]])))
-- [---] <- width: 5
-- [------] <- width: 8
-- [   ---] <- width: 8
-- [---
--     ---] <- width: 8
width :: Doc -> (Int -> Doc) -> Doc
width doc f
  = column (\colStart ->
        doc <> column (\colEnd ->
            f (colEnd - colStart)))

-- | Layout a document depending on the page width, if one has been specified.
--
-- >>> let doc = "prefix" <+> pageWidth (\l -> brackets ("Width:" <+> pretty l))
-- >>> putDocW 32 (vsep [indent n doc | n <- [0,4,8]])
-- prefix [Width: 32]
--     prefix [Width: 32]
--         prefix [Width: 32]
--
-- Whether the page width is @'Just' n@ or @'Nothing'@ depends on the layouter.
-- Of the default layouters, @'layoutCompact'@ uses @'Nothing'@, and all others
-- @'Just' <pagewidth>@.
pageWidth :: (Maybe Int -> Doc) -> Doc
pageWidth = PageWidth



-- | @('fill' i x)@ layouts document @x@. It then appends @space@s until the
-- width is equal to @i@. If the width of @x@ is already larger, nothing is
-- appended.
--
-- This function is quite useful in practice to output a list of bindings:
--
-- >>> let types = [("empty","Doc"), ("nest","Int -> Doc -> Doc"), ("fillSep","[Doc] -> Doc")]
-- >>> let ptype (name, tp) = fill 5 (pretty name) <+> "::" <+> pretty tp
-- >>> putDoc ("let" <+> align (vcat (map ptype types)))
-- let empty :: Doc
--     nest  :: Int -> Doc -> Doc
--     fillSep :: [Doc] -> Doc
fill :: Int -> Doc -> Doc
fill f doc = width doc (\w -> spaces (f - w))

-- | @('fillBreak' i x)@ first layouts document @x@. It then appends @space@s
-- until the width is equal to @i@. If the width of @x@ is already larger than
-- @i@, the nesting level is increased by @i@ and a @line@ is appended. When we
-- redefine @ptype@ in the example given in 'fill' to use @'fillBreak'@, we get
-- a useful variation of the output:
--
-- >>> let types = [("empty","Doc"), ("nest","Int -> Doc -> Doc"), ("fillSep","[Doc] -> Doc")]
-- >>> let ptype (name, tp) = fillBreak 5 (pretty name) <+> "::" <+> pretty tp
-- >>> putDoc ("let" <+> align (vcat (map ptype types)))
-- let empty :: Doc
--     nest  :: Int -> Doc -> Doc
--     fillSep
--           :: [Doc] -> Doc
fillBreak :: Int -> Doc -> Doc
fillBreak f x = width x (\w ->
    if w > f
        then nest f line'
        else spaces (f - w))

-- | Insert a number of spaces.
spaces :: Int -> Doc
spaces n | n <= 0    = mempty
         | otherwise = unsafeText (T.replicate n " ")



-- | @('enclose' l r x)@ encloses document @x@ between documents @l@ and @r@
-- using @'<>'@.
--
-- >>> putDoc (enclose "A" "Z" "·")
-- A·Z
--
-- @
-- 'enclose' l r x = l '<>' x '<>' r
-- @
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r x = l <> x <> r

-- | >>> putDoc (squotes "·")
-- '·'
squotes :: Doc -> Doc
squotes = enclose squote squote

-- | >>> putDoc (dquotes "·")
-- "·"
dquotes :: Doc -> Doc
dquotes = enclose dquote dquote

-- | >>> putDoc (parens "·")
-- (·)
parens :: Doc -> Doc
parens = enclose lparen rparen

-- | >>> putDoc (angles "·")
-- <·>
angles :: Doc -> Doc
angles = enclose langle rangle

-- | >>> putDoc (brackets "·")
-- [·]
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

-- | >>> putDoc (braces "·")
-- {·}
braces :: Doc -> Doc
braces = enclose lbrace rbrace

-- | >>> putDoc squote
-- '
squote :: Doc
squote = pretty '\''
-- | >>> putDoc dquote
-- "
dquote :: Doc
dquote = pretty '"'

-- | >>> putDoc lparen
-- (
lparen :: Doc
lparen = pretty '('

-- | >>> putDoc rparen
-- )
rparen :: Doc
rparen = pretty ')'

-- | >>> putDoc langle
-- <
langle :: Doc
langle = pretty '<'

-- | >>> putDoc rangle
-- >
rangle :: Doc
rangle = pretty '>'

-- | >>> putDoc lbracket
-- [
lbracket :: Doc
lbracket = pretty '['
-- | >>> putDoc rbracket
-- ]
rbracket :: Doc
rbracket = pretty ']'

-- | >>> putDoc lbrace
-- {
lbrace :: Doc
lbrace = pretty '{'
-- | >>> putDoc rbrace
-- }
rbrace :: Doc
rbrace = pretty '}'

-- | >>> putDoc semi
-- ;
semi :: Doc
semi = pretty ';'

-- | >>> putDoc colon
-- :
colon :: Doc
colon = pretty ':'

-- | >>> putDoc comma
-- ,
comma :: Doc
comma = pretty ','

-- | >>> putDoc ("a" <> space <> "b")
-- a b
--
-- This is mostly used via @'<+>'@,
--
-- >>> putDoc ("a" <+> "b")
-- a b
space :: Doc
space = pretty ' '

-- | >>> putDoc dot
-- .
dot :: Doc
dot = pretty '.'

-- | >>> putDoc slash
-- /
slash :: Doc
slash = pretty '/'

-- | >>> putDoc backslash
-- \
backslash :: Doc
backslash = pretty '\\'

-- | >>> putDoc equals
-- =
equals :: Doc
equals = pretty '='

-- | >>> putDoc pipe
-- |
pipe :: Doc
pipe = pretty '|'



black :: Doc -> Doc
black = StylePush (SColor SForeground SVivid SBlack)
red :: Doc -> Doc
red = StylePush (SColor SForeground SVivid SRed)
green :: Doc -> Doc
green = StylePush (SColor SForeground SVivid SGreen)
yellow :: Doc -> Doc
yellow = StylePush (SColor SForeground SVivid SYellow)
blue :: Doc -> Doc
blue = StylePush (SColor SForeground SVivid SBlue)
magenta :: Doc -> Doc
magenta = StylePush (SColor SForeground SVivid SMagenta)
cyan :: Doc -> Doc
cyan = StylePush (SColor SForeground SVivid SCyan)
white :: Doc -> Doc
white = StylePush (SColor SForeground SVivid SWhite)
dullblack :: Doc -> Doc
dullblack = StylePush (SColor SForeground SDull SBlack)
dullred :: Doc -> Doc
dullred = StylePush (SColor SForeground SDull SRed)
dullgreen :: Doc -> Doc
dullgreen = StylePush (SColor SForeground SDull SGreen)
dullyellow :: Doc -> Doc
dullyellow = StylePush (SColor SForeground SDull SYellow)
dullblue :: Doc -> Doc
dullblue = StylePush (SColor SForeground SDull SBlue)
dullmagenta :: Doc -> Doc
dullmagenta = StylePush (SColor SForeground SDull SMagenta)
dullcyan :: Doc -> Doc
dullcyan = StylePush (SColor SForeground SDull SCyan)
dullwhite :: Doc -> Doc
dullwhite = StylePush (SColor SForeground SDull SWhite)

onblack :: Doc -> Doc
onblack = StylePush (SColor SBackground SVivid SBlack)
onred :: Doc -> Doc
onred = StylePush (SColor SBackground SVivid SRed)
ongreen :: Doc -> Doc
ongreen = StylePush (SColor SBackground SVivid SGreen)
onyellow :: Doc -> Doc
onyellow = StylePush (SColor SBackground SVivid SYellow)
onblue :: Doc -> Doc
onblue = StylePush (SColor SBackground SVivid SBlue)
onmagenta :: Doc -> Doc
onmagenta = StylePush (SColor SBackground SVivid SMagenta)
oncyan :: Doc -> Doc
oncyan = StylePush (SColor SBackground SVivid SCyan)
onwhite :: Doc -> Doc
onwhite = StylePush (SColor SBackground SVivid SWhite)
ondullblack :: Doc -> Doc
ondullblack = StylePush (SColor SBackground SDull SBlack)
ondullred :: Doc -> Doc
ondullred = StylePush (SColor SBackground SDull SRed)
ondullgreen :: Doc -> Doc
ondullgreen = StylePush (SColor SBackground SDull SGreen)
ondullyellow :: Doc -> Doc
ondullyellow = StylePush (SColor SBackground SDull SYellow)
ondullblue :: Doc -> Doc
ondullblue = StylePush (SColor SBackground SDull SBlue)
ondullmagenta :: Doc -> Doc
ondullmagenta = StylePush (SColor SBackground SDull SMagenta)
ondullcyan :: Doc -> Doc
ondullcyan = StylePush (SColor SBackground SDull SCyan)
ondullwhite :: Doc -> Doc
ondullwhite = StylePush (SColor SBackground SDull SWhite)

-- | Render the enclosed document in __bold__.
bold :: Doc -> Doc
bold = StylePush SBold

-- | Render the enclosed document in /italics/.
italics :: Doc -> Doc
italics = StylePush SItalicized

-- | Render the enclosed document underlined.
underline :: Doc -> Doc
underline = StylePush SUnderlined

-- | Remove all styling information.
--
-- Although 'plain' is idempotent,
--
-- @
-- 'plain' . 'plain' = 'plain'
-- @
--
-- it should not be used without caution, for each invocation traverses the
-- entire contained document. The most common place to use 'plain' is just
-- before layouting.
plain :: Doc -> Doc
plain = \case
    FlatAlt x y   -> FlatAlt (plain x) (plain y)
    Cat x y       -> Cat (plain x) (plain y)
    Nest i x      -> Nest i (plain x)
    Union x y     -> Union (plain x) (plain y)
    Column f      -> Column (plain . f)
    PageWidth f   -> PageWidth (plain . f)
    Nesting f     -> Nesting (plain . f)
    StylePush _ x -> plain x

    x@Fail{}  -> x
    x@Empty{} -> x
    x@Char{}  -> x
    x@Text{}  -> x
    x@Line{}  -> x

    StylePop -> error "StylePop should only appear during layouting, never during composition"



-- | @('optimize' doc)@ restructures the @doc@ so that it can sometimes be
-- rendered more efficiently (see notes below). A document always renders
-- identical to its unoptimized version.
--
-- = Purpose
--
-- When layouting a 'Doc' to a 'SimpleDoc', every component of the input is
-- translated directly to the simpler output format. This sometimes yields
-- undesirable chunking when many pieces have been concatenated together.
--
-- For example,
--
-- >>> putDoc ("a" <> "b" <> pretty 'c' <> "d")
-- abcd
--
-- results in a chain of four entries in a 'SimpleDoc', although this is fully
-- equivalent to the tightly packed
--
-- >>> putDoc "abcd"
-- abcd
--
-- which is only a single 'SimpleDoc' entry.
--
-- 'optimize' does exactly that: compact consecutive text nodes in an input
-- document.
--
-- = When to use 'optimize'
--
-- Optimizing documents takes some time to do and traverses the entire result
-- document, so it should not be done too often. It is a good idea to 'optimize'
-- static text with many use sites that can be represented by a single text
-- node and share this optimized version,
--
-- >>> oftenUsed = optimize ("a" <> "b" <> pretty 'c' <> "d")
-- >>> putDoc (hsep (replicate 5 oftenUsed))
-- abcd abcd abcd abcd abcd
--
-- but not if the 'optimize' is repeatedly evaluated because it is inside a
-- function called with many parameters, like in
--
-- @
-- x = 'nesting' (\\i -> 'optimize' (…i…))
-- @
--
-- Here, every use of @x@ would re-optimize the computed document each time the
-- layouter tries to use @x@. This applies even when the 'nesting' is inside the
-- 'optimize', like in
--
-- @
-- y = 'optimize' ('nesting' (\\i -> …i…))
-- @
--
-- So take-away message is: 'optimize' only things that contain lots of strings,
-- no computations, and share the result.
optimize :: Doc -> Doc
optimize = \case

    -- TODO: This does not optimize Cat xs (Cat Empty Empty) to xs yet.

    -- Remove empty documents
    Cat Empty x -> optimize x
    Cat x Empty -> optimize x

    -- Associate to the right, optimize aggressively
    Cat (Cat x y) z -> optimize (Cat (optimize x) (optimize (Cat y z)))

    -- Fuse consecutive text elements
    Cat (Char c) (Cat (Char c') x) -> optimize (Cat (Text (T.pack [c, c'])) x)
    Cat (Text t) (Cat (Char c') x) -> optimize (Cat (Text (T.snoc t c')) x)
    Cat (Text t) (Cat (Text t') x) -> optimize (Cat (Text (t <> t')) x)
    Cat x y@Cat{}                  -> Cat x (optimize y)
    Cat x y                        -> Cat (optimize x) (optimize y)
    -- quchen: I tried improving this by using {lazy text, text builders} here,
    --         but this was not good for performance.

    -- Fuse consecutive nestings
    Nest !i (Nest j x) -> optimize (Nest (i+j) x)
    Nest  i x          -> Nest i (optimize x)

    StylePush _ Empty -> Empty
    StylePush s x     -> StylePush s (optimize x)

    -- Don't optimize, recurse
    FlatAlt x1 x2 -> FlatAlt (optimize x1) (optimize x2)
    Union x1 x2   -> Union (optimize x1) (optimize x2)
    Column f      -> Column (optimize . f)
    PageWidth f   -> PageWidth (optimize . f)
    Nesting f     -> Nesting (optimize . f)

    -- Don't optimize
    x@Fail{}  -> x
    x@Empty{} -> x
    x@Char{}  -> x
    x@Text{}  -> x
    x@Line{}  -> x

    StylePop -> error "StylePop should only appear during layouting, never during optimization"



-- List of nesting level/document pairs yet to be rendered. Saves one
-- indirection over [(Int, Doc)].
data Docs = Nil | Cons !Int Doc Docs

-- | This is the default pretty printer which is used by 'show', 'putDoc' and
-- 'hPutDoc'. @(layoutPretty ribbonfrac width x)@ layouts document @x@ with a
-- page width of @width@ and a ribbon width of @(ribbonfrac * width)@
-- characters. The ribbon width is the maximal amount of non-indentation
-- characters on a line. The parameter @ribbonfrac@ should be between @0.0@ and
-- @1.0@. If it is lower or higher, the ribbon width will be 0 or @width@
-- respectively.
layoutPretty :: Float -> Int -> Doc -> SimpleDoc
layoutPretty = layoutFits fits1

-- | A slightly smarter layouting algorithm with more lookahead. It provides
-- earlier breaking on deeply nested structures For example, consider this
-- python-ish pseudocode:
--
-- @fun(fun(fun(fun(fun([abcdefg, abcdefg])))))@
--
-- If we put a 'softline'' (+ 'nest' 2) after each open parenthesis, and align
-- the elements of the list to match the opening brackets, this will layout with
-- @'layoutPretty'@ and a page width of 20 as:
--
-- @
-- fun(fun(fun(fun(fun([
--                     | abcdef,
--                     | abcdef,
--                     ]
--   )))))             |
-- @
--
-- Where the 20 character boundary has been marked with @|@. Because
-- @'layoutPretty'@ only uses only one line lookahead, it sees that the first
-- line fits, and is stuck putting the second and third lines after the 20
-- character mark. In contrast, @'layoutSmart'@ will continue to check the
-- potential document up to the end of the indentation level. Thus, it will
-- format the document as:
--
-- @
-- fun(                |
--   fun(              |
--     fun(            |
--       fun(          |
--         fun([       |
--               abcdef,
--               abcdef,
--             ]       |
--   )))))             |
-- @
--
-- This fits within the 20 character boundary.
layoutSmart :: Float -> Int -> Doc -> SimpleDoc
layoutSmart = layoutFits fitsR

layoutFits
    :: FittingPredicate
    -> Float -- ^ Ribbon fraction, typically around @0.5@
    -> Int   -- ^ Page width, often @80@
    -> Doc
    -> SimpleDoc
layoutFits (FP fits) rfrac maxColumns doc = best 0 0 (Cons 0 doc Nil)
  where
    ribbonWidth = max 0 (min maxColumns (round (fromIntegral maxColumns * rfrac)))

    -- * current column >= current nesting level
    -- * current column - current indentaion = number of chars inserted in line
    best
        :: Int -- ^ Current nesting level
        -> Int -- ^ Current column, i.e. "where the cursor is"
        -> Docs -- ^ Documents remaining to be handled (in order)
        -> SimpleDoc
    best _ _ Nil = SEmpty
    best !nl !cc (Cons !i d ds) = case d of
        Fail          -> SFail
        Empty         -> best nl cc ds
        Char c        -> SChar c (best nl (cc + 1) ds)
        Text t        -> SText t (best nl (cc + T.length t) ds)
        Line          -> SLine i (best i i ds)
        FlatAlt x _   -> best nl cc (Cons i x ds)
        Cat x y       -> best nl cc (Cons i x (Cons i y ds))
        Nest j x      -> best nl cc (Cons (i+j) x ds)
        Union x y     -> let x' = best nl cc (Cons i x ds)
                             y' = best nl cc (Cons i y ds)
                         in selectNicer nl cc x' y'
        Column f      -> best nl cc (Cons i (f cc) ds)
        PageWidth f   -> best nl cc (Cons i (f (Just maxColumns)) ds)
        Nesting f     -> best nl cc (Cons i (f i) ds)
        StylePush s x -> SStylePush s (best nl cc (Cons i x (Cons i StylePop ds)))
        StylePop      -> SStylePop (best nl cc ds)

    selectNicer
        :: Int       -- ^ Indentation of current line
        -> Int       -- ^ Current column
        -> SimpleDoc -- ^ Choice A. Invariant: first lines must be longer than B's.
        -> SimpleDoc -- ^ Choice B.
        -> SimpleDoc -- ^ The nicer one among A and B, depending on which one
                     --   fits better.
    selectNicer lineIndent currentColumn x y
      | fits maxColumns minNestingLevel availableWidth x = x
      | otherwise = y
      where
        minNestingLevel = min lineIndent currentColumn
        availableWidth
          = let columnsLeftInLine   = maxColumns - currentColumn
                columnsLeftInRibbon = lineIndent + ribbonWidth - currentColumn
            in min columnsLeftInLine columnsLeftInRibbon

-- | Decide whether a 'SimpleDoc' fits the constraints given, namely
--
--   - page width
--   - minimum nesting level to fit in
--   - width in which to fit the first line
newtype FittingPredicate = FP (Int -> Int -> Int -> SimpleDoc -> Bool)

-- | @fits1@ does 1 line lookahead.
fits1 :: FittingPredicate
fits1 = FP (\_p _m w -> go w)
  where
    go :: Int -- ^ Width in which to fit the first line
       -> SimpleDoc
       -> Bool
    go w _ | w < 0        = False
    go _ SFail            = False
    go _ SEmpty           = True
    go w (SChar _ x)      = go (w - 1) x
    go w (SText t x)      = go (w - T.length t) x
    go _ SLine{}          = True
    go w (SStylePush _ x) = go w x
    go w (SStylePop x)    = go w x

-- | @fitsR@ has a little more lookahead: assuming that nesting roughly
-- corresponds to syntactic depth, @fitsR@ checks that not only the current line
-- fits, but the entire syntactic structure being formatted at this level of
-- indentation fits. If we were to remove the second case for @SLine@, we would
-- check that not only the current structure fits, but also the rest of the
-- document, which would be slightly more intelligent but would have exponential
-- runtime (and is prohibitively expensive in practice).
fitsR :: FittingPredicate
fitsR = FP go
  where
    go :: Int -- ^ Page width
       -> Int -- ^ Minimum nesting level to fit in
       -> Int -- ^ Width in which to fit the first line
       -> SimpleDoc
       -> Bool
    go _ _ w _
      | w < 0                 = False
    go _ _ _ SFail            = False
    go _ _ _ SEmpty           = True
    go p m w (SChar _ x)      = go p m (w - 1) x
    go p m w (SText t x)      = go p m (w - T.length t) x
    go p m _ (SLine i x)
      | m < i                 = go p m (p - i) x
      | otherwise             = True
    go p m w (SStylePush _ x) = go p m w x
    go p m w (SStylePop x)    = go p m w x

-- | @(layoutCompact x)@ layouts document @x@ without adding any indentation.
-- Since no \'pretty\' printing is involved, this layouter is very fast. The
-- resulting output contains fewer characters than a pretty printed version and
-- can be used for output that is read by other programs.
--
-- This layouting function does not add any colorisation information.
layoutCompact :: Doc -> SimpleDoc
layoutCompact doc = scan 0 [doc]
  where
    scan _ [] = SEmpty
    scan !k (d:ds) = case d of
        Fail          -> SFail
        Empty         -> scan k ds
        Char c        -> SChar c (scan (k+1) ds)
        Text t        -> SText t (scan (k + T.length t) ds)
        FlatAlt x _   -> scan k (x:ds)
        Line          -> SLine 0 (scan 0 ds)
        Cat x y       -> scan k (x:y:ds)
        Nest _ x      -> scan k (x:ds)
        Union _ y     -> scan k (y:ds)
        Column f      -> scan k (f k:ds)
        PageWidth f   -> scan k (f Nothing:ds)
        Nesting f     -> scan k (f 0:ds)
        StylePush _ x -> scan k (x:ds)
        StylePop      -> scan k ds

instance Show Doc where
    showsPrec _ doc = displayString (layoutPretty 0.4 80 doc)

displayString :: SimpleDoc -> ShowS
displayString = \case
    SFail          -> error "@SFail@ can not appear uncaught in a layouted @SimpleDoc@"
    SEmpty         -> id
    SChar c x      -> showChar c . displayString x
    SText t x      -> showString (T.unpack t) . displayString x
    SLine i x      -> showString ('\n':replicate i ' ') . displayString x
    SStylePush _ x -> displayString x
    SStylePop x    -> displayString x


-- $migration
--
-- If you're already familiar with (ansi-)wl-pprint, you'll recognize many
-- functions in this module, and they work just the same way. However, a couple
-- of definitions are missing:
--
--   - @char@, @string@, @double@, … – these are all special cases of the
--     overloaded @'pretty'@ function.
--   - @\<$>@, @\<$$>@, @\</>@, @\<//>@ are special cases of
--     @'vsep'@, @'vcat'@, @'fillSep'@, @'fillCat'@ with only two documents.
--   - If you need 'String' output, use 'T.unpack' on the generated renderings.
--   - The /display/ functions are moved to the rendering submodules.
--   - The /render/ functions are called /layout/ functions.



-- $history
--
-- This module is based on previous work by Daan Leijen and Max Bolingbroke, who
-- implemented and significantly extended the prettyprinter given by a paper by
-- Phil Wadler in his 1997 paper "A Prettier Printer", by adding lots of
-- convenience functions, styling, and new functionality. Their package,
-- <http:/hackage.haskell.org/package/ansi-wl-pprint ansi-wl-pprint> is widely
-- used in the Haskell ecosystem.
--
-- However, ansi-wl-pprint is showing its age, resulting in a couple of issues:
--
--   - Definitions clashing with others that are now standard Haskell, such as
--     @\<$>@
--   - Hard to read operators, such as @\<//>@
--   - Some undocumented definitions, not many examples
--   - Based on 'String'
--
-- This modified package addresses and modernizes these issues:
--
--   - No clashing definitions
--   - All but the essential @'<>'@ and @'<+>'@ operators removed
--   - Everything extensively documented, with references to other functions and
--     runnable code examples
--   - 'Text' instead of 'String'



-- $algebra
--
-- The functions in this library satisfy many algebraic laws.
--
-- The 'text' function is a homomorphism from text concatenation to document
-- concatenation:
--
-- @
-- 'text' (s '<>' t) = 'text' s '<>' 'text' t
-- 'text' 'mempty' = 'mempty'
-- @
--
-- The 'char' function behaves like one-element text:
--
-- @
-- 'char' c = 'text' ('T.singleton' c)
-- @
--
-- The 'nest' function is a homomorphism from addition to document composition.
-- 'nest' also distributes through document concatenation and is absorbed by
-- 'text' (without newlines) and 'align':
--
-- @
-- 'nest' (i '+' j) x = 'nest' i ('nest' j x)
-- 'nest' 0 x = x
-- 'nest' i (x '<>' y) = 'nest' i x '<>' 'nest' i y
-- 'nest' i 'mempty' = 'mempty'
-- 'nest' i ('text' t) = 'text' t -- no newline in t
-- 'nest' i ('align' x) = 'align' x
-- @
--
-- The 'group' function is absorbed by 'mempty'. 'group' is commutative with
-- 'nest' and 'align':
--
-- @
-- 'group' 'mempty' = 'mempty'
-- 'group' ('text' s '<>' x) = 'text' s '<>' 'group' x
-- 'group' ('nest' i x) = 'nest' i ('group' x)
-- 'group' ('align' x) = 'align' ('group' x)
-- @
--
-- The 'align' function is absorbed by 'mempty' and 'text'. 'align' is
-- idempotent:
--
-- @
-- 'align' 'mempty' = 'mempty'
-- 'align' ('text' s) = 'text' s
-- 'align' ('align' x) = 'align' x
-- @
--
-- From the laws of the primitive functions, we can derive many other laws for
-- the derived functions. For example, the /spaced/ operator '<+>' is defined
-- as:
--
-- @
-- x '<+>' y = x '<>' 'space' '<>' y
-- @
--
-- It follows that '<+>' is associative and that '<+>' and '<>' associate with
-- each other:
--
-- @
-- x '<+>' (y '<+>' z) = (x '<+>' y) '<+>' z
-- x '<>' (y '<+>' z) = (x '<>' y) '<+>' z
-- x '<+>' (y <> z) = (x '<+>' y) '<>' z
-- @
