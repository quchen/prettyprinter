{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.ANSI.Leijen
-- Copyright   :  Daan Leijen (c) 2000, http://www.cs.uu.nl/~daan
--                Max Bolingbroke (c) 2008, http://blog.omega-prime.co.uk
--                David Luposchainsky (c) 2016, http://github.io/quchen
-- License     :  BSD-style (see the file LICENSE.md)
--
-- Maintainer  :  David Luposchainsky <dluposchainsky at google>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines a prettyprinter to format text in a flexible and
-- convenient way.
--
-- It is based on previous work by Daan Leijen and Max Bolingbroke, who
-- implemented and significantly extended the prettyprinter given by a paper by
-- Phil Wadler in his 1997 paper "A Prettier Printer", by adding lots of
-- convenience functions, styling, and new functionality. Their package,
-- <http:/hackage.haskell.org/package/ansi-wl-pprint ansi-wl-pprint>/ is widely
-- used in the Haskell ecosystem.
--
-- However, ansi-wl-pprint is showing its age, resulting in several
-- shortcomings:
--
--   - Definitions clashing with others that are now standard Haskell, such as
--     @\<$>@
--   - Hard to read operators, such as @\<//>@
--   - Some undocumented definitions, not many examples
--   - Based on 'String' instead of 'Text'

module Data.Text.PrettyPrint.Doc (
    -- * The algebra of pretty-printing
    -- $DocumentAlgebra

    -- * Documents
    Doc,

    -- * Basic functions
    char, text, nest, line, line', group, softline, softline', hardline,

    -- * Alignment functions
    --
    -- | The functions in this section cannot be described by Wadler's original
    -- functions. They align their output relative to the current output
    -- position — in contrast to @nest@ which always aligns to the current
    -- nesting level. This deprives these functions from being \`optimal\'. In
    -- practice however they prove to be very useful. The functions in this
    -- section should be used with care, since they are more expensive than the
    -- other functions. For example, @align@ shouldn't be used to pretty print
    -- all top-level declarations of a language, but using @hang@ for let
    -- expressions is fine.
    align, hang, indent, encloseSep, list, tupled,

    -- * Operators
    (<>), (<+>),

    -- * List functions
    hsep, vsep, fillSep, sep, vcat, fillCat, cat, punctuate,

    -- * Reactive/conditional rendering
    column, columns, nesting, width,

    -- * Filler functions
    fill, fillBreak,

    -- * Bracketing functions
    enclose, squotes, dquotes, parens, angles, braces, brackets,

    -- * Named character functions
    lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket, squote,
    dquote, semi, colon, comma, space, dot, backslash, equals,

    -- * Styling

    Style(..), SColor(..), SIntensity(..), SLayer(..),

    -- ** Forecolor functions
    black, red, green, yellow, blue, magenta, cyan, white, dullblack, dullred,
    dullgreen, dullyellow, dullblue, dullmagenta, dullcyan, dullwhite,

    -- ** Backcolor functions
    onblack, onred, ongreen, onyellow, onblue, onmagenta, oncyan, onwhite,
    ondullblack, ondullred, ondullgreen, ondullyellow, ondullblue,
    ondullmagenta, ondullcyan, ondullwhite,

    -- ** Font style functions
    bold, italics, underline,

    -- ** Formatting elimination functions
    plain,

    -- * Pretty class
    Pretty(..),

    -- * Rendering and displaying documents

    -- ** Simple (i.e., rendered) documents
    SimpleDoc(..),
    renderPretty, renderCompact, renderSmart,

) where

import           Data.Monoid
import qualified Data.Semigroup as Semi (Semigroup ((<>)))
import           Data.String    (IsString (..))
import           Data.Text      (Text)
import qualified Data.Text      as T



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text.PrettyPrint.Doc.Display.Terminal



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

-- | @('encloseSep' l r sep xs)@ concatenates the documents @xs@ separated by
-- @sep@, and encloses the resulting document by @l@ and @r@. The documents are
-- rendered horizontally if that fits the page, otherwise they are aligned
-- vertically. All separators are put in front of the elements.
--
-- >>> let doc = encloseSep lbracket rbracket comma (map pretty [1,20,300,4000])
-- >>> putDocW 80 ("list" <+> doc)
-- list [1,20,300,4000]
-- >>> putDocW 40 ("list" <+> doc)
-- list [1
--      ,20
--      ,300
--      ,4000]
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



-- | @('punctuate' p xs)@ appends @p@ to all but the last document in @xs@.
--
-- >>> let docs = map text (T.words "lorem ipsum dolor sit amet")
-- >>> putDocW 80 (cat (punctuate comma docs))
-- lorem,ipsum,dolor,sit,amet
-- >>> putDocW 20 (cat (punctuate comma docs))
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


-- | @'sep' xs@ tries rendering the documents @xs@ separated with 'space's, and
-- if this does not fit the page, separates them with newlines.
--
-- In other words, @'sep'@ is @'vsep'@ with @'group'@ baked in.
--
-- >>> let doc = "some" <+> sep ["text", "to", "lay", "out"]
-- >>> putDocW 80 doc
-- some text to lay out
-- >>> putDocW 20 doc
-- some text
-- to
-- lay
-- out
sep :: [Doc] -> Doc
sep = group . vsep

-- | @('fillSep' xs)@ concatenates the documents @xs@ horizontally with @'<+>'@
-- as long as it fits the page, then inserts a @'line'@ and continues doing that
-- for all documents in @xs@.
--
-- (@'line'@ means that if 'group'ed, the documents are separated with a 'space'
-- instead of newlines.)
--
-- >>> let docs = map text (T.words "lorem ipsum dolor sit amet")
-- >>> putDocW 80 (fillSep docs)
-- lorem ipsum dolor sit amet
-- >>> putDocW 32 (fillSep docs)
-- lorem ipsum
-- dolor sit
-- amet
-- >>> putDocW 80 ("Docs:" <+> group (fillSep docs))
-- Docs: lorem ipsum dolor sit amet
fillSep :: [Doc] -> Doc
fillSep = concatWith (\x y -> x <> softline <> y)

-- | @('hsep' xs)@ concatenates all documents @xs@ horizontally with @'<+>'@.
--
-- >>> let docs = map text (T.words "lorem ipsum dolor sit amet")
-- >>> putDoc (hsep docs)
-- lorem ipsum dolor sit amet
hsep :: [Doc] -> Doc
hsep = concatWith (<+>)

-- | @('vsep' xs)@ concatenates all documents @xs@ above each other. If a
-- 'group' undoes the line breaks inserted by @vsep@, the documents are
-- separated with a space instead.
--
-- Using 'vsep' alone yields
--
-- >>> putDoc ("some" <+> vsep (["text", "to", "lay", "out"]))
-- some text
-- to
-- lay
-- out
--
-- 'group'ing a 'vsep' separates the documents with a space if it fits the page
-- (and does nothing otherwise). See the @'sep'@ convenience function for this
-- use case.
--
-- The 'align' function can be used to align the documents under their first
-- element:
--
-- >>> putDoc ("some" <+> align (vsep (["text", "to", "lay", "out"])))
-- some text
--      to
--      lay
--      out
vsep :: [Doc] -> Doc
vsep = concatWith above

-- | @('cat' xs)@ tries rendering the documents @xs@ separated with nothing, and
-- if this does not fit the page, separates them with newlines.
--
-- In other words, @'sep'@ is @'vsep'@ with @'group'@ baked in, just like
-- @'sep'@ is to @'vsep'@.
--
-- >>> let docs = map text (T.words "lorem ipsum dolor")
-- >>> putDocW 80 ("Docs:" <+> cat docs)
-- Docs: loremipsumdolor
-- >>> putDocW 40 ("Docs:" <+> cat docs)
-- Docs: lorem
-- ipsum
-- dolor
cat :: [Doc] -> Doc
cat = group . vcat

-- | @('fillCat' xs)@ concatenates documents @xs@ horizontally with @'<>'@ as
-- long as it fits the page, then inserts a @'line''@ and continues doing that
-- for all documents in @xs@. This is similar to how an ordinary word processor
-- lays out the text if you just keep typing after you hit the maximum line
-- length.
--
-- (@'line''@ means that if 'group'ed, the documents are separated with nothing
-- instead of newlines.)
--
-- >>> let docs = map text (T.words "lorem ipsum dolor")
-- >>> putDocW 80 ("Docs:" <+> fillCat docs)
-- Docs: loremipsumdolor
-- >>> putDocW 40 ("Docs:" <+> fillCat docs)
-- Docs: loremipsum
-- dolor
-- >>> putDocW 80 ("Docs:" <+> group (fillCat docs))
-- Docs: loremipsumdolor
fillCat :: [Doc] -> Doc
fillCat = concatWith (\x y -> x <> softline' <> y)

-- | @(hcat xs)@ concatenates all documents @xs@ horizontally with @(\<\>)@.
--
-- >>> let docs = map text (T.words "lorem ipsum dolor")
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
-- >>> let docs = map text (T.words "lorem ipsum dolor")
-- >>> putDocW 80 (vcat docs)
-- lorem
-- ipsum
-- dolor
-- >>> putDocW 40 (group (vcat docs))
-- loremipsumdolor
vcat :: [Doc] -> Doc
vcat = concatWith above'

-- | 'foldr', with an empty special case for empty arguments.
concatWith :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
concatWith _ [] = mempty
concatWith f ds = foldr1 f ds

-- | @(x '<+>' y)@ concatenates document @x@ and @y@ with a @space@ in between.
--
-- >>> putDoc ("hello" <+> "world")
-- hello world
(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> space <> y

-- | @x `above` y@ concatenates @x@ and @y@ with a 'line' in between.
above :: Doc -> Doc -> Doc
above x y = x <> line <> y

-- | @x `above'` y@ concatenates document @x@ and @y@ with a @line'@ in
-- between.
above' :: Doc -> Doc -> Doc
above' x y = x <> line' <> y

-- | @softline@ behaves like @'space'@ if the resulting output fits the page,
-- otherwise like @'line'@.
--
-- >>> putDocW 80 ("lorem ipsum" <> softline <> "dolor sit amet")
-- lorem ipsum dolor sit amet
-- >>> putDocW 40 ("lorem ipsum" <> softline <> "dolor sit amet")
-- lorem ipsum
-- dolor sit amet
softline :: Doc
softline = group line

-- | @'softline''@ is like @'softline'@, but behaves like @'mempty'@ if the
-- resulting output does not fit on the page (instead of @'space'@).
--
-- In other words, @'line'@ is to @'line''@ how @'softline'@ is to
-- @'softline''@.
--
-- >>> putDocW 80 ("lorem ipsum" <> softline' <> "dolor sit amet")
-- lorem ipsumdolor sit amet
-- >>> putDocW 40 ("lorem ipsum" <> softline' <> "dolor sit amet")
-- lorem ipsum
-- dolor sit amet
softline' :: Doc
softline' = group line'

-- | >>> putDoc (squotes "…")
-- '…'
squotes :: Doc -> Doc
squotes = enclose squote squote

-- | >>> putDoc (dquotes "…")
-- "…"
dquotes :: Doc -> Doc
dquotes = enclose dquote dquote

-- | >>> putDoc (braces "…")
-- {…}
braces :: Doc -> Doc
braces = enclose lbrace rbrace

-- | >>> putDoc (parens "…")
-- (…)
parens :: Doc -> Doc
parens = enclose lparen rparen

-- | >>> putDoc (angles "…")
-- <…>
angles :: Doc -> Doc
angles = enclose langle rangle

-- | >>> putDoc (brackets "…")
-- […]
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

-- | @('enclose' l r x)@ encloses document @x@ between documents @l@ and @r@
-- using @'<>'@.
--
-- >>> putDoc (enclose "AAA" "ZZZ" "…")
-- AAA…ZZZ
enclose :: Doc -> Doc -> Doc -> Doc
enclose l r x = l <> x <> r

-- | >>> putDoc lparen
-- (
lparen :: Doc
lparen = char '('
-- | >>> putDoc rparen
-- )
rparen :: Doc
rparen = char ')'
-- | >>> putDoc langle
-- <
langle :: Doc
langle = char '<'
-- | >>> putDoc rangle
-- >
rangle :: Doc
rangle = char '>'
-- | >>> putDoc lbrace
-- {
lbrace :: Doc
lbrace = char '{'
-- | >>> putDoc rbrace
-- }
rbrace :: Doc
rbrace = char '}'
-- | >>> putDoc lbracket
-- [
lbracket :: Doc
lbracket = char '['
-- | >>> putDoc rbracket
-- ]
rbracket :: Doc
rbracket = char ']'

-- | >>> putDoc squote
-- '
squote :: Doc
squote = char '\''
-- | >>> putDoc dquote
-- "
dquote :: Doc
dquote = char '"'
-- | >>> putDoc semi
-- ;
semi :: Doc
semi = char ';'
-- | >>> putDoc colon
-- :
colon :: Doc
colon = char ':'
-- | >>> putDoc comma
-- ,
comma :: Doc
comma = char ','
-- | >>> putDoc ("a" <> space <> "b")
-- a b
--
-- This is mostly used via @'<+>'@,
--
-- >>> putDoc ("a" <+> "b")
-- a b
space :: Doc
space = char ' '
-- | >>> putDoc dot
-- .
dot :: Doc
dot = char '.'
-- | >>> putDoc backslash
-- \
backslash :: Doc
backslash = char '\\'
-- | >>> putDoc equals
-- =
equals :: Doc
equals = char '='

-- | @('text' t)@ concatenates all characters in @t@ using @line@ for newline
-- characters and @char@ for all other characters. The 'IsString' instance of
-- 'Doc' uses this function.
--
-- >>> putDoc "hello\nworld"
-- hello
-- world
text :: Text -> Doc
text t = case T.uncons t of
    Nothing -> Empty
    Just ('\n', rest) -> line <> text rest
    _otherwise -> vsep (map unsafeText (T.splitOn "\n" t))



-----------------------------------------------------------
-- overloading "pretty"
-----------------------------------------------------------

-- | The member @'prettyList'@ is only used to define the @instance
-- 'Pretty' a => 'Pretty' [a]@. In normal circumstances only the @'pretty'@
-- function is used.
class Pretty a where

    -- | >>> putDoc (pretty 1 <+> pretty "hello" <+> pretty 1.234)
    -- 1 hello 1.234
    pretty :: a -> Doc

    -- | >>> putDoc (prettyList [1, 23, 456])
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
instance Pretty () where
    pretty _ = "()"

-- | >>> putDoc (pretty True)
-- True
instance Pretty Bool where
    pretty = unsafeText . T.pack . show

-- | >>> putDoc (pretty 'c')
-- c
-- >>> putDoc (pretty ("string" :: [Char]))
-- string
instance Pretty Char where
    pretty = unsafeText . T.singleton
    prettyList = text . fromString

-- | >>> putDoc (pretty (123 :: Int))
-- 123
instance Pretty Int where
    pretty = unsafeText . T.pack . show

-- | >>> putDoc (pretty (2^123 :: Integer))
-- 10633823966279326983230456482242756608
instance Pretty Integer where
    pretty = unsafeText . T.pack . show

-- | >>> putDoc (pretty (123e4 :: Float))
-- 1230000.0
instance Pretty Float where
    pretty = unsafeText . T.pack . show

-- | >>> putDoc (pretty (123e4 :: Double))
-- 1230000.0
instance Pretty Double where
    pretty = unsafeText . T.pack . show

-- | >>> putDoc (pretty (123, "hello"))
-- (123,hello)
instance (Pretty a,Pretty b) => Pretty (a,b) where
            pretty (x,y) = tupled [pretty x, pretty y]

-- | >>> putDoc (pretty (123, "hello", False))
-- (123,hello,False)
instance (Pretty a,Pretty b,Pretty c) => Pretty (a,b,c) where
                pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

-- | >>> putDoc (pretty (Just True))
-- True
-- >>> putDoc (brackets (pretty (Nothing :: Maybe Bool)))
-- []
instance Pretty a => Pretty (Maybe a) where
    pretty Nothing = mempty
    pretty (Just x) = pretty x



-- | Insert a number of spaces.
spaces :: Int -> Doc
spaces n | n <= 0    = mempty
         | otherwise = unsafeText (T.replicate n " ")

-- | @('fill' i x)@ renders document @x@. It then appends @space@s until the
-- width is equal to @i@. If the width of @x@ is already larger, nothing is
-- appended. This function is quite useful in practice to output a list of
-- bindings. The following example demonstrates this.
--
-- >>> let types = [("empty","Doc"), ("nest","Int -> Doc -> Doc"), ("fillSep","[Doc] -> Doc")]
-- >>> let ptype (name, tp) = fill 5 (text name) <+> "::" <+> text tp
-- >>> putDoc ("let" <+> align (vcat (map ptype types)))
-- let empty :: Doc
--     nest  :: Int -> Doc -> Doc
--     fillSep :: [Doc] -> Doc
fill :: Int -> Doc -> Doc
fill f doc = width doc (\w ->
    if w >= f
        then mempty
        else spaces (f - w))

-- | @('fillBreak' i x)@ first renders document @x@. It then appends @space@s
-- until the width is equal to @i@. If the width of @x@ is already larger than
-- @i@, the nesting level is increased by @i@ and a @line@ is appended. When we
-- redefine @ptype@ in the example given in 'fill' to use @'fillBreak'@, we get
-- a useful variation of the output:
--
-- >>> let types = [("empty","Doc"), ("nest","Int -> Doc -> Doc"), ("fillSep","[Doc] -> Doc")]
-- >>> let ptype (name, tp) = fillBreak 5 (text name) <+> "::" <+> text tp
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

-- | @('width' doc f)@ renders the document 'doc', and makes the width of it
-- available to a function.
--
-- >>> let annotate doc = width (brackets doc) (\w -> " <- width:" <+> pretty w)
-- >>> putDoc (align (vsep (map annotate ["123", "123456", indent 3 "4567", vsep ["123", indent 3 "4567"]])))
-- [123] <- width: 5
-- [123456] <- width: 8
-- [   4567] <- width: 9
-- [123
--    4567] <- width: 8
width :: Doc -> (Int -> Doc) -> Doc
width doc f
  = column (\colStart ->
        doc <> column (\colEnd ->
            f (colEnd - colStart)))

-----------------------------------------------------------
-- semi primitive: Alignment and indentation
-----------------------------------------------------------

-- | @('indent' i x)@ indents document @x@ with @i@ spaces.
--
-- >>> :{
-- putDocW 40 (indent 4 (fillSep (map text
--     (T.words "The indent function indents these words!"))))
-- :}
--     The indent
--     function indents
--     these words!
indent :: Int -> Doc -> Doc
indent i d = hang i (spaces i <> d)

-- | @('hang' i x)@ renders document @x@ with a nesting level set to the current
-- column plus @i@.
--
-- >>> let someWords = map text (T.words "The hang function indents these words!")
-- >>> putDocW 40 (hang 4 (fillSep someWords))
-- The hang
--     function indents
--     these words!
hang :: Int -> Doc -> Doc
hang i d = align (nest i d)

-- | @('align' x)@ renders document @x@ with the nesting level set to the
-- current column. It is used for example to implement 'hang'.
--
-- As an example, we will put a document right above another one, regardless of
-- the current nesting level:
--
-- >>> putDoc ("lorem" <+> (align (vsep ["ipsum", "dolor"])))
-- lorem ipsum
--       dolor
align :: Doc -> Doc
align d = column (\k -> nesting (\i -> nest (k - i) d)) -- nesting might be negative!



-----------------------------------------------------------
-- Primitives
-----------------------------------------------------------

-- | The abstract data type @Doc@ represents pretty documents.
--
-- More specifically, a value of type @Doc@ represents a non-empty set of
-- possible renderings of a document. The rendering functions select one of
-- these possibilities.
--
-- The simplest renderer is via the 'Show' class. @('show' doc)@ pretty prints
-- document @doc@ with a page width of 80 characters and a ribbon width of 32
-- characters.
--
-- >>> putStrLn (show (vsep ["hello", "world"]))
-- hello
-- world
data Doc =
      Fail -- ^ Occurs when flattening a line. It is a bug if this is possible.
    | Empty -- ^ The empty document; unit of 'Cat' (observationally)
    | Char Char -- ^ invariant: char is not '\n'
    | Text Text -- ^ invariant: text doesn't contain '\n'
    | Line -- ^ Line break
    | FlatAlt Doc Doc -- ^ Render the first doc, but when flattened (via group), render the second.
    | Cat Doc Doc -- ^ Concatenation of two documents
    | Nest !Int Doc -- ^ Document indented by a number of columns
    | Union Doc Doc -- ^ invariant: first lines of first doc longer than the first lines of the second doc
    | Column  (Int -> Doc)
    | Columns (Maybe Int -> Doc)
    | Nesting (Int -> Doc)
    | Style Style Doc -- ^ Add style information
    | UnStyle -- ^ Remove one 'Style' information (only used during rendering).
              --   Invariant: each of these must be paired with an 'UnStyle'.

data Style =
      SItalicized
    | SBold
    | SUnderlined
    | SColor SLayer SIntensity SColor

data SColor = SBlack | SRed | SGreen | SYellow | SBlue | SMagenta | SCyan | SWhite
data SIntensity = SVivid | SDull
data SLayer = SForeground | SBackground

-- | The data type @SimpleDoc@ represents rendered documents and is used by the
-- display functions.
--
-- Whereas values of the data type 'Doc' represent non-empty sets of possible
-- renderings of a document, values of the data type @SimpleDoc@ represent
-- single renderings of a document.
--
-- The library provides two default display functions 'displayLazyText' and
-- 'displayIO'. You can provide your own display function by writing a function
-- from a @SimpleDoc@ to your own output format.
data SimpleDoc =
      SFail
    | SEmpty
    | SChar Char SimpleDoc
    | SText Text SimpleDoc
    | SLine !Int SimpleDoc   -- ^ @Int@ = indentation level for the line
    | SStyle Style SimpleDoc -- ^ Style and styled document. Invariant: each of
                             --   these must be paired with a 'SUnStyle'.
    | SUnStyle SimpleDoc     -- ^ Discard one previous style information

-- | Empty document, and direct concatenation (without adding any spacing).
instance Monoid Doc where
    mempty = Empty
    mappend = (Semi.<>)
    mconcat = hcat

instance Semi.Semigroup Doc where
    (<>) = Cat

-- MCB: also added when "pretty" got the corresponding instances:
instance IsString Doc where
    fromString = text . T.pack

-- | @('char' c)@ contains the literal character @c@. If the character is a
-- a newline (@'\n'@), consider using 'line' instead.
char :: Char -> Doc
char '\n' = line
char c = Char c

-- | @('unsafeText' s)@ contains the literal string @s@. The string must not
-- contain any newline (@'\n'@) characters, since this is an invariant of the
-- 'Doc' type. If you're not sure, use the safer 'text'.
unsafeText :: Text -> Doc
unsafeText t = case T.compareLength t 1 of
    LT -> Empty
    EQ -> Char (T.head t)
    GT -> Text t

-- | The @line@ document advances to the next line and indents to the current
-- nesting level.
--
-- @line@ behaves like @'space'@ if the line break is undone by 'group'.
--
-- >>> let doc = "lorem ipsum" <> line <> "dolor sit amet"
-- >>> putDoc doc
-- lorem ipsum
-- dolor sit amet
-- >>> putDoc (group doc)
-- lorem ipsum dolor sit amet
line :: Doc
line = FlatAlt Line space

-- | @'line''@ behaves like @'line'@, but behaves like @'mempty'@ if the line
-- break is undone by 'group' (instead of @'space'@).
--
-- >>> let doc = "lorem ipsum" <> line' <> "dolor sit amet"
-- >>> putDoc doc
-- lorem ipsum
-- dolor sit amet
-- >>> putDoc (group doc)
-- lorem ipsumdolor sit amet
line' :: Doc
line' = FlatAlt Line mempty

-- | A @'hardline'@ is always rendered as a line break, even when 'group'ed.
--
-- >>> let doc = "lorem ipsum" <> hardline <> "dolor sit amet"
-- >>> putDoc doc
-- lorem ipsum
-- dolor sit amet
-- >>> putDoc (group doc)
-- lorem ipsum
-- dolor sit amet
hardline :: Doc
hardline = Line

-- | @('nest' i x)@ renders document @x@ with the current indentation level
-- increased by @i@. See also 'hang', 'align' and 'indent'.
--
-- >>> putDoc (vsep [nest 4 (vsep ["lorem", "ipsum", "dolor"]), "sit", "amet"])
-- lorem
--     ipsum
--     dolor
-- sit
-- amet
nest :: Int -> Doc -> Doc
nest = Nest

-- | Render a document depending on which column it starts being rendered.
-- 'align' is implemented in terms of 'column'.
--
-- >>> let doc = "prefix" <+> column (\l -> brackets ("Column:" <+> pretty l))
-- >>> putDoc (vsep [indent n doc | n <- [0,4,8]])
-- prefix [Column: 7]
--     prefix [Column: 11]
--         prefix [Column: 15]
column :: (Int -> Doc) -> Doc
column = Column

-- | Render a document depending on the 'nest'ing level of the current line.
-- 'align' is implemented in terms of 'nesting'.
--
-- >>> let doc = "prefix" <+> nesting (\l -> brackets ("Nested:" <+> pretty l))
-- >>> putDoc (vsep [indent n doc | n <- [0,4,8]])
-- prefix [Nested: 0]
--     prefix [Nested: 4]
--         prefix [Nested: 8]
nesting :: (Int -> Doc) -> Doc
nesting = Nesting

-- | Render a document depending on the page width.
--
-- >>> let doc = "prefix" <+> columns (\l -> brackets ("Width:" <+> pretty l))
-- >>> putDocW 64 (vsep [indent n doc | n <- [0,4,8]])
-- prefix [Width: 64]
--     prefix [Width: 64]
--         prefix [Width: 64]
--
-- Whether the page width is @'Just' n@ or @Nothing@ depends on the renderer. Of
-- the default renderers, @'renderCompact'@ uses @'Nothing'@, and all others
-- @'Just' <pagewidth>@.
columns :: (Maybe Int -> Doc) -> Doc
columns = Columns

-- | @('group' x)@ undoes all line breaks in document @x@. The resulting line is
-- added to the current line if that fits the page. Otherwise, the document @x@
-- is rendered without any changes.
--
-- See 'vcat' and 'line' for examples.
group :: Doc -> Doc
group x = Union (flatten x) x

flatten :: Doc -> Doc
flatten = \case
    FlatAlt _ y   -> y
    Cat x y       -> Cat (flatten x) (flatten y)
    Nest i x      -> Nest i (flatten x)
    Line          -> Fail
    Union x _     -> flatten x
    Column f      -> Column (flatten . f)
    Columns f     -> Columns (flatten . f)
    Nesting f     -> Nesting (flatten . f)
    Style s x     -> Style s (flatten x)
    other         -> other

-----------------------------------------------------------
-- Colors
-----------------------------------------------------------

black :: Doc -> Doc
black = Style (SColor SForeground SVivid SBlack)
red :: Doc -> Doc
red = Style (SColor SForeground SVivid SRed)
green :: Doc -> Doc
green = Style (SColor SForeground SVivid SGreen)
yellow :: Doc -> Doc
yellow = Style (SColor SForeground SVivid SYellow)
blue :: Doc -> Doc
blue = Style (SColor SForeground SVivid SBlue)
magenta :: Doc -> Doc
magenta = Style (SColor SForeground SVivid SMagenta)
cyan :: Doc -> Doc
cyan = Style (SColor SForeground SVivid SCyan)
white :: Doc -> Doc
white = Style (SColor SForeground SVivid SWhite)
dullblack :: Doc -> Doc
dullblack = Style (SColor SForeground SDull SBlack)
dullred :: Doc -> Doc
dullred = Style (SColor SForeground SDull SRed)
dullgreen :: Doc -> Doc
dullgreen = Style (SColor SForeground SDull SGreen)
dullyellow :: Doc -> Doc
dullyellow = Style (SColor SForeground SDull SYellow)
dullblue :: Doc -> Doc
dullblue = Style (SColor SForeground SDull SBlue)
dullmagenta :: Doc -> Doc
dullmagenta = Style (SColor SForeground SDull SMagenta)
dullcyan :: Doc -> Doc
dullcyan = Style (SColor SForeground SDull SCyan)
dullwhite :: Doc -> Doc
dullwhite = Style (SColor SForeground SDull SWhite)

onblack :: Doc -> Doc
onblack = Style (SColor SBackground SVivid SBlack)
onred :: Doc -> Doc
onred = Style (SColor SBackground SVivid SRed)
ongreen :: Doc -> Doc
ongreen = Style (SColor SBackground SVivid SGreen)
onyellow :: Doc -> Doc
onyellow = Style (SColor SBackground SVivid SYellow)
onblue :: Doc -> Doc
onblue = Style (SColor SBackground SVivid SBlue)
onmagenta :: Doc -> Doc
onmagenta = Style (SColor SBackground SVivid SMagenta)
oncyan :: Doc -> Doc
oncyan = Style (SColor SBackground SVivid SCyan)
onwhite :: Doc -> Doc
onwhite = Style (SColor SBackground SVivid SWhite)
ondullblack :: Doc -> Doc
ondullblack = Style (SColor SBackground SDull SBlack)
ondullred :: Doc -> Doc
ondullred = Style (SColor SBackground SDull SRed)
ondullgreen :: Doc -> Doc
ondullgreen = Style (SColor SBackground SDull SGreen)
ondullyellow :: Doc -> Doc
ondullyellow = Style (SColor SBackground SDull SYellow)
ondullblue :: Doc -> Doc
ondullblue = Style (SColor SBackground SDull SBlue)
ondullmagenta :: Doc -> Doc
ondullmagenta = Style (SColor SBackground SDull SMagenta)
ondullcyan :: Doc -> Doc
ondullcyan = Style (SColor SBackground SDull SCyan)
ondullwhite :: Doc -> Doc
ondullwhite = Style (SColor SBackground SDull SWhite)

bold :: Doc -> Doc
bold = Style SBold

italics :: Doc -> Doc
italics = Style SItalicized

underline :: Doc -> Doc
underline = Style SUnderlined

-----------------------------------------------------------
-- Removing formatting
-----------------------------------------------------------

-- | Removes all colorisation, emboldening and underlining from a document
plain :: Doc -> Doc
plain = \case
    Fail        -> Fail
    e@Empty     -> e
    c@(Char _)  -> c
    t@(Text _)  -> t
    l@Line      -> l
    FlatAlt x y -> FlatAlt (plain x) (plain y)
    Cat x y     -> Cat (plain x) (plain y)
    Nest i x    -> Nest i (plain x)
    Union x y   -> Union (plain x) (plain y)
    Column f    -> Column (plain . f)
    Columns f   -> Columns (plain . f)
    Nesting f   -> Nesting (plain . f)
    Style _ x   -> plain x
    UnStyle     -> mempty

-----------------------------------------------------------
-- Renderers
-----------------------------------------------------------

-----------------------------------------------------------
-- renderPretty: the default pretty printing algorithm
-----------------------------------------------------------

-- list of indentation/document pairs; saves an indirection over [(Int,Doc)]
data Docs = Nil
          | Cons !Int Doc Docs

-- | This is the default pretty printer which is used by 'show', 'putDoc' and
-- 'hPutDoc'. @(renderPretty ribbonfrac width x)@ renders document @x@ with a
-- page width of @width@ and a ribbon width of @(ribbonfrac * width)@
-- characters. The ribbon width is the maximal amount of non-indentation
-- characters on a line. The parameter @ribbonfrac@ should be between @0.0@ and
-- @1.0@. If it is lower or higher, the ribbon width will be 0 or @width@
-- respectively.
renderPretty :: Float -> Int -> Doc -> SimpleDoc
renderPretty = renderFits fits1

-- | A slightly smarter rendering algorithm with more lookahead. It provides
-- provide earlier breaking on deeply nested structures For example, consider
-- this python-ish pseudocode:
--
-- @fun(fun(fun(fun(fun([abcdefg, abcdefg])))))@
--
-- If we put a softline' (+ nesting 2) after each open parenthesis, and align
-- the elements of the list to match the opening brackets, this will render with
-- @renderPretty@ and a page width of 20 as:
--
-- @
-- fun(fun(fun(fun(fun([
--                     | abcdef,
--                     | abcdef,
--                     ]
--   )))))             |
-- @
--
-- Where the 20c. boundary has been marked with |. Because @renderPretty@ only
-- uses one-line lookahead, it sees that the first line fits, and is stuck
-- putting the second and third lines after the 20-c mark. In contrast,
-- @renderSmart@ will continue to check that the potential document up to the
-- end of the indentation level. Thus, it will format the document as:
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
-- Which fits within the 20c. boundary.
renderSmart :: Float -> Int -> Doc -> SimpleDoc
renderSmart = renderFits fitsR

renderFits
    :: (Int -> Int -> Int -> SimpleDoc -> Bool) -- ^ Fitting predicate, e.g. 'fits1'
    -> Float -- ^ Ribbon fraction
    -> Int   -- ^ Page width, often @80@
    -> Doc
    -> SimpleDoc
renderFits fits rfrac pageWidth doc
    -- I used to do a @SSGR [Reset]@ here, but if you do that it will result in
    -- any rendered @Doc@ containing at least some ANSI control codes. This may
    -- be undesirable if you want to render to non-ANSI devices by simply not
    -- making use of the ANSI color functions I provide.
    --
    -- What I "really" want to do here is do an initial Reset iff there is some
    -- ANSI color within the Doc, but that's a bit fiddly. I'll fix it if
    -- someone complains!
  = best 0 0 (Cons 0 doc Nil)
  where
    ribbonWidth = max 0 (min pageWidth (round (fromIntegral pageWidth * rfrac)))

    -- * current column >= current line's indentation
    -- * current column - current indentaion = number of chars inserted in line
    best
        :: Int -- ^ Current line's indentation
        -> Int -- ^ Current column
        -> Docs -- ^ Documents remaining to be handled (in order)
        -> SimpleDoc
    best _ _ Nil = SEmpty
    best lineIndent currentColumn (Cons i d ds) = case d of
        Fail -> SFail

        -- If the next chunk to convert is empty, we simply continue.
        Empty -> best lineIndent currentColumn ds

        -- To render a character, insert it and increase the column count by
        -- one.
        Char c -> let !col' = currentColumn+1
                  in SChar c (best lineIndent col' ds)

        -- To render text, insert it and increase the column count by the length
        -- of the inserted text. Note that it is an invariant of 'Text' to not
        -- contain any newlines, so we need not worry about wrapping and
        -- resetting the column count.
        Text t -> let !col' = currentColumn+T.length t
                  in SText t (best lineIndent col' ds)

        -- Insert a line break, and reset the current column to the current
        -- indentation level.
        Line -> SLine i (best i i ds)

        -- An unflattened pair of alternatives is simply rendered as the first
        -- alternative.
        FlatAlt x _ -> best lineIndent currentColumn (Cons i x ds)

        -- The concatenation of two documents is expanded to render one after
        -- the other (duh).
        Cat x y -> best lineIndent currentColumn (Cons i x (Cons i y ds))

        -- A nested document is rendered by increasing the indentation index,
        -- and then rendering the contained document.
        Nest j x -> let !i' = i+j
                    in best lineIndent currentColumn (Cons i' x ds)

        -- The union of two documents tries rendering the first, and if this
        -- does not fit the layout constraints, falls back to the second.
        Union x y -> nicest (best lineIndent currentColumn (Cons i x ds))
                            (best lineIndent currentColumn (Cons i y ds))

        -- A column-aware document is rendered by providing the contained
        -- function with the current column.
        Column f -> best lineIndent currentColumn (Cons i (f currentColumn) ds)

        -- A page width aware document is rendered by providing the contained
        -- function with the page width.
        Columns f -> best lineIndent currentColumn (Cons i (f (Just pageWidth)) ds)

        -- A nesting-aware document is rendered by providing the contained
        -- function with the current indentation level.
        Nesting f -> best lineIndent currentColumn (Cons i (f i) ds)

        -- A styled document is rendered by rendering the contained document
        -- with style information added, and appending an 'UnStyle' to revert it
        -- once its scope is left.
        Style s x -> SStyle s (best lineIndent currentColumn (Cons i x (Cons i UnStyle ds)))

        UnStyle -> SUnStyle (best lineIndent currentColumn ds)
      where
        -- Invariant: first lines of A are longer than the first lines of B.
        nicest
            :: SimpleDoc -- ^ Choice A
            -> SimpleDoc -- ^ Choice B
            -> SimpleDoc
        nicest x y
          | fits pageWidth (min lineIndent currentColumn) maxAllowableColumn x = x
          | otherwise = y
          where
            columnsUntilLineFiled = pageWidth - currentColumn
            columnsUntilRibbonFilled = ribbonWidth - currentColumn
            maxAllowableColumn = min columnsUntilLineFiled (columnsUntilRibbonFilled + lineIndent)

-- @fits1@ does 1 line lookahead.
fits1
    :: Int -- ^ Page width
    -> Int -- ^ Minimum nesting level to fit in
    -> Int -- ^ Width in which to fit the first line
    -> SimpleDoc
    -> Bool
fits1 _ _ w _ | w < 0    = False
fits1 _ _ _ SFail        = False
fits1 _ _ _ SEmpty       = True
fits1 p m w (SChar _ x)  = fits1 p m (w - 1) x
fits1 p m w (SText t x)  = fits1 p m (w - T.length t) x
fits1 _ _ _ SLine{}      = True
fits1 p m w (SStyle _ x) = fits1 p m w x
fits1 p m w (SUnStyle x) = fits1 p m w x

-- @fitsR@ has a little more lookahead: assuming that nesting roughly
-- corresponds to syntactic depth, @fitsR@ checks that not only the current line
-- fits, but the entire syntactic structure being formatted at this level of
-- indentation fits. If we were to remove the second case for @SLine@, we would
-- check that not only the current structure fits, but also the rest of the
-- document, which would be slightly more intelligent but would have exponential
-- runtime (and is prohibitively expensive in practice).
fitsR
    :: Int -- ^ Page width
    -> Int -- ^ Minimum nesting level to fit in
    -> Int -- ^ Width in which to fit the first line
    -> SimpleDoc
    -> Bool
fitsR _ _ w _
  | w < 0                = False
fitsR _ _ _ SFail        = False
fitsR _ _ _ SEmpty       = True
fitsR p m w (SChar _ x)  = fitsR p m (w - 1) x
fitsR p m w (SText t x)  = fitsR p m (w - T.length t) x
fitsR p m _ (SLine i x)
  | m < i                = fitsR p m (p - i) x
  | otherwise            = True
fitsR p m w (SStyle _ x) = fitsR p m w x
fitsR p m w (SUnStyle x) = fitsR p m w x

-----------------------------------------------------------
-- renderCompact: renders documents without indentation
--  fast and fewer characters output, good for machines
-----------------------------------------------------------

-- | @(renderCompact x)@ renders document @x@ without adding any indentation.
-- Since no \'pretty\' printing is involved, this renderer is very fast. The
-- resulting output contains fewer characters than a pretty printed version and
-- can be used for output that is read by other programs.
--
-- This rendering function does not add any colorisation information.
renderCompact :: Doc -> SimpleDoc
renderCompact doc = scan 0 [doc]
  where
    scan _ [] = SEmpty
    scan k (d:ds) = case d of
        Fail        -> SFail
        Empty       -> scan k ds
        Char c      -> let !k' = k+1
                       in SChar c (scan k' ds)
        Text t      -> let !k' = k+T.length t
                       in SText t (scan k' ds)
        FlatAlt x _ -> scan k (x:ds)
        Line        -> SLine 0 (scan 0 ds)
        Cat x y     -> scan k (x:y:ds)
        Nest _ x    -> scan k (x:ds)
        Union _ y   -> scan k (y:ds)
        Column f    -> scan k (f k:ds)
        Columns f   -> scan k (f Nothing:ds)
        Nesting f   -> scan k (f 0:ds)
        Style _ x   -> scan k (x:ds)
        UnStyle     -> scan k ds



instance Show Doc where
    showsPrec _ doc = displayString (renderPretty 0.4 80 doc)

displayString :: SimpleDoc -> ShowS
displayString = \case
    SFail      -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
    SEmpty     -> id
    SChar c x  -> showChar c . displayString x
    SText t x  -> showString (T.unpack t) . displayString x
    SLine i x  -> showString ('\n':replicate i ' ') . displayString x
    SStyle _ x -> displayString x
    SUnStyle x -> displayString x
