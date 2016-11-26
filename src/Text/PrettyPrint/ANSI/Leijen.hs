{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.ANSI.Leijen
-- Copyright   :  Daan Leijen (c) 2000, http://www.cs.uu.nl/~daan
--                Max Bolingbroke (c) 2008, http://blog.omega-prime.co.uk
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- This module is an extended implementation of the functional pretty printer
-- given by Philip Wadler (1997):
--
-- @
--      \"A prettier printer\"
--      Draft paper, April 1997, revised March 1998.
--      <http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf>
-- @
--
-- In their bare essence, the functions given by Wadler are not expressive
-- enough to describe some commonly occurring layouts. This library adds new
-- primitives to describe these layouts and works well in practice.
--
-- The library is based on a single way to concatenate documents, which is
-- associative and has both a left and right unit.  This simple design leads to
-- an efficient and short implementation. The simplicity is reflected in the
-- predictable behaviour of the functions which make them easy to use in
-- practice.
--
-- A thorough description of the primitive functions and their implementation
-- can be found in Philip Wadler's paper. The main differences with his original
-- paper are:
--
-- * The nil document is called 'mempty'.
--
-- * The above function is called '<$>'. The operator '</>' is used for soft
--   line breaks.
--
-- * There are three new primitives: 'align', 'fill' and 'fillBreak'. These are
--   very useful in practice.
--
-- * There are many additional useful functions, like 'fillSep' and 'list'.
--
-- * There are two renderers: 'renderPretty' for pretty printing, and
--   'renderCompact' for quickly rendered, compact output more suitable for
--   generating input to other programs.
--
-- * The pretty printing algorithm used by 'renderPretty' extends the algorithm
--   given by Wadler to take into account a \"ribbon width\", i.e., a desired
--   maximum number of non-indentation characters to output on any one line.
--
-- * There are two displayers, 'displayS' for strings and 'displayIO' for
--   file-based output.
--
-- * There is a 'Pretty' class.
--
-- * The implementation uses optimised representations and strictness
--   annotations.
--
-- * The library has been extended to allow formatting text for output to ANSI
--   style consoles. New functions allow control of foreground and background
--   color and the ability to make parts of the text bold or underlined.
--
-----------------------------------------------------------
module Text.PrettyPrint.ANSI.Leijen (
   -- * The algebra of pretty-printing
   -- $DocumentAlgebra

   -- * Documents
   Doc,

   -- * Basic functions
   char, text, (<>), nest, line, line', group, softline, softline',
   hardline,

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
   (<+>),

   -- * List functions
   hsep, vsep, fillSep, sep, vcat, fillCat, cat, punctuate,

   -- * Filler functions
   fill, fillBreak,

   -- * Bracketing functions
   enclose, squotes, dquotes, parens, angles, braces, brackets,

   -- * Named character functions
   lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket, squote,
   dquote, semi, colon, comma, space, dot, backslash, equals,

   -- * ANSI formatting functions
   --
   -- | This terminal formatting functionality is, as far as possible, portable
   -- across platforms with their varying terminals. However, note that to
   -- display ANSI colors and formatting will only be displayed on Windows
   -- consoles if the 'Doc' value is output using the 'putDoc' function or one
   -- of its friends.  Rendering the 'Doc' to a 'String' and then outputing
   -- /that/ will only work on Unix-style operating systems.

   -- ** Forecolor functions
   black, red, green, yellow, blue, magenta, cyan, white, dullblack, dullred,
   dullgreen, dullyellow, dullblue, dullmagenta, dullcyan, dullwhite,

   -- ** Backcolor functions
   onblack, onred, ongreen, onyellow, onblue, onmagenta, oncyan, onwhite,
   ondullblack, ondullred, ondullgreen, ondullyellow, ondullblue, ondullmagenta,
   ondullcyan, ondullwhite,

   -- ** Emboldening functions
   bold, debold,

   -- ** Underlining functions
   underline, deunderline,

   -- ** Formatting elimination functions
   plain,

   -- * Pretty class
   Pretty(..),

   -- * Rendering and displaying documents

   -- ** Simple (i.e., rendered) documents
   SimpleDoc(..),
   renderPretty, renderCompact, renderSmart,
   displayT,
   displayIO,

   -- ** Simultaneous rendering and displaying of documents
   putDoc, hPutDoc, putDocW, hPutDocW,

   -- * Undocumented
   column, columns, nesting, width
   ) where

import System.IO (Handle, hPutChar, stdout)

import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , Underlining (..)
    , hSetSGR
    , setSGRCode
    )

import Data.Maybe  (catMaybes)
import Data.String (IsString (..))

-- NB: if you import more from Data.Semigroup make sure the
--     build-depends version range is still accurate
-- NB2: if you consider re-exporting Semigroup((<>)) take into account
--      that only starting with semigroup-0.8 `infixr 6 <>` was used!
import qualified Data.Semigroup as Semi (Semigroup ((<>)))

import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy.Builder as LTB

infixr 6 <+>



-- $DocumentAlgebra
-- The functions in this library satisfy many algebraic laws.
--
-- The concatenation operator '<>' is associative and has 'mempty' as a left and
-- right unit:
--
--     > x <> (y <> z) = (x <> y) <> z
--     > x <> empty = x
--     > empty <> x = x
--
-- The 'text' function is a homomorphism from string concatenation to document
-- concatenation:
--
--     > text (s ++ t) = text s <> text t
--     > "" = empty
--
-- The 'char' function behaves like one-element text:
--
--     > char c = text [c]
--
-- The 'nest' function is a homomorphism from addition to document
-- composition.  'nest' also distributes through document concatenation and is
-- absorbed by 'text' and 'align':
--
--     > nest (i + j) x = nest i (nest j x)
--     > nest 0 x = x
--     > nest i (x <> y) = nest i x <> nest i y
--     > nest i empty = empty
--     > nest i (text s) = text s
--     > nest i (align x) = align x
--
-- The 'group' function is absorbed by 'mempty'.  'group' is commutative with
-- 'nest' and 'align':
--
--     > group empty = empty
--     > group (text s <> x) = text s <> group x
--     > group (nest i x) = nest i (group x)
--     > group (align x) = align (group x)
--
-- The 'align' function is absorbed by 'mempty' and 'text'. 'align' is
-- idempotent:
--
--     > align empty = empty
--     > align (text s) = text s
--     > align (align x) = align x
--
-- From the laws of the primitive functions, we can derive many other laws for
-- the derived functions.  For example, the /above/ operator '<$>' is defined
-- as:
--
--     > x <$> y = x <> line <> y
--
-- It follows that '<$>' is associative and that '<$>' and '<>' associate with
-- each other:
--
--     > x <$> (y <$> z) = (x <$> y) <$> z
--     > x <> (y <$> z) = (x <> y) <$> z
--     > x <$> (y <> z) = (x <$> y) <> z
--
-- Similar laws also hold for the other line break operators '</>', '<$$>', and
-- '<//>'.

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XLambdaCase



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

-- | @(encloseSep l r sep xs)@ concatenates the documents @xs@ separated by
-- @sep@ and encloses the resulting document by @l@ and @r@.  are rendered
-- horizontally if that fits the page. Otherwise they are aligned vertically.
-- All separators are put in front of the elements. For example, the function
-- 'list' can be defined with @encloseSep@:
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
encloseSep left right sep ds = case ds of
    []  -> left <> right
    [d] -> left <> d <> right
    _   -> align (cat (zipWith (<>) (left : repeat sep) ds) <> right)



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

-- | @(text t)@ concatenates all characters in @t@ using @line@ for newline
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
-- >>> :{
-- putDocW 40 (hang 4 (fillSep (map text
--     (T.words "The hang function indents these words!"))))
-- :}
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
-- >>> show (vsep ["hello", "world"])
-- "\"hello\\nworld\""
data Doc =
      Fail
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
    | Color ConsoleLayer ColorIntensity
            Color Doc -- ^ Introduces coloring /around/ the embedded document
    | Intensify ConsoleIntensity Doc
    | Italicize Bool Doc
    | Underline Underlining Doc
    | RestoreFormat (Maybe (ColorIntensity, Color))  -- Only used during the rendered phase, to signal a SGR should be issued to restore the terminal formatting.
                    (Maybe (ColorIntensity, Color))  -- These are the colors to revert the current forecolor/backcolor to (i.e. those from before the start of the Color block).
                    (Maybe ConsoleIntensity)         -- Intensity to revert to.
                    (Maybe Bool)                     -- Italicization to revert to.
                    (Maybe Underlining)              -- Underlining to revert to.

-- | The data type @SimpleDoc@ represents rendered documents and is used by the
-- display functions.
--
-- Whereas values of the data type 'Doc' represent non-empty sets of possible
-- renderings of a document, values of the data type @SimpleDoc@ represent
-- single renderings of a document.
--
-- The library provides two default display functions 'displayT' and
-- 'displayIO'. You can provide your own display function by writing a function
-- from a @SimpleDoc@ to your own output format.
data SimpleDoc =
      SFail
    | SEmpty
    | SChar Char SimpleDoc
    | SText Text SimpleDoc
    | SLine !Int SimpleDoc -- ^ @Int@ = indentation level for the line
    | SSGR [SGR] SimpleDoc

-- | The empty document is, indeed, empty. Although @mempty@ has no content, it
-- does have a \'height\' of 1 and behaves exactly like @(text \"\")@ (and is
-- therefore not a unit of @\<$\>@).
instance Monoid Doc where
    mempty = Empty
    mappend = (Semi.<>)
    mconcat = hcat

instance Semi.Semigroup Doc where
    (<>) = Cat

-- MCB: also added when "pretty" got the corresponding instances:
instance IsString Doc where
    fromString = text . T.pack

-- | @(char c)@ contains the literal character @c@. If the character is a
-- a newline (@'\n'@), consider using 'line' instead.
char :: Char -> Doc
char '\n' = line
char c = Char c

-- | @(unsafeText s)@ contains the literal string @s@. The string must not
-- contain any newline (@'\n'@) characters. If you're not sure, use the safer
-- (but less performant) 'text'.
unsafeText :: Text -> Doc
unsafeText  t
  | T.null t = Empty
  | otherwise = Text t

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
-- >>> putDoc (vsep [nest 2 (vsep ["hello", "world"]), "!"])
-- hello
--   world
-- !
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
    Color l i c x -> Color l i c (flatten x)
    Intensify i x -> Intensify i (flatten x)
    Italicize b x -> Italicize b (flatten x)
    Underline u x -> Underline u (flatten x)
    other         -> other

-----------------------------------------------------------
-- Colors
-----------------------------------------------------------

-- | Displays a document with the black forecolor
black :: Doc -> Doc
-- | Displays a document with the red forecolor
red :: Doc -> Doc
-- | Displays a document with the green forecolor
green :: Doc -> Doc
-- | Displays a document with the yellow forecolor
yellow :: Doc -> Doc
-- | Displays a document with the blue forecolor
blue :: Doc -> Doc
-- | Displays a document with the magenta forecolor
magenta :: Doc -> Doc
-- | Displays a document with the cyan forecolor
cyan :: Doc -> Doc
-- | Displays a document with the white forecolor
white :: Doc -> Doc
-- | Displays a document with the dull black forecolor
dullblack :: Doc -> Doc
-- | Displays a document with the dull red forecolor
dullred :: Doc -> Doc
-- | Displays a document with the dull green forecolor
dullgreen :: Doc -> Doc
-- | Displays a document with the dull yellow forecolor
dullyellow :: Doc -> Doc
-- | Displays a document with the dull blue forecolor
dullblue :: Doc -> Doc
-- | Displays a document with the dull magenta forecolor
dullmagenta :: Doc -> Doc
-- | Displays a document with the dull cyan forecolor
dullcyan :: Doc -> Doc
-- | Displays a document with the dull white forecolor
dullwhite :: Doc -> Doc
(black, dullblack) = colorFunctions Black
(red, dullred) = colorFunctions Red
(green, dullgreen) = colorFunctions Green
(yellow, dullyellow) = colorFunctions Yellow
(blue, dullblue) = colorFunctions Blue
(magenta, dullmagenta) = colorFunctions Magenta
(cyan, dullcyan) = colorFunctions Cyan
(white, dullwhite) = colorFunctions White

-- | Displays a document with a forecolor given in the first parameter
color :: Color -> Doc -> Doc
-- | Displays a document with a dull forecolor given in the first parameter
dullcolor :: Color -> Doc -> Doc
color = Color Foreground Vivid
dullcolor = Color Foreground Dull

colorFunctions :: Color -> (Doc -> Doc, Doc -> Doc)
colorFunctions what = (color what, dullcolor what)

-- | Displays a document with the black backcolor
onblack :: Doc -> Doc
-- | Displays a document with the red backcolor
onred :: Doc -> Doc
-- | Displays a document with the green backcolor
ongreen :: Doc -> Doc
-- | Displays a document with the yellow backcolor
onyellow :: Doc -> Doc
-- | Displays a document with the blue backcolor
onblue :: Doc -> Doc
-- | Displays a document with the magenta backcolor
onmagenta :: Doc -> Doc
-- | Displays a document with the cyan backcolor
oncyan :: Doc -> Doc
-- | Displays a document with the white backcolor
onwhite :: Doc -> Doc
-- | Displays a document with the dull black backcolor
ondullblack :: Doc -> Doc
-- | Displays a document with the dull red backcolor
ondullred :: Doc -> Doc
-- | Displays a document with the dull green backcolor
ondullgreen :: Doc -> Doc
-- | Displays a document with the dull yellow backcolor
ondullyellow :: Doc -> Doc
-- | Displays a document with the dull blue backcolor
ondullblue :: Doc -> Doc
-- | Displays a document with the dull magenta backcolor
ondullmagenta :: Doc -> Doc
-- | Displays a document with the dull cyan backcolor
ondullcyan :: Doc -> Doc
-- | Displays a document with the dull white backcolor
ondullwhite :: Doc -> Doc
(onblack, ondullblack) = oncolorFunctions Black
(onred, ondullred) = oncolorFunctions Red
(ongreen, ondullgreen) = oncolorFunctions Green
(onyellow, ondullyellow) = oncolorFunctions Yellow
(onblue, ondullblue) = oncolorFunctions Blue
(onmagenta, ondullmagenta) = oncolorFunctions Magenta
(oncyan, ondullcyan) = oncolorFunctions Cyan
(onwhite, ondullwhite) = oncolorFunctions White

-- | Displays a document with a backcolor given in the first parameter
oncolor :: Color -> Doc -> Doc
-- | Displays a document with a dull backcolor given in the first parameter
ondullcolor :: Color -> Doc -> Doc
oncolor = Color Background Vivid
ondullcolor = Color Background Dull

oncolorFunctions :: Color -> (Doc -> Doc, Doc -> Doc)
oncolorFunctions what = (oncolor what, ondullcolor what)

-----------------------------------------------------------
-- Console Intensity
-----------------------------------------------------------

-- | Displays a document in a heavier font weight
bold :: Doc -> Doc
bold = Intensify BoldIntensity

-- | Displays a document in the normal font weight
debold :: Doc -> Doc
debold = Intensify NormalIntensity

-- NB: I don't support FaintIntensity here because it is not widely supported by
-- terminals.

-----------------------------------------------------------
-- Italicization
-----------------------------------------------------------

{-

I'm in two minds about providing these functions, since italicization is so
rarely implemented. It is especially bad because "italicization" may cause the
meaning of colors to flip, which will look a bit weird, to say the least...

-- | Displays a document in italics. This is not widely supported, and it's use
-- is not recommended
italicize :: Doc -> Doc
italicize = Italicize True

-- | Displays a document with no italics
deitalicize :: Doc -> Doc
deitalicize = Italicize False

-}

-----------------------------------------------------------
-- Underlining
-----------------------------------------------------------

-- | Displays a document with underlining
underline :: Doc -> Doc
underline = Underline SingleUnderline

-- | Displays a document with no underlining
deunderline :: Doc -> Doc
deunderline = Underline NoUnderline

-- NB: I don't support DoubleUnderline here because it is not widely supported by terminals.

-----------------------------------------------------------
-- Removing formatting
-----------------------------------------------------------

-- | Removes all colorisation, emboldening and underlining from a document
plain :: Doc -> Doc
plain = \case
    Fail            -> Fail
    e@Empty         -> e
    c@(Char _)      -> c
    t@(Text _)      -> t
    l@Line          -> l
    FlatAlt x y     -> FlatAlt (plain x) (plain y)
    Cat x y         -> Cat (plain x) (plain y)
    Nest i x        -> Nest i (plain x)
    Union x y       -> Union (plain x) (plain y)
    Column f        -> Column (plain . f)
    Columns f       -> Columns (plain . f)
    Nesting f       -> Nesting (plain . f)
    Color _ _ _ x   -> plain x
    Intensify _ x   -> plain x
    Italicize _ x   -> plain x
    Underline _ x   -> plain x
    RestoreFormat{} -> Empty

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
renderFits fits rfrac w x
    -- I used to do a @SSGR [Reset]@ here, but if you do that it will result in
    -- any rendered @Doc@ containing at least some ANSI control codes. This may
    -- be undesirable if you want to render to non-ANSI devices by simply not
    -- making use of the ANSI color functions I provide.
    --
    -- What I "really" want to do here is do an initial Reset iff there is some
    -- ANSI color within the Doc, but that's a bit fiddly. I'll fix it if
    -- someone complains!
  = best 0 0 Nothing Nothing Nothing Nothing Nothing (Cons 0 x Nil)
  where
    -- r :: the ribbon width in characters
    r = max 0 (min w (round (fromIntegral w * rfrac)))

    -- i: current column in the output
    -- n: indentation of current line
    -- k: current column
    --   Therefore:
    --     - k >= n
    --     - k - n = count of inserted characters in current line
    best _ _ _ _ _ _ _ Nil = SEmpty
    best n k mb_fc mb_bc mb_in mb_it mb_un (Cons i d ds) = case d of
      Fail          -> SFail
      Empty         -> best_typical n k ds
      Char c        -> let !k' = k+1          in SChar c (best_typical n k' ds)
      Text t        -> let !k' = k+T.length t in SText t (best_typical n k' ds)
      Line          -> SLine i (best_typical i i ds)
      FlatAlt x _   -> best_typical n k (Cons i x ds)
      Cat x y       -> best_typical n k (Cons i x (Cons i y ds))
      Nest j x      -> let !i' = i+j in best_typical n k (Cons i' x ds)
      Union x y     -> nicest n k (best_typical n k (Cons i x ds))
                                  (best_typical n k (Cons i y ds))
      Column f      -> best_typical n k (Cons i (f k) ds)
      Columns f     -> best_typical n k (Cons i (f (Just w)) ds)
      Nesting f     -> best_typical n k (Cons i (f i) ds)
      Color l t c x -> SSGR [SetColor l t c] (best n k mb_fc' mb_bc' mb_in mb_it mb_un (Cons i x ds_restore))
        where
          mb_fc' = case l of { Background -> mb_fc; Foreground -> Just (t, c) }
          mb_bc' = case l of { Background -> Just (t, c); Foreground -> mb_bc }
      Intensify t x -> SSGR [SetConsoleIntensity t] (best n k mb_fc mb_bc (Just t) mb_it mb_un (Cons i x ds_restore))
      Italicize t x -> SSGR [SetItalicized t] (best n k mb_fc mb_bc mb_in (Just t) mb_un (Cons i x ds_restore))
      Underline u x -> SSGR [SetUnderlining u] (best n k mb_fc mb_bc mb_in mb_it (Just u) (Cons i x ds_restore))
      RestoreFormat mb_fc' mb_bc' mb_in' mb_it' mb_un' -> SSGR sgrs (best n k mb_fc' mb_bc' mb_in' mb_it' mb_un' ds)
        where
          -- We need to be able to restore the entire SGR state, hence we
          -- carry around what we believe that state should be in all the
          -- arguments to this function. Note that in some cases we could
          -- avoid the Reset of the entire state, but not in general.
          sgrs = Reset : catMaybes [
              fmap (uncurry (SetColor Foreground)) mb_fc',
              fmap (uncurry (SetColor Background)) mb_bc',
              fmap SetConsoleIntensity mb_in',
              fmap SetItalicized mb_it',
              fmap SetUnderlining mb_un'
            ]
      where
        best_typical n' k' ds' = best n' k' mb_fc mb_bc mb_in mb_it mb_un ds'
        ds_restore = Cons i (RestoreFormat mb_fc mb_bc mb_in mb_it mb_un) ds

        -- Invariant: first lines of A are longer than the first lines of B.
        nicest
            :: Int -- ^ indentation of current line
            -> Int -- ^ current column
            -> SimpleDoc -- ^ Choice A
            -> SimpleDoc -- ^ Choice B
            -> SimpleDoc
        nicest n k x y
          | fits w (min n k) width x = x
          | otherwise = y
          where
            width = min (w - k) (r - k + n)

-- @fits1@ does 1 line lookahead.
fits1
    :: Int -- ^ Page width
    -> Int -- ^ Minimum nesting level to fit in
    -> Int -- ^ Width in which to fit the first line
    -> SimpleDoc
    -> Bool
fits1 _ _ w _ | w < 0   = False
fits1 _ _ _ SFail       = False
fits1 _ _ _ SEmpty      = True
fits1 p m w (SChar _ x) = fits1 p m (w - 1) x
fits1 p m w (SText t x) = fits1 p m (w - T.length t) x
fits1 _ _ _ SLine{}     = True
fits1 p m w (SSGR _ x)  = fits1 p m w x

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
  | w < 0               = False
fitsR _ _ _ SFail       = False
fitsR _ _ _ SEmpty      = True
fitsR p m w (SChar _ x) = fitsR p m (w - 1) x
fitsR p m w (SText t x) = fitsR p m (w - T.length t) x
fitsR p m _ (SLine i x)
  | m < i               = fitsR p m (p - i) x
  | otherwise           = True
fitsR p m w (SSGR _ x)  = fitsR p m w x

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
renderCompact x = scan 0 [x]
  where
    scan _ [] = SEmpty
    scan k (d:ds) = case d of
        Fail            -> SFail
        Empty           -> scan k ds
        Char c          -> let k' = k+1 in seq k' (SChar c (scan k' ds))
        Text t          -> let k' = k+T.length t in seq k' (SText t (scan k' ds))
        FlatAlt x _     -> scan k (x:ds)
        Line            -> SLine 0 (scan 0 ds)
        Cat x y         -> scan k (x:y:ds)
        Nest _ x        -> scan k (x:ds)
        Union _ y       -> scan k (y:ds)
        Column f        -> scan k (f k:ds)
        Columns f       -> scan k (f Nothing:ds)
        Nesting f       -> scan k (f 0:ds)
        Color _ _ _ x   -> scan k (x:ds)
        Intensify _ x   -> scan k (x:ds)
        Italicize _ x   -> scan k (x:ds)
        Underline _ x   -> scan k (x:ds)
        RestoreFormat{} -> scan k ds



-----------------------------------------------------------
-- Displayers:  displayS and displayIO
-----------------------------------------------------------

-- | @displayT simpleDoc@ takes the output @simpleDoc@ from a rendering function
-- and transforms it to lazy text (for use in the 'Show' class).
--
-- > showWidth :: Int -> Doc -> Text
-- > showWidth w x = displayT (renderPretty 0.4 w x) ""
--
-- ANSI color information will be discarded by this function unless you are
-- running on a Unix-like operating system. This is due to a technical
-- limitation in Windows ANSI support.
displayT :: SimpleDoc -> LTB.Builder
displayT = \case
    SFail     -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
    SEmpty    -> mempty
    SChar c x -> LTB.singleton c <> displayT x
    SText t x -> LTB.fromText t <> displayT x
    SLine i x -> LTB.singleton '\n' <> LTB.fromText (T.replicate i " ") <> displayT x
    SSGR s x  -> LTB.fromString (setSGRCode s) <> displayT x

-- | @(displayIO handle simpleDoc)@ writes @simpleDoc@ to the file handle
-- @handle@. This function is used for example by 'hPutDoc':
--
-- > hPutDoc handle doc = displayIO handle (renderPretty 0.4 80 doc)
--
-- Any ANSI colorisation in @simpleDoc@ will be output.
displayIO :: Handle -> SimpleDoc -> IO ()
displayIO h = display
  where
    display = \case
        SFail     -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
        SEmpty    -> pure ()
        SChar c x -> hPutChar h c *> display x
        SText t x -> T.hPutStr h t *> display x
        SLine i x -> hPutChar h '\n' *> T.hPutStr h (T.replicate i " ") *> display x
        SSGR s x  -> hSetSGR h s *> display x

-----------------------------------------------------------
-- default pretty printers: show, putDoc and hPutDoc
-----------------------------------------------------------
instance Show Doc where
  showsPrec _ doc = shows (displayT (renderPretty 0.4 80 doc))

-- | @putDoc doc@ pretty prints document @doc@ to standard output, with a page
-- width of 80 characters and a ribbon width of 32 characters (see
-- 'renderPretty' for documentation of those values.)
--
-- >>> putDoc ("hello" <+> "world")
-- hello world
--
-- Any ANSI colorisation in @doc@ will be output.
putDoc :: Doc -> IO ()
putDoc = hPutDoc stdout

-- | 'putDoc', but with a page width parameter.
putDocW :: Int -> Doc -> IO ()
putDocW = hPutDocW stdout

-- | 'putDoc', but instead of using 'stdout', print to a user-provided handle,
-- e.g. a file or a socket. Uses a line length of 80, and a ribbon width of 32
-- characters (see 'renderPretty' for documentation of those values).
--
-- > main = withFile "someFile.txt" (\h -> hPutDoc h (vcat ["vertical", "text"]))
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc h doc = displayIO h (renderPretty 0.4 80 doc)

-- | 'hPutDocW', but with a page width parameter.
hPutDocW :: Handle -> Int -> Doc -> IO ()
hPutDocW h w doc = displayIO h (renderPretty 0.4 w doc)
