-- |
-- Module      :  Data.Text.Prettyprint.Doc
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
-- There are two key concepts to laying a document out: the available width, and
-- 'group'ing.
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
--
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
module Data.Text.Prettyprint.Doc (
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

    -- * Reactive/conditional layouts
    --
    -- | Lay documents out differently based on current position and the page
    -- layout.
    column, nesting, width, pageWidth,

    -- * Filler functions
    --
    -- | Fill up available space
    fill, fillBreak,

    -- * General convenience
    --
    -- | Useful helper functions.
    plural, enclose,

    -- * Bracketing functions
    --
    -- | Enclose documents in common ways.
    squotes, dquotes, parens, angles, brackets, braces,

    -- * Named characters
    --
    -- | Convenience definitions for common characters
    lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket, squote,
    dquote, semi, colon, comma, space, dot, slash, backslash, equals, pipe,

    -- ** Annotations
    annotate,
    unAnnotate,

    -- * Optimization
    --
    -- Render documents faster
    fuse, FusionDepth(..),

    -- * Layout
    --
    -- | Laying a 'Doc'ument out produces a straightforward 'SimpleDoc' based on
    -- parameters such as page width and ribbon size, by evaluating how a 'Doc'
    -- fits these constraints the best. There are various ways to render a
    -- 'SimpleDoc'. For the common case of rendering a 'SimpleDoc' as plain
    -- 'Text' take a look at "Data.Text.Prettyprint.Doc.Render.Text".
    SimpleDoc(..),
    PageWidth(..), LayoutOptions(..), defaultLayoutOptions,
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



import Data.Monoid
import Data.Text.Prettyprint.Doc.Internal

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text.Prettyprint.Doc.Render.Text
-- >>> let layoutOptions w = LayoutOptions { layoutRibbonFraction = 1, layoutPageWidth = CharsPerLine w }
-- >>> let putDocW w doc = renderIO System.IO.stdout (layoutPretty (layoutOptions w) doc)



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
--   - Instead of providing an own colorization function for each
--     color\/intensity\/layer combination, they have been combined in 'color',
--     'colorDull', 'bgColor', and 'bgColorDull' functions.



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
