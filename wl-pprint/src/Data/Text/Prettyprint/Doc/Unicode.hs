{-# LANGUAGE OverloadedStrings #-}

-- | A collection of predefined unicode values for use with the "i"ter.
module Data.Text.Prettyprint.Doc.Unicode {-# WARNING "This module is unstable, and will likely be split up into multiple different ones in the future." #-} (
    -- * Quotes

    -- ** Enclosing
    d9966quotes,
    d6699quotes,
    s96quotes,
    s69quotes,
    dGuillemetsOut,
    dGuillemetsIn,
    sGuillemetsOut,
    sGuillemetsIn,

    -- ** Standalone
    b99dquote,
    t66dquote,
    t99dquote,
    b9quote,
    t6quote,
    t9quote,

    rdGuillemet,
    ldGuillemet,
    rsGuillemet,
    lsGuillemet,


    -- * Various typographical symbols
    bullet,
    endash,

    -- * Currencies
    euro,
    cent,
    yen,
    pound,
) where



import Data.Text.Prettyprint.Doc



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.IO as T
-- >>> import Data.Text.Prettyprint.Doc.Render.Text
-- >>> let putDocW w doc = renderIO System.IO.stdout (layoutPretty (RibbonFraction 1.0) (PageWidth w) doc)






-- | Double „99-66“ quotes, as used in German typography.
--
-- >>> putDoc (d9966quotes "·")
-- „·“
d9966quotes :: Doc -> Doc
d9966quotes = enclose b99dquote t66dquote

-- | Double „66-99“ quotes, as used in English typography.
--
-- >>> putDoc (d6699quotes "·")
-- “·”
d6699quotes :: Doc -> Doc
d6699quotes = enclose t66dquote t99dquote

-- | Single „9-6“ quotes, as used in German typography.
--
-- >>> putDoc (s96quotes "·")
-- ‚·‘
s96quotes :: Doc -> Doc
s96quotes = enclose b9quote t6quote

-- | Single „6-9“ quotes, as used in English typography.
--
-- >>> putDoc (s69quotes "·")
-- ‘·’
s69quotes :: Doc -> Doc
s69quotes = enclose t6quote t9quote

-- | Double guillemets, pointing outwards (without adding any spacing).
--
-- >>> putDoc (dGuillemetsOut "·")
-- «·»
dGuillemetsOut :: Doc -> Doc
dGuillemetsOut = enclose ldGuillemet rdGuillemet

-- | Double guillemets, pointing inwards (without adding any spacing).
--
-- >>> putDoc (dGuillemetsIn "·")
-- »·«
dGuillemetsIn :: Doc -> Doc
dGuillemetsIn = enclose rdGuillemet ldGuillemet

-- | Single guillemets, pointing outwards (without adding any spacing).
--
-- >>> putDoc (sGuillemetsOut "·")
-- ‹·›
sGuillemetsOut :: Doc -> Doc
sGuillemetsOut = enclose lsGuillemet rsGuillemet

-- | Single guillemets, pointing inwards (without adding any spacing).
--
-- >>> putDoc (sGuillemetsIn "·")
-- ›·‹
sGuillemetsIn :: Doc -> Doc
sGuillemetsIn = enclose rsGuillemet lsGuillemet

-- | Bottom “99” style double quotes.
--
-- >>> putDoc b99dquote
-- „
b99dquote :: Doc
b99dquote = "„"

-- | Top “66” style double quotes.
--
-- >>> putDoc t66dquote
-- “
t66dquote :: Doc
t66dquote = "“"

-- | Top “99” style double quotes.
--
-- >>> putDoc t99dquote
-- ”
t99dquote :: Doc
t99dquote = "”"

-- | Bottom “9” style single quote.
--
-- >>> putDoc b9quote
-- ‚
b9quote :: Doc
b9quote = "‚"

-- | Top “66” style single quote.
--
-- >>> putDoc t6quote
-- ‘
t6quote :: Doc
t6quote = "‘"

-- | Top “9” style single quote.
--
-- >>> putDoc t9quote
-- ’
t9quote :: Doc
t9quote = "’"

-- | Right-pointing double guillemets
--
-- >>> putDoc rdGuillemet
-- »
rdGuillemet :: Doc
rdGuillemet = "»"

-- | Left-pointing double guillemets
--
-- >>> putDoc ldGuillemet
-- «
ldGuillemet :: Doc
ldGuillemet = "«"

-- | Right-pointing single guillemets
--
-- >>> putDoc rsGuillemet
-- ›
rsGuillemet :: Doc
rsGuillemet = "›"

-- | Left-pointing single guillemets
--
-- >>> putDoc lsGuillemet
-- ‹
lsGuillemet :: Doc
lsGuillemet = "‹"

-- | >>> putDoc bullet
-- •
bullet :: Doc
bullet = "•"

-- | >>> putDoc endash
-- –
endash :: Doc
endash = "–"

-- | >>> putDoc euro
-- €
euro :: Doc
euro = "€"

-- | >>> putDoc cent
-- ¢
cent :: Doc
cent = "¢"

-- | >>> putDoc yen
-- ¥
yen :: Doc
yen = "¥"

-- | >>> putDoc pound
-- £
pound :: Doc
pound = "£"
