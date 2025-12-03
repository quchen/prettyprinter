-- | Render 'SimpleDocStream' in a terminal.
module Prettyprinter.Render.Terminal (
    -- * Styling
    AnsiStyle,
    Color(..),

    -- ** Font color
    color, colorDull,

    -- ** Background color
    bgColor, bgColorDull,

    -- ** Font style
    bold, italicized, underlined, faint,

    -- ** Internal markers
    --
    -- | These should only be used for writing adaptors to other libraries; for
    -- the average use case, use 'bold', 'bgColorDull', etc.
    Intensity(..),
    Bold(..),
    Underlined(..),
    Italicized(..),

    -- * Conversion to ANSI-infused 'Text'
    renderLazy, renderStrict,

    -- * Render directly to 'stdout'
    renderIO,

    -- ** Convenience functions
    putDoc, hPutDoc,
) where

import Prettyprinter.Render.Terminal.Internal
