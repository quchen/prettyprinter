module Prettyprinter.Render.String (
    renderString,
    renderShowS,
) where

import Prettyprinter.Internal (SimpleDocStream, renderShowS)

-- | Render a 'SimpleDocStream' to a 'String'.
renderString :: SimpleDocStream ann -> String
renderString s = renderShowS s ""
