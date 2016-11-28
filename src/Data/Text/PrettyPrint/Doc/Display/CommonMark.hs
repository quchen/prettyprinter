{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Display 'SimpleDoc' as common markdown AKA CommonMark in 'Text' format.
module Data.Text.PrettyPrint.Doc.Display.CommonMark (
    displayLazy,
    displayStrict,
) where



import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as LTB

import Data.Text.PrettyPrint.Doc



-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XLambdaCase
-- >>> import qualified Data.Text.IO as T
-- >>> import qualified Data.Text.Lazy.IO as LT



-- | Add Markdown-style markers for emphasis and strong emphasis. Other styles
-- are ignored.
--
-- >>> let doc = "This text" <+> italics ("is emphasized" <+> bold "even stronger" <> "!")
-- >>> let pprint = LT.putStrLn . displayLazy . renderPretty 0.4 40
-- >>> pprint doc
-- This text *is emphasized **even stronger**!*
-- >>> pprint (red doc)
-- This text *is emphasized **even stronger**!*
displayLazy :: SimpleDoc -> LT.Text
displayLazy doc
  = let (resultBuilder, remainingStyles) = runState (execWriterT (build doc)) []
    in if null remainingStyles
        then LTB.toLazyText resultBuilder
        else error ("There are "
                    <> show (length remainingStyles)
                    <> " unpaired styles! Please report this as a bug.")

build :: SimpleDoc -> WriterT LTB.Builder (State [Style]) ()
build = \case
    SFail -> error "@SFail@ can not appear uncaught in a rendered @SimpleDoc@"
    SEmpty -> pure ()
    SChar c x -> do tell (LTB.singleton c)
                    build x
    SText t x -> do tell (LTB.fromText t)
                    build x
    SLine i x -> do tell (LTB.singleton '\n' )
                    tell (LTB.fromText (T.replicate i " "))
                    build x
    SStylePush s x -> do
        lift (modify (s:))
        tell (styleToMarker s)
        build x
    SStylePop x -> do
        s <- lift get >>= \case
            [] -> error "Attempted to pop a style off an empty stack.\
                        \ Please report this as a bug."
            s':ss -> lift (put ss) *> pure s'
        tell (styleToMarker s)
        build x

styleToMarker :: Style -> LTB.Builder
styleToMarker = \case
    SItalicized -> LTB.fromString "*"
    SBold       -> LTB.fromString "**"
    _other      -> mempty

-- | Strict version of 'displayLazy'.
displayStrict :: SimpleDoc -> Text
displayStrict = LT.toStrict . displayLazy
