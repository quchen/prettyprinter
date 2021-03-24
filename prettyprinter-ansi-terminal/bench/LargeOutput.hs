{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This benchmark is derived from the large-output benchmark in prettyprinter, but contains additional annotations.
module Main (main) where

import Prelude        ()
import Prelude.Compat

import           Control.DeepSeq
import           Control.Monad.Compat
import           Data.Char
import           Data.Map                              (Map)
import qualified Data.Map                              as M
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import qualified Data.Text.Lazy                        as TL
import           Gauge
import           GHC.Generics
import           Prettyprinter
import           Prettyprinter.Render.Terminal         as Terminal
import qualified Prettyprinter.Render.Text             as Text
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random



newtype Program = Program Binds deriving (Show, Generic)
newtype Binds = Binds (Map Text LambdaForm) deriving (Show, Generic)
data LambdaForm = LambdaForm ![Text] ![Text] !Expr deriving (Show, Generic)
data Expr
    = Let Binds Expr
    | Case Expr [Alt]
    | AppF Text [Text]
    | AppC Text [Text]
    | AppP Text Text Text
    | LitE Int
    deriving (Show, Generic)
data Alt = Alt Text [Text] Expr deriving (Show, Generic)

instance NFData Program
instance NFData Binds
instance NFData LambdaForm
instance NFData Expr
instance NFData Alt

instance Arbitrary Program where arbitrary = fmap Program arbitrary
instance Arbitrary Binds where
    arbitrary = do
        NonEmpty xs <- arbitrary
        pure (Binds (M.fromList xs))
instance Arbitrary LambdaForm where
    arbitrary = LambdaForm <$> fromTo 0 2 arbitrary <*> fromTo 0 2 arbitrary <*> arbitrary

instance Arbitrary Expr where
    arbitrary = (oneof . map scaled)
        [ Let <$> arbitrary <*> arbitrary
        , Case <$> arbitrary <*> (do NonEmpty xs <- arbitrary; pure xs)
        , AppF <$> arbitrary <*> fromTo 0 3 arbitrary
        , AppC <$> ucFirst arbitrary <*> fromTo 0 3 arbitrary
        , AppP <$> arbitrary <*> arbitrary <*> arbitrary
        , LitE <$> arbitrary ]
instance Arbitrary Alt where arbitrary = Alt <$> ucFirst arbitrary <*> fromTo 0 3 arbitrary <*> arbitrary
instance Arbitrary Text where
    arbitrary = do
        n <- choose (3,6)
        str <- replicateM n (elements ['a'..'z'])
        if str `elem` ["let", "in", "case", "of"]
            then arbitrary
            else pure (T.pack str)

ucFirst :: Gen Text -> Gen Text
ucFirst gen = do
    x <- gen
    case T.uncons x of
        Nothing -> pure x
        Just (t,ext) -> pure (T.cons (toUpper t) ext)

anCol :: Color -> Doc AnsiStyle -> Doc AnsiStyle
anCol = annotate . color

prettyProgram :: Program -> Doc AnsiStyle
prettyProgram (Program binds) = annotate italicized $ prettyBinds binds

prettyBinds :: Binds -> Doc AnsiStyle
prettyBinds (Binds bs) = align (vsep (map prettyBinding (M.assocs bs)))
  where
    prettyBinding (var, lambda) = pretty var <+> anCol Red "=" <+> prettyLambdaForm lambda

prettyLambdaForm :: LambdaForm -> Doc AnsiStyle
prettyLambdaForm (LambdaForm free bound body) = prettyExp . (<+> anCol Blue "->") . prettyBound . prettyFree $ anCol Blue "\\"
  where
    prettyFree | null free = id
               | otherwise = (<> anCol Blue lparen <> hsep (map pretty free) <> anCol Blue rparen)
    prettyBound | null bound = id
                | null free = (<> hsep (map pretty bound))
                | otherwise = (<+> hsep (map pretty bound))
    prettyExp = (<+> prettyExpr body)

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr = \case
    Let binds body ->
        align (vsep [ anCol Red "let" <+> align (prettyBinds binds)
                    , anCol Red "in" <+> prettyExpr body ])

    Case scrutinee alts -> vsep
        [ anCol Yellow "case" <+> prettyExpr scrutinee <+> anCol Yellow "of"
        , indent 4 (align (vsep (map prettyAlt alts))) ]

    AppF f [] -> annotate bold . anCol Green $ pretty f
    AppF f args -> annotate bold . anCol Green $ pretty f <+> hsep (map pretty args)

    AppC c [] -> annotate bold . anCol Green $ pretty c
    AppC c args -> annotate bold . anCol Green $ pretty c <+> hsep (map pretty args)

    AppP op x y -> annotate bold . anCol Green $ pretty op <+> pretty x <+> pretty y

    LitE lit -> annotate bold . anCol Green $ pretty lit

prettyAlt :: Alt -> Doc AnsiStyle
prettyAlt (Alt con [] body) = pretty con <+> anCol Yellow "->" <+> prettyExpr body
prettyAlt (Alt con args body) = pretty con <+> hsep (map pretty args) <+> anCol Yellow "->" <+> prettyExpr body

scaled :: Gen a -> Gen a
scaled = scale (\n -> n * 2 `quot` 3)

fromTo :: Int -> Int -> Gen b -> Gen b
fromTo a b gen = do
    n <- choose (min a b, max a b)
    resize n gen

randomProgram
    :: Int -- ^ Seed
    -> Int -- ^ Generator size
    -> Program
randomProgram seed size = let MkGen gen = arbitrary in gen (mkQCGen seed) size

main :: IO ()
main = do
    let prog = randomProgram 1 60
        layoutOpts = defaultLayoutOptions { layoutPageWidth = Unbounded }
        renderedProg = (renderLazy . layoutPretty layoutOpts . prettyProgram) prog
        (progLines, progWidth) = let l = TL.lines renderedProg in (length l, maximum (map TL.length l))
    putDoc ("Program size:" <+> pretty progLines <+> "lines, maximum width:" <+> pretty progWidth)

    let render :: (SimpleDocStream AnsiStyle -> TL.Text) -> Program -> TL.Text
        render r = r . layoutPretty layoutOpts . prettyProgram

    rnf prog `seq` T.putStrLn "Starting benchmark…"

    defaultMain
        [ bench "prettyprinter-ansi-terminal" $ nf (render Terminal.renderLazy) prog
        , bench "prettyprinter" $ nf (render Text.renderLazy) prog
        ]
