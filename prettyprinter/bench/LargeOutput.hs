{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Prelude        ()
import Prelude.Compat

import           Control.DeepSeq
import           Control.Monad.Compat
import           Data.Char
import           Data.Map                              (Map)
import qualified Data.Map                              as M
import           Prettyprinter.Util.Compat.Text        (Text)
import qualified Prettyprinter.Util.Compat.Text        as T
import qualified Prettyprinter.Util.Compat.Text.IO     as T
import qualified Prettyprinter.Util.Compat.Text.Lazy   as TL
import           Prettyprinter
import           Prettyprinter.Render.Text
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random
import           Test.Tasty.Bench
import qualified Text.PrettyPrint.ANSI.Leijen          as WL



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

instance Pretty Program where pretty (Program binds) = pretty binds
instance Pretty Binds where
    pretty (Binds bs) = align (vsep (map prettyBinding (M.assocs bs)))
      where
        prettyBinding (var, lambda) = pretty var <+> "=" <+> pretty lambda

instance Pretty LambdaForm where
    pretty (LambdaForm free bound body) = (prettyExp . (<+> "->") . prettyBound . prettyFree) "\\"
      where
        prettyFree | null free = id
                   | otherwise = (<> lparen <> hsep (map pretty free) <> rparen)
        prettyBound | null bound = id
                    | null free = (<> hsep (map pretty bound))
                    | otherwise = (<+> hsep (map pretty bound))
        prettyExp = (<+> pretty body)

instance Pretty Expr where
    pretty = \expr -> case expr of
        Let binds body ->
            align (vsep [ "let" <+> align (pretty binds)
                        , "in" <+> pretty body ])

        Case scrutinee alts -> vsep
            [ "case" <+> pretty scrutinee <+> "of"
            , indent 4 (align (vsep (map pretty alts))) ]

        AppF f [] -> pretty f
        AppF f args -> pretty f <+> hsep (map pretty args)

        AppC c [] -> pretty c
        AppC c args -> pretty c <+> hsep (map pretty args)

        AppP op x y -> pretty op <+> pretty x <+> pretty y

        LitE lit -> pretty lit

instance Pretty Alt where
    pretty (Alt con [] body) = pretty con <+> "->" <+> pretty body
    pretty (Alt con args body) = pretty con <+> hsep (map pretty args) <+> "->" <+> pretty body

instance WL.Pretty Program where pretty (Program binds) = WL.pretty binds
instance WL.Pretty Binds where
    pretty (Binds bs) = WL.align (WL.vsep (map prettyBinding (M.assocs bs)))
      where
        prettyBinding (var, lambda) = WL.pretty var WL.<+> "=" WL.<+> WL.pretty lambda

instance WL.Pretty Text where
    pretty = WL.string . T.unpack

instance WL.Pretty LambdaForm where
    pretty (LambdaForm free bound body) = (prettyExp . (WL.<+> "->") . prettyBound . prettyFree) "\\"
      where
        prettyFree | null free = id
                   | otherwise = (<> WL.lparen <> WL.hsep (map WL.pretty free) <> WL.rparen)
        prettyBound | null bound = id
                    | null free = (<> WL.hsep (map WL.pretty bound))
                    | otherwise = (WL.<+> WL.hsep (map WL.pretty bound))
        prettyExp = (WL.<+> WL.pretty body)

instance WL.Pretty Expr where
    pretty = \expr -> case expr of
        Let binds body ->
            WL.align (WL.vsep [ "let" WL.<+> WL.align (WL.pretty binds)
                        , "in" WL.<+> WL.pretty body ])

        Case scrutinee alts -> WL.vsep
            [ "case" WL.<+> WL.pretty scrutinee WL.<+> "of"
            , WL.indent 4 (WL.align (WL.vsep (map WL.pretty alts))) ]

        AppF f [] -> WL.pretty f
        AppF f args -> WL.pretty f WL.<+> WL.hsep (map WL.pretty args)

        AppC c [] -> WL.pretty c
        AppC c args -> WL.pretty c WL.<+> WL.hsep (map WL.pretty args)

        AppP op x y -> WL.pretty op WL.<+> WL.pretty x WL.<+> WL.pretty y

        LitE lit -> WL.pretty lit

instance WL.Pretty Alt where
    pretty (Alt con [] body) = WL.text (T.unpack con) WL.<+> "->" WL.<+> WL.pretty body
    pretty (Alt con args body) = WL.text (T.unpack con) WL.<+> WL.hsep (map WL.pretty args) WL.<+> "->" WL.<+> WL.pretty body

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
        renderedProg = (renderLazy . layoutPretty defaultLayoutOptions { layoutPageWidth = Unbounded } . pretty) prog
        (progLines, progWidth) = let l = TL.lines renderedProg in (length l, maximum (map TL.length l))
    putDoc ("Program size:" <+> pretty progLines <+> "lines, maximum width:" <+> pretty progWidth)

    let renderWith :: (Doc ann -> SimpleDocStream ann) -> Program -> TL.Text
        renderWith f = renderLazy . f . pretty

    let _80ColumnsLayoutOptions = defaultLayoutOptions { layoutPageWidth = AvailablePerLine 80 0.5 }
        unboundedLayoutOptions  = defaultLayoutOptions { layoutPageWidth = Unbounded }

    rnf prog `seq` T.putStrLn "Starting benchmark…"

    defaultMain
        [ bgroup "80 characters, 50% ribbon"
            [ bgroup "prettyprinter"
                [ bench "layoutPretty"  (nf (renderWith (layoutPretty _80ColumnsLayoutOptions)) prog)
                , bench "layoutSmart"   (nf (renderWith (layoutSmart  _80ColumnsLayoutOptions)) prog)
                , bench "layoutCompact" (nf (renderWith layoutCompact                         ) prog)
                ]
            , bench "ansi-wl-pprint" (nf (\x -> (WL.displayS . WL.renderPretty 0.5 80 . WL.pretty) x "") prog) ]
        , bgroup "Infinite/large page width"
            [ bgroup "prettyprinter"
                [ bench "layoutPretty"  (nf (renderWith (layoutPretty unboundedLayoutOptions)) prog)
                , bench "layoutSmart"   (nf (renderWith (layoutSmart  unboundedLayoutOptions)) prog)
                , bench "layoutCompact" (nf (renderWith layoutCompact                        ) prog)
                ]
            , bench "ansi-wl-pprint" (nf (\x -> (WL.displayS . WL.renderPretty 1 (fromIntegral progWidth + 10) . WL.pretty) x "") prog) ]
        ]
