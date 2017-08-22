{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, TypeOperators #-}
module Data.Functor.Classes.Pretty
( Pretty1(..)
, Pretty2(..)
, module Pretty
, pretty1
, Pretty1Of(..)
, Pretty2Of(..)
) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Prettyprint.Doc as Pretty
import GHC.Generics

class Pretty1 f where
  liftPretty :: (a -> Doc ann) -> ([a] -> Doc ann) -> f a -> Doc ann
  default liftPretty :: (Generic1 f, GPretty1 (Rep1 f)) => (a -> Doc ann) -> ([a] -> Doc ann) -> f a -> Doc ann
  liftPretty p pl = gliftPretty p pl . from1

  liftPrettyList :: (a -> Doc ann) -> ([a] -> Doc ann) -> [f a] -> Doc ann
  liftPrettyList p pl = list . map (liftPretty p pl)

class Pretty2 f where
  liftPretty2 :: (a -> Doc ann) -> ([a] -> Doc ann) -> (b -> Doc ann) -> ([b] -> Doc ann) -> f a b -> Doc ann
  liftPrettyList2 :: (a -> Doc ann) -> ([a] -> Doc ann) -> (b -> Doc ann) -> ([b] -> Doc ann) -> [f a b] -> Doc ann
  liftPrettyList2 pa pla pb plb = list . map (liftPretty2 pa pla pb plb)

pretty1 :: (Pretty a, Pretty1 f) => f a -> Doc ann
pretty1 = liftPretty pretty prettyList

instance Pretty1 [] where
  liftPretty _ pl = pl

instance Pretty1 NonEmpty where
  liftPretty _ pl = pl . toList

instance Pretty1 Maybe where
  liftPretty p _ = maybe emptyDoc p

instance Pretty2 Either where
  liftPretty2 pL _ _ _ (Left l)  = pL l
  liftPretty2 _ _ pR _ (Right r) = pR r

instance Pretty l => Pretty1 (Either l) where
  liftPretty = liftPretty2 pretty prettyList

instance Pretty2 (,) where
  liftPretty2 pA _ pB _ (a, b) = tupled [ pA a, pB b ]

instance Pretty a => Pretty1 ((,) a) where
  liftPretty = liftPretty2 pretty prettyList


newtype Pretty1Of f a = Pretty1Of { unPretty1Of :: f a }
  deriving (Eq, Ord, Show)

instance Pretty1 f => Pretty1 (Pretty1Of f) where
  liftPretty p pl = liftPretty p pl . unPretty1Of

instance (Pretty1 f, Pretty a) => Pretty (Pretty1Of f a) where
  pretty = pretty1


newtype Pretty2Of f a b = Pretty2Of { unPretty2Of :: f a b }
  deriving (Eq, Ord, Show)

instance Pretty2 f => Pretty2 (Pretty2Of f) where
  liftPretty2 pA plA pB plB = liftPretty2 pA plA pB plB . unPretty2Of

instance (Pretty2 f, Pretty a) => Pretty1 (Pretty2Of f a) where
  liftPretty = liftPretty2 pretty prettyList

instance (Pretty2 f, Pretty a, Pretty b) => Pretty (Pretty2Of f a b) where
  pretty = pretty1


-- Generics

class GPretty1 f where
  gliftPretty :: (a -> Doc ann) -> ([a] -> Doc ann) -> f a -> Doc ann
  gcollectPretty :: (a -> Doc ann) -> ([a] -> Doc ann) -> f a -> [Doc ann]
  gcollectPretty p pl a = [gliftPretty p pl a]

instance GPretty1 U1 where
  gliftPretty _ _ _ = emptyDoc

instance GPretty1 Par1 where
  gliftPretty p _ (Par1 a) = p a

instance Pretty c => GPretty1 (K1 i c) where
  gliftPretty _ _ (K1 a) = pretty a

instance Pretty1 f => GPretty1 (Rec1 f) where
  gliftPretty p pl (Rec1 a) = liftPretty p pl a

instance GPretty1 f => GPretty1 (M1 D c f) where
  gliftPretty p pl (M1 a) = gliftPretty p pl a

instance (Constructor c, GPretty1 f) => GPretty1 (M1 C c f) where
  gliftPretty p pl m = nest 2 (vsep (pretty (conName m) : gcollectPretty p pl (unM1 m)))

instance GPretty1 f => GPretty1 (M1 S c f) where
  gliftPretty p pl (M1 a) = gliftPretty p pl a

instance (GPretty1 f, GPretty1 g) => GPretty1 (f :+: g) where
  gliftPretty p pl (L1 l) = gliftPretty p pl l
  gliftPretty p pl (R1 r) = gliftPretty p pl r

instance (GPretty1 f, GPretty1 g) => GPretty1 (f :*: g) where
  gliftPretty p pl (a :*: b) = gliftPretty p pl a <+> gliftPretty p pl b
  gcollectPretty p pl (a :*: b) = gcollectPretty p pl a <> gcollectPretty p pl b

instance (Pretty1 f, GPretty1 g) => GPretty1 (f :.: g) where
  gliftPretty p pl (Comp1 a) = liftPretty (gliftPretty p pl) (list . map (gliftPretty p pl)) a
