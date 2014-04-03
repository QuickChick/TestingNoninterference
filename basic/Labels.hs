module Labels where

import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.Monad

import LaTeX

-- The very simple lattice we are using
data Label = L | H
  deriving (Eq, Ord, Read, Show)

instance LaTeX Label where
  toLaTeX L = "\\low"
  toLaTeX H = "\\high"

instance Arbitrary Label where
  arbitrary = frequency [(1,return L),(1,return H)]
  shrink lab = [L | lab==H]

-- Arbitrary labeled data
data Labeled a = Labeled {lab :: Label, value :: a}
  deriving (Eq, Ord, Read)

instance Functor Labeled where
  fmap f (Labeled l a) = Labeled l (f a)

instance Show a => Show (Labeled a) where
  show s = show (value s) ++ "@" ++ show (lab s)

instance LaTeX a => LaTeX (Labeled a) where
  toLaTeX (Labeled l a) = toLaTeX a ++ " \\labelsym " ++ toLaTeX l

instance Arbitrary a => Arbitrary (Labeled a) where
  arbitrary = liftM2 Labeled arbitrary arbitrary
  shrink (Labeled a b) = map (uncurry Labeled) (shrink (a,b))

-- QuickCheck generator combinator for labeled data
labeled :: Gen a -> Gen (Labeled a)
labeled = liftM2 Labeled arbitrary

withLab :: Labeled a -> Label -> Labeled a
withLab labeled lab = labeled{lab=lab}
infix 1 `withLab`

class Lub a where 
  lub :: a -> a -> a

instance Lub Label where
  lub = max

-- a@l `tainting` b@r == b@(l `lub` r); this allows for an easy
-- way to compute the proper label for some piece of data by combining it with
-- all the other data which affected it; as long as the current data comes last,
-- everything will work properly.
tainting :: Labeled a -> Labeled b -> Labeled b
tainting a b = Labeled (lab a `lub` lab b) (value b)

instance Num a => Num (Labeled a) where
  Labeled l m + Labeled l' m' = Labeled (max l l') (m+m')
  Labeled l m - Labeled l' m' = Labeled (max l l') (m-m')
  Labeled l m * Labeled l' m' = Labeled (max l l') (m*m')
  abs (Labeled l m) = Labeled l (abs m)
  signum (Labeled l m) = Labeled l (signum m)
  fromInteger n = Labeled L (fromInteger n)

