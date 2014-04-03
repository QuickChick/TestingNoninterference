{-# LANGUAGE FlexibleContexts #-}

module Observable where

import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.Monad
import Control.Applicative
import Data.Function

import LaTeX
import Labels
import Flags

data Variation a = Variation a a
  deriving Eq

instance (Eq a, Show a) => Show (Variation a) where
  show (Variation a a') 
    | a == a' = show a
    | otherwise = let (pre,l,l',post) = common (show a) (show a')
                  in concat [pre, "{", l, "/", l', "}", post]
    where
      common xs ys = let (pre,  xs', ys')  = commonL False xs ys
                         (tsop, sx', sy') = (commonL False `on` reverse) xs' ys'
                     in (pre, reverse sx', reverse sy', reverse tsop)

instance (Eq a, LaTeX a) => LaTeX (Variation a) where
  toLaTeX (Variation a a')
    | a == a'   = toLaTeX a
    | otherwise = let (pre,l,l',post) = (common `on` toLaTeX) a a'
                  in concat [pre, "\\variation{", l, "}{", l', "}", post]
    where
      common xs ys = let (pre,  xs', ys')  = commonL True xs ys
                         (tsop, sx', sy') = (commonL False `on` reverse) xs' ys'
                     in (pre, reverse sx', reverse sy', reverse tsop)

commonL :: Bool -> String -> String -> (String, String, String)
commonL checkBackslash = common'
  where common' xs     []                 = ([],xs,[])
        common' []     ys                 = ([],[],ys)
        common' (x:xs) (y:ys)
          | x == y    = let (zs,xs',ys') = common' xs ys
                        in case (checkBackslash,x,zs) of
                             (True,'\\',[]) -> (zs,'\\':xs,'\\':ys)
                             _              -> (x:zs,xs',ys')
          | otherwise = ([],x:xs,y:ys)

instance Functor Variation where
  fmap f (Variation a a') = Variation (f a) (f a')

-- (~~~) represents an equivalence relation which is not capable of
-- discerning the difference between H-labeled data.  So a@L ~~~ b@L
-- iff a == b, and a@H ~~~ b@H for all a and b.  Differently-labeled
-- values are never equivalent.  The vary function changes H-tagged
-- values in a structure to arbitrary other values; it should always
-- be the case that liftA2 (~~~) (vary x) (pure x) is true.
class Observable a where
  (~~~) :: a -> a -> Bool
     -- low-indistinguishable
  vary  :: a -> Gen a
     -- produces ~~~ outputs
  shrinkV :: Variation a -> [Variation a]
     -- demands ~~~ inputs and produces ~~~ outputs

errorShrinkV :: (Show a, Observable a) => String -> Variation a -> b
errorShrinkV inst (Variation a a') =
  error $ "shrinkV for " ++ inst ++ " received " ++ (if a ~~~ a'
                                                       then "unhandled ~~~ arguments"
                                                       else "non-~~~ arguments" ++ show a ++ "\n" ++ show a')

instance (Arbitrary a, Observable a) => Arbitrary (Variation a) where
  arbitrary = do a <- arbitrary
                 a' <- vary a
                 return $ Variation a a'
  shrink = shrinkV

instance (Flaggy DynFlags, Arbitrary a, Observable a) => Observable (Labeled a) where
  (Labeled L x) ~~~ (Labeled L y) = x ~~~ y
  (Labeled H _) ~~~ (Labeled H _) = True
  (Labeled _ x) ~~~ (Labeled _ y) =
    case atom_equiv getFlags of
      LabelsObservable -> False
      LabelsNotObservable -> x ~~~ y
      HighEquivEverything -> True

  vary _ = error "Observable (Labeled a) implements no vary"

  shrinkV (Variation (Labeled L x) (Labeled L x')) | x ~~~ x' =
    map (fmap $ Labeled L) . shrinkV $ Variation x x'
  shrinkV (Variation (Labeled H x) (Labeled H x')) =
    (if x ~~~ x' then
      [Variation (Labeled L x) (Labeled L x')]
    else
      [Variation (Labeled L x) (Labeled L x),
       Variation (Labeled L x') (Labeled L x')])
   ++ 
    [Variation (Labeled H y) (Labeled H y') | (y,y') <- shrink (x,x') ]
   ++ 
    [Variation (Labeled H y) (Labeled H y') | y <- shrink x, y' <- shrink x' ]
  shrinkV (Variation (Labeled l x) (Labeled l' y)) =
    case atom_equiv getFlags of
      LabelsObservable -> []
      LabelsNotObservable ->
        if l==H || l'==H then
          [Variation (Labeled L x) (Labeled L y) | x ~~~ y] ++
          [Variation (Labeled l x') (Labeled l' y') |
               x ~~~ y
             , Variation x' y' <- shrinkV (Variation x y) ]
        else []
      HighEquivEverything ->
        if l==H || l'==H then
          [Variation (Labeled L x) (Labeled L y) | x ~~~ y] ++
          [Variation (Labeled l x') (Labeled l' y) | x' <- shrink x] ++
          [Variation (Labeled l x) (Labeled l' y') | y' <- shrink y]
        else []

prop_shrinkV :: (Observable a, Arbitrary a) => Variation a -> Bool 
prop_shrinkV = all (\ (Variation v v') -> v ~~~ v') . shrinkV

instance Observable Int where
  (~~~)                 = (==)
  vary = return
  shrinkV (Variation i i') = [Variation j j | j <- shrink i]

instance Observable Bool where
  (~~~) = (==)
  vary = return
  shrinkV (Variation i i') = [Variation j j | j <- shrink i]

instance (Show a, Observable a) => Observable (Maybe a) where
  Just x  ~~~ Just y  = x ~~~ y
  Nothing ~~~ Nothing = True
  _       ~~~ _       = False
  
  vary (Just x) = Just <$> vary x
  vary Nothing  = pure Nothing
  
  shrinkV (Variation Nothing Nothing)   = []
  shrinkV (Variation (Just x) (Just y)) =
    Variation Nothing Nothing : map (fmap Just) (shrinkV $ Variation x y)
  shrinkV v = errorShrinkV "Maybe a" v

instance (Show a, Observable a) => Observable [a] where
  xs ~~~ ys             = length xs == length ys && and (zipWith (~~~) xs ys)
  vary = mapM vary
  shrinkV (Variation [] []) = []
  shrinkV (Variation (a:as) (a':as')) = 
       Variation as as'
    :  [Variation (v:as) (v':as') | Variation v v' <- shrinkV (Variation a a')]
    ++ [Variation (a:vs) (a':vs') | Variation vs vs' <- shrinkV (Variation as as')]
  shrinkV v = errorShrinkV "[a]" v

instance (Observable a,Observable b) => Observable (a,b) where
  (a,b) ~~~ (a',b') = a ~~~ a' && b ~~~ b'
  vary (a,b) = liftM2 (,) (vary a) (vary b)
  shrinkV (Variation (a,b) (a',b')) = 
      [Variation (v,b) (v',b') | Variation v v' <- shrinkV (Variation a a')]
   ++ [Variation (a,v) (a',v') | Variation v v' <- shrinkV (Variation b b')]

instance (Observable a,Observable b,Observable c) => Observable (a,b,c) where
  (a,b,c) ~~~ (a',b',c') = (a,(b,c)) ~~~ (a',(b',c'))
  vary (a,b,c) = liftM3 (,,) (vary a) (vary b) (vary c)
  shrinkV  = map (fmap $ \ (a,(b,c)) -> (a,b,c)) .
                 shrinkV . fmap (\ (a,b,c) -> (a,(b,c)))

prop_observable_refl  :: Observable a => a -> Bool
prop_observable_sym   :: (Show a, Observable a) => a -> Property 
prop_observable_trans :: (Show a, Observable a) => a -> Property
prop_observable_refl  a = a ~~~ a
prop_observable_sym   a = forAll (vary a) $ \b -> (a ~~~ b) && (b ~~~ a)
prop_observable_trans a = forAll (vary a) $ \b ->
                          forAll (vary b) $ \c ->
                            a ~~~ c

prop_observable :: (Show a, Observable a) => a -> Property
prop_observable a = 
  forAll (vary a) (~~~ a)
