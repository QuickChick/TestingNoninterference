module SingleStateArb where

import Test.QuickCheck

import Control.Monad

import Labels
import Generation
import Shrinking
import Primitives
import Machine
import Flags

stateOfVar :: Variation State -> State
stateOfVar (Var _ s _) = s

instance Arbitrary State where
    arbitrary = liftM stateOfVar . genVariationState $ 
                defaultFlags { strategy = GenByExec }
    shrink x = filter (/= x) $ map stateOfVar $  shrinkV (Var H x x)