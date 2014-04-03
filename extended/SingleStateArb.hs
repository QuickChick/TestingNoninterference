module SingleStateArb where

import Test.QuickCheck

import Control.Monad

import Labels
import Generation
import Shrinking
import Primitives
import Machine

stateOfVar :: Variation State -> State
stateOfVar (Var _ s _) = s

instance Arbitrary State where
    arbitrary = liftM stateOfVar genVariationState
    shrink x = filter (/= x) $ map stateOfVar $  shrinkV (Var H x x)