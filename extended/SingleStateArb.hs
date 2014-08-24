{-# LANGUAGE FlexibleInstances #-}
module SingleStateArb where

import Test.QuickCheck

import Control.Monad

import Labels
import Generation
import Shrinking
import Primitives
import Machine
import Flags
import Memory
import Instructions

stateOfVar :: Variation (State i m) -> (State i m)
stateOfVar (Var _ s _) = s

instance Arbitrary (State IMem (Mem Atom)) where
    arbitrary = liftM stateOfVar . genVariationState $ llniConfig defaultFlags 
    shrink x = filter (/= x) $ map stateOfVar $  shrinkV (Var H x x)