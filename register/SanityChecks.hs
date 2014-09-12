{-# LANGUAGE TupleSections, RecordWildCards #-}
module SanityChecks where

import Test.QuickCheck
import Test.QuickCheck.Property

import Machine
import Reachability
import Rules
import SingleStateArb()
import Primitives
import Indist
import Generation
import Flags
import Memory 
import Instructions

--import Pretty
--import Text.PrettyPrint (text,($$))
--import qualified Text.PrettyPrint as PP

propStampsWellFormed :: Property
propStampsWellFormed = forAllShrink arbitrary shrink 
                       (\st -> wellFormed (st :: State IMem (Mem Atom)))

propPreservesWellFormed :: Int -> RuleTable -> State IMem (Mem Atom) -> Property
propPreservesWellFormed 0 _ _ = property True
propPreservesWellFormed noSteps t st =
    if wellFormed st then 
        case exec t st of 
          Just st' -> propPreservesWellFormed (noSteps - 1) t st'
          Nothing  -> property True
    else property False

propGenIndist :: Property
propGenIndist = forAll (genVariationState (llniConfig defaultFlags)) aux 
    where aux :: Variation (State IMem (Mem Atom)) -> Bool
          aux (Var obs st1 st2) = indist obs st1 st2

