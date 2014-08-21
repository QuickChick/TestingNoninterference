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

--import Pretty
--import Text.PrettyPrint (text,($$))
--import qualified Text.PrettyPrint as PP

propStampsWellFormed :: Property
propStampsWellFormed = forAllShrink arbitrary shrink wellFormed

propPreservesWellFormed :: Int -> RuleTable -> State -> Property
propPreservesWellFormed 0 _ _ = property True
propPreservesWellFormed noSteps t st =
    if wellFormed st then 
        case exec t st of 
          Just st' -> propPreservesWellFormed (noSteps - 1) t st'
          Nothing  -> property True
    else property False

propGenIndist :: Property
propGenIndist = forAll (genVariationState (llniConfig defaultFlags)) aux 
    where aux :: Variation State -> Bool
          aux (Var obs st1 st2) = indist obs st1 st2

