{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module EENI where

import Debug.Trace

import Test.QuickCheck
import Test.QuickCheck.Property

import Machine
import Primitives
import Labels
import Instructions
import Rules
import Memory

import Control.Monad

import Indist    
import Generation
import Shrinking
import Flags
    
import Pretty
import Text.PrettyPrint (($$), text)
import qualified Text.PrettyPrint as PP

propEENI :: (MemC m Atom, IMemC i, Indist i, Indist m) => 
            Flags -> RuleTable -> Variation (State i m) -> Property
propEENI Flags{..} t (Var obs st1 st2) =
--    traceShow ("Here!") $
    let st1' = last $ execN noSteps t st1
        st2' = last $ execN noSteps t st2
        isLowState st = isLow (pcLab $ pc st) obs
        isHalted st = instrLookup (imem st) (pc st) == Just Halt
    in isLowState st1' && isLowState st2' && isHalted st1' && isHalted st2' 
       ==> indist obs st1' st2'

