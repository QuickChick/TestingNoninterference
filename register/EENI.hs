{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module EENI where

import Test.QuickCheck

import Machine
import Primitives
import Labels
import Instructions
import Rules
import Memory

import Indist    
import Flags
    
propEENI :: (MemC m Atom, IMemC i, Indist i, Indist m) => 
            Flags -> RuleTable -> Variation (State i m) -> Property
propEENI Flags{..} t (Var obs st1 st2) =
--    traceShow ("Here!") $
    let st1' = last $ execN noSteps t st1
        st2' = last $ execN noSteps t st2
        isLowState st = isLow (pcLab $ pc st) obs
        isHalted st = instrLookup (imem st) (pc st) == Just Halt
    in isLowState st1' && isLowState st2' && isHalted st1' && isHalted st2' 
       ==> 
       case testProp of 
         TestEENI -> indist obs st1' st2'
         TestEENI_Weak -> 
             let p (Atom (VInt _) l) = l `flowsTo` obs 
                 p _ = False
             in filter p (unRegSet $ regs st1') == filter p (unRegSet $ regs st2')
         _ -> error "Unexpected (EENI)"
