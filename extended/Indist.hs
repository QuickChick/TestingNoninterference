{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Indist where

import Data.Maybe

import Debug.Trace
import System.IO.Unsafe


import Labels
import Primitives
import Memory
import Machine
import Instructions

-- Indistinguishability type class
class Indist a where
    indist :: Label -> a -> a -> Bool

instance Indist Instr where
    indist = const (==)

instance Indist Label where
    indist = const (==)

instance Indist Int where
    indist = const (==)

-- Value indistinguishability
-- * Ignores the label
-- * For pointers, it is syntactic equality because of the per-level allocator
instance Indist Value where
    indist _ (VInt x) (VInt y) = x == y
    indist _ (VLab x) (VLab y) = x == y
    indist _ (VPtr x) (VPtr y) = x == y
    indist _ _ _ = False
              
-- Atom indistinguishability
-- * Equal Labels (public labels)
-- ** Low Labels  -> Indist Values
-- ** High Labels -> True
instance Indist Atom where
    indist obs (Atom v1 l1) (Atom v2 l2) 
        | l1 /= l2         = False
        | l1 `flowsTo` obs = indist obs v1 v2
        | otherwise        = True

-- PC indistinguishability 
-- * Both High -> Ok
-- * At least one PC low -> syntactic equality
-- ! The pc protects it's own label !
instance Indist PtrAtom where
    indist obs (PAtm i1 l1) (PAtm i2 l2) 
        | isLow l1 obs || isLow l2 obs =
            l1 == l2 && i1 == i2
        | otherwise = True

-- List Indistinguishability 
instance Indist a => Indist [a] where
    indist obs [] [] = True
    indist obs (x:xs) (y:ys) = indist obs x y && indist obs xs ys
    indist _ _ _ = False

-- RegSet Indistinguishability
instance Indist RegSet where
    indist obs (RegSet as) (RegSet as') = indist obs as as'
        
-- Frame Indistinguishability
-- * If both stamps are low 
--   + Equal labels 
--   + Low Labels -> Indistinguishable data
-- * Otherwise both need to be high
-- * LL: Unclear : One high, one low?
instance Indist a => Indist (Frame a) where
    indist obs (Frame s1 l1 as1) (Frame s2 l2 as2) 
        | isLow s1 obs || isLow s2 obs =
            l1 == l2 && (if isLow l1 obs 
                         then indist obs as1 as2
                         else True)
        | otherwise = True
        

-- Memory Indistinguishability
-- * Get all memory frames with stamps below the obs level, in the same order
-- * Ensure they are pairwise indistinguishable
instance Indist (Mem Atom) where
    indist obs m1 m2 = indist obs (getFrames m1) (getFrames m2)
        where getFrames m = catMaybes $ map (getFrame m) (getBlocksBelow obs m)

-- Cropping the high part of the stack

isLowStkElt :: Label -> StkElt -> Bool
isLowStkElt obs (StkElt (pc,_,_,_)) = isLow (pcLab pc) obs

filterStack :: Label -> Stack -> Stack
filterStack obs (Stack s) = Stack $ filter (isLowStkElt obs) s

-- CH: unused
-- cropTop :: Label -> Stack -> Stack
-- cropTop _ (Stack []) = Stack []
-- cropTop obs s@(Stack (StkElt (pc, _, _ ,_):s')) =
--     if isLow (pcLab pc) obs then s else cropTop obs $ Stack s'

-- Indistinghuishability of *LOW-PC* Stack Elements
instance Indist StkElt where
    indist obs (StkElt (pc1, l1, rs1, r1)) (StkElt (pc2, l2, rs2, r2)) =
        indist obs pc1 pc2 
        && indist obs l1 l2 
        && indist obs rs1 rs2 
        && indist obs r1 r2
               
instance Indist Stack where
    indist obs s1 s2 = 
        indist obs (unStack $ filterStack obs s1) (unStack $ filterStack obs s2)

-- State indistinguishability
-- * If both pc's are high, memories and stacks must be indistinguishable
--   ++ Could be weakened with reachability
-- * If at least one is low, pairwise indist

debug :: String -> Bool -> Bool
--debug s x = if not x then unsafePerformIO $ do putStrLn s >> return x else x
debug s x = x

instance (IMemC i, MemC m Atom, Indist m, Indist i) =>Indist (State i m) where 
    indist obs (State imem1 mem1 s1 regs1 pc1) (State imem2 mem2 s2 regs2 pc2) =
        debug "IMemory" (indist obs imem1 imem2)
        && debug "Memory" (indist obs mem1 mem2)
        && debug "Stack" (indist obs s1 s2)
        && if isLow (pcLab pc1) obs || isLow (pcLab pc2) obs then
               indist obs pc1 pc2
               && debug "Registers" (indist obs regs1 regs2 )
           else True
