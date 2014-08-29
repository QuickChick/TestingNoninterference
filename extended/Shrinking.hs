{-# LANGUAGE RecordWildCards, FlexibleInstances #-}
module Shrinking where

import Control.Monad

import Test.QuickCheck

import Data.List

import Primitives
import Machine
import Instructions
import Labels
import Memory

-- Basic Shrink Instances

instance Arbitrary Instr where
    arbitrary = error "No arbitrary instruction"
    shrink Noop = []
    shrink _    = [Noop]

instance Arbitrary Label where
    arbitrary = error "No arbitrary label"
    shrink L = []
    shrink l = filter (/= l) $ labelsBelow l

instance Arbitrary Pointer where
    arbitrary = error "No arbitrary pointer"
    shrink (Ptr b i) = map (Ptr b) $ shrink i

instance Arbitrary Value where
    arbitrary = error "No arbitrary value"
    shrink (VInt x) = map VInt $ shrink x
    shrink (VPtr x) = VInt 0 : (map VPtr $ shrink x)
    shrink (VLab x) = VInt 0 : (map VLab $ shrink x)

instance Arbitrary Atom where
    arbitrary = error "No arbitrary atom"
    shrink (Atom v l) = [Atom v' l | v' <- shrink v] 
                     ++ [Atom v l' | l' <- shrink l]

-- ShrinkV gets indist Variations and returns indist
class ShrinkV a where
    shrinkV :: Variation a -> [Variation a]

instance ShrinkV Label where
    shrinkV (Var obs l1 l2) = map (join (Var obs)) $ shrink l1

------ Shrink Instructions (incomplete)

shrinkToNoops :: Label -> [Instr] -> [Instr] -> [Variation [Instr]]
shrinkToNoops obs l1 l2 = aux l1 l2 
    where aux [] [] = []
          aux (Noop:t1) (Noop:t2) = 
              [Var obs (Noop:t1') (Noop:t2') | Var obs t1' t2' <- aux t1 t2]
          aux (h1:t1) (h2:t2) = 
              (Var obs (Noop:t1) (Noop:t2)) 
              : [Var obs (Noop:t1') (Noop:t2') | Var obs t1' t2' <- aux t1 t2]
          aux _ _ = error "Unequal length instruction list (shrinkToNoops)"

noopRemove :: Label -> [Instr] -> [Instr] -> [Variation [Instr]]
noopRemove obs l1 l2 = aux l1 l2 
    where aux [] [] = []
          aux (Noop:t1) (Noop:t2) = 
              (Var obs t1 t2) 
              : [Var obs (Noop:t1') (Noop:t2') | Var obs t1' t2' <- aux t1 t2]
          aux (h1:t1) (h2:t2) = 
              [Var obs (Noop:t1') (Noop:t2') | Var obs t1' t2' <- aux t1 t2]
          aux _ _ = error "Unequal length instruction list (noopRemove)"

decrCpt :: Atom -> Maybe Atom
decrCpt (Atom (VInt n) l) = Just $ Atom (VInt $ n-1) l
decrCpt _ = Nothing

decrCptRegsV :: Variation RegSet -> [Variation RegSet]
decrCptRegsV (Var obs (RegSet rs1) (RegSet rs2)) =
    [Var obs (RegSet rs1') (RegSet rs2') 
         | Var obs rs1' rs2' <- aux rs1 rs2]
        where aux [] [] = []
              aux (h1:t1) (h2:t2) = 
                  case (decrCpt h1, decrCpt h2) of 
                    (Just h1', Just h2') -> ((Var obs (h1':t1) (h2':t2)):)
                    _ -> id 
                  $ [Var obs (h1:t1') (h2:t2') | Var obs t1' t2' <- aux t1 t2]
              aux _ _ = error "Register Sets of unequal length"

instance ShrinkV Value where
    shrinkV (Var obs (VInt x) (VInt _)) = 
        [Var obs (VInt x') (VInt x') | x' <- shrink x]
    shrinkV (Var obs (VPtr x) (VPtr _)) = 
        [Var obs (VPtr x') (VPtr x') | x' <- shrink x]
    shrinkV (Var obs (VLab x) (VLab _)) = 
        [Var obs (VLab x') (VLab x') | x' <- shrink x]
    shrinkV _ = error "Not indistinguishable Values (shrinkV Value)"

instance ShrinkV Atom where
    -- l1 == l2
    shrinkV (Var obs (Atom v1 l) (Atom v2 l2))
        | l /= l2 = error "Not equal labels in atoms (shrinkV atom)"
        | isLow l obs = 
            [Var obs (Atom v1' l) (Atom v2' l) 
                 | Var obs v1' v2' <- shrinkV (Var obs v1 v2)]
        | otherwise = -- High atoms 
            [Var obs a (Atom v2 l) | a <- map (flip Atom l) $ shrink v1] 
            ++ [Var obs (Atom v1 l) a | a <- map (flip Atom l) $ shrink v2] 
            -- TODO: Shrink the labels of the atoms

instance ShrinkV a => ShrinkV [a] where
    shrinkV (Var obs (h1:t1) (h2:t2)) =
        (Var obs t1 t2) 
        :  [Var obs (h1':t1) (h2':t2) | Var obs h1' h2' <- shrinkV (Var obs h1 h2)]
        ++ [Var obs (h1:t1') (h2:t2') | Var obs t1' t2' <- shrinkV (Var obs t1 t2)]
    shrinkV _ = []

----- ShrinkV Memories -----
shrinkFrameLabel :: Frame Atom -> [Frame Atom]
shrinkFrameLabel (Frame st l as) = [Frame st l' as | l' <- shrink l]

shrinkFrameAtoms :: Frame Atom -> [Frame Atom]
shrinkFrameAtoms (Frame st l as) = [Frame st l as' | as' <- shrink as]

-- TODO: Shrink stamps?? :/
instance ShrinkV (Frame Atom) where
    -- assumes st1 == st2
    shrinkV (Var obs f1@(Frame st1 l1 as1) f2@(Frame st2 l2 as2)) 
        | st1 /= st2 = error "Unequal stamps (shrinkV Frame)"
        | isHigh st1 obs && isHigh st2 obs = 
            [Var obs f1' f2' 
                 | f1' <- shrinkFrameLabel f1 ++ shrinkFrameAtoms f1,
                   f2' <- shrinkFrameLabel f2 ++ shrinkFrameAtoms f2]
        | isHigh (st1 `lub` l1) obs && isHigh (st2 `lub` l2) obs =
            concatMap (\l ->         
                  if isLow (st1 `lub` l) obs then 
                    [Var obs (Frame st1 l as1) (Frame st1 l as1),
                     Var obs (Frame st1 l as2) (Frame st1 l as2)]
                  else [Var obs (Frame st1 l as1) (Frame st1 l as2)]
                   ) (shrink l1) ++ -- l1 == l2
            [Var obs f1' f2' | f1' <- shrinkFrameAtoms f1, 
                               f2' <- shrinkFrameAtoms f2] 
        | otherwise = 
            concatMap (\l -> 
              [Var obs (Frame st1 l as1) (Frame st1 l as1),
               Var obs (Frame st1 l as2) (Frame st1 l as2)]
             ) (shrink l1) ++ -- l1 == l2
            [Var obs (Frame st1 l1 as1') (Frame st2 l2 as2') 
                 | Var obs as1' as2' <- shrinkV (Var obs as1 as2)]

instance ShrinkV (Mem Atom) where
    shrinkV (Var obs m1 m2) = 
        let lowB1 = getBlocksBelow obs m1
            lowB2 = getBlocksBelow obs m2
            highLabs = (labelsBelow H) \\ (labelsBelow obs) 
            highB1 = concatMap (getBlocksAtLevel m1) highLabs
            highB2 = concatMap (getBlocksAtLevel m2) highLabs
            low1 = map (ap (,) $ getFrame m1) lowB1
            low2 = map (ap (,) $ getFrame m2) lowB2
        in undefined

------ Shrink Stacks ------

-- While shrinking the stacks I must keep the corresponding 
-- stack elements in sync
instance ShrinkV Stack where
    shrinkV (Var obs (Stack s1) (Stack s2)) = 
        case (s1, s2) of 
          ([], []) -> []
          (((se1@(StkElt (pc1,_,_,_))):s1'), 
           ((se2@(StkElt (pc2,_,_,_))):s2')) ->
           -- TODO: Shrink individual stack elements? 
           if isLow (pcLab pc1) obs && isLow (pcLab pc2) obs then 
               -- Both are low - try to delete the stack elements
               (Var obs (Stack s1') (Stack s2')) : 
               [Var obs (Stack (se1:s1'')) (Stack (se2:s2'')) 
                    | Var obs (Stack s1'') (Stack s2'') 
                      <- shrinkV (Var obs (Stack s1') (Stack s2'))]
           else if isLow (pcLab pc1) obs then
                    -- Machine 2 is High, can remove that top element
                    (Var obs (Stack s1) (Stack s2'))
                    : shrinkV (Var obs (Stack s1) (Stack s2'))
                else  -- Machine 1 is Low
                    (Var obs (Stack s1') (Stack s2))
                    : shrinkV (Var obs (Stack s1') (Stack s2))
          _ -> [Var obs (Stack []) (Stack [])]

instance ShrinkV a => ShrinkV (Frame a) where
    shrinkV (Var obs (Frame stamp1 label1 data1) (Frame stamp2 label2 data2))
        | stamp1 /= stamp2 = error "unequal stamps (shrinkV Frame)"
        | otherwise = [] -- TODO: Write this 
        
shrinkStacks :: Variation (State i m) -> [Variation (State i m)]
shrinkStacks (Var obs st1 st2) = 
    [Var obs st1{stack = stk1'} st2{stack = stk2'} 
         | Var obs stk1' stk2' <- shrinkV $ Var obs (stack st1) (stack st2)]

----------------- Removing a register --------------------

cDecr :: RegPtr -> RegPtr -> RegPtr
cDecr lim r = if lim <= r then r-1 else r

decrRegInstr :: RegPtr -> Instr -> Instr 
decrRegInstr r i = 
    case i of 
      Lab r1 r2        -> Lab     (cDecr r r1) (cDecr r r2)
      MLab r1 r2       -> MLab    (cDecr r r1) (cDecr r r2)
      PcLab r1         -> PcLab   (cDecr r r1)
      BCall r1 r2 r3   -> BCall   (cDecr r r1) (cDecr r r2) (cDecr r r3)
      BRet             -> BRet
      PutLab l r1      -> PutLab  l (cDecr r r1) 
      Noop             -> Noop
      Put n r1         -> Put n   (cDecr r r1)
      BinOp o r1 r2 r3 -> BinOp o (cDecr r r1) (cDecr r r2) (cDecr r r3)
      Jump r1          -> Jump    (cDecr r r1)
      Bnz n r1         -> Bnz n   (cDecr r r1)
      Load r1 r2       -> Load    (cDecr r r1) (cDecr r r2)
      Store r1 r2      -> Store   (cDecr r r1) (cDecr r r2)
      Write r1 r2      -> Write   (cDecr r r1) (cDecr r r2)
      Upgrade r1 r2    -> Upgrade (cDecr r r1) (cDecr r r2)
      Alloc r1 r2 r3   -> Alloc   (cDecr r r1) (cDecr r r2) (cDecr r r3)
      PSetOff r1 r2 r3 -> PSetOff (cDecr r r1) (cDecr r r2) (cDecr r r3)
      Halt             -> Halt
      MSize r1 r2      -> MSize   (cDecr r r1) (cDecr r r2)
      PGetOff r1 r2    -> PGetOff (cDecr r r1) (cDecr r r2)
      Mov r1 r2        -> Mov     (cDecr r r1) (cDecr r r2)

removeRegStkElt :: RegPtr -> StkElt -> StkElt
removeRegStkElt r' (StkElt (pc,l,rs,r)) = 
    let (front, _:back) = splitAt r' (unRegSet rs) 
    in StkElt (pc,l, RegSet $ front ++ back, cDecr r' r)

removeReg :: IMemC i => State i m -> RegPtr -> State i m
removeReg st@State{..} r = 
   let (front, _:back) = splitAt r (unRegSet regs) 
       imem'  = mapIMem (decrRegInstr r) imem
       stack' = mapStack' (removeRegStkElt r) stack
   in st{imem = imem', regs = RegSet $ front ++ back, stack = stack'}

removeRegisters :: IMemC i => Variation (State i m) -> [Variation (State i m)]
removeRegisters (Var obs st1 st2) =
    let rs1 = unRegSet $ regs st1
        rs2 = unRegSet $ regs st2
        allRegs = [0 .. length rs1 - 1]
        st1' = map (removeReg st1) allRegs
        st2' = map (removeReg st2) allRegs
    in map (uncurry $ Var obs) (zip st1' st2')

--- Shrink Register Contents --- 

-- Should only be done when pc's are low
shrinkRegisterContentsV :: Variation (State i m) -> [Variation (State i m)]
shrinkRegisterContentsV (Var obs st1 st2) = 
    if isLow (pcLab $ pc st1) obs && isLow (pcLab $ pc st2) obs then
    [ Var obs st1{regs = RegSet regs1'} 
              st2{regs = RegSet regs2'} 
    | Var obs regs1' regs2' 
        <- shrinkV $ Var obs (unRegSet $ regs st1) (unRegSet $ regs st2)]
    else []

{-
--- Shrink Instructions --- 

instance ShrinkV Instr where
    shrinkV (Var o x x') 
        | x == x' = [Var o y y | y <- shrink x]
        | otherwise = error "Instructions not the same in shrinkV"

shrinkInstructionsV :: IMemC i => Variation (State i m) -> [Variation (State i m)]
shrinkInstructionsV (Var obs st1 st2) = 
    [ Var obs st1{imem = imem1'} st2{imem = imem2'} 
          | Var obs imem1' imem2' 
              <- shrinkV $ Var obs (imem st1) (imem st2)]
-}
--- Combining everything ---

instance IMemC i => ShrinkV (State i m) where
    shrinkV v@(Var obs st1 st2) = 
        removeRegisters v 
        ++ shrinkStacks v
        ++ shrinkRegisterContentsV v
--        ++ shrinkInstructionsV v
    
