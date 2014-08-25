{-# LANGUAGE TupleSections, RecordWildCards, FlexibleInstances #-}
module Pretty where

-- import Debug.Trace
    
import Memory
import Primitives
import Labels
import Machine
import Instructions

import Data.Maybe

import Text.PrettyPrint (Doc, (<+>), ($$), (<>), text)
import qualified Text.PrettyPrint as PP 

class PP a where
    pp :: a -> Doc
instance (PP a, PP b) => PP (a,b) where
    pp (a,b) = PP.parens $ pp a <> PP.comma <+> pp b

instance PP a => PP [a] where
    pp l = aux 0 l
         where aux :: PP a => Int -> [a] -> Doc
               aux _ [] = PP.empty
               aux n (h:t) = pp n <+> PP.colon <+> pp h $$ aux (n+1) t

instance PP a => PP (Frame a) where
    pp (Frame stamp label atoms) = 
        text "Stamp: " <+> pp stamp $$
        text "Label: " <+> pp label $$ 
        text "Data:  " <+> PP.nest 8 (pp atoms)

instance PP Int where
    pp = PP.text . show

instance PP Label where
    pp = PP.text . show

instance PP Block where
    pp (Block n l) = pp (n,l)

instance PP BinOpT where
    pp BAdd  = PP.char '+'
    pp BMult = PP.char '*'
    pp BFlowsTo = PP.text "Flows"
    pp BJoin = PP.text "/\\"

instance PP Instr where
    pp (Lab r1 r2)        = text "Lab"     <+> pp r1 <+> pp r2
    pp (MLab r1 r2)       = text "MLab"    <+> pp r1 <+> pp r2
    pp (PcLab r1)         = text "PcLab"   <+> pp r1
    pp (BCall r1 r2 r3)   = text "BCall"   <+> pp r1 <+> pp r2 <+> pp r3
    pp (BRet)             = text "BRet"
    pp (PutLab l r1)      = text "PutBot"  <+> pp l  <+> pp r1
    pp (Noop)             = text "Noop"
    pp (Put n r1)         = text "Put"     <+> pp n  <+> pp r1
    pp (BinOp o r1 r2 r3) = text "BinOp"   <+> pp o  <+> pp r1 <+> pp r2 <+> pp r3
    pp (Jump r1)          = text "Jump"    <+> pp r1
    pp (Bnz n r1)         = text "Bnz"     <+> pp n  <+> pp r1
    pp (Load r1 r2)       = text "Load"    <+> pp r1 <+> pp r2
    pp (Store r1 r2)      = text "Store"   <+> pp r1 <+> pp r2
    pp (Write r1 r2)      = text "Write"   <+> pp r1 <+> pp r2
    pp (Alloc r1 r2 r3)   = text "Alloc"   <+> pp r1 <+> pp r2 <+> pp r3
    pp (PSetOff r1 r2 r3) = text "PSetOff" <+> pp r1 <+> pp r2 <+> pp r3
    pp (Halt)             = text "Halt"
    pp (PGetOff r1 r2)    = text "PGetOff" <+> pp r1 <+> pp r2
    pp (MSize r1 r2)      = text "MSize"   <+> pp r1 <+> pp r2
    pp (Mov r1 r2)        = text "Mov  "   <+> pp r1 <+> pp r2

instance PP Pointer where
    pp (Ptr b n) = PP.parens $ pp b <+> PP.colon <+> pp n

instance PP Value where
    pp (VInt x) = pp x
    pp (VPtr x) = pp x
    pp (VLab x) = pp x

instance PP Atom where
    pp (Atom v l) = pp v <+> PP.char '@' <+> pp l

instance PP PtrAtom where
    pp (PAtm n l) = PP.brackets (pp n) <+> PP.char '@' <+> pp l

instance PP RegSet where
    pp (RegSet rs) = text "Registers: " $$ (PP.nest 2 $ pp rs)

instance PP StkElt where
    pp (StkElt (pc, l, rs, r)) = 
        text "Stack Element: " $$ 
             PP.nest 2 (text "RetPC: "    <+> pp pc $$ 
                        text "RetLabel: " <+> pp l $$
                        pp rs $$
                        text "Target: " <+> pp r)

instance PP Stack where
    pp (Stack s) = text "Stack: " $$ PP.nest 2 (pp s)

instance (Show a, Read a, Eq a, PP a) => PP (Mem a) where
    pp m = text "Memory: " $$ PP.nest 2 (PP.vcat $ map ppLevel [L, M1, M2, H])
        where ppLevel :: Label -> Doc 
              ppLevel l = let blocks = getBlocksAtLevel m l 
                              frames = catMaybes $ map (getFrame m) blocks
                          in case frames of 
                               [] -> PP.empty 
                               _  -> text "Level " <+> pp l <> PP.colon $$ 
                                     PP.nest 2 (pp frames)

instance PP (State IMem (Mem Atom)) where
    pp s@State{..} = 
        text "State:" $$ PP.nest 2 (
          text "PC: " <+> pp pc $$                                    
          text "IMem: " <+> pp imem $$ 
          pp mem $$ 
          pp stack $$ 
          pp regs)

ppVar :: (PP a, PP b) => a -> b -> Doc
ppVar a b = PP.braces $ pp a <> PP.char '/' <> pp b

ppPrim :: (Eq a, PP a) => Variation a -> Doc
ppPrim (Var obs x y) | x == y    = pp x 
                     | otherwise = ppVar x y

instance PP (Variation Int) where
    pp = ppPrim

instance PP (Variation Label) where
    pp = ppPrim

instance PP (Variation Block) where
    pp (Var obs b1@(Block n1 l1) b2@(Block n2 l2)) =
        pp (Var obs n1 n2, Var obs l1 l2)

instance PP (Variation Pointer) where
    pp (Var obs (Ptr b1 n1) (Ptr b2 n2)) = 
        PP.parens $ pp (Var obs b1 b2) 
              <+> PP.colon <+> pp (Var obs n1 n2)

instance PP (Variation Value) where
    pp = ppPrim

instance PP (Variation Atom) where
    pp (Var obs a1@(Atom v1 l1) a2@(Atom v2 l2)) 
        | a1 == a2 = pp a1 
        | v1 /= v2 && l1 /= l2 = ppVar a1 a2
        | otherwise = pp (Var obs v1 v2) <+> PP.char '@' 
                      <+> pp (Var obs l1 l2)

instance PP (Variation PtrAtom) where
    pp (Var obs p1@(PAtm n1 l1) p2@(PAtm n2 l2)) 
        | p1 == p2 = pp p1
        | n1 /= n2 && l1 /= l2 = ppVar p1 p2
        | otherwise = pp (Var obs (VInt n1) (VInt n2))
                      <+> PP.char '@' <+> pp (Var obs l1 l2)

instance PP (Variation RegSet) where
    pp (Var obs (RegSet rs1) (RegSet rs2)) = 
        text "Registers: " $$ 
        pp (zipWith (Var obs) rs1 rs2)

instance PP (Variation StkElt) where
    pp (Var obs (StkElt (pc1, l1, rs1, r1)) (StkElt (pc2, l2, rs2, r2))) = 
        text "Stack Element: " $$ 
             PP.nest 2 (text "RetPC: "    <+> pp (Var obs pc1 pc2) $$ 
                        text "RetLabel: " <+> pp (Var obs l1 l2) $$
                        pp (Var obs rs1 rs2) $$
                        text "Target: " <+> pp (Var obs r1 r2))

instance PP (Variation Stack) where
    pp (Var obs (Stack s1) (Stack s2)) =
        text "Stack: " $$ PP.nest 2 (pp (zipWith (Var obs) s1 s2))

instance PP (Variation (Frame Atom)) where
    pp (Var obs f1@(Frame s1 l1 a1) f2@(Frame s2 l2 a2)) 
        | s1 == s2 = 
            text "Stamp: " <+> pp s1 $$
            text "Label: " <+> pp (Var obs l1 l2) $$ 
            text "Data:  " <+> PP.nest 8 (pp (zipWith (Var obs) a1 a2))
        | otherwise = ppVar f1 f2

instance PP (Variation Memory) where
    pp (Var obs m1 m2) = 
        text "Memory: " $$ PP.nest 2 (PP.vcat $ map ppLevel [L, M1, M2, H])
        where ppLevel :: Label -> Doc 
              ppLevel l = 
                let frames1 = catMaybes $ map (getFrame m1) $ getBlocksAtLevel m1 l
                    frames2 = catMaybes $ map (getFrame m2) $ getBlocksAtLevel m2 l
                in if frames1 == frames2 then
                       case frames1 of 
                         [] -> PP.empty
                         _  -> text "Level " <+> pp l <> PP.colon $$ 
                               PP.nest 2 (pp frames1)
                   else if l `flowsTo` obs then
                            pp (zipWith (Var obs) frames1 frames2)
                        else ppVar frames1 frames2

instance PP (Variation (State IMem (Mem Atom))) where
    pp (Var obs st1 st2) = 
        text "Observer: " <+> pp obs $$ 
        text "State:" $$ PP.nest 2 (
          text "PC: " <+> pp (Var obs (pc st1) (pc st2)) $$
          text "IMem: " <+> ppPrim (Var obs (imem st1) (imem st2)) $$ 
          pp (Var obs (mem st1) (mem st2)) $$ 
          pp (Var obs (stack st1) (stack st2)) $$ 
          pp (Var obs (regs st1) (regs st2)))
