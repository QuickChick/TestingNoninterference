{-# LANGUAGE TupleSections, RecordWildCards #-}
module Instructions where

import Labels
import Memory

type RegPtr = Int

data Pointer = Ptr Block Int
             deriving (Eq, Show, Read)

data Value = VInt Int 
           | VPtr Pointer
           | VLab Label
             deriving (Eq, Show, Read)

data Atom = Atom Value Label
            deriving (Eq, Show, Read)

data PtrAtom = PAtm Int Label
               deriving (Eq, Show, Read)
             
data BinOpT = BAdd 
            | BMult 
            | BFlowsTo 
            | BJoin 
              deriving (Eq, Show, Read)

evalBinop :: BinOpT -> Value -> Value -> Maybe Value
evalBinop BAdd  (VInt x) (VInt y) = Just . VInt $ x + y
evalBinop BMult (VInt x) (VInt y) = Just . VInt $ x * y
evalBinop BJoin    (VLab l1) (VLab l2) = Just . VLab $ lub l1 l2
evalBinop BFlowsTo (VLab l1) (VLab l2) = Just . VInt $ flows l1 l2 
evalBinop _ _ _ = Nothing

-- Conversion to register machine. 
data Instr = Lab     RegPtr RegPtr
           | MLab    RegPtr RegPtr
           | PcLab   RegPtr
           | BCall   RegPtr RegPtr RegPtr
           | BRet   
           | PutLab  Label  RegPtr
           | Noop
           | Put     Int    RegPtr
           | BinOp   BinOpT RegPtr RegPtr RegPtr
           | Jump    RegPtr
           | Bnz     Int    RegPtr
           | Load    RegPtr RegPtr
           | Store   RegPtr RegPtr
           | Write   RegPtr RegPtr
           | Alloc   RegPtr RegPtr RegPtr
           | PSetOff RegPtr RegPtr RegPtr
           | Halt    
           | PGetOff RegPtr RegPtr
           | MSize   RegPtr RegPtr 
           | Mov     RegPtr RegPtr
             deriving (Show, Eq, Read)

data InstrKind = LAB
               | MLAB
               | PCLAB
               | BCALL
               | BRET           
               | PUTLAB
               | NOOP           
               | PUT
               | BINOP
               | JUMP
               | BNZ
               | LOAD
               | STORE
               | WRITE
               | ALLOC
               | PSETOFF
               | HALT
               | PGETOFF
               | MSIZE
               | MOV
             deriving (Show, Eq, Read, Ord)

allInstrKind :: [InstrKind]
allInstrKind = [ LAB
               , MLAB
               , PCLAB
               , BCALL
               , BRET           
               , PUTLAB
               , NOOP           
               , PUT
               , BINOP
               , JUMP
               , BNZ
               , LOAD
               , STORE
               , WRITE
               , ALLOC
               , PSETOFF
               , HALT
               , PGETOFF
               , MSIZE 
               , MOV ]

opcodeOfInstr :: Instr -> Maybe InstrKind
opcodeOfInstr (Lab _ _      ) = Just LAB
opcodeOfInstr (MLab _ _     ) = Just MLAB
opcodeOfInstr (PcLab _      ) = Just PCLAB
opcodeOfInstr (BCall _ _ _  ) = Just BCALL
opcodeOfInstr (BRet         ) = Just BRET
opcodeOfInstr (PutLab _ _   ) = Just PUTLAB
opcodeOfInstr (Noop         ) = Just NOOP
opcodeOfInstr (Put _ _      ) = Just PUT
opcodeOfInstr (BinOp _ _ _ _) = Just BINOP
opcodeOfInstr (Jump _       ) = Just JUMP
opcodeOfInstr (Bnz _ _      ) = Just BNZ
opcodeOfInstr (Load _ _     ) = Just LOAD
opcodeOfInstr (Store _ _    ) = Just STORE
opcodeOfInstr (Write _ _    ) = Just WRITE
opcodeOfInstr (Alloc _ _ _  ) = Just ALLOC
opcodeOfInstr (PSetOff _ _ _) = Just PSETOFF
opcodeOfInstr (PGetOff _ _  ) = Just PGETOFF
opcodeOfInstr (MSize _ _    ) = Just MSIZE
opcodeOfInstr (Mov _ _      ) = Just MOV
opcodeOfInstr (Halt         ) = Nothing 
