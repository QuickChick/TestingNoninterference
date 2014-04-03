{-# LANGUAGE TupleSections, RecordWildCards #-}
module Instructions where

type RegPtr = Int

data BinOpT = BAdd | BMult deriving (Eq, Show, Read)

evalBinop :: BinOpT -> Int -> Int -> Int
evalBinop BAdd  = (+)
evalBinop BMult = (*)

-- Conversion to register machine. 
data Instr = Lab     RegPtr RegPtr
           | MLab    RegPtr RegPtr
           | PcLab   RegPtr
           | BCall   RegPtr RegPtr RegPtr
           | BRet   
           | FlowsTo RegPtr RegPtr RegPtr
           | LJoin   RegPtr RegPtr RegPtr
           | PutBot  RegPtr
           | Noop
           | Put     Int    RegPtr
           | BinOp   BinOpT RegPtr RegPtr RegPtr
           | Jump    RegPtr
           | Bnz     Int    RegPtr
           | Load    RegPtr RegPtr
           | Store   RegPtr RegPtr
           | Alloc   RegPtr RegPtr RegPtr
           | PSetOff RegPtr RegPtr RegPtr
           | Output  RegPtr
           | Halt    
           | PGetOff RegPtr RegPtr
           | MSize   RegPtr RegPtr 
             deriving (Show, Eq, Read)

data InstrKind = LAB
               | MLAB
               | PCLAB
               | BCALL
               | BRET           
               | FLOWSTO
               | LJOIN
               | PUTBOT
               | NOOP           
               | PUT
               | BINOP
               | JUMP
               | BNZ
               | LOAD
               | STORE
               | ALLOC
               | PSETOFF
               | OUTPUT
               | HALT
               | PGETOFF
               | MSIZE
             deriving (Show, Eq, Read, Ord)

allInstrKind :: [InstrKind]
allInstrKind = [ LAB
               , MLAB
               , PCLAB
               , BCALL
               , BRET           
               , FLOWSTO
               , LJOIN
               , PUTBOT
               , NOOP           
               , PUT
               , BINOP
               , JUMP
               , BNZ
               , LOAD
               , STORE
               , ALLOC
               , PSETOFF
               , OUTPUT
               , HALT
               , PGETOFF
               , MSIZE ]

opcodeOfInstr :: Instr -> Maybe InstrKind
opcodeOfInstr (Lab _ _      ) = Just LAB
opcodeOfInstr (MLab _ _     ) = Just MLAB
opcodeOfInstr (PcLab _      ) = Just PCLAB
opcodeOfInstr (BCall _ _ _  ) = Just BCALL
opcodeOfInstr (BRet         ) = Just BRET
opcodeOfInstr (FlowsTo _ _ _) = Just FLOWSTO
opcodeOfInstr (LJoin _ _ _  ) = Just LJOIN
opcodeOfInstr (PutBot _     ) = Just PUTBOT
opcodeOfInstr (Noop         ) = Just NOOP
opcodeOfInstr (Put _ _      ) = Just PUT
opcodeOfInstr (BinOp _ _ _ _) = Just BINOP
opcodeOfInstr (Jump _       ) = Just JUMP
opcodeOfInstr (Bnz _ _      ) = Just BNZ
opcodeOfInstr (Load _ _     ) = Just LOAD
opcodeOfInstr (Store _ _    ) = Just STORE
opcodeOfInstr (Alloc _ _ _  ) = Just ALLOC
opcodeOfInstr (PSetOff _ _ _) = Just PSETOFF
opcodeOfInstr (Output _     ) = Just OUTPUT
opcodeOfInstr (PGetOff _ _  ) = Just PGETOFF
opcodeOfInstr (MSize _ _    ) = Just MSIZE
opcodeOfInstr (_            ) = Nothing 