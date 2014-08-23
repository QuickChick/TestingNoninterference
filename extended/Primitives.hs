{-# LANGUAGE TupleSections, RecordWildCards #-}
module Primitives where

import Memory
import Labels
import Instructions

type IMem = [Instr]

index :: Int -> [a] -> Maybe a
index _ []    = Nothing
index 0 (h:_) = Just h
index n (_:t) = index (n-1) t

update :: Int -> a -> [a] -> Maybe [a]
update _ _ [] = Nothing 
update 0 a (_:t) = Just $ a : t 
update n a (h:t) = fmap (h:) $ update (n-1) a t

instrLookup :: IMem -> PtrAtom -> Maybe Instr
instrLookup m (PAtm i _) = index i m

inc :: PtrAtom -> PtrAtom
inc (PAtm i l) = PAtm (i+1) l
                      
pcLab :: PtrAtom -> Label
pcLab (PAtm _ l) = l

atomLab :: Atom -> Label
atomLab (Atom _ l) = l

type Register = Atom
data RegSet = RegSet {unRegSet :: [Register]}
            deriving (Eq, Show, Read)

readR :: RegPtr -> RegSet -> Maybe Register
readR n (RegSet r) = index n r

writeR :: RegPtr -> Register -> RegSet -> Maybe RegSet 
writeR n a (RegSet r) = fmap RegSet $ update n a r

data StkElt = StkElt (PtrAtom, Label, RegSet, RegPtr)
              deriving (Eq, Show, Read)

data Stack = Stack { unStack :: [StkElt]}
    deriving (Eq, Show, Read)

mapStkElt :: (Atom -> Atom) -> StkElt -> StkElt
mapStkElt f (StkElt (pc,l,rs,r)) = 
    StkElt (pc, l, RegSet $ map f $ unRegSet rs, r)

mapStack :: (Atom -> Atom) -> Stack -> Stack
mapStack f (Stack s) = Stack $ map (mapStkElt f) s

mapStack' :: (StkElt -> StkElt) -> Stack -> Stack
mapStack' f (Stack s) = Stack $ map f s

joinAtom :: Atom -> Label -> Atom
joinAtom (Atom a l) l' = Atom a $ l `lub` l'

joinPtrAtom :: PtrAtom -> Label -> PtrAtom
joinPtrAtom (PAtm n l) l' = PAtm n $ l `lub` l'

type Memory = Mem Atom

alloc :: Int -> Label -> Label -> Atom -> Memory -> Maybe (Block, Memory)
alloc size label stamp atom mem 
      | size < 0 = Nothing
      | size > 42 = Nothing -- Discard *large* allocations
      | otherwise = 
          Just $ allocate mem stamp $ Frame stamp label $ replicate size atom
    
load :: Memory -> Pointer -> Maybe Atom
load m (Ptr frame address) = do 
  Frame _ _ as <- getFrame m frame
  index address as

store :: Memory -> Pointer -> Atom -> Maybe Memory
store m (Ptr f addr) a = do
  Frame stamp lab as <- getFrame m f
  as' <- update addr a as
  updFrame m f $ Frame stamp lab as'

msize :: Memory -> Pointer -> Maybe Int
msize m (Ptr fp i) = do
  Frame _ _ as <- getFrame m fp
  return $ length as
    
mlab :: Memory -> Pointer -> Maybe Label
mlab m (Ptr fp i) = do
  Frame _ l _ <- getFrame m fp
  return l

type Trace = [(Value, Label)]

data Variation a = Var Label a a
                 deriving (Eq, Show, Read)

-- Convert All Blocks taking care of EVERYTHING
--class 
