{-# LANGUAGE RecordWildCards, TupleSections #-}
module Generation where

import Test.QuickCheck

import Control.Monad
import Control.Applicative

import Data.Maybe
import Debug.Trace

import Labels
import Memory
import Primitives
import Instructions
import Rules
import Machine
import Flags

import Zipper


-- Thread some information for better generation
data Info = MkInfo { flags :: Flags
                   , codeLen :: Int
                   , dataLen :: [(Block, Int)]
                   , noRegs  :: Int }

class SmartGen a where
    smartGen :: Info -> Gen a

instance Arbitrary BinOpT where
    arbitrary = elements [BAdd, BMult]
    shrink BAdd  = []
    shrink BMult = [BAdd]

-- Generate a label below another
genLabelBelow :: Label -> Gen Label
genLabelBelow = elements . labelsBelow

-- Generate a label in [l1, l2]
genLabelBetweenLax :: Label -> Label -> Gen Label
genLabelBetweenLax l1 l2 = 
    elements $ filter (\l -> isLow l1 l) $ labelsBelow l2

-- Generate a label in (l1, l2]
genLabelBetweenStrict :: Label -> Label -> Gen Label
genLabelBetweenStrict l1 l2 = 
    elements $ filter (\l -> isLow l1 l && l /= l1) $ labelsBelow l2

instance SmartGen Label where
    smartGen _ = genLabelBelow H

-- Will always produce an "inbounds" pointer
instance SmartGen Pointer where
    smartGen (MkInfo _ _ dfs _) = do 
      (mf, len) <- elements dfs 
      addr <- choose (0, len - 1)
      return $ Ptr mf addr

instance SmartGen Int where
    smartGen _ = frequency [(1, pure 0), (10, choose (0,10))]

instance SmartGen Value where
    smartGen info@(MkInfo _ cl dfs _) = 
        frequency [(1, liftM VInt $ smartGen info)
                  ,(1, liftM VCpt $ choose (0, cl - 1))
                  ,(1, liftM VPtr $ smartGen info)
                  ,(1, liftM VLab $ smartGen info)]

instance SmartGen Atom where
    smartGen info = liftM2 Atom (smartGen info) (smartGen info)

instance SmartGen PtrAtom where
    smartGen info@(MkInfo Flags{..} cl _ _) = do
        case strategy of 
          GenLLNI -> liftM (PAtm 0) (smartGen info)
          GenSSNI -> liftM2 PAtm (choose (0, cl - 1)) (smartGen info)

instance SmartGen RegSet where
    smartGen info = fmap RegSet $ vectorOf (noRegs info) (smartGen info)

-- Creates a stack element whose PC label is between two labels, strict or lax
-- based on the function argument
smartGenStkElt :: (Label -> Label -> Gen Label) -> Label -> Label -> Info -> 
                  Gen StkElt
smartGenStkElt f lowPC highPC info = do
  regs        <- smartGen info
  PAtm addr _ <- smartGen info
  target      <- choose (0, noRegs info - 1)
  retLab      <- smartGen info
  pcLab       <- f lowPC highPC
  return $ StkElt (PAtm addr pcLab, retLab, regs, target)
    
smartGenStack :: PtrAtom -> Info -> Gen Stack
smartGenStack pc info = 
    frequency [(1, return $ Stack [])
              ,(9, liftM (Stack . return) $ 
                 smartGenStkElt genLabelBetweenLax bot (pcLab pc) info)]

isInt :: Atom -> Bool
isInt (Atom (VInt _) _) = True
isInt _ = False

isPtr :: Atom -> Bool
isPtr (Atom (VPtr _) _) = True
isPtr _ = False

isCpt :: Atom -> Bool
isCpt (Atom (VCpt _) _) = True
isCpt _ = False

isLab :: Atom -> Bool
isLab (Atom (VLab _) _) = True
isLab _ = False

-- Group Register into categories based on their contents
groupRegisters :: [Register] -> 
                  [RegPtr] -> [RegPtr] -> [RegPtr] -> [RegPtr] -> Int ->
                  ([RegPtr], [RegPtr], [RegPtr], [RegPtr])
groupRegisters [] dptr cptr num lab n = (dptr, cptr, num, lab)
groupRegisters (r:rs) dptr cptr num lab n 
    | isInt r = groupRegisters rs dptr cptr (n : num) lab (n + 1)
    | isPtr r = groupRegisters rs (n : dptr) cptr num lab (n + 1)
    | isCpt r = groupRegisters rs dptr (n : cptr) num lab (n + 1)
    | isLab r = groupRegisters rs dptr cptr num (n : lab) (n + 1)
    | otherwise = error "Cases are exhaustive"
    
containsRet :: Stack -> Bool
containsRet (Stack s) = not $ null s

-- Generates a structurally valid instruction
-- LL: TODO: Fix weights. AND DO SOMETHING ABOUT BRETS!
ainstrSSNI :: State -> Gen Instr 
ainstrSSNI st@State{..} = 
    let (dptr, cptr, num, lab) = groupRegisters (unRegSet regs) [] [] [] [] 0
        genRegPtr = choose (0, length (unRegSet regs) - 1)
    in frequency $ 
           [(1, pure Noop)
           ,(0, pure Halt)
           ,(10, liftM PcLab genRegPtr)
           ,(10, liftM2 Lab genRegPtr genRegPtr)] ++
           [(10, liftM2 MLab (elements dptr) genRegPtr) | not $ null dptr] ++
           [(10, liftM3 FlowsTo (elements lab) (elements lab) genRegPtr)
            | not $ null lab] ++
           [(10, liftM3 LJoin (elements lab) (elements lab) genRegPtr)
            | not $ null lab] ++
           [(10, liftM PutBot genRegPtr)] ++
           [(10, liftM3 BCall (elements cptr) (elements lab) genRegPtr)
            | not $ null lab || null cptr ] ++
           [(100, pure BRet) | containsRet stack] ++
           [(50, liftM3 Alloc (elements num) (elements lab) genRegPtr)
            | not $ null num || null lab] ++
           [(10, liftM2 Load (elements dptr) genRegPtr) 
            | not $ null dptr] ++
           [(100, liftM2 Store (elements dptr) genRegPtr)
            | not $ null dptr] ++
           [(10, liftM Jump (elements cptr)) | not $ null cptr] ++
           [(10, liftM2 Bnz (choose (-1, 2)) (elements num))
            | not $ null num] ++
           [(10, liftM Output (elements num)) | not $ null num] ++
           [(10, liftM3 PSetOff (elements dptr) (elements num) genRegPtr)
            | not $ null dptr || null num] ++
           [(10, liftM2 Put arbitrary genRegPtr)] ++
           [(10, liftM4 BinOp arbitrary (elements num) (elements num) genRegPtr)
            | not $ null num] ++
           [(10, liftM2 MSize (elements dptr) genRegPtr) | not $ null dptr] ++
           [(10, liftM2 PGetOff (elements dptr) genRegPtr) 
            | not $ null dptr]

popInstrSSNI :: State -> Gen State 
popInstrSSNI s@State{..} = do 
  imem' <- ainstrSSNI s >>= return . replicate (length imem) 
  return s{imem = imem'}

-- Generates a structurally valid instruction
-- LL: TODO: Fix weights. AND DO SOMETHING ABOUT BRETS!
ainstrLLNI :: State -> Gen Instr 
ainstrLLNI st@State{..} = 
    let (dptr, cptr, num, lab) = groupRegisters (unRegSet regs) [] [] [] [] 0
        genRegPtr = choose (0, length (unRegSet regs) - 1)
    in frequency $ 
           [(1, pure Noop)
           ,(0, pure Halt)
           ,(10, liftM PcLab genRegPtr)
           ,(10, liftM2 Lab genRegPtr genRegPtr)] ++
           [(10, liftM2 MLab (elements dptr) genRegPtr) | not $ null dptr] ++
           [(10, liftM3 FlowsTo (elements lab) (elements lab) genRegPtr)
            | not $ null lab] ++
           [(10, liftM3 LJoin (elements lab) (elements lab) genRegPtr)
            | not $ null lab] ++
           [(10, liftM PutBot genRegPtr)] ++
           [(10, liftM3 BCall (elements cptr) (elements lab) genRegPtr)
            | not $ null lab || null cptr ] ++
           [(10, pure BRet) | containsRet stack] ++
           [(10, liftM3 Alloc (elements num) (elements lab) genRegPtr)
            | not $ null num || null lab] ++
           [(10, liftM2 Load (elements dptr) genRegPtr) 
            | not $ null dptr] ++
           [(10, liftM2 Store (elements dptr) genRegPtr)
            | not $ null dptr] ++
           [(10, liftM Jump (elements cptr)) | not $ null cptr] ++
           [(10, liftM2 Bnz (choose (-1, 2)) (elements num))
            | not $ null num] ++
           [(10, liftM Output (elements num)) | not $ null num] ++
           [(10, liftM3 PSetOff (elements dptr) (elements num) genRegPtr)
            | not $ null dptr || null num] ++
           [(10, liftM2 Put arbitrary genRegPtr)] ++
           [(10, liftM4 BinOp arbitrary (elements num) (elements num) genRegPtr)
            | not $ null num] ++
           [(10, liftM2 MSize (elements dptr) genRegPtr) | not $ null dptr] ++
           [(10, liftM2 PGetOff (elements dptr) genRegPtr) 
            | not $ null dptr]

copyInstructions :: Zipper (Maybe Instr) -> State -> State 
copyInstructions z s = s{imem = map (fromMaybe Noop) (toList z)}

genExecHelper :: RuleTable -> State -> State -> Int -> Zipper (Maybe Instr) 
              -> Gen State
genExecHelper _ s0 s 0 z = return $ copyInstructions z s0
genExecHelper table s0 s tries zipper = do
  (zipper',i) <- case current zipper of 
                   Nothing -> {- traceShow "Generatin" $-} do
                     -- No instruction. Generate
                     i <- ainstrLLNI s 
                     return (zipper{current=Just i}, i)
                   Just i -> return (zipper,i)
  case exec' table s i of
    Just (_, s') -> 
--       traceShow ("Executed", s ,s') $
        let (PAtm addr _) = (pc s') in
        case moveZipper zipper' addr of
          Just zipper'' -> 
              genExecHelper table s0 s' (tries-1) zipper''
          Nothing -> 
              -- PC out of bounds. End generation
              return $ copyInstructions zipper' s0
    Nothing -> return $ copyInstructions zipper' s0

popInstrLLNI :: RuleTable -> State -> Gen State
popInstrLLNI table s@State{..} = do
  let len = length imem
  genExecHelper table s s (3 * len) (fromList $ replicate len Nothing) 

popInstr :: Flags -> State -> Gen State
popInstr Flags{..} s =
    case strategy of 
      GenSSNI -> popInstrSSNI s
      GenLLNI -> popInstrLLNI defaultTable s

------------------------- VARIATIONS --------------------------

class SmartVary a where
    smartVary :: Label -> Info -> a -> Gen a

instance SmartVary Value where
    smartVary _ info (VInt _) = liftM VInt $ smartGen info
    smartVary _ info (VPtr p) = liftM VPtr $ smartGen info
    smartVary _ info (VCpt p) = liftM VCpt $ choose (0, codeLen info - 1)
    smartVary _ info (VLab p) = liftM VLab $ smartGen info

instance SmartVary Atom where
    smartVary obs info a@(Atom v l) 
        | l `flowsTo` obs = pure a
        | otherwise = liftM2 Atom (smartVary obs info v) (pure l)

-- Varying a high pc's label needs to be higher than obs!
instance SmartVary PtrAtom where
    smartVary obs info pc@(PAtm _ lpc) 
        | isLow lpc obs = pure pc
        | otherwise = do
           PAtm addr' _' <- smartGen info
           lpc' <- genLabelBetweenStrict obs H
           return $ PAtm addr' lpc'

instance (SmartGen a, SmartVary a) =>  SmartVary (Frame a) where
    smartVary obs info f@(Frame stamp label atoms) 
        | isHigh stamp obs = 
            liftM2 (Frame stamp) (smartGen info) genData
        | isHigh (stamp `lub` label) obs =
            liftM (Frame stamp label) genData
        | otherwise =
            liftM (Frame stamp label) $ mapM (smartVary obs info) atoms
                where len = length atoms
                      genData = do
                        dataLength' <- choose (len, len+1)
                        vectorOf dataLength' $ smartGen info

varySingleFrame :: (SmartGen a, SmartVary a) => 
                   Label -> Info -> Mem a -> Block -> Gen (Mem a)
varySingleFrame obs info mem block = 
    case getFrame mem block of 
      Just f -> do
        f' <- smartVary obs info f 
        case updFrame mem block f' of 
          Just mem' -> pure mem'
          Nothing   -> error "varySingleFrame Failed"
      Nothing -> error "varySingleFrame Failed"

instance (SmartGen a, SmartVary a) => SmartVary (Mem a) where
    smartVary obs info mem = 
        foldM (varySingleFrame obs info) mem (getAllBlocks H mem)

instance SmartVary a => SmartVary [a] where
    smartVary obs info = mapM (smartVary obs info)
        
instance SmartVary RegSet where
    smartVary obs info (RegSet rs) = 
        fmap RegSet $ smartVary obs info rs

instance SmartVary StkElt where
    smartVary obs info (StkElt (pc, lab, rs, r)) 
        | isLow (pcLab pc) obs = 
            fmap (StkElt . (pc,lab,,r)) $ smartVary obs info rs
        | otherwise = 
            fmap (StkElt . (pc,lab,,r)) $ smartGen info 

-- Not complete! Extra high stack locations created in vary State if possible
instance SmartVary Stack where
    smartVary obs info (Stack s) = 
        fmap Stack $ smartVary obs info s

-- Vary state 
instance SmartVary State where
    smartVary obs info s@State{..} 
        | isLow (pcLab pc) obs = do
           regs'  <- smartVary obs info regs
           mem'   <- smartVary obs info mem
           stack' <- smartVary obs info stack
           return $ s{regs = regs', mem = mem', stack = stack'}
        | otherwise = do 
           pc'    <- smartVary obs info pc 
           mem'   <- smartVary obs info mem
           -- Need to vary stack length here!
           stack' <- smartVary obs info stack
           regs'  <- smartGen info 
           return $ s{regs = regs', mem = mem', stack = stack', pc = pc'}

-- Some constants, to be tweaked with Flaggy later on
data Parameters = Parameters { minFrames :: Int
                             , maxFrames :: Int
                             , minFrameSize :: Int
                             , maxFrameSize :: Int
                             , minCodeSize :: Int
                             , maxCodeSize :: Int
                             , noRegisters :: Int } 

getParameters :: Flags -> Parameters 
getParameters Flags{..} =
    case strategy of
      GenSSNI -> Parameters 2 2 2 2 2 2 10
      GenLLNI -> Parameters 2 4 2 8 5 42 10

-- Stamps start out bottom. Fill them up later!
genInitMem :: Flags -> Gen (Memory, [(Block, Int)]) 
genInitMem flags = do 
  let Parameters{..} = getParameters flags
      aux 0 ml = return ml
      aux n (m,l) = do 
        frameSize <- choose (minFrameSize, maxFrameSize) 
        label     <- genLabelBelow H
        let (block, m') = alloc frameSize label bot (Atom (VInt 0) bot) m
        aux (n-1) (m', (block, frameSize - n) : l)
  noFrames <- choose (minFrames, maxFrames) 
  aux noFrames (Memory.empty, [])

populateFrame :: Info -> Memory -> Block -> Gen Memory
populateFrame info mem block = 
    case getFrame mem block of 
      Just (Frame stamp label atoms) -> do
        atoms' <- vectorOf (length atoms) (smartGen info) 
        case updFrame mem block (Frame stamp label atoms') of 
          Just mem' -> return mem'
          Nothing -> error "populate Failed"
      Nothing -> error "populate Failed"

populateMemory :: Info -> Memory -> Gen Memory
populateMemory info mem = 
    foldM (populateFrame info) mem (getAllBlocks H mem)

-- LL: Add stamp instantiation - was unneeded in Coq? Suspicious...
instantiateStamps :: State -> Gen State 
instantiateStamps = return 

genVariationState :: Flags -> Gen (Variation State)
genVariationState flags = do 
  let Parameters{..} = getParameters flags
  (initMem, dfs) <- genInitMem flags
  codeSize <- choose (minCodeSize, maxCodeSize)
  let imem = replicate codeSize Noop
      info = MkInfo flags codeSize dfs noRegisters
  pc    <- smartGen info
  regs  <- smartGen info
  stack <- smartGenStack pc info
  mem   <- populateMemory info initMem
  let state0 = State{..}
  state1 <- popInstr flags state0                    
  st  <- instantiateStamps state1
  obs <- genLabelBetweenLax L H -- in case we change to arbitrary number
  st' <- smartVary obs info st 
  return $ Var obs st st'
  
