{-# LANGUAGE RecordWildCards, TupleSections, FlexibleContexts, 
  FlexibleInstances #-}
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
    shrink BMult = [BAdd]
    shrink _     = []

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
    smartGen (MkInfo _ cl _ _) 
        = frequency [(1, pure 0), 
                     (10, choose (0,cl-1))]

instance SmartGen Value where
    smartGen info@(MkInfo _ cl dfs _) = 
        frequency [--(1, liftM VInt $ smartGen info)
                   (1, liftM VInt $ choose (0, cl - 1))
                  ,(1, liftM VPtr $ smartGen info)
                  ,(1, liftM VLab $ smartGen info)]

instance SmartGen Atom where
    smartGen info = liftM2 Atom (smartGen info) (smartGen info)

instance SmartGen PtrAtom where
    smartGen info@(MkInfo Flags{..} cl _ _) = do
        case strategy of 
          GenByExec -> liftM (PAtm 0) (smartGen info)
          GenSSNI   -> liftM2 PAtm (choose (0, cl - 1)) (smartGen info)

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

isCpt :: Int -> Atom -> Bool
isCpt imemLen (Atom (VInt x) _) = 0 <= x && x < imemLen
isCpt _ _ = False

isLab :: Atom -> Bool
isLab (Atom (VLab _) _) = True
isLab _ = False

-- Group Register into categories based on their contents
groupRegisters :: Int -> [Register] -> 
                  [RegPtr] -> [RegPtr] -> [RegPtr] -> [RegPtr] -> Int ->
                  ([RegPtr], [RegPtr], [RegPtr], [RegPtr])
groupRegisters il [] dptr cptr num lab n = (dptr, cptr, num, lab)
groupRegisters il (r:rs) dptr cptr num lab n 
    | isCpt il r = groupRegisters il rs dptr (n : cptr) (n : num) lab (n + 1)
    | isInt r = groupRegisters il rs dptr cptr (n : num) lab (n + 1)
    | isPtr r = groupRegisters il rs (n : dptr) cptr num lab (n + 1)
    | isLab r = groupRegisters il rs dptr cptr num (n : lab) (n + 1)
    | otherwise = error "Cases are exhaustive"
    
containsRet :: Stack -> Bool
containsRet (Stack s) = not $ null s

-- Generates a structurally valid instruction
ainstr :: IMemC i => Flags -> Int -> (State i m) -> Gen Instr 
ainstr f hltWeight st@State{..} = 
    let (dptr, cptr, num, lab) = groupRegisters (imLength imem) 
                                 (unRegSet regs) [] [] [] [] 0
        genRegPtr = choose (0, length (unRegSet regs) - 1)
        ifNaive x y = if genInstrDist f == Naive then y else x
        hltW = if testProp f == TestEENI then hltWeight else 0
    in frequency $ 
           [(5, pure Noop)
           ,(hltW, pure Halt)
           ,(10, liftM PcLab genRegPtr)
           ,(10, liftM2 Lab genRegPtr genRegPtr)] ++
           [(10, liftM2 MLab (elements dptr) genRegPtr) | not $ null dptr] ++
           [(10, liftM2 PutLab (genLabelBelow H) genRegPtr)] ++
           [(10, liftM3 BCall (elements cptr) (elements lab) genRegPtr)
            | not $ null lab || null cptr ] ++
           [(ifNaive 20 10, pure BRet) | containsRet stack] ++
           [(ifNaive 13 10, liftM3 Alloc (elements num) (elements lab) genRegPtr)
            | not $ null num || null lab] ++
           [(ifNaive 13 10, liftM2 Load (elements dptr) genRegPtr) 
            | not $ null dptr] ++
           [(ifNaive 30 10, liftM2 Store (elements dptr) genRegPtr)
            | not $ null dptr] ++
           [(ifNaive 30 10, liftM2 Write (elements dptr) genRegPtr)
            | not $ null dptr] ++
           [(ifNaive 30 10, liftM2 Update (elements dptr) (elements lab))
            | not $ null dptr || null lab] ++
           [(10, liftM Jump (elements cptr)) | not $ null cptr] ++
           [(10, liftM2 Bnz (choose (-1, 2)) (elements num))
            | not $ null num] ++
           [(10, liftM3 PSetOff (elements dptr) (elements num) genRegPtr)
            | not $ null dptr || null num] ++
           [(10, liftM2 Put arbitrary genRegPtr)] ++
           [(10, liftM4 BinOp arbitrary (elements num) (elements num) genRegPtr)
            | not $ null num] ++
           [(10, liftM2 MSize (elements dptr) genRegPtr) | not $ null dptr] ++
           [(10, liftM2 PGetOff (elements dptr) genRegPtr) 
            | not $ null dptr] ++
           [(10, liftM2 Mov genRegPtr genRegPtr)]

popInstrSSNI :: IMemC i => Flags -> (State i m) -> Gen (State i m) 
popInstrSSNI f s@State{..} = do 
  imem' <- ainstr f 0 s >>= return . fromInstrList . replicate (imLength imem) 
  return s{imem = imem'}

copyInstructions :: IMemC i => [Maybe Instr] -> (State i m) -> (State i m) 
copyInstructions z s = s{imem = fromInstrList $ map (fromMaybe Noop) z}

genExecHelper :: (IMemC i, MemC m Atom) =>Flags -> RuleTable -> State i m -> 
                 State i m -> Int -> Int -> Zipper (Maybe Instr) 
              -> Gen (State i m)
genExecHelper f _ s0 s 0 _ z = return $ copyInstructions (toList z) s0
genExecHelper f table s0 s tries hltWeight zipper = do
  (zipper',i) <- case current zipper of 
                   Nothing -> do
                     -- No instruction. Generate
                     i <- ainstr f hltWeight s 
                     return (zipper{current=Just i}, i)
                   Just i -> return (zipper,i)
  case exec' table s i of
    Just s' -> 
--       traceShow ("Executed", s ,s') $
        let (PAtm addr _) = (pc s') in
        case moveZipper zipper' addr of
          Just zipper'' -> 
              genExecHelper f table s0 s' (tries-1) (hltWeight+1) zipper''
          Nothing -> 
              -- PC out of bounds. End generation
              return $ copyInstructions (toList zipper') s0
    Nothing -> return $ copyInstructions (toList zipper') s0

-- Naive-inefficient way of doing genByExec
genExecHelperLst :: (IMemC i, MemC m Atom) =>Flags -> RuleTable -> State i m -> 
                 State i m -> Int -> [Maybe Instr]
              -> Gen (State i m)
genExecHelperLst f _ s0 s 0 instrs = return $ copyInstructions instrs s0
genExecHelperLst f table s0 s tries instrs = do
  let (PAtm idx _) = pc s 
  (instrs',i) <- case instrs !! idx of 
                   Nothing -> do
                     -- No instruction. Generate
                     i <- ainstr f 0 s 
                     return $ (fromJust $ update idx (Just i) instrs, i)
                   Just i -> return (instrs,i)
  case exec table (copyInstructions instrs' s) of
    Just s' -> 
        let (PAtm addr _) = (pc s') in
        if addr < 0 || addr >= length instrs' then
            -- PC out of bounds. End generation
              return $ copyInstructions instrs' s0
        else genExecHelperLst f table s0 s' (tries-1) instrs'
    Nothing -> return $ copyInstructions instrs s0

popInstrLLNI :: (IMemC i, MemC m Atom, Show i, Show m) => Flags -> RuleTable -> State i m -> 
                Gen (State i m)
popInstrLLNI f table s@State{..} = do
  let len = imLength imem
  case memType f of 
    MemList -> 
      genExecHelperLst f table s s (3 * len) (replicate len Nothing)
    MemMap ->
      genExecHelper f table s s (3 * len) 0 (fromList $ replicate len Nothing) 

popInstr :: (Show i, Show m, IMemC i, MemC m Atom) => Flags -> (State i m) -> Gen (State i m)
popInstr f@Flags{..} s =
    case strategy of 
      GenSSNI   -> popInstrSSNI f s
      GenByExec -> popInstrLLNI f defaultTable s

------------------------- VARIATIONS --------------------------

class SmartVary a where
    smartVary :: Label -> Info -> a -> Gen a

instance SmartVary Value where
    smartVary _ info (VInt _) = liftM VInt $ smartGen info
    smartVary _ info (VPtr p) = liftM VPtr $ smartGen info
-- CH: even if we don't have VCpt any more, could still try to turn
-- valid code pointers more often into other code pointers
--    smartVary _ info (VCpt p) = liftM VCpt $ choose (0, codeLen info - 1)
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

varySingleFrame :: (MemC m Atom) =>
                   Label -> Info -> m -> Block -> Gen m
varySingleFrame obs info mem block = 
    case getFrame mem block of 
      Just f -> do
        f' <- smartVary obs info f 
        case updFrame mem block f' of 
          Just mem' -> pure mem'
          Nothing   -> error "varySingleFrame Failed"
      Nothing -> error "varySingleFrame Failed"

instance SmartVary (Mem Atom) where
    smartVary obs info mem = 
        foldM (varySingleFrame obs info) mem (getBlocksBelow H mem)

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
instance (MemC m Atom, IMemC i, SmartVary m) => SmartVary (State i m) where
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
      GenSSNI   -> Parameters 2 2 2 2 2 noSteps 10
      GenByExec -> Parameters 2 4 2 8 5 noSteps 10

-- Stamps start out bottom. Fill them up later!
genInitMem :: MemC m Atom => Flags -> Gen (m, [(Block, Int)]) 
genInitMem flags = do 
  let Parameters{..} = getParameters flags
      aux 0 ml = return ml
      aux n (m,l) = do 
        frameSize <- choose (minFrameSize, maxFrameSize) 
        label     <- genLabelBelow H
        let Just (block, m') = alloc frameSize label bot (Atom (VInt 0) bot) m --Assume frameSize > 0 
        aux (n-1) (m', (block, frameSize - n) : l)
  noFrames <- choose (minFrames, maxFrames) 
  aux noFrames (Memory.empty, [])

populateFrame :: MemC m Atom => Info -> m -> Block -> Gen m
populateFrame info mem block = 
    case getFrame mem block of 
      Just (Frame stamp label atoms) -> do
        atoms' <- vectorOf (length atoms) (smartGen info) 
        case updFrame mem block (Frame stamp label atoms') of 
          Just mem' -> return mem'
          Nothing -> error "populate Failed"
      Nothing -> error "populate Failed"

populateMemory :: MemC m Atom => Info -> m -> Gen m
populateMemory info mem = 
    foldM (populateFrame info) mem (getBlocksBelow H mem)

-- LL: Add stamp instantiation - was unneeded in Coq? Suspicious...
instantiateStamps :: (State i m) -> Gen (State i m) 
instantiateStamps = return 

--defaultFlags :: Flags 
--defaultFlags = Flags.defaultFlags

genVariationState :: (MemC m Atom, IMemC i, SmartVary m, Show i, Show m) => 
                     Flags -> Gen (Variation (State i m))
genVariationState flags = do 
  let Parameters{..} = getParameters flags
  (initMem, dfs) <- genInitMem flags
  codeSize <- choose (minCodeSize, maxCodeSize)
  let imem = fromInstrList $ replicate codeSize Noop
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
