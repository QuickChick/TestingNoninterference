{-# LANGUAGE RecordWildCards #-}
module Reachability where

import Machine
import Memory
import Labels
import Primitives

import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-- Given a State, make sure the stamp configuration is ok
-- Essentially, for each of the (4) labels, we calculate every memory frame 
-- that is reachable through a series of observable pointers and we make sure 
-- that the stamp of all those frames is also observable
wellFormed :: State -> Bool
wellFormed st = all (wellFormedLabel st) (labelsBelow H)

wellFormedLabel :: State -> Label -> Bool
wellFormedLabel st obs = all (flip isLow obs . stamp) $ reachable obs st

reachable :: Label -> State -> [Block]
reachable obs st = reachableFrom obs st Set.empty $ getRootSet obs st

reachableFrom :: Label -> State -> Set Block -> [Block] -> [Block]
reachableFrom obs st visited [] = Set.toList visited
reachableFrom obs st@State{..} visited (x:xs) 
    | Set.member x visited = reachableFrom obs st visited xs
    | otherwise =
        case getFrame mem x of 
          Just (Frame _ lab atoms) ->
              let new = if isLow lab obs then getBlocksFromAtoms obs st atoms
                        else [] 
                  worklist' = filter (flip Set.member visited) new
              in reachableFrom obs st (Set.insert x visited) worklist'
          Nothing -> reachableFrom obs st (Set.insert x visited) xs

getBlocksFromAtoms :: Label -> State -> [Atom] -> [Block]
getBlocksFromAtoms obs st atoms = 
    nub $ catMaybes $ map extractBlock $ filter (isLowPointer obs) atoms

getRootSet :: Label -> State -> [Block]
getRootSet obs st@State{..} = 
    getRootSetStack obs st stack $ 
      if isLow (pcLab pc) obs 
      then getBlocksFromAtoms obs st $ unRegSet regs
      else []

-- TODO: Fix for efficiency? (nub)
getRootSetStack :: Label -> State -> Stack -> [Block] -> [Block] 
getRootSetStack obs _ (Stack []) acc = acc
getRootSetStack obs st (Stack (StkElt (pc,_,rs,_):s')) acc =
    let new = if isLow (pcLab pc) obs 
              then getBlocksFromAtoms obs st $ unRegSet rs
              else [] 
    in getRootSetStack obs st (Stack s') $ nub $ new ++ acc

extractBlock :: Atom -> Maybe Block
extractBlock (Atom (VPtr (Ptr fp _)) _) = Just fp
extractBlock _ = Nothing

isLowPointer :: Label -> Atom -> Bool
isLowPointer obs (Atom (VPtr _) l) = isLow l obs
isLowPointer _ _ = False

-- REVERSE - Stamp Calculation for a single block
-- Get all the labels that allow us to reach the block
-- and get the meet of all of them.
generateStamp :: State -> Block -> Label
generateStamp st@State{..} block = 
    let candidates = filter (\l -> elem block $ reachable l st) $ labelsBelow H
    in foldr meet H candidates

stampAtom :: Block -> Label -> Atom -> Atom
stampAtom block stamp' a@(Atom (VPtr (Ptr block' off)) l) 
    | block == block' = Atom (VPtr (Ptr (putStamp stamp' block) off)) l
    | otherwise = a
stampAtom block stamp' a = a

stampFrame :: Block -> Label -> Frame Atom -> Frame Atom
stampFrame block st2 fr@(Frame st1 label atoms) 
    | stamp block == st1 = Frame st2 label (map (stampAtom block st2) atoms)
    | otherwise = Frame st1 label (map (stampAtom block st2) atoms)

putStampAllBlocks :: Block -> Label -> State -> State
putStampAllBlocks block stamp st@State{..} = 
    let mem'   = mapMemory (stampFrame block stamp) mem
        stack' = mapStack (stampAtom block stamp) stack
        regs'  = RegSet $ map (stampAtom block stamp) $ unRegSet regs
    in st{mem = mem', stack = stack', regs = regs'} 

-- Deterministically generate as high stamps as possible
generateStamps :: State -> State
generateStamps st@State{..} = 
    foldr (\block st -> putStampAllBlocks block (generateStamp st block) st) 
          st (getAllBlocks H mem)

