{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module TMUTMMRoutine where

import Control.Applicative
import Data.Function
import Data.List

import TMULabels
import TMUInstr
import TMUFlags

{---------------------------------- The TMU -----------------------------------}

-- Since memory only stores ints, we define a way to map instructions and labels
-- to integers.

instrToInt :: Instr -> Int
instrToInt Noop       = 0
instrToInt Add        = 1
instrToInt Sub        = 2
instrToInt (Push _)   = 3
instrToInt Pop        = 4
instrToInt Load       = 5
instrToInt Store      = 6
instrToInt Jump       = 7
instrToInt JumpNZ     = 8
instrToInt (Call _ _) = 9
instrToInt (Return _) = 10 
instrToInt Halt       = 11
instrToInt LabelOf    = 12

labToInt :: Maybe Label -> Int
labToInt Nothing  = 0
labToInt (Just L) = 1
labToInt (Just H) = 2

intToLab :: Int -> Maybe Label
intToLab 1 = Just L
intToLab 2 = Just H
intToLab _ = Nothing

-- The size of the TMU cache, and how that's divided into key Atoms and value Atoms.
tmuCacheSize :: Flaggy DynFlags => Int
tmuCacheSize    = case which_tmm_routine getFlags of
                      NoTMMRoutine -> 0
                      _            -> 7
                             -- instr, tag1, tag2, tag3, tagPC, tagRes, tagResPC
tmuCacheKeySize, tmuCacheValSize :: Int
tmuCacheKeySize = 5
tmuCacheValSize = 2

-- Where in the cache the various pieces live
tmuCacheInstr, tmuCacheTag1, tmuCacheTag2, tmuCacheTag3, tmuCacheTagPC, tmuCacheTagRes, tmuCacheTagResPC :: Int
tmuCacheInstr    = 0
tmuCacheTag1     = 1
tmuCacheTag2     = 2
tmuCacheTag3     = 3
tmuCacheTagPC    = 4
tmuCacheTagRes   = 5
tmuCacheTagResPC = 6

tmmRoutine :: Flaggy DynFlags => [Instr]
tmmRoutine = case which_tmm_routine getFlags of
  AllInstructionsLow   -> allInstructions L
  AllInstructionsHigh  -> allInstructions H
  AllInstructionsFault -> allInstructionsFault
  CorrectTMMRoutine    -> tmmRoutineFromTable correctTable
  NoTMMRoutine         -> []

allInstructions :: Label -> [Instr]
allInstructions lab =
  let store addr = [ Push . Labeled H . labToInt $ Just lab
                   , Push $ Labeled H addr
                   , Store ]
  in store tmuCacheTagRes ++ store tmuCacheTagResPC ++ [Return False]

allInstructionsFault :: [Instr]
allInstructionsFault = [Push (Labeled H $ -1), Jump]

data TMMRule = TMMRule { trInstr    :: Instr
                       , trLab1     :: Maybe Label
                       , trLab2     :: Maybe Label
                       , trLab3     :: Maybe Label
                       , trLabPC    :: Label
                       , trLabRes   :: Label
                       , trLabResPC :: Label }
             deriving (Eq, Read, Show)

tmmRoutineFromTable :: [TMMRule] -> [Instr]
tmmRoutineFromTable = (++ [push $ -1, Jump]) . concat . snd . mapAccumL rule 0
  where
    push = Push . Labeled H
    
    jumpIfNeq maddr val dest = [ push dest
                               , push maddr, Load
                               , push val
                               , Sub
                               , JumpNZ ]
    
    storeAt loc val = [push val, push loc, Store]
    
    rule base TMMRule{..} =
      let routine' = do
            check <- concat <$> mapM (uncurry jumpIfNeq)
                       [ (tmuCacheInstr, instrToInt trInstr)
                       , (tmuCacheTag1,  labToInt trLab1)
                       , (tmuCacheTag2,  labToInt trLab2)
                       , (tmuCacheTag3,  labToInt trLab3)
                       , (tmuCacheTagPC, labToInt $ Just trLabPC) ]
            return $  check
                   ++ storeAt tmuCacheTagRes   (labToInt $ Just trLabRes)
                   ++ storeAt tmuCacheTagResPC (labToInt $ Just trLabResPC)
                   ++ [Return False]
          routine = fix $ routine' . (+ base) . length
      in (base + length routine, routine)

correctTable :: [TMMRule]
correctTable = table
  [ Noop         &- [ (x,x,x,L) ~> (L,L)
                    , (x,x,x,H) ~> (H,H) ]
  
  , Add          &- [ (l,l,x,L) ~> (L,L)
                    , (l,l,x,H) ~> (L,H)
                    , (l,h,x,L) ~> (H,L)
                    , (l,h,x,H) ~> (H,H)
                    , (h,l,x,L) ~> (H,L)
                    , (h,l,x,H) ~> (H,H)
                    , (h,h,x,L) ~> (H,L)
                    , (h,h,x,H) ~> (H,H) ]
  
  , Sub          &- [ (l,l,x,L) ~> (L,L)
                    , (l,l,x,H) ~> (L,H)
                    , (l,h,x,L) ~> (H,L)
                    , (l,h,x,H) ~> (H,H)
                    , (h,l,x,L) ~> (H,L)
                    , (h,l,x,H) ~> (H,H)
                    , (h,h,x,L) ~> (H,L)
                    , (h,h,x,H) ~> (H,H) ]
  
  , Push 0       &- [ (l,x,x,L) ~> (L,L)
                    , (l,x,x,H) ~> (L,H)
                    , (h,x,x,L) ~> (H,L)
                    , (h,x,x,H) ~> (H,H) ]
  
  , Pop          &- [ (x,x,x,L) ~> (L,L)
                    , (x,x,x,H) ~> (H,H) ]
  
  , Load         &- [ (l,l,x,L) ~> (L,L)
                    , (l,l,x,H) ~> (L,H)
                    , (l,h,x,L) ~> (H,L)
                    , (l,h,x,H) ~> (H,H)
                    , (h,l,x,L) ~> (H,L)
                    , (h,l,x,H) ~> (H,H)
                    , (h,h,x,L) ~> (H,L)
                    , (h,h,x,H) ~> (H,H) ]
  
  , Store        &- [ (l,l,l,L) ~> (L,L)
                    , (l,l,h,L) ~> (L,L)
                    , (l,l,h,H) ~> (H,H)
                    , (l,h,l,L) ~> (H,L)
                    , (l,h,h,L) ~> (H,L)
                    , (l,h,h,H) ~> (H,H)
                    , (h,l,h,L) ~> (H,L)
                    , (h,l,h,H) ~> (H,H)
                    , (h,h,h,L) ~> (H,L)
                    , (h,h,h,H) ~> (H,H) ]
  
  , Jump         &- [ (l,x,x,L) ~> (L,L)
                    , (l,x,x,H) ~> (H,H)
                    , (h,x,x,L) ~> (H,H)
                    , (h,x,x,H) ~> (H,H) ]
  
  , JumpNZ       &- [ (l,l,x,L) ~> (L,L)
                    , (l,l,x,H) ~> (H,H)
                    , (l,h,x,L) ~> (L,H) -- ASZ: See note [JumpNZ two labels]
                    , (l,h,x,H) ~> (H,H)
                    , (h,l,x,L) ~> (H,H)
                    , (h,l,x,H) ~> (H,H)
                    , (h,h,x,L) ~> (H,H)
                    , (h,h,x,H) ~> (H,H) ]
  
  , Call 0 False &- [ (l,x,x,L) ~> (L,L)
                    , (l,x,x,H) ~> (H,H)
                    , (h,x,x,L) ~> (L,H) -- ASZ & LL: Uncovered bug; was HH
                    , (h,x,x,H) ~> (H,H) ]
  
  -- ASZ & LL: Uncovered bug; return needs to handle *two* arguments.
  , Return False &- [ (x,l,x,L) ~> (H,L) -- ASZ: See note [Return no argument]
                    , (x,l,x,H) ~> (H,L)   
                    , (x,h,x,L) ~> (H,H)   
                    , (x,h,x,H) ~> (H,H) 
                    , (l,l,x,L) ~> (L,L)
                    , (l,l,x,H) ~> (H,L)   
                    , (l,h,x,L) ~> (L,H)   
                    , (l,h,x,H) ~> (H,H) 
                    , (h,l,x,L) ~> (H,L)
                    , (h,l,x,H) ~> (H,L)   
                    , (h,h,x,L) ~> (H,H)   
                    , (h,h,x,H) ~> (H,H) ]
  
  , Halt         &- [ (l,x,x,L) ~> (L,L)
                    , (l,x,x,H) ~> (L,H)
                    , (h,x,x,L) ~> (H,L)
                    , (h,x,x,H) ~> (H,H) ] ]
  where
    x = Nothing
    l = Just L
    h = Just H
    
    (~>) = (,)
    (&-) trInstr =
      map (\( (trLab1,trLab2,trLab3,trLabPC)
            , (trLabRes,trLabResPC) )        -> TMMRule{..})
    
    table lrules = let rules = concat lrules
                       keys  = map (\TMMRule{..} -> ( instrToInt trInstr
                                                    , trLab1
                                                    , trLab2
                                                    , trLab3
                                                    , trLabPC ))
                                   rules
                   in if keys == nub keys
                      then rules
                      else error "This table does not represent a function!"

-- ASZ: Note [JumpNZ two Labels]:
-- There are two possible labPCs for JumpNZ, since the jumped-to address only
-- taints the PC if we jump.  What's the best way to handle this?  Should we
-- return the not-taken/the taken PC in the labRes slot when they differ?
-- (Note: they only differ in the one case.)

-- ASZ: Note [Return no argument]:
-- If Return doesn't return anything on top of the stack, we still need to
-- provide a label for what *would* be returned.  Since it doesn't matter, we
-- always return H.  (I believe this is what I do in general here.)
