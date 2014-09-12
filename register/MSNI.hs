{-# LANGUAGE FlexibleContexts #-}
module MSNI where

import Debug.Trace

import Test.QuickCheck
import Test.QuickCheck.Property

import Machine
import Primitives
import Labels
import Instructions
import Rules
import Memory

import Control.Monad

import Indist    
import Generation
import Shrinking
import Flags
    
import Pretty
import Text.PrettyPrint (($$), text)
import qualified Text.PrettyPrint as PP

import System.IO.Unsafe

propMSNI :: (MemC m Atom, IMemC i, Indist i, Indist m) => 
            Flags -> RuleTable -> Variation (State i m) -> Property
propMSNI f t (Var obs st1 st2) = 
    let sts1 = execN (noSteps f) t st1
        sts2 = execN (noSteps f) t st2
    in indist obs st1 st2 ==> msniAux f obs st1 st2 (tail sts1) (tail sts2)

isLowState :: Label -> State i m -> Bool
isLowState obs st = isLow (pcLab $ pc st) obs

isHighState :: Label -> State i m -> Bool
isHighState obs st = not $ isLow (pcLab $ pc st) obs

dbg :: String -> Bool -> Bool
--dbg s x = if not x then unsafePerformIO $ do putStrLn s >> return x else x
dbg s x = x

-- Invariant: indist st1 st2 
-- -> Both low or both high! 
msniAux :: (MemC m Atom, IMemC i, Indist m, Indist i) => 
           Flags -> Label -> 
           (State i m) -> (State i m) -> [(State i m)] -> [(State i m)] -> Bool
msniAux f _ _ _ [] [] = True
msniAux f obs st1 st2 [] (st2':sts2)
    | isLowState obs st2 || isLowState obs st2' = msniAux f obs st1 st2' [] sts2
    | otherwise     = indist obs st2 st2' && msniAux f obs st1 st2' [] sts2
msniAux f obs st1 st2 (st1':sts1) []
    | isLowState obs st1 || isLowState obs st1' = msniAux f obs st1' st2 sts1 []
    | otherwise     = indist obs st1 st1' && msniAux f obs st1' st2 sts1 []
msniAux f obs st1 st2 (st1':sts1) (st2':sts2) 
    | isHighState obs st1 && isHighState obs st1' =
        dbg "1H -> 1H" $ 
        indist obs st1 st1' && msniAux f obs st1' st2 sts1 (st2':sts2)
    | isHighState obs st2 && isHighState obs st2' = 
        dbg "2H -> 2H" $ 
        indist obs st2 st2' && msniAux f obs st1 st2' (st1':sts1) sts2
    | isLowState obs st1 && isLowState obs st2 = 
        dbg "Both L -> *" $ 
        indist obs st1' st2' && msniAux f obs st1' st2' sts1 sts2
    | isLowState obs st1' && isLowState obs st2' = 
        dbg "Both * -> L" $ 
        indist obs st1' st2' && msniAux f obs st1' st2' sts1 sts2
    | otherwise = error $ "Unforeseen case: " ++ show (obs, pcLab (pc st1), pcLab (pc st2))

