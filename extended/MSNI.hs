module MSNI where

import Debug.Trace

import Test.QuickCheck
import Test.QuickCheck.Property

import Machine
import Primitives
import Labels
import Instructions
import Rules

import Control.Monad

import Indist    
import Generation
import Shrinking
import Flags
    
import Pretty
import Text.PrettyPrint (($$), text)
import qualified Text.PrettyPrint as PP

import System.IO.Unsafe

testMSNI :: Flags -> RuleTable -> Property
testMSNI f t = 
  forAllShrink (genVariationState f) (const []) $ propMSNI f t

propMSNI :: Flags -> RuleTable -> Variation State -> Property
propMSNI f t (Var obs st1 st2) = 
    let (tr1, sts1) = execN (noSteps f) t st1
        (tr2, sts2) = execN (noSteps f) t st2
        tracesOK = observeComp (observe obs tr1) (observe obs tr2)
    in indist obs st1 st2 ==> tracesOK && msniAux f obs st1 st2 (tail sts1)
       (tail sts2)

isLowState :: Label -> State -> Bool
isLowState obs st = isLow (pcLab $ pc st) obs

isHighState :: Label -> State -> Bool
isHighState obs st = not $ isLow (pcLab $ pc st) obs

dbg :: String -> Bool -> Bool
--dbg s x = if not x then unsafePerformIO $ do putStrLn s >> return x else x
dbg s x = x

-- Invariant: indist st1 st2 
-- -> Both low or both high! 
msniAux :: Flags -> Label -> 
           State -> State -> [State] -> [State] -> Bool
msniAux f _ _ _ [] [] = True
msniAux f obs st1 st2 [] (st2':sts2)
    | isLowState obs st2 || isLowState obs st2' = True -- Termination insensitive
    | otherwise = 
      indist obs st2 st2' && msniAux f obs st1 st2' [] sts2
msniAux f obs st1 st2 (st1':sts1) []
    | isLowState obs st1 || isLowState obs st1' = True -- Termination insensitive
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

