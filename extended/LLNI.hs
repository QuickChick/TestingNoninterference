{-# LANGUAGE RecordWildCards #-}

module LLNI where

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

propLLNI :: Flags -> RuleTable -> Variation State -> Property
propLLNI Flags{..} t (Var obs st1 st2) =
--    traceShow ("Here!") $
    let sts1 = execN noSteps t st1
        sts2 = execN noSteps t st2
        isLowState st = isLow (pcLab $ pc st) obs
        sts1' = filter isLowState sts1
        sts2' = filter isLowState sts2
    in {- whenFail (mapM_ (\v@(Var o s1 s2) -> do
                            if not $ indist o s1 s2 then 
                                putStrLn "Indist here!"
                            else return ()
                            putStrLn . PP.render $ pp v) $ zipWith (Var obs) sts1' sts2')-}
      property
--    in property $ --whenFail (putStrLn (show sts1') >> putStrLn (show sts2')) $
       $ (and $ zipWith (indist obs) sts1' sts2')

testLLNI :: Flags -> RuleTable -> Property
testLLNI f t = 
    forAllShrink (genVariationState f) (const []) $ propLLNI f t