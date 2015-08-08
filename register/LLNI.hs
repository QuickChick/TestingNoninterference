{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module LLNI where

import Test.QuickCheck

import Machine
import Primitives
import Labels
import Instructions
import Rules
import Memory

import Indist    
import Flags
    
propLLNI :: (MemC m Atom, IMemC i, Indist i, Indist m) => 
            Flags -> RuleTable -> Variation (State i m) -> Property
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
