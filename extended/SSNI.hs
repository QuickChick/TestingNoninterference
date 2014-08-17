module SSNI where

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

testSSNI :: Flags -> RuleTable -> Property
testSSNI f t = 
  forAllShrink (genVariationState f) (const []) $ propSSNI f t

propSSNI :: Flags -> RuleTable -> Variation State -> Property
propSSNI f t (Var obs st1 st2) = 
  let collect' = case collectF f of
                   CollectInstrCode -> 
                       collect (join $ fmap opcodeOfInstr 
                                     $ instrLookup (imem st1) (pc st1))
                   _ -> property 
  in collect' $
  let isLowState st = isLow (pcLab $ pc st) obs in
  if indist obs st1 st2 then 
      case exec t st1 of 
        Just (tr1, st1') ->
            if isLowState st1 then
                case exec t st2 of 
                  Just (tr2, st2') ->
                      property $ 
--                      collect "LOW -> LOW" $ 

                      -- Both took a low step
{-                        whenFail (putStrLn $ PP.render 
                                 $ text "Low Step\nStarting State:\n" $$ 
                                   pp v $$ 
                                   text "Ending State:\n" $$ 
                                   pp (Var obs st1' st2')) $ -}
                      observeComp (observe obs tr1) (observe obs tr2)
                      && indist obs st1' st2'
                  Nothing -> 
                      -- 1 took a low step and 2 failed
                      {- collect "LOW ->*, 2 X" $ -} property rejected
            else -- st1 is High
                if isLowState st1' then
                    case exec t st2 of 
                      Just (tr2, st2') ->
                          if isLowState st2' then 
                              -- High -> low
{-                              whenFail (PP.render 
                                 $ text "Low Step\nStarting State:\n" $$ 
                                   pp v $$ 
                                   text "Ending State:\n" $$ 
                                   pp (Var obs st1' st2')) $ 
-}
--                              collect "High -> Low" $ 
                              property $
                              observeComp (observe obs tr1) (observe obs tr2)
                              && indist obs st1' st2'
                          else -- 1 High -> Low, 2 -> High -> High. Check 2
                              property $ --collect "High -> High" $ 
                              (observe obs tr2) == []
                              && indist obs st2 st2'
                      Nothing ->
                          -- 1 High -> Low, two failed. Reject
                          property rejected
                else -- 1: High -> High
{-                    whenFail (putStrLn $ PP.render 
                              $ text "HighStep:\n" $$ 
                                pp (Var obs st1 st1') $$ 
                                (text . show $ indist obs st1 st1')
                             ) $ -}
                    property $ --collect "High -> High" $ 
                    observe obs tr1 == [] && indist obs st1 st1'
        Nothing -> property rejected -- 1 Failed
  else -- not indistinguishable!
      property rejected

                    

             