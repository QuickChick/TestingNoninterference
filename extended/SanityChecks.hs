{-# LANGUAGE TupleSections, RecordWildCards #-}
module SanityChecks where

import Test.QuickCheck
import Test.QuickCheck.Property

import Machine
import Reachability
import Rules
import SingleStateArb()

--import Pretty
--import Text.PrettyPrint (text,($$))
--import qualified Text.PrettyPrint as PP

propStampsWellFormed :: Property
propStampsWellFormed = forAllShrink arbitrary shrink wellFormed

propPreservesWellFormed :: RuleTable -> State -> Property
propPreservesWellFormed t st =
    wellFormed st ==> 
    case exec t st of 
      Just (_, st') -> property $ 
{-          whenFail (putStrLn . PP.render $ 
                       text "Not well formed:" $$ 
                       text "Original State:" $$
                       pp st $$
                       text "Final State:" $$ 
                       pp st'
                   ) $ -} 
                       wellFormed st'
      Nothing -> property rejected
