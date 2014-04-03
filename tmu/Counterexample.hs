{-# LANGUAGE ImplicitParams, FlexibleContexts #-}

module Counterexample where

import Test.QuickCheck
import System.Console.CmdArgs
import Control.Monad

import TMULabels
import TMUFlags
import TMUObservable
import TMUInstr

import TMUTMMRoutine (tmmRoutine)
import TMUAbstract

import TMUDriver hiding (main)

counterexample :: Flaggy DynFlags => Variation AS
counterexample = Variation (as 0) (as 1)

as :: Flaggy DynFlags => Int -> AS
as i = AS { apc  = Labeled H (length tmmRoutine)
          , amem = []
          , astk = [ ARet (Labeled L (length tmmRoutine,False))
                   , AData (Labeled L i) ]
          , aimem = [Return, Noop, Noop] }

main :: IO ()
main = do
  flags <- cmdArgs dynFlagsDflt
  let ?dfs = flags
  unless (as 0 ~~~ as 1) $ error "not equivalent"
  quickCheckWith stdArgs{maxSuccess = 1} . prop_low_lockstep .
    Smart 0 $ Shrink2 counterexample
