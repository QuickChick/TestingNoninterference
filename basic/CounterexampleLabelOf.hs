{-# LANGUAGE ImplicitParams, FlexibleContexts #-}

module Counterexample where

import Test.QuickCheck
import System.Console.CmdArgs
import Control.Monad

import TMULabels
import TMUFlags
import TMUObservable
import TMUInstr

import TMUTMMRoutine (tmmRoutine, tmuCacheSize)
import TMUAbstract

import Util
import Trace
import Machine
import Control.Monad.Identity

-- The counterexample against labelOf would look something like this

-- we would need a label m between L and H, but we don't have it
m :: Label
m = H

stepId :: Flaggy DynFlags => AS -> Identity AS
stepId = defaultStep "TMUAbstract.AS" $ return . astepFn

traceId :: Flaggy DynFlags => AS -> Int -> [AS]
traceId cs n = runIdentity $ stepN (liftM Just . stepId) cs n

prop_print :: (Flaggy DynFlags) =>
              Smart (Shrink2 (Variation AS)) -> Property
prop_print (Smart _ (Shrink2 (Variation as as'))) =
  shrinking shrink (steps,steps) $ \(n,n') ->
    -- CH: fixed 50 since otherwise n gets shrunk to 1
    forAll (liftM2 (,) (traceN as 50) (traceN as' 50)) $
    \(Trace ass, Trace ass') ->
         whenFail (when (show_counterexamples getFlags) $
                   do 
                      print (zipWith Variation (aimem as) (aimem as'))
                      putStrLn "--- Common execution prefix:"
                      let n = length (takeWhile (\ (as,as') -> 
                                                    apc as == apc as') 
                                        (zip ass ass'))
                      printLockstep (take n (zip ass ass'))
                      putStrLn "--- Machine 1 continues..."
                      printTrace (drop n ass)
                      putStrLn "--- Machine 2 continues..."
                      printTrace (drop n ass')
                      ) $ False
  where steps = tmu_abstract_steps getFlags
        printLockstep l = 
          forM_ l $ \ (as,as') -> 
            do putStr (show (apc as))
               putStr "\tM="
               putStr (show (zipWith Variation (amem as) (amem as')))
               putStr "\tS="
               putStr (show (zipWith Variation (astk as) (astk as')))
               putStr "\tnext="
               let len = absAdjustIAddr (value $ apc as) 
               if isIndex len (aimem as) &&
                  isIndex len (aimem as') then print (Variation (aimem as !! len)
                                                                (aimem as' !! len))
                                          else putStrLn "<eof>"
        printTrace l = 
          forM_ l $ \ as -> 
            do putStr (show (apc as))
               putStr "\tM="
               putStr (show (amem as))
               putStr "\tS="
               putStr (show (astk as))
               putStr "\tnext="
               let len = absAdjustIAddr (value $ apc as) 
               if isIndex len (aimem as) then print (aimem as !! len)
                                         else putStrLn "<eof>"

as :: Flaggy DynFlags => Bool -> AS
as i = AS { apc  = Labeled L startIMem
          , amem = []
          , astk = [ AData (Labeled m (if i then (startIMem + 1)
                                       else (startIMem + 3)))
                   , AData (Labeled L 0)
                   , ARet (Labeled L (startIMem + 4,True))]
          , aimem = [Jump, Push (Labeled H (startIMem + 3)), Jump, Return, LabelOf] }
  where startIMem = length tmmRoutine

counterexample :: Flaggy DynFlags => Variation AS
counterexample = Variation (as True) (as False)

{- Can only get this trace after adding a label M, and making m = M
[Jump,Push 2051@H,Jump,Return,LabelOf]
--- Common execution prefix:
2048@L	M=[]	S=[{AData 2049@M/AData 2051@M},AData 0@L,ARet (2052,True)@L]	next=Jump
--- Machine 1 continues...
2049@M	M=[]	S=[AData 0@L,ARet (2052,True)@L]	next=Push 2051@H
2050@M	M=[]	S=[AData 2051@H,AData 0@L,ARet (2052,True)@L]	next=Jump
2051@H	M=[]	S=[AData 0@L,ARet (2052,True)@L]	next=Return
2052@L	M=[]	S=[AData 0@H]	next=LabelOf
2053@L	M=[]	S=[AData 2@L]	next=<eof>
--- Machine 2 continues...
2051@M	M=[]	S=[AData 0@L,ARet (2052,True)@L]	next=Return
2052@L	M=[]	S=[AData 0@M]	next=LabelOf
2053@L	M=[]	S=[AData 1@L]	next=<eof>
-}

as' :: Flaggy DynFlags => Bool -> AS
as' i = AS { apc  = Labeled L startIMem
          , amem = [Labeled m 0]
          , astk = [ AData (Labeled m (if i then (startIMem + 1)
                                       else (startIMem + 2)))
                   , AData (Labeled L (tmuCacheSize)) -- store address
                   , AData (Labeled H 0)              -- store payload
                   , ARet (Labeled L (startIMem + 3, False))
                   , AData (Labeled L (tmuCacheSize)) -- load address
                   ]
          , aimem = [Jump
                    , Store
                    , Return
                    , Load
                    , LabelOf] }
  where startIMem = length tmmRoutine

counterexample' :: Flaggy DynFlags => Variation AS
counterexample' = Variation (as' True) (as' False)

{-
[Jump,Store,Return,Load,LabelOf]
--- Common execution prefix:
2048@L	M=[0@M]	S=[{AData 2049@M/AData 2050@M},AData 6@L,AData 0@H,ARet (2051,False)@L,AData 6@L]	next=Jump
--- Machine 1 continues...
2049@M	M=[0@M]	S=[AData 6@L,AData 0@H,ARet (2051,False)@L,AData 6@L]	next=Store
2050@M	M=[0@H]	S=[ARet (2051,False)@L,AData 6@L]	next=Return
2051@L	M=[0@H]	S=[AData 6@L]	next=Load
2052@L	M=[0@H]	S=[AData 0@H]	next=LabelOf
2053@L	M=[0@H]	S=[AData 2@L]	next=<eof>
--- Machine 2 continues...
2050@M	M=[0@M]	S=[AData 6@L,AData 0@H,ARet (2051,False)@L,AData 6@L]	next=Return
2051@L	M=[0@M]	S=[AData 6@L]	next=Load
2052@L	M=[0@M]	S=[AData 0@M]	next=LabelOf
2053@L	M=[0@M]	S=[AData 1@L]	next=<eof>
-}

main :: IO ()
main = do
  flags <- cmdArgs dynFlagsDflt
  let ?dfs = flags
--  unless (as True ~~~ as False) $ error "not equivalent"
  quickCheckWith stdArgs{maxSuccess = 1} . prop_print .
    Smart 0 $ Shrink2 counterexample'

