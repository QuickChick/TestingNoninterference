{-# LANGUAGE ImplicitParams, FlexibleContexts #-}

module ObsCounterExample where

import TMUAbstract
import TMUInstr
import TMUObservable
import TMULabels

import Machine
import Trace
import TMUFlags
import TMUDriver
import Test.QuickCheck

import Debug.Trace(traceShow)

as1 :: AS
as1 = (AS { amem  = [Labeled L 0],
            aimem = [Call 0 False, 
                     Push (Labeled L 2), Push (Labeled L 0), Store,
                     Push (Labeled L 1), Push (Labeled L 0), Store, Return False],
            astk  = [AData (Labeled L 4)],
            apc   = Labeled L 0
          })

as2 :: AS
as2 = (AS { amem  = [Labeled L 0],
            aimem = [Call 0 False, 
                     Push (Labeled L 2), Push (Labeled L 0), Store,
                     Push (Labeled L 1), Push (Labeled L 0), Store, Return False],
            astk  = [AData (Labeled H 7)],
            apc   = Labeled L 0
          })

as1' :: AS
as1' = (AS { amem  = [],
             aimem = [Call 0 False, 
                      Noop,
                      Noop,
                      Return False],
             astk  = [AData (Labeled L 2)],
             apc   = Labeled L 0
           })
as2' :: AS
as2' = (AS { amem  = [],
             aimem = [Call 0 False, 
                      Noop,
                      Noop,
                      Return False],
             astk  = [AData (Labeled H 3)],
             apc   = Labeled L 0
           })


v :: Smart (Shrink2 (Variation AS))
v = Smart 0 $ Shrink2 $ Variation as1 as2

testSpecificVariation :: Flaggy DynFlags => (Smart (Shrink2 (Variation AS))) -> IO ()
testSpecificVariation v =
    quickCheck (gen_prop_noninterference_observable_lists observe v)
        where observe = filter ((==L) . lab. apc)

{-[0@L]
[Call 0 0, Push 1@L, Push 0@L, Store, Return]
[1@L/4@H]-}

runAS :: Flaggy DynFlags => AS -> Int -> IO (Trace AS)
runAS as n = do 
  ass' <- sample' $ traceN as n 
  return (head ass')
         
