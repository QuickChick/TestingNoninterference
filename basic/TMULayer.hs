{-# LANGUAGE ImplicitParams, FlexibleContexts, UndecidableInstances,
    RecordWildCards, TupleSections, MultiParamTypeClasses #-}

module TMULayer where

import Machine

import TMULabels
import TMUFlags

import TMUTMMRoutine
import TMUAbstract 
import TMUConcrete

-- TODO: This should be in its own section (and, before we get to it,
-- we should include the testing code that demonstrates
-- noninterference)

-- This type class instance says that CS is a concrete version of AS / AS is an
-- abstract version of CS.
instance Flaggy DynFlags => Layer CS AS where
  -- When making an AS concrete, we initialize its tmuCache with nonsense data
  -- and make sure it's not running in priviliged mode.
  concretize AS{..} =
    CS { mem  = replicate tmuCacheSize (Labeled H (-1)) ++ amem
       , imem = tmmRoutine ++ aimem
       , stk  = map conv astk
       , pc   = apc
       , priv = False }
    where conv (AData d) = CData d
          conv (ARet  r) = CRet  r False
  
  abstract CS{..} = AS { amem  = drop tmuCacheSize mem
                       , aimem = drop (length tmmRoutine) imem
                       , astk  = map conv stk
                       , apc   = pc }
    where conv (CData d)   = AData d
          conv (CRet  r _) = ARet  r
  
  -- We can't abstract a concrete state if we're in the middle of the TMM
  abstractable CS{..} =  value pc >= length tmmRoutine
                      && not priv
                      && not (any cstkRetPriv $ filter (not . isCData) stk)
  
  -- The execution of the concrete machine is as follows:
  --    1. Some abstractable state not in the TMU
  --    2. ... several non-abstractable TMM states ...
  --    3. The same state as the first one, but with the TMU updated
  --    4. The next abstractable state
  -- We want stepUntilAbstractable to take us from 1 to *4*, not to 3.  Thus, if
  -- we just exited the TMU (the previous state was privileged), we aren't done
  -- with our abstract step.  I think this logic is correct.  (Note that if the
  -- first abstractable state is in the TMU cache, then steps 2 & 3 don't exist,
  -- and everything should work fine with or without the `not (priv cs)` check.)
  finishedAbstractStep cs cs' = not (priv cs) && abstractable cs'
