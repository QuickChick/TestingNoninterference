
{-
  Plan: multicore alloc routine

        State = pair of machines sharing data memory
        Step  = step one of the machines and copy the memory
        Instruction for atomic section
        More powerful stack instructions (swap, dup, ..?)
          - to make the atomic part of the allocator small
          - to not have return address mess up for different threads

  Question: does the property hold? Probably not.

  Ideas (rough notes):

    - Change the abstract wf function so that unallocated memory
      cannot be accessed (and ignore it for purposes of testing
      equality).  Then maybe we get a property that is true.

    - Can we keep snapshots to allow us to roll back the core that is
      still in the allocator?  Maybe we can generalize the abstraction
      function to a history-dependent abstraction function...

  More notes

    - a multicore is wellformed if any of the cores are wellformed, as long as
      one core can make progress the machine can make progress

    - but, in the singlecore case we used wf on the abstract level to decide
      when to stop a test execution. The problem in the multicore case is that
      a wf abstract state may contain stuck abstract cores which aren't stuck
      when concretized. Thus the concrete machine can choose to step one of the
      bad cores. Tricky.

-}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies #-} -- For the TH magic
module MulticoreAlloc where

import Control.Applicative
import Test.QuickCheck
import Text.PrettyPrint

import Machine
import SinglecoreAlloc hiding (main)
import Pretty
import Util
import CollectTests

data MultiCore s = MS { unMS :: [s] }
  deriving (Eq, Functor)

instance Pretty s => Show (MultiCore s) where
  show = show . pretty

instance Pretty s => Pretty (MultiCore s) where
  pretty (MS ss) = text "MS" <+> list (map pretty ss)

instance (Eq s, Pretty s, Machine s) => Machine (MultiCore s) where

  isStep (MS ss0) (MS ss1) =
    case stepped of
      [(s0, s1)] -> isStep s0 s1
      _ -> False
    where
      stepped = filter (uncurry (/=)) $ zip ss0 ss1

  step (MS ss) = do
    let ns = [ (n, s) | (n, s) <- zip [0..] ss, isWF s ]
    (n, s) <- elements ns
    s <- step s
    return $ MS $ update n s ss

  wf (MS ss)
    | any isWF ss 
    = WF 
    | otherwise 
    = IF "No wf machine"

instance Layer (MultiCore CS) (MultiCore AS) where
  concretize = fmap concretize'
    where
      -- Non-wf abstract cores might still map to wf concrete cores, so if we
      -- concretize naively the concrete machine might pick one of the bad
      -- cores to step. To prevent this we set the pc to -1 in non-wf cores.
      concretize' as
        | isWF as   = cs
        | otherwise = cs { pc = -1 }
        where cs = concretize as
  abstract   = fmap abstract    -- not correct
  abstractable = any abstractable . unMS

  finishedAbstractStep (MS ss0) (MS ss1) =
    any (\(s0, s1) -> s0 /= s1 && abstractable s1) (zip ss0 ss1)

instance Arbitrary s => Arbitrary (MultiCore s) where
  arbitrary = MS <$> vectorOf 2 arbitrary
  shrink (MS ss) = do
    n <- [0..length ss - 1]
    let (ss0, s : ss1) = splitAt n ss
    s <- shrink s
    return $ MS $ ss0 ++ s : ss1

prop_mcorrect :: Property
prop_mcorrect = gen_prop_correct (length alloc * 10) 10 arbitrary $ \css ass prop ->
  whenFail (do
    mapM_ print (ass :: [MultiCore AS])
    putStrLn "---"
    mapM_ print (css :: [MultiCore CS])
  ) $
  -- collect ((5 *) $ (`div` 5) $ length $ nub pcs) $
  -- collect (maximum $ 0 : map snd (bagify pcs)) $
  -- collect (length ass) $
  -- collect pccount $
  -- collect (bagify pcs) $
  -- aggregate [ opCode (aimem as !! (apc as - length alloc)) | as <- ass ] $
  prop

multicoreAllocProps :: [(Name,[(Type,Property)])]
multicoreAllocProps = $(moduleProps [t|()|] [])

main :: IO ()
main = defaultTestingMain multicoreAllocProps
