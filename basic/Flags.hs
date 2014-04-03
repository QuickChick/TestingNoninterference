{-# LANGUAGE ImplicitParams, DeriveDataTypeable #-}
module TMUFlags where

import Data.Typeable
import Data.Data

data TMMRoutineConfig
  = AllInstructionsLow
  | AllInstructionsHigh
  | AllInstructionsFault
  | CorrectTMMRoutine
  | NoTMMRoutine
  deriving (Eq, Read, Show, Data, Typeable)

data GenStrategy
  = GenNaive              -- Arbitrary memory/stack/instruction stream.
  | GenWeighted
  | GenSequence

  | GenByExec             -- See Note [GenByExec] in TMUAbstractGen.
  | GenByExec1
  | GenByExec2
  | GenByExec3
  | GenByExec4

  | GenVariational
  | GenVariational1
  | GenVariational2
  | GenVariational3
  | GenVariational4

{-
  | GenByExecBothBranches -- See Note [GenByExecBothBranches] in TMUAbstractGen.
  | GenByExecAllBranchesFwd
  | GenByExecAllBranchesFwd2
  | GenByExecAllBranchesFwd3
-}

  | GenByFwdExec          -- See Note [GenByExecFwdOnly] in TMUAbstractGen.
  | GenTinySSNI

  deriving (Eq, Read, Show, Data, Typeable)

allStrategies :: [GenStrategy]
-- All individual strategies: NB /not/ the iterate one
allStrategies
  = [ GenNaive
    , GenWeighted
    , GenSequence
    , GenByExec             
    , GenByExec1
    , GenByExec2
    , GenByExec3
    , GenByExec4

    , GenVariational
    , GenVariational1
    , GenVariational2
    , GenVariational3
    , GenVariational4

{-
    , GenNaiveInstrOnly     
    , GenByExecBothBranches 
    , GenByExecAllBranchesFwd
    , GenByExecAllBranchesFwd2
    , GenByExecAllBranchesFwd3
-}
    , GenByFwdExec 
    , GenTinySSNI ] 


data TMUProperty
  = 
    -- Abstract machine
    PropSynopsisNonInterference
  | PropLLNI
  | PropSSNI
  | PropEENI
    
    -- broken variant
  | PropEENInoLow
  
    -- Profiling
  | PropJustProfile -- Profiling execution lengths
                    -- and termination reasons.
  | PropJustProfileVariation
                    -- Profile execution of a test
                    -- and its variation

  deriving (Eq, Read, Show, Data, Typeable)

isTestableProperty :: TMUProperty -> Bool
isTestableProperty s
  | PropJustProfile <- s = False
  | PropJustProfileVariation <- s = False
  | otherwise = True


-- CH: if it comes to generating configurations,
--     we will want to internalize the "Bug"/"Variant" part
--     which is now just a naming convention
data IfcSemantics
  = IfcDefault
      -- correct default configuration
  | IfcBugPushNoTaint
      -- push drops taint
  | IfcBugPopPopsReturns
      -- pop pops return addresses
  | IfcBugArithNoTaint
      -- addition and subtraction don't taint their result
  | IfcBugLoadNoTaint
      -- load loses the label of the memory location
  | IfcBugStoreNoValueTaint
      -- store drops all taint
  | IfcBugStoreNoPointerTaint
      -- store forgets to taint by the label of the address
  | IfcBugStoreNoPcTaint
      -- store forgets to taint by the PC label

  | IfcBugJumpNoRaisePc
      -- jumping to a high address does not raise the pc
  | IfcBugJumpLowerPc
      -- jumping to a low address lowers the pc
  
  | IfcBugJumpNZNoRaisePcTaken
      -- conditional jumps with high condition or address don't
      -- raise pc when jumping occurs
  | IfcBugJumpNZNoRaisePcNotTaken
      -- conditional jumps with high condition don't raise pc
      -- when no jumping occurs 
  
  | IfcBugCallNoRaisePc
      -- calling a high address does not raise the pc
  | IfcBugReturnNoTaint
      -- returning from a high address forgets to taint the return value
  | IfcBugValueOrVoidOnReturn
      -- we choose whether to return a value or not at return time
      -- instead of function call time
  
  | IfcBugAllowWriteDownThroughHighPtr
      -- allow stores to a currently low memory cell
      -- through a high pointer
  | IfcBugAllowWriteDownWithHighPc
      -- allow stores to a currently low memory cell with high pc

  | IfcVariantDisallowStoreThroughHighPtr
      -- this correct variant disallows any store through a high pointer,
      -- irrespective of the current label of the written memory cell
      -- (this is more restrictive than preventing high pointer write-downs)
  | IfcVariantWriteDownAsNoop
      -- this correct variant turns both kinds of write-downs
      -- into NoOps (instead of just stopping the machine)

  deriving (Eq, Read, Show, Data, Typeable)

  

allIfcBugs :: [IfcSemantics]
-- NB: Just the bug list, no more!
allIfcBugs
  = [ IfcBugArithNoTaint
    , IfcBugPushNoTaint
    , IfcBugPopPopsReturns
    , IfcBugLoadNoTaint
    , IfcBugStoreNoValueTaint
    , IfcBugStoreNoPointerTaint
    , IfcBugAllowWriteDownThroughHighPtr
    , IfcBugJumpNoRaisePc
    , IfcBugJumpLowerPc
    , IfcBugStoreNoPcTaint
    , IfcBugAllowWriteDownWithHighPc
    , IfcBugCallNoRaisePc
    , IfcBugReturnNoTaint
    , IfcBugValueOrVoidOnReturn
    , IfcBugJumpNZNoRaisePcTaken
    , IfcBugJumpNZNoRaisePcNotTaken
    ]

readIfcSemanticsList :: DynFlags -> [IfcSemantics]
-- Reads the string representing IfcSemantics
readIfcSemanticsList df
  | ifc_semantics df == "*" = allIfcBugs
  | otherwise = read (ifc_semantics df)

readIfcSemantics :: DynFlags -> [IfcSemantics]
-- Reads a parsed and cached version of IfcSemantics, which may be
-- replaced by a singleton list for generating tables of bug detection
-- times. 
readIfcSemantics df
  = ifc_semantics_singleton df

data GenInstrs
  = InstrsBasic -- Generate only very basic instructions (Add/Push/Noop/Load/Store/Halt)
  | InstrsJumpy -- + allowed to generate jumps
  | InstrsCally -- + allowed to generate calls/returns
  | InstrsTMM  -- + Sub and JumpNZ
  | InstrsLabelOf -- + LabelOf (unclear status at the moment)
  deriving (Eq, Read, Show, Data, Typeable,
            Ord -- n.b.!
            )

callsAllowed :: GenInstrs -> Bool
callsAllowed = (>= InstrsCally)

jumpAllowed :: GenInstrs -> Bool
jumpAllowed = (>= InstrsJumpy)

tmmAllowed :: GenInstrs -> Bool
tmmAllowed = (>= InstrsTMM)

labelOfAllowed :: GenInstrs -> Bool
labelOfAllowed = (>= InstrsLabelOf)

data StartingAS = StartInitial | StartQuasiInitial | StartArbitrary
                deriving (Eq, Read, Show, Ord, Enum, Bounded, Data, Typeable)

data Equiv = EquivMem | EquivLow | EquivWrongFull | EquivFull
                deriving (Eq, Read, Show, Ord, Enum, Bounded, Data, Typeable)

data AtomEquiv = LabelsObservable    -- correct + default
                                     -- labels are not fully observable,
                                     -- but since we only have 2 of
                                     -- them this makes no difference
               | LabelsNotObservable -- incorrect
               | HighEquivEverything -- incorrect
               deriving (Eq, Read, Show, Ord, Enum, Bounded, Data, Typeable)

data StkEltEquiv = TagOnTop   -- correct + default (high values and high
                              -- return addresses are distinguishable)                   
                 | LabelOnTop -- incorrect (all high values indistinguishable
                              --            from all high return addresses)
                 deriving (Eq, Read, Show, Ord, Enum, Bounded, Data, Typeable)

data TMUDriver
  = TMUDriver { -- Generation behavior

               gen_instrs       :: GenInstrs
             
             , gen_strategy     :: GenStrategy
               
             , gen_instrs_range :: (Int,Int)
                  -- (x,y) <- gen_instrs_range
                  -- Generate an arbitrary number of instructions >= x and <= y

             , starting_as :: StartingAS
             , equiv :: Equiv
             
             , smart_ints :: Bool -- affects both generation and variation

               -- Shrinking behavior
               -- CH: TODO: how about a datatype?
             , shrink_nothing :: Bool
             , shrink_to_noop :: Bool
             , shrink_noops   :: Bool

               -- Bugs in indistinguishability relation
             , atom_equiv :: AtomEquiv
             , stk_elt_equiv :: StkEltEquiv
               
               -- Information flow semantics               
             , ifc_semantics :: String
               -- A string that represents an [IfcSemantics], or "*"
               
               -- CH: the name singleton is misleading
             , ifc_semantics_singleton :: [IfcSemantics]
               -- A cached version of ifc_semantics, see TMUDriver.hs

               -- TMM routine
             , which_tmm_routine :: TMMRoutineConfig
               
               -- Other configuration settings
             , tmu_abstract_steps    :: Int -- How many steps to test for
             , tmu_concrete_steps    :: Int -- Max # of concrete steps between abs. states

             , tmu_timeout           :: Int -- Timeout in seconds
             , tmu_max_tests         :: Int -- Maximum # of tests (subsidiary to timeout)
             , tmu_max_discard_ratio :: Int
             , tmu_prop_test         :: TMUProperty -- The property to test
             , tmu_extrapol_mul      :: Int
             , tmu_extrapol_add      :: Int
             , show_counterexamples  :: Bool -- print counterexamples of not?
             , conf_max_call_args    :: Int

             , latex_output          :: Bool
             , print_all_datapoints  :: Bool -- print all times taken to find bugs
             , run_timeout_tests     :: Bool
             }
  deriving (Eq, Read, Show, Data, Typeable)

type DynFlags = TMUDriver

getMaxBugs :: DynFlags -> Int
getMaxBugs f = tmu_extrapol_add f + (tmu_timeout f) * (tmu_extrapol_mul f)

-- The default setting for flags should produce a correct machine
dynFlagsDflt :: DynFlags
dynFlagsDflt
  = TMUDriver { gen_instrs      = InstrsCally
              , gen_strategy     = GenByExec
              , gen_instrs_range = (20,50)
               
              , starting_as = StartQuasiInitial
              , equiv = EquivFull
              
              , smart_ints = True
              
              , shrink_nothing = False
              , shrink_to_noop = True
              , shrink_noops   = True 
             
              , atom_equiv = LabelsObservable
              , stk_elt_equiv = TagOnTop
              
              , ifc_semantics = "[IfcDefault]"
              , ifc_semantics_singleton = [IfcDefault]
                                                      
              , which_tmm_routine = CorrectTMMRoutine
              
              , tmu_abstract_steps    = 50
              , tmu_concrete_steps    = 5000 -- ASZ: See Note [TMU Concrete Steps]
              , tmu_timeout           = 1
              , tmu_max_tests         = maxBound `div` 100 -- See Notes [Max Tests Too Large]
              , tmu_max_discard_ratio = 30
              , tmu_prop_test         = PropLLNI
              , tmu_extrapol_mul      = 10
              , tmu_extrapol_add      = 1000
              , show_counterexamples  = False
              , conf_max_call_args    = 2
              , latex_output          = False
              
              , print_all_datapoints  = False
              , run_timeout_tests     = True
              }

-- Note [Max Tests Too Large]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If the maximum number of TMU tests to run (tmu_max_tests) is too large,
-- QuickCheck may (somewhat surprisingly) die by "giving up"---that is,
-- producing too many discards.  The reason for this is integer overflow.  In
-- order to calculate the maximum number of allowed discards, QuickCheck
-- computes `maxDiscardRatio * maxSuccess`¹.  If that multiplication overflows,
-- then we might get a negative number and immediately fail.  Or we might happen
-- to get a large positive number; this actually happened to us accidentally.
-- The default value for `maxDiscardRatio` is 10, and it so happens that while
-- `10 * (maxBound :: Int) = -10`, dividing by two first produced
-- `10 * ((maxBound :: Int) `div` 2) = 9223372036854775798`, which---although
-- rather large---is still smaller than `maxBound `div` 2`.
--
-- Consequently, how large should we make this value when we're working with
-- timeouts?  Well, suppose that (a) we're working with 64-bit integers (which
-- we are); and (b) that every test is one operation on a 4 GHz machine, taking
-- 0.25 ns (which is a vast underestimate, but a lower bound).  Then:
--   * maxBound tests will take 73.12 years;
--   * maxBound/2 tests will take 36.56 years;
--   * maxBound/10 tests will take 7.312 years;
--   * maxBound/20 tests will take 3.656 years;
--   * maxBound/50 tests will take 1.462 years; and
--   * maxBound/100 tests will take 0.7312 years.
-- Thus, any of those values is MORE than reasonable; going with the last
-- provides plenty of space for a nice discard ratio.
--
-- ¹ For QuickCheck 2.5.1.1, this is in the definition of
-- Test.QuickCheck.Test.quickCheckWithResult, at line 110;
-- http://hackage.haskell.org/packages/archive/QuickCheck/2.5.1.1/doc/html/src/Test-QuickCheck-Test.html#line-110
-- and/or
-- http://hackage.haskell.org/packages/archive/QuickCheck/2.5.1.1/doc/html/src/Test-QuickCheck-Test.html#quickCheckWithResult will take you there.

-- Note [TMU Concrete Steps]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- Morally, this perhaps ought to be relative to which_tm_routine, but we don't
-- have any non-terminating-TMM bugs.


class Flaggy a where
  getFlags :: a




