{-# LANGUAGE FlexibleContexts, UndecidableInstances, RecordWildCards,
    TupleSections #-}

module Generation where

import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List ( find, isInfixOf )

import Util
import GenericMachine

import Labels
import Flags
import Instr

import Machine

-- TODO: The amount of duplication in this file is amazing

-- import Debug.Trace

{- In this module we define and experiment with random AS
 - machine generation.
 - ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -}

genAS :: Flaggy DynFlags 
      => (AS -> Gen AS) -- Variation generation
      -> Gen AS
genAS vary
  = gen_as $ gen_strategy getFlags
  where gen_as GenNaive             = genNaive
        gen_as GenWeighted          = genWeighted
        gen_as GenSequence          = genSequence

        gen_as GenByExec            = genByExec 0
        gen_as GenByExec1           = genByExec 2
        gen_as GenByExec2           = genByExec 4 
        gen_as GenByExec3           = genByExec 6 
        gen_as GenByExec4           = genByExec 8

        gen_as GenVariational       = genByExecVariational 0 vary
        gen_as GenVariational1      = genByExecVariational 2 vary
        gen_as GenVariational2      = genByExecVariational 4 vary
        gen_as GenVariational3      = genByExecVariational 6 vary
        gen_as GenVariational4      = genByExecVariational 8 vary

{-
        gen_as GenByExecBothBranches     = genByExecBothBranches
        gen_as GenByExecAllBranchesFwd   = genByExecAllBranchesFwd
        gen_as GenByExecAllBranchesFwd2  = genByExecAllBranchesFwd2
        gen_as GenByExecAllBranchesFwd3  = genByExecAllBranchesFwd3
-}
        gen_as GenByFwdExec    = genByFwdExec

        gen_as GenTinySSNI     = genTinySSNI

{----------------------------------------------------------------------
Note [GenNaive]
~~~~~~~~~~~~~~~~~~
No cleverness here. Generate absolutely random memory/stack and
instruction stream; but still respect the smart_ints flag
-----------------------------------------------------------------------}
genNaive :: Flaggy DynFlags => Gen AS
genNaive = mkWeighted (const 1)

instrWeights :: Flaggy DynFlags => InstrKind -> Int
instrWeights = case prop_test getFlags of
  PropEENI -> weights_EENI
  PropLLNI -> weights_LLNI
  PropJustProfile -> weights_EENI -- Using weights_EENI for profiling
  PropJustProfileVariation -> weights_EENI
  _        -> error "genWeighted: unsupported property"

genWeighted :: Flaggy DynFlags => Gen AS
genWeighted = mkWeighted instrWeights

genSequence :: Flaggy DynFlags => Gen AS
genSequence = do
  amemSize <- sized $ \n -> choose (0,n)
  amem <- case starting_as getFlags of
    StartInitial -> initMem
    _ -> sequence [ arbitrary | _ <- [1..amemSize] ]
  aimemSize <- choose $ gen_instrs_range getFlags
  let genInstrMem :: Int -> -- Current step
                     [Instr] -> -- Instructions generated so far
                     Int -> -- Maximum number of instructions
                     Gen [Instr]
      genInstrMem i acc maxSize
        | i >= maxSize = return acc
        | otherwise = do
          instrs <-
            frequency $
            [ (10, (:[]) <$> genWeightedInstr instrWeights' gInt) ] ++
            [ (1, push2AndDo Add <$> labeled gInt <*> labeled gInt) ] ++
            [ (1, pushAndDo Load <$> labeled maddr) ] ++
            [ (1, push2AndDo Store <$> labeled gInt <*> labeled maddr) ] ++
            [ (1, pushAndDo Jump <$> labeled iaddr) | jumpy ] ++
            [ (1, do { c_args <- choose (0, maxArgs)
                     ; c_ret  <- arbitrary
                     ; addr <- labeled iaddr
                     ; proc_size <- choose (0, (maxSize - i) `div` 2)
                     ; proc <- genInstrMem 0 [] proc_size
                     ; b <- arbitrary
                     ; return $ pushAndDo (Call c_args c_ret) addr
                       ++ proc ++ [Return b]}) | cally ]

          genInstrMem (i + length instrs) (instrs ++ acc) maxSize

      gInt = smartInt aimemSize amemSize

      instrWeights' CALL = 0
      instrWeights' RETURN = 0
      instrWeights' HALT = 120
      instrWeights' kind = instrWeights kind

      iaddr = genValidIAddr aimemSize
      maddr = genValidMAddr amemSize

      pushAndDo i a = [Push a, i]
      push2AndDo i a1 a2 = [Push a1, Push a2, i]
      maxArgs = conf_max_call_args getFlags
      cally  = callsAllowed (gen_instrs getFlags)
      jumpy  = jumpAllowed  (gen_instrs getFlags)
      genAll = jumpAllowed  (gen_instrs getFlags)

  aimem <- genInstrMem 0 [] aimemSize
  (astk,apcl) <- case starting_as getFlags of
       StartInitial      -> liftA2 (,) (pure []) (pure L)
       StartQuasiInitial -> liftA2 (,) (listOf $ AData <$> labeled gInt) (pure L)
       StartArbitrary    -> arbitrary
  return AS {apc = Labeled apcl 0, ..}

-- Here somebody has just repeated the frequency table for ainstr with
-- no extra cleverness! TODO: try to share these tables
weights_LLNI :: Flaggy DynFlags => InstrKind -> Int
weights_LLNI PUSH = -- we generate significantly more pushes
  if starting_as getFlags == StartInitial then 300 else 150
  -- CH: TODO: tweak this more
  -- should make a distinction between Basic and Cally?
weights_LLNI NOOP = 1   -- we avoid noops
weights_LLNI HALT = 5   -- we avoid halts
weights_LLNI _    = 40

-- A fresh set of weights for EENI
weights_EENI :: Flaggy DynFlags => InstrKind -> Int
weights_EENI PUSH =  -- we generate significantly more pushes
  if starting_as getFlags == StartInitial then 400 else 200
  -- CH: TODO: tweak this more
  -- why is there a distinction here wrt LLNI?
  -- is it just to account for NOOPs and HALTs which have different weights?
weights_EENI HALT = 70   -- we generate slightly more halts
weights_EENI _    = 40

mkWeighted :: Flaggy DynFlags => (InstrKind -> Int) -> Gen AS
mkWeighted w = do
  aimemSize <- choose $ gen_instrs_range getFlags
  (mem,stk,pc) <- initAS aimemSize
  let amemSize = length mem
  imem <- mapM (const $ genWeightedInstr w (smartInt aimemSize amemSize)) [0..aimemSize-1]
  return AS { apc = pc, amem = mem, astk = stk, aimem = imem }

{-   
  aimemSize <- choose $ gen_instrs_range getFlags
  (stk,mem) <- 

  
  amemSize <- sized $ \n -> choose (0,n)
  amem <- case starting_as getFlags of
    StartInitial -> initMem
    _ -> vectorOf amemSize (smartInt aimemSize amemSize)
    
  aimem <- mapM (const $ genWeightedInstr w (smartInt aimemSize amemSize)) [0..aimemSize-1]
  (astk,apcl) <- case starting_as getFlags of
       StartInitial      -> liftA2 (,) (pure []) (pure L)
       StartQuasiInitial -> liftA2 (,) arbitrary (pure L)
       StartArbitrary    -> arbitrary
  return $ AS {apc = Labeled apcl 0, ..}

 where gen_stk = listOf $
 -}               

genWeightedInstr :: Flaggy DynFlags => (InstrKind -> Int) -> Gen Int -> Gen Instr
genWeightedInstr w gint = frequency $
      [ (w NOOP,    pure Noop) ] ++
      [ (w HALT,    pure Halt) ] ++
      [ (w ADD,     pure Add) ] ++
      [ (w PUSH,    liftM Push (labeled gint)) ] ++
      [ (w POP,     pure Pop) ] ++
      [ (w STORE,   pure Store) ] ++
      [ (w LOAD,    pure Load) ] ++
      [ (w CALL,    liftM2 Call (choose (0, maxArgs)) arbitrary) | cally ] ++ 
      [ (w RETURN,  liftM Return arbitrary) | cally ] ++
      [ (w JUMP,    pure Jump) | jumpy ]
    where maxArgs = conf_max_call_args getFlags
          cally   = callsAllowed   $ gen_instrs getFlags
          jumpy   = jumpAllowed    $ gen_instrs getFlags

-- generate valid code and data addresses more often
smartInt :: Flaggy DynFlags => Int -> Int -> Gen Int
smartInt = smartIntWeighted (1,1,1)

smartIntWeighted :: Flaggy DynFlags => (Int,Int,Int) -> Int -> Int -> Gen Int
smartIntWeighted (int_weight,imem_weight,mem_weight) aimemSize amemSize
  | smart_ints getFlags
  = frequency $
    [ (int_weight, int)
    , (mem_weight, genValidMAddr amemSize)
    ] ++
    [ (imem_weight, genValidIAddr aimemSize) | not_basic ]
  | otherwise
  = arbitrary
  where not_basic = gen_instrs getFlags /= InstrsBasic

genValidIAddr :: Flaggy DynFlags => Int -> Gen Int
genValidIAddr aimemSize
  = choose (0, aimemSize)

genValidMAddr :: Flaggy DynFlags => Int -> Gen Int
genValidMAddr amemSize
  = choose (0, amemSize)


{----------------------------------------------------------------------
Note [GenNaiveInstrOnly]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
What if we generate a more informed memory and stack, and a completely
random instruction stream? Actually, experiments DV run show it's not
better than GenNaive in terms of the profile of the tests (i.e. 98% of
the programs still crash/terminate very fast), but I am keeping this
here for reference.
-----------------------------------------------------------------------}
genNaiveInstrOnly :: Flaggy DynFlags => Gen AS
genNaiveInstrOnly
  = do { aimem_size      <- choose (gen_instrs_range getFlags)
       ; (amem,astk,apc) <- initAS aimem_size
       ; aimem <- replicateM aimem_size arbitrary
       ; return AS{..} }

initMem :: Gen [Atom]
initMem = listOf . pure $ Labeled L 0

initAS :: Flaggy DynFlags => Int -> Gen ([Atom],[AStkElt],Atom)
-- Generate random initial AS stack, memory, and PC.  We pass initAS the
-- size of aimem, to be used for a well-informed selection of the
-- contents of memory and stack.
initAS n = case starting_as getFlags of
  StartInitial ->
    liftA3 (,,) initMem (pure []) (pure . Labeled L $ 0)
  StartQuasiInitial ->
    do (stk,mem) <- stkMem n
       pure (stk,mem,Labeled L 0)
  StartArbitrary ->
    do (stk,mem) <- stkMem n 
       pcl <- arbitrary
       pure (stk,mem,Labeled pcl 0)
  -- where
  --   stkMem =
  --     do { amemSize  <- sized $ \n -> choose (0,n)
                        
  --        ; amem_init <- vectorOf amemSize (labeled $ smartInt n amemSize)
  --                       -- DV: used to be: frequency [(1,int),(2,genValidIAddr n)]
                        
  --        ; astk_init <-
  --             listOf $ frequency $
  --             [ (5, fmap AData (labeled $ smartIntWeighted (1,1,4) n amemSize)) ] ++ 
  --             [ (1, fmap ARet  (labeled $ aret_rand_pair)) | callsAllowed (gen_instrs getFlags) ] 
  --        ; return (amem_init, astk_init) }
  --   aret_rand_pair = liftA2 (,) (genValidIAddr n) arbitrary


stkMem :: Flaggy DynFlags => Int -> Gen ([Atom], [AStkElt])
stkMem n -- imem size
  = do { amemSize  <- sized $ \x -> choose (0,x)
                        
       ; amem_init <- vectorOf amemSize (labeled $ smartInt n amemSize)
           -- DV: used to be: frequency [(1,int),(2,genValidIAddr n)]
                        
       ; astk_init <-
              listOf $ frequency $
              [ (5, fmap AData (labeled $ smartIntWeighted (1,1,4) n amemSize)) ] ++ 
              [ (1, fmap ARet  (labeled aret_rand_pair)) | callsAllowed (gen_instrs getFlags) ] 
       ; return (amem_init, astk_init) }
  where aret_rand_pair = liftA2 (,) (genValidIAddr n) arbitrary

{----------------------------------------------------------------------
Note [GenByExec]
~~~~~~~~~~~~~~~~
This is the first more clever "generation-by-execution" strategy. We
first create an initial memory and stack using the informed initAS
function.

We then start executing from the initial PC. If there is no
instruction there, then we generate some instructions that look
reasonable in this state (with ainstrs). If there is an instruction
there, we simply execute it. If for some reason we reach an ill-formed
state (e.g. because of a backwards jump to an already generated
instruction which then causes the state to be ill formed) we just stop
generating instructions.

Because we are doing actual execution including backward jumps, we
have to be careful about looping. In particular if we exceed 3 *
total_is, we just bail out with whatever instruction stream we have.

The advantage of this strategy is that it boosts up the number of
successful tests. One disadvantage is that we see a lot of empty
blocks of instructions,e.g:

  Push _, Push _ , Jmp, Noop, Noop, Noop, ..., Noop, Load, Push _

and so the "behaviour content" of these tests is not extremely
high. In particular if we ever generate a conditional jump:

15: JumpNZ 35
16:  
..
35: ...

assume that the stack has a non-0 on top, then we will jump to 35 and
continue generating instructions from then on, and /only/ if we get
lucky we will jump back to the block in-between 16-35 and generate
interesting instructions for the non-taken branch. Of course, a
variation of the original state may very well result in that branch
not taken, so it might be important to generate code for it always,
and not just opportunistically (this is what GenByExecBothBranches
will do.
-----------------------------------------------------------------------}
genByExec :: Flaggy DynFlags => Int -> Gen AS
genByExec lookahead
  = do { total_is <- choose (gen_instrs_range getFlags)
       ; (init_mem,init_stk,init_pc) <- initAS total_is
       ; let init_as = AS { astk  = init_stk
                          , amem  = init_mem
                          , aimem = replicate total_is Noop
                          , apc   = init_pc }
       ; (is, _mis) <- genIS total_is lookahead (init_as, replicate total_is Nothing)
       ; return $ init_as { aimem = is, apc = init_pc } }

genByExecVariational :: Flaggy DynFlags 
                     => Int 
                     -> (AS -> Gen AS) 
                     -> Gen AS
genByExecVariational lookahead vary
  = do { total_is <- choose (gen_instrs_range getFlags)
       ; (init_mem,init_stk,init_pc) <- initAS total_is
       ; let init_as = AS { astk  = init_stk
                          , amem  = init_mem
                          , aimem = replicate total_is Noop
                          , apc   = init_pc }
       ; (_is, mis) <- genIS total_is lookahead (init_as, replicate total_is Nothing)

       -- We are going to now vary the initial state and go again hoping to fill in some
       -- more Nothings (Noops) with instructions generated from a variational execution
       ; init_as_varied <- vary init_as
       ; let is_varied  = aimem init_as_varied
             mis_varied = zipWith prefer_varied is_varied mis
       ; (is, mis_final) <- genIS total_is lookahead (init_as_varied, mis_varied)
       ; let final_is = zipWith prefer_fst mis mis_final

       ; return $ init_as { aimem = final_is, apc = init_pc } }

  where prefer_varied i Nothing  = Nothing
        prefer_varied i (Just _) = Just i
        prefer_fst (Just i) _    = i
        prefer_fst Nothing s     = fromMaybe Noop s

genIS :: Flaggy DynFlags 
      => Int   -- Total instructions to generate
      -> Int   -- lookahead (in steps)
      -> (AS, [Maybe Instr]) -- Initial AS and instruction stream, in accordance with AS
      -> Gen ([Instr], [Maybe Instr]) -- For convenience both forms
genIS total_is lookahead (as_init, mis_init)
  = go 0 total_is as_init 0 mis_init
  where
    go :: Int -- Since we are executing, use this to avoid looping
       -> Int -- Remaining instructions to generate
       -> AS  -- Current state
       -> Int -- HALT weight (increasing!)
       -> [Maybe Instr] -- Current instruction stream
                        -- same as in AS with Nothing <- Noop
       -> Gen ([Instr], [Maybe Instr])
    go c r as halt_weight istream
      | r <= 0 || c >= 3 * total_is || not (isWF as)
      = -- trace ("Generation stopped!\n" ++
        --           "isWF = " ++ show (wf as) ++ "\n" ++
        --           "PC   = " ++ show iptr ++ "\n" ++  
        --           (if iptr `isIndex` istream then "Instr = " ++ show (istream !! iptr) ++ "\n" else "") ++ 
        --           "Stack = " ++ show (take 1 (astk as)) ++ "\n" ++ 
        --           "Amemsize = " ++ show (length (amem as)) ++ "\n" ++ 
        --           "abAdjustAddr = " ++ (case (take 1 (astk as)) of 
        --                                  [AData v] -> show (absAdjustAddr (value $ v)) 
        --                                  _ -> "")) $
        return (aimem as, istream)
      | Just instr <- istream !! iptr
        -- Already generated instruction, just execute
      = -- trace ("Executing: " ++ show instr ++ " (" ++ show iptr ++ ") " ++ show (take 5 (astk as))) $ 
        go (c+1) r (astepFn as) halt_weight istream 
      | otherwise
        -- Must generate instructions  
      = do { let (ispref,ispost) = splitAt iptr istream
           ; let slack           = length (takeWhile isNothing ispost) -- How much space?
           ; (aimem', istream', overwritten)
               <- tryGenerate total_is slack as (ispref, ispost) lookahead halt_weight
           ; go (c+1) (r - overwritten) (as {aimem = aimem'}) (halt_weight+1) istream' }
    
      where iptr = value (apc as)


tryGenerate :: Flaggy DynFlags
            => Int -- total instructions allowed
            -> Int -- slack 
            -> AS  -- current AS 
            -> ([Maybe Instr],[Maybe Instr]) -- current stream 
            -> Int -- lookahead (step count)
            -> Int -- HALT weight
            -> Gen ([Instr], [Maybe Instr], Int) 
            -- Returns final aimem, instruction stream, 
            -- and how many instructions were generated
tryGenerate total_is slack as (ispref, ispost) n halt_weight = go n
  where
    go n = do { is <- ainstrs total_is slack as halt_weight
              ; let (overwritten,ispost') = replaceNoops is ispost
                    istream'              = ispref ++ ispost'
                    aimem'                = map (fromMaybe Noop) istream'
              ; if n == 0 then
                     -- (trace ("Generated " ++ show is ++ " ( in stack = " ++ show (take 2 (astk as))) $  
                     return (aimem', istream', overwritten)
                else case trystep n (as { aimem = aimem' }) of
                  Nothing -> go (n-1)
                  Just {} ->
                      return (aimem', istream', overwritten) 
              }
        where trystep 0 x = Just x
              trystep n x | isWF x = trystep (n-1) (astepFn x)
                          | IF msg <- wf x, "halt" `isInfixOf` msg = Just x -- DV!
                          | otherwise = Nothing

replaceNoops :: [Instr]            
             -> [Maybe Instr]      
             -> (Int, [Maybe Instr])
-- Replaces as many Nothings as possible from the start of
-- second input (mis) with instructions from the first (is). 
-- Pre: first list must be shorter than position where second becomes Just.
replaceNoops is mis
  = let fis = map Just is ++ replicate (length mis - length is) Nothing
        overwritten = min (length is) (length (takeWhile isNothing mis))
    in (overwritten, zipWith merge fis mis)
  where merge Nothing i = i
        merge (Just i) (Nothing) = Just i
        merge (Just i) (Just j)  = Just j




{----------------------------------------------------------------------
Note [GenByExecBothBranches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is a slight variation of GenByExec but it does not leave as many
holes behind as GenByExec for conditional branches. It moves to the
immediately subsequent PC, storing the target address and state. If we
later have to generate an instruction in a position that is the target
of a branch we have an option of using either the stored state or the
current.

Holes will still be left behind for unconditional jumps and it may not
be the case that the target of a conditional jump is actually ever
reached.
-----------------------------------------------------------------------}
type JmpTbl = [(Int,AS)] -- (pc,stored_state) pairs
genByExecBothBranches :: Flaggy DynFlags => Gen AS
genByExecBothBranches
  = do { total_is <- choose (gen_instrs_range getFlags)
       ; (init_mem,init_stk,init_pc) <- initAS total_is
       ; let init_as = AS { astk  = init_stk
                          , amem  = init_mem
                          , aimem = replicate total_is Noop
                          , apc   = init_pc }
       ; is <- genIS total_is init_as
       ; return $ init_as { aimem = is, apc = init_pc } }
  where
    genIS total_is as_init
      = go 0 total_is [] as_init $ replicate total_is Nothing
      where
        go :: Int     -- Since we are executing, use this to avoid looping
           -> Int     -- Remaining instructions to generate
           -> JmpTbl  -- Jump table, storing targets of branches
           -> AS      -- Current state
           -> [Maybe Instr] -- Current instruction stream
                            -- same as in AS with Nothing <- Noop
           -> Gen [Instr]
        go c r jmpTbl as istream
          | r <= 0 || c >= 3 * total_is || not (isWF as)
          = return $ map (fromMaybe Noop) istream
          | Just instr <- istream !! iptr
          -- Already generated instruction, just execute
          = go (c+1) r jmpTbl (astepFn as) istream
          | otherwise
          -- Must generate instructions: but which state should we prefer? 
          -- Option (i):  the stored if any 
          -- Option (ii): the current
          -- Not very clear which is the best option ... 
          = do { let as' | Just as_stored <- lookup (value $ apc as) jmpTbl
                         = as_stored { aimem = aimem as }
                         | otherwise = as
               ; let (ispref,ispost) = splitAt iptr istream
               ; let slack           = length (takeWhile isNothing ispost) -- How much space?
               ; is <- ainstrs total_is slack as' 0
               ; let (overwritten,ispost') = replaceNoops is ispost
                     istream'              = ispref ++ ispost'
                     aimem'                = map (fromMaybe Noop) istream'
                 -- And try again. Same pc, only update the instruction stream.
               ; go (c+1) (r - overwritten) jmpTbl (as' {aimem = aimem'}) istream' }
          where iptr = value (apc as)


{--------------------------------------------------------------------------
Note [GenByExecAllBranchesFwd]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This only fills-in the instruction stream in-order (hence the Fwd)
leaving no holes behind. The most essential difference to GenByFwdExec
below is that it does not use any ``taint'' information to avoid
storing subsequent jump addresses. Instead, if the current state is
well-formed, it may get stored.

Experimentally it seems to behave worse regarding jump tainting, although
it achieves a much higher-rate of "normal executions" (WF and pc out of
range). DV: This is a bit of a mystery yet to me, to be investigated ...
---------------------------------------------------------------------------}
genByExecAllBranchesFwd :: Flaggy DynFlags => Gen AS
genByExecAllBranchesFwd
  = do { total_is <- choose (gen_instrs_range getFlags)
       ; (init_mem,init_stk,init_pc) <- initAS total_is
       ; let init_as = AS { astk  = init_stk
                          , amem  = init_mem
                          , aimem = replicate total_is Noop
                          , apc   = init_pc }
       ; is <- genIS total_is init_as
       ; return (-- trace ("Generated is = " ++ show is) $
                 init_as { aimem = is, apc = init_pc }) }
  where
    genIS total_is as_init
      = go 0 total_is [] as_init $ replicate total_is Nothing
      where
        go :: Int     -- Since we are executing, use this to avoid looping
           -> Int     -- Remaining instructions to generate
           -> JmpTbl  -- Jump table, storing targets of branches
           -> AS      -- Current state
           -> [Maybe Instr] -- Current instruction stream
                            -- same as in AS with Nothing <- Noop
           -> Gen [Instr]
        go _c r jmpTbl as istream
          | r <= 0 || not (iptr `isIndex` istream)
          = return $ map (fromMaybe Noop) istream
          | Just instr <- istream !! iptr
          -- Already generated instruction, just execute
          = -- trace ("generating instruction (iptr = " ++ show iptr ++ "):" ++ show instr) $
            if isWF as then 
              let jumpStk n = case astk as !! n of
                                AData (Labeled l d) -> Just d
                                _                   -> Nothing
                  jump = case instr of
                    Jump     -> jumpStk 0
                    Call _ _ -> jumpStk 0
                    Return _ -> 
                      case find (not . isAData) (astk as) of 
                        Just (ARet (Labeled _ (pc,_))) -> Just pc
                        _                              -> Nothing
                    _        -> Nothing
                  as_next = astepFn as 
                  jmpTbl' = case jump of
                              Just pc | pc > value (apc as) -> (pc,as_next):jmpTbl
                              _ -> jmpTbl
               in go (_c+1) r jmpTbl' (as_next { apc = apc as + 1 }) istream
            else
              -- Starting state is not well-formed for this instruction
              -- For instance because instruction generation does not take IF into account
              -- Not much we can do:
              --    (i) either return completely with no-ops
              --    (ii) keep going in case the state was tainted, optimistically.
              -- We choose (ii)
                go (_c+1) r jmpTbl (as { apc = apc as + 1 }) istream
          | otherwise
          -- Must generate instructions: but which state should we prefer? 
          -- Option (i):  the stored if any 
          -- Option (ii): the current
          -- Not very clear which is the best option ... 
          = do { let as' | Just as_stored <- lookup (value $ apc as) jmpTbl
                         = -- trace "Found stored state" $
                           as_stored { aimem = aimem as }
                         | otherwise = as
               ; let (ispref,ispost) = splitAt iptr istream
               ; let slack           = length (takeWhile isNothing ispost) -- How much space?
               ; is <- ainstrs total_is slack as' 0
               ; let (overwritten,ispost') = replaceNoops is ispost
                     istream'              = ispref ++ ispost'
                     aimem'                = map (fromMaybe Noop) istream'
                 -- And try again. Same pc, only update the instruction stream.
               ; go (_c+1) (r - overwritten) jmpTbl (as' {aimem = aimem'}) istream' }
          where iptr = value (apc as)


genByExecAllBranchesFwd2 :: Flaggy DynFlags => Gen AS
genByExecAllBranchesFwd2
  = do { total_is <- choose (gen_instrs_range getFlags)
       ; (init_mem,init_stk,init_pc) <- initAS total_is
       ; let init_as = AS { astk  = init_stk
                          , amem  = init_mem
                          , aimem = replicate total_is Noop
                          , apc   = init_pc }
       ; is <- genIS total_is init_as
       ; return (-- trace ("Generated is = " ++ show is) $
                 init_as { aimem = is, apc = init_pc }) }
  where
    genIS total_is as_init
      = go 0 False total_is [] as_init $ replicate total_is Nothing
      where
        go :: Int     -- Since we are executing, use this to avoid looping
           -> Bool 
           -> Int     -- Remaining instructions to generate
           -> JmpTbl  -- Jump table, storing targets of branches
           -> AS      -- Current state
           -> [Maybe Instr] -- Current instruction stream
                            -- same as in AS with Nothing <- Noop
           -> Gen [Instr]
        go _c tainted r jmpTbl as istream
          | r <= 0 || not (iptr `isIndex` istream)
          = return $ map (fromMaybe Noop) istream
          | Just instr <- istream !! iptr
          -- Already generated instruction, just execute
          = -- trace ("generating instruction (iptr = " ++ show iptr ++ "):" ++ show instr) $
            let tainted' = tainted || isJust (lookup (value $ apc as) jmpTbl)
            in
            if isWF as then 
              let jumpStk n = case astk as !! n of
                                AData (Labeled l d) -> Just d
                                _                   -> Nothing
                  jump = case instr of
                    Jump     -> jumpStk 0
                    Call _ _ -> jumpStk 0
                    Return _ -> 
                      case find (not . isAData) (astk as) of 
                        Just (ARet (Labeled _ (pc,_))) -> Just pc
                        _                              -> Nothing
                    _        -> Nothing
                  as_next = astepFn as 
                  jmpTbl' = case jump of
                              Just pc | pc > value (apc as)
                                      , not tainted -- Add to fwd table if not tainted
                                      -> (pc,as_next):jmpTbl
                              _ -> jmpTbl
               in go (_c+1) tainted' r jmpTbl' (as_next { apc = apc as + 1 }) istream
            else
              -- Starting state is not well-formed for this instruction
              -- For instance because instruction generation does not take IF into account
              -- Not much we can do:
              --    (i) either return completely with no-ops
              --    (ii) keep going in case the state was tainted, optimistically.
              -- We choose (ii)
                go (_c+1) tainted' r jmpTbl (as { apc = apc as + 1 }) istream
          | otherwise
          -- Must generate instructions: but which state should we prefer? 
          -- Option (i):  the stored if any 
          -- Option (ii): the current
          -- Not very clear which is the best option ... 
          = do { let as' | Just as_stored <- lookup (value $ apc as) jmpTbl
                         = -- trace "Found stored state" $
                           as_stored { aimem = aimem as }
                         | otherwise = as
               ; let (ispref,ispost) = splitAt iptr istream
               ; let slack           = length (takeWhile isNothing ispost) -- How much space?
               ; is <- ainstrs total_is slack as' 0
               ; let (overwritten,ispost') = replaceNoops is ispost
                     istream'              = ispref ++ ispost'
                     aimem'                = map (fromMaybe Noop) istream'
                 -- And try again. Same pc, only update the instruction stream.
               ; let tainted' = tainted || isJust (lookup (value $ apc as) jmpTbl)
               ; go (_c+1) tainted' (r - overwritten) jmpTbl (as' {aimem = aimem'}) istream' }
          where iptr = value (apc as)


data Execute = Execute Int

genByExecAllBranchesFwd3 :: Flaggy DynFlags => Gen AS
genByExecAllBranchesFwd3
  = do { total_is <- choose (gen_instrs_range getFlags)
       ; (init_mem,init_stk,init_pc) <- initAS total_is
       ; let init_as = AS { astk  = init_stk
                          , amem  = init_mem
                          , aimem = replicate total_is Noop
                          , apc   = init_pc }
       ; is <- genIS total_is init_as
       ; return (-- trace ("Generated is = " ++ show is) $
                 init_as { aimem = is, apc = init_pc }) }
  where
    genIS total_is as_init
      = go (Execute 0) False [] as_init
      where
        go :: Execute
           -> Bool 
           -> JmpTbl  -- Jump table, storing targets of branches
           -> AS      -- Current state
           -> Gen [Instr]
        go execute tainted jmpTbl as
          | not (iptr `isIndex` aimem as)
          = return $ aimem as
          | Execute n <- execute
          , n > 0
          , instr <- aimem as !! iptr
          = -- trace "foo" $
            if as_better /= as then go (Execute 0) tainted jmpTbl as
            else
              let as_next
                    | isWF as_better = astepFn as_better
                    | otherwise      = as_better { apc = apc as_better + 1 }
                  this_pc = value $ apc as_better
                  next_pc = value $ apc as_next

                  jmpTbl'
                    | next_pc > this_pc + 1
                    , not tainted = (next_pc, as_next) : jmpTbl
                    | otherwise = jmpTbl
                  tainted' = tainted || next_pc /= this_pc + 1 || not (isWF as_better)
          
              in go (Execute (n-1)) tainted' jmpTbl' $
                 as_next { apc = apc as_better + 1 }

          | otherwise -- Execute 0 <- execute -- Must generate some fresh ones
          = -- trace ("bar" ++ "as = " ++ show as ++ "\n" ++
            --        "as length = " ++ show (length $ aimem as) ++ "\n" ++
            --        "as_better = " ++ show as_better ++ "\n" ++
            --        "as_better length = " ++ show (length $ aimem as_better)) $
            let (ispref,ispost) = splitAt iptr (aimem as_better)
                slack = length $ takeWhile (== Noop) ispost
            in
            do { is_to_use <- ainstrs total_is slack as_better 0
               ; let ispost'  = is_to_use ++ drop (length is_to_use) ispost
                     istream' = ispref ++ ispost'
                     tainted' = tainted && isNothing (lookup (value $ apc as) jmpTbl)
                     
               ; go (Execute (length is_to_use)) tainted' jmpTbl $
                 as_better {aimem = istream'} }
          where iptr = value (apc as)

                as_better
                  -- If we are tainted and there is a stored one, it's more accurate so use it
                  | Just as_stored <- lookup (value $ apc as) jmpTbl
                  , tainted
                  = as_stored { aimem = aimem as, apc = apc as_stored `tainting` apc as }
                  | otherwise = as
{-                
                slack           = length (takeWhile isNothing ispost) -- How much space?
                is_any_stored   = find is_index_stored [1 ..(slack-1)]
                is_index_stored i = tainted &&
                                      isJust (lookup (iptr + 0 + i) jmpTbl)
                real_slack = case is_any_stored of
                                Nothing -> slack
                                Just i  -> i
            in
            do { is_to_use <- ainstrs total_is real_slack as'
                   -- Attempt to create instructions
                   -- If you created more than one, is any subsequent instr address stored
                   -- and we are tainted?

--                ; is_to_use <- ainstr total_is slack as' >>= \x -> return [x]
{-
               ; (i:is) <- if is_any_stored
                           then -- return (head is_attempt:[]) 
                             do { j <- ainstr total_is slack as'
                                ; return (j:[]) } -- Or rather?: (head is_first_attempt)
                           else return is_attempt
-}
              -- ; 
              -- (i:is) <- do { x <- ainstr total_is slack as'
              --                 ; return [x] }
              --              -- do { x <- ainstrs total_is slack as'
              --              --    ; return x }
               ; let (overwritten,ispost') = replace_noops is_to_use 0 ispost
                     istream'              = ispref ++ ispost'
                     aimem'                = map (fromMaybe Noop) istream'
                 -- And try again. Same pc, only update the instruction stream.
                     tainted' = tainted && isNothing (lookup (value $ apc as) jmpTbl)
               ; go (_c+1) tainted' (r - overwritten) jmpTbl (as' {aimem = aimem'}) istream' }
          where replace_noops (i:is) n (Nothing:rest) = replace_noops is (n+1) (Just i:rest)
                replace_noops _ n p = (n,p)
                iptr = absAdjustIAddr $ value (apc as)

-}



genByFwdExec :: Flaggy DynFlags => Gen AS
genByFwdExec
  = do { total_is <- choose (gen_instrs_range getFlags) -- How many instructions to make
       ; (init_mem,init_stk,init_pc) <- initAS total_is
       ; let init_as = AS { astk  = init_stk
                          , amem  = init_mem
                          , aimem = []  -- replicate total_is Noop
                          , apc   = init_pc }
       ; is <- genIs total_is init_as
       ; return $ init_as { aimem = is } }
  where
      -- This is the generation code we've been using from the start
      genIs n as = genIs' False [] n n as [] 0

      -- The tbl maps pc's to states stored when encountering forward jumps.
      -- The tainted flag records if the current state is accurate. Important:
      -- ignore forward jumps in tainted states.
      genIs' _ _ _ 0 as _ halting_weight = return $ aimem as
      genIs' tainted tbl n m as0 is halting_weight = do
        let pc = 0 + length (aimem as0)
            stored = lookup pc tbl
            fromFwdJump = isJust stored && tainted
            as | not tainted = as0
               | otherwise   = (fromMaybe as0 stored) { aimem = aimem as0 }
        (i, is) <- case is of
          i:is | not fromFwdJump
                 -> return (i,is) 
--                    trace ("Generated: (pc = " ++ show (pc - 0) ++ ")" ++ 
--                              show i) $ (i, is)
          _ -> do
            i:is <- ainstrs n (n - length (aimem as)) as halting_weight -- n - |aimem| is the `slack` we have
            return (i,is)
                 -- trace ("Generated: (pc = " ++ show (pc - 0) ++ ")" ++ 
                 --             show i) $ (i, is)
        let as' = as { aimem = aimem as0 ++ [i],
                       -- Hack: pretend that the next instr is the one we generate
                       -- even if it isn't
                       apc = apc as `tainting` Labeled L pc }
        -- () <- unless (wf as') $ error $ "bad as: " ++ show as' ++ "\nis = " ++ show is
        as'' <- if isWF as' then step as' else return as'
        
        let jumpStk n = case astk as !! n of
                          AData (Labeled l d) -> Just d
                          _                   -> Nothing
            jump = case i of
                     Jump     -> jumpStk 0
                     Call _ _ -> jumpStk 0
                     Return _ -> 
                       case find (not . isAData) (astk as) of 
                         Just (ARet (Labeled _ (pc,_))) -> Just pc
                         _                              -> Nothing
                     _        -> Nothing

            tainted' = tainted && isNothing stored

            tbl' | tainted' = tbl
                 | otherwise = case jump of
                    Just p | p > pc -> (p, as'') : tbl
                    _               -> tbl

            tainted'' = maybe False (/= pc + 1) jump || tainted'
        genIs' tainted'' tbl' n (m - 1) as'' is (halting_weight+1)
  

{------------------------ Random abstract instructions ------------------------}

-- Generate a random instruction in the given abstract machine, assuming we
-- have an instruction memory of the given size (where this DOESN'T count the
-- tmmRoutine).  This means that when we generate a random instruction, we
-- don't start at 0, but just after the length of the tmmRoutine.
ainstr :: Flaggy DynFlags
       => Int
       -> Int
       -> AS
       -> Int -- HALT weight
       -> Gen Instr
ainstr imem_size slack as@(AS{amem=mem, astk=stk}) halt_weight =
  frequency $
    [ (1,  pure Noop) ] ++
    [ (5 + halt_extra_weight,  pure Halt) ] ++
    [ (40, pure Jump) | nstk >= 1
                      , let vt' = vtop
                      , vt' >= 0 && vt' < imem_size
                      , jumpy ] ++
    [ (40, liftM2 Call (choose (0, (nstk-1) `min` maxArgs)) arbitrary)
                         | nstk >= 1
                         , let vt' = vtop
                         , vt' >= 0 && vt' < imem_size
                         , cally ] ++
    -- If we're generating the last instruction, don't bother
    -- generating anything except Jump, JumpNZ, and Call.
    if slack <= 1 then [] else
--    if length (aimem as) + 1  then [] else
    [ (40, pure Add) | nstk >= 2 ] ++
    [ (300, Push <$> labeled (smartInt imem_size $ length mem)) ] ++
    [ (40, pure Pop) |
      if IfcBugPopPopsReturns `elem` ifc_semantics_singleton getFlags 
        then not (null stk)
        else nstk >= 1 ] ++
    [ (60, pure Store) | nstk >= 2
                       , vtop `isIndex` mem
                       , extra_label_checks
                       ] ++
    [ (60, pure Load) | nstk >= 1
                      , vtop `isIndex` mem ] ++
    [ (40, liftM Return arbitrary) | Just r <- [ astkReturns <$>
                                                 find (not . isAData) stk]
                                   , nstk >= if r then 1 else 0 
                                   , cally ]
  where

    halt_extra_weight
      |prop `elem` [PropEENI, PropJustProfile, PropJustProfileVariation]
      = halt_weight * 10
      | otherwise = 0
    
    extra_label_checks =
      {-# SCC "extra_label_checks" #-}
      case wfChecks (instrChecks Store as) of
        WF -> True
        _ -> False
    
    nstk    = length $ takeWhile isAData stk
    vtop    = astkValue $ head stk
    vsnd    = astkValue $ stk !! 1
--    lint    = labeled int
    maxArgs = conf_max_call_args getFlags
    cally   = callsAllowed (gen_instrs getFlags)
    jumpy   = jumpAllowed  (gen_instrs getFlags)

    prop = prop_test getFlags 

-- Like the above, but generate multiple instructions; mostly, this is just a
-- clever way to do interesting things with Jump, Load, and Store by placing a
-- Push in front of them.
ainstrs :: Flaggy DynFlags
        => Int  -- Total instructions we have to generate
        -> Int  -- Slack
        -> AS
        -> Int  -- HALT weight 
        -> Gen [Instr]
ainstrs imem_size slack as@(AS{amem = mem, astk = stk}) halt_weight =
  -- trace ("calling ainstrs, imem_size = " ++ show imem_size ++
  --        "\nslack = " ++ show slack ++
  --        "\namem  = " ++ show mem ++ 
  --        "\nastk  = " ++ show stk) $
  frequency $
    [ (10, (:[]) <$> ainstr imem_size slack as halt_weight) ] ++
    if slack <= 2 then [] else
    -- if length (aimem as) + 2 >= imem then [] else
    [ (1, pushAndDo Load  <$> labeled maddr) | not (null mem) ] ++

    [ (3, pushAndStoreChecked as) | not (null mem), not (null stk) ] ++

-- Used to be:
--    [ (2, pushAndDo Store <$> labeled maddr) | not (null mem)
--                                               , length stk >= 1 ] ++
    
    [ (1, pushAndDo Jump  <$> labeled iaddr) | jumpy ] ++
    [ (1, do { c_args <- choose(0, maxArgs `min` length (takeWhile isAData stk))
             ; c_ret  <- arbitrary
             ; pushAndDo (Call c_args c_ret) <$> labeled iaddr }) | cally ]
  where
    iaddr = if smart_ints getFlags
            then imem_size `upfrom` 0
            else int
    maddr = frequency
            [ (1, length mem `upfrom` 0)
-- DV: used to be like this 
-- but this is a bug: min associates to the 
-- left so if the length of the memory is e.g 1
-- we get out of address violations ...  
--          , (9, ((length mem `min` 0+2) `upfrom` 0)) 
            , (9, (length mem `min` 3) `upfrom` 0)
            -- Instead I will just reuse very often the first three memory locations
            ]
            -- ensure we reuse locations often
    pushAndStoreChecked as =
      {-# SCC "pushAndStoreChecked" #-}
      let is_wf WF = True
          is_wf _  = False
          all_addresses
            = take 3 [0 .. (0 + length (amem as) - 1)] 
          goodA a = wfChecks $ instrChecks Store (astepInstr as (Push a))
          good_maddrs w l =
            let vls = filter (is_wf . goodA . Labeled l) all_addresses
            in if vls == empty then []
               else [(w, elements (map (Labeled l) vls))]
          push_maddr = frequency $
                       good_maddrs 200 L ++ good_maddrs 5 H ++ [(1, labeled maddr)]
      in pushAndDo Store <$> push_maddr

    pushAndDo i a = [Push a, i]
    maxArgs = conf_max_call_args getFlags
    cally  = callsAllowed (gen_instrs getFlags)
    jumpy  = jumpAllowed  (gen_instrs getFlags)
    genAll = jumpAllowed  (gen_instrs getFlags)

-- instance Flaggy DynFlags => Arbitrary AS where
--   arbitrary = genAS
  
--   shrink as@AS{..}= 
--     shrinkByExecuting as
--     ++
--     -- Don't shrink the PC
--     [ AS{amem=amem', aimem=aimem', astk=astk', apc=apc}
--     | (amem',aimem',astk') <- shrink (amem,aimem,astk) ]

-- shrinkByExecuting :: Flaggy DynFlags => AS -> [AS]
-- shrinkByExecuting as@AS{..} 
--   | not (isWF as) || not (isWF as') = []
--   | otherwise = [(astepFn as'){aimem=update noops Noop aimem}]
--   where noops = length (takeWhile (== Noop) aimem)
--         as'   = as{apc = apc + fromIntegral noops}


{----------------------------------------------------------------------
Note [GenTinyArbitrary]
~~~~~~~~~~~~~~~~~~
Specialized generation for single-step noninterference.
-----------------------------------------------------------------------}

genTinySSNI :: Flaggy DynFlags => Gen AS
genTinySSNI
  | StartArbitrary <- starting_as getFlags
  = do { amem  <- mapM (const arbitrary) [0..2]
       ; astkSize <- frequency [(x,return x) | x <- [1..15] ]
       ; astk  <- mapM (\_ -> oneof [arbitrary, AData <$> labeled genDataPtr])
                  [0..(astkSize-1)]
         -- making sure there is at least one return address on the stack
       ; extraRet <- frequency [(10, liftM (\x->[x]) (ARet <$> arbitrary)),
                                (1, return [])]
       ; apcl  <- elements [L,H]
       ; let as = AS { amem  = amem
                     , aimem = [undefined] -- to be filled in later
                     , astk  = astk ++ extraRet
                     , apc   = Labeled apcl 0 }
       ; instr1 <- ainstr' as
         -- often generate related instructions
       ; instr2 <- frequency [(10, varyInstr instr1), (1, ainstr' as)]
       ; return $ as { aimem = [instr1, instr2] } }
  | otherwise
  = error "Only use this generator for starting in an arbitrary state!"
  where genDataPtr = if smart_ints getFlags
                     then choose (0,2+0)
                     else int
        -- adapted from TMUAbstractObs since I couldn't import it (circular)
        varyInstr (Push a)   = Push <$> varyAtom a
        varyInstr (Return _) = Return <$> arbitrary
        varyInstr i          = pure i
        varyInt i = oneof [genDataPtr, int]
        varyAtom (Labeled H i) = Labeled H <$> varyInt i
        varyAtom a             = pure a



-- A variant of ainstr tweaked for use in genTinySSNI.
-- This uses an almost uniform distribution for instructions.
-- It also doesn't enforce that jump/call targets are correct;
-- on the contrary, they often are not (size of aimem = 1)
ainstr' :: Flaggy DynFlags
       => AS
       -> Gen Instr
ainstr' as@(AS{amem=mem, astk=stk}) =
  frequency $
    [ (1, pure Noop) ] ++
    [ (1, pure Halt) ] ++
    [ (10, pure Add) | nstk >= 2 ] ++
    [ (10, Push <$> lint) ] ++
    [ (10, pure Pop) | if IfcBugPopPopsReturns `elem` ifc_semantics_singleton getFlags 
                         then not (null stk)
                         else nstk >= 1 ] ++
    [ (20, pure Store) | nstk >= 2
                       , vtop `isIndex` mem ] ++
    [ (20, pure Load) | nstk >= 1
                      , vtop `isIndex` mem ] ++
    [ (10, liftM2 Call (choose (0, (nstk-1) `min` maxArgs)) arbitrary)
                         | nstk >= 1
                         , cally ] ++
    [ (20, liftM Return arbitrary) | Just r <- [ astkReturns <$>
                                                find (not . isAData) stk]
                                   , nstk >= if r then 1 else 0 
                                   , cally ] ++
    [ (10, pure Jump) | nstk >= 1
                      , jumpy ]
  where
    nstk    = length $ takeWhile isAData stk
    vtop    = astkValue $ head stk
    vsnd    = astkValue $ stk !! 1
    lint    = labeled int
    maxArgs = conf_max_call_args getFlags
    cally   = callsAllowed (gen_instrs getFlags)
    jumpy   = jumpAllowed (gen_instrs getFlags)
