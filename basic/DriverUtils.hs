{-# LANGUAGE FlexibleContexts, UndecidableInstances, RecordWildCards,
    TupleSections, ScopedTypeVariables, NamedFieldPuns #-}

module DriverUtils where

import Test.QuickCheck
import qualified Test.QuickCheck.Property as QCProp
import Test.QuickCheck.State 
import Test.QuickCheck.Random
import Control.Monad
import Control.Applicative
import Control.Arrow()
import Data.Function
import Data.List (sortBy, groupBy, nub, isInfixOf, intercalate)

import Text.Printf
import System.CPUTime
import Data.Maybe
import GHC.Float

import Data.IORef

import Util
import GenericMachine
import Trace
import Aggregate
import Average
import LaTeX

import Labels
import Flags
import Observable
import Instr

import Machine
import Generation () -- Import just Arbitrary
import ObservableInst ()

import Timeout (timeout')




{----------------------------- Properties to test -----------------------------}

{----- Abstract machine-----}

-- This tests that at every step of the observed executions, the
-- abstract machines are indistinguishable from each other with
-- respect to (~~~).  
-- (NB: We use 'Fixed' to prevent QuickCheck from shrinking things.)
-- TODO Consider using Smart (Shrink2 _), but NOT the other way.  
-- TODO We changed the aimem to be shrunk with the (new) ShrinkTail, but we
-- might? not want that for Shrink2.  
-- prop_secure :: Smart (Variation AS) -> Property 
-- We have some evidence that if an execution fails, it fails before 
-- ~40 or not at all
gen_prop_noninterference :: (Flaggy DynFlags, Testable prop)
                         => ([AS] -> a) -> (a -> a -> prop)
                         -> Smart (Shrink2 (Variation AS)) -> Property
gen_prop_noninterference observe compare 
                  (Smart _ (Shrink2 (Variation as as'))) =
  let step_prop (n,n') 
        = forAll (liftM2 (,) (traceN as n) (traceN as' n')) $ 
          \(Trace ass, Trace ass') ->
              let obAss  = observe ass
                  obAss' = observe ass'
              in 
                whenFail (when (show_counterexamples getFlags) $
                            (if latex_output getFlags then printLaTeX else printPlain)
                              as as' ass ass') $
        -- ( case stat_collect getFlags of
        --      StatWF          -> collect (wf (last ass))
        --      StatExecLengths -> collect (10 * (length ass `div` 10))
        --      StatPCCoverage  -> collect (10 * ((round $ (iptr_coverage as ass)) `div` 10))
        --      StatNoopsExecuted -> collect (noops_executed ass)
        --      StatJumpOnZTaken -> aggregate (jumpOnZtaken ass)
        -- )
        --   -- collect (min (length ass) (length ass'))
        -- $ 
              compare obAss obAss'
   in
   -- step_prop (steps,steps)
   shrinking shrink (steps,steps) step_prop

-- This does not seem to have an effect. Why???
--   shrinking shrinkNothing (steps,steps) step_prop -- step_prop (steps,steps) 
 
 where steps = step_no getFlags
        
       iptr_coverage as ass =
           -- How many distinct PCs are execute from the generated
           -- instruction stream
           fromIntegral (100 * length (nub (map (value . apc) ass))) /
                                       fromIntegral (length (aimem as))
       noops_executed =
           foldr (\x s -> if isWF x then
                            case aimem x !! value (apc x) of
                              Noop -> s + 1
                              _    -> s
                          else s) 0

printPlain :: Flaggy DynFlags => AS -> AS -> [AS] -> [AS] -> IO ()
printPlain as as' ass ass' = do
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

  where printLockstep l = -- For printPlain
           forM_ l $ \ (as,as') -> 
             do putStr (show (apc as))
                putStr "\tM="
                putStr (show (zipWith Variation (amem as) (amem as')))
                putStr "\tS="
                putStr (show (zipWith Variation (astk as) (astk as')))
                putStr "\tnext="
                let len = value $ apc as
                if isIndex len (aimem as) &&
                   isIndex len (aimem as') then print (Variation (aimem as !! len)
                                                                 (aimem as' !! len))
                                           else putStrLn "<eof>"

        printTrace l = -- For printPlain
           forM_ l $ \ as -> 
             do putStr (show (apc as))
                putStr "\tM="
                putStr (show (amem as))
                putStr "\tS="
                putStr (show (astk as))
                putStr "\tnext="
                let len = value $ apc as
                if isIndex len (aimem as) then print (aimem as !! len)
                                          else putStrLn "<eof>"

printLaTeX :: Flaggy DynFlags => AS -> AS -> [AS] -> [AS] -> IO ()
printLaTeX as as' ass ass' = do
  putStrLn "\\begin{tabular}{mlmlmlml}"
  putStrLn $  "  \\multicolumn{4}{mc}{"
           ++ "i = \\left[\\begin{array}{l}"
           ++ ( intercalate ", \\\\ "
              . map (intercalate ", ") . chunks 5
              . map toLaTeX $ (zipWith Variation `on` aimem) as as' )
           ++ "\\end{array}\\right]} \\\\"

  putStrLn "  \\addlinespace\\toprule"
  putStrLn "  \\pc & m & s & i(\\pc) \\\\"
  putStrLn "  \\midrule"

  -- The reason we don't use `span` here is that if we do, the
  -- continuations of machines 1 and 2 only last as long as the shortest
  -- machine (thanks to zip).  Thus, we have to use
  -- `drop (length same) ...` later.
  let same = takeWhile (uncurry ((==) `on` apc)) $ zip ass ass'

  let putTableRow :: (LaTeX m, LaTeX s) => Atom -> m -> s -> String -> IO ()
      putTableRow pc mem stk instr = do
        putStr   "  \\counterexampleline "
        putStr   $ "\\CEPC{" ++ toLaTeX pc ++ "}"
        putStr   " & "
        putLaTeX mem
        putStr   " & "
        putLaTeX stk
        putStr   " & "
        putStr   instr
        putStrLn " \\\\"

  forM_ same $ \(as,as') ->
    putTableRow (apc as)
                ((zipWith Variation `on` amem) as as')
                ((zipWith Variation `on` astk) as as')
                $ let pc     = value $ apc as
                      instr  = aimem as  !! pc
                      instr' = aimem as' !! pc
                  in case (isIndex pc $ aimem as, isIndex pc $ aimem as') of
                       (True,  True)  -> toLaTeX $ Variation instr instr'
                       (True,  False) -> "\\variation{" ++ toLaTeX instr
                                      ++ "}{\\mathord{-}}"
                       (False, True)  -> "\\variation{\\mathord{-}}{"
                                      ++ toLaTeX instr ++ "}"
                       (False, False) -> "\\mathord{-}"

  let printMachine name mach = unless (null mach) $ do
        putStrLn "  \\midrule"
        putStrLn $  "  \\multicolumn{4}{l}{Machine "
                 ++ name ++ " continues\\ldots} \\\\"
        forM_ mach $ \as ->
          putTableRow (apc as) (amem as) (astk as) $
                      let pc = value $ apc as
                      in if isIndex pc $ aimem as
                           then toLaTeX $ aimem as !! pc
                           else "\\mathord{-}"
  printMachine "1" $ drop (length same) ass
  printMachine "2" $ drop (length same) ass'

  putStrLn "\\end{tabular}"

gen_prop_noninterference_observable_lists :: (Flaggy DynFlags, Observable a)
                                          => ([AS] -> [a])
                                          -> Smart (Shrink2 (Variation AS)) -> Property
gen_prop_noninterference_observable_lists observe =
  gen_prop_noninterference observe (\a b -> and (zipWith (~~~) a b))

-- DD: For both non-interference and low-lockstep, the pc is observed.
--     How does it combine with the flag if_labels_observable set to
--     false?
prop_semantic_noninterference :: Flaggy DynFlags
                              => Smart (Shrink2 (Variation AS)) -> Property
prop_semantic_noninterference =
  gen_prop_noninterference_observable_lists observe 
  where observe = map head . groupBy (~~~) . map amem

prop_low_lockstep :: Flaggy DynFlags
                  => Smart (Shrink2 (Variation AS)) -> Property
prop_low_lockstep =
  gen_prop_noninterference (splitLast . filter ((== L) . lab . apc)) (~=~)
  where
    splitLast []                    = ([],   Nothing)
    splitLast ass | isHalted lastAS = (ass', Just lastAS)
                  | otherwise       = (ass,  Nothing)
      where (ass',lastAS) =
              let get [as]     = ([],as)
                  get (as:ass) = let (ass',lastAS) = get ass
                                 in (as:ass',lastAS)
                  get []       =
                    error "prop_low_lockstep: There must be at least one AS!"
              in get ass
    
    isHalted AS{..} = let iptr = value apc
                      in iptr `isIndex` aimem && aimem !! iptr == Halt
    
    ([],Just as1) ~=~ ([],Just as2) = as1 ~~~ as2
    ([],Just _)   ~=~ (_:_,_)       = False
    (_:_,_)       ~=~ ([],Just _)   = False
    ([],Nothing)  ~=~ _             = True
    _             ~=~ ([],Nothing)  = True
    (as1:ass1,l1) ~=~ (as2:ass2,l2) = as1 ~~~ as2 && (ass1,l1) ~=~ (ass2,l2)

prop_end_to_end_aux :: Flaggy DynFlags
                   => Bool -> Smart (Shrink2 (Variation AS)) -> Property
prop_end_to_end_aux break =
   gen_prop_noninterference observe compare 
   where observe ass | is_halting last_as = (ass, Just last_as)
                     | otherwise          = (ass, Nothing)
           where last_as = last ass
                 is_halting as@AS{..} 
                   | let iptr = value apc
                   , iptr `isIndex` aimem
                   , Halt <- aimem !! iptr
                   = break || (lab apc == L)
                   | otherwise
                   = False
         compare (ass1,m1) (ass2,m2)
           = -- collect (any ((== H) . lab . apc) ass1,
             --          any ((== H) . lab . apc) ass2) $
             -- collect (wf (last ass1), wf (last ass2)) $
             compare' m1 m2
         compare' (Just as1) (Just as2)
           = collect "Both halt in L" $
             as1 ~~~ as2
         compare' _ _
           = False ==> True
-- Uncomment lines above and below to collect info to debug discards
--         compare' _ _ =  property True
            -- collect "At least one doesn't halt in L" True

prop_end_to_end :: Flaggy DynFlags
                   => Smart (Shrink2 (Variation AS)) -> Property
prop_end_to_end = prop_end_to_end_aux False

prop_end_to_end_broken :: Flaggy DynFlags
                   => Smart (Shrink2 (Variation AS)) -> Property
prop_end_to_end_broken = prop_end_to_end_aux True



prop_single_step :: Flaggy DynFlags => Smart (Shrink2 (Variation AS)) -> Property
prop_single_step (Smart _ (Shrink2 (Variation as1 as2))) =
  forAll (liftA2 (,) (step' as1) (step' as2)) $ \(mas1',mas2') ->
--   collect (length $ filter (not . isAData) $ astk as1) $
--   collect (head $ words $ show $ head (aimem as1)) $
    let collect s = id in
    whenFail (when (show_counterexamples getFlags) $
              (if latex_output getFlags then printLaTeX else printPlain)
                as1 as2 (as1 : maybeToList mas1')
                        (as2 : maybeToList mas2')) $
    case lab $ apc as1 of
      L -> case (mas1', mas2') of
             (Just as1', Just as2') -> collect "L ->  _" $ property $ as1' ~~~ as2'
             (Nothing,   Just _)    -> collect "prop4" $ property $ not $ successful as1
             (Just _,    Nothing)   -> collect "prop4" $ property $ not $ successful as2
             _                      -> abandon
      H -> case mas1' of
             Just as1' -> case lab $ apc as1' of
                            H -> collect "H ->  H" $ property $ as1 ~~~ as1'
                            L -> case mas2' of
                                   Just as2' | lab (apc as2') == L -> collect "H ->  L" $ property $ as1' ~~~ as2'
                                   Just as2' | lab (apc as2') == H -> abandon
                                   Nothing -> abandon
                                   _       -> error "won't happen"
             Nothing   -> abandon
  where xs ? i | 0 <= i && i < length xs = Just $ xs !! i
               | otherwise               = Nothing
        successful as@AS{..} = (aimem ? value apc) == Just Halt
        step' as | isWF as   = Just <$> step as
                 | otherwise = pure Nothing
        abandon = False ==> True


{----- Main -----}

-- withTimeout :: Flaggy DynFlags => IO a -> IO a
-- withTimeout c = timeout' (toInteger (tmu_timeout getFlags) * 1000000) c


profileTests :: Flaggy DynFlags
             => IO ()
profileTests
  = do { putStrLn "% Profiling"
       ; clear
       ; gen <- newQCGen
       ; r <- quickCheckWithResult stdArgs { maxSuccess = 30000 -- Big enough for profiling
                                           , replay     = Just (gen, 42)
                                           , chatty     = False } $
              \(as :: AS) ->
              forAll (traceN as (step_no getFlags)) $
              \(Trace ass) ->
                collect (show_wf as ass) $
                Average.record (length ass) $
                property True
       ; if latex_output getFlags
         then do { a <- average
                 ; let astr = formatRealFloat FFFixed (Just 2) (a-1)
                 ; putStrLn "\\ifonlylen%"
                 ; putStrLn (astr ++ "%")
                 ; putStrLn "\\else%"
                 ; putStrLn "\\begin{tabular}{rl}"
                 ; putStrLn $ "\\multicolumn{2}{l}{Average number of execution steps: "
                            ++ astr ++ "}\\\\\\midrule"
                 ; let ls = filter (\(l,n)->n>0) $
                            sortBy (\(l,n) (l',n') -> compare n' n) (labels r)
                 ; mapM_ (\(l,n) -> printf "%d\\%% & %s \\\\\n" n (read l :: String)) ls 
                 ; putStrLn "\\end{tabular}"
                 ; putStrLn "\\fi%"
                 }
         else
           print (labels r)

       }
  where
    show_wf as ass = show_wf_nicely (wf (last ass))
    show_wf_nicely WF = "well-formed"
    show_wf_nicely (IF excuse) = excuse


isHaltingAS :: Flaggy DynFlags => AS -> Bool
isHaltingAS as@AS{..}
  | let iptr = value apc
  , iptr `isIndex` aimem
  , Halt <- aimem !! iptr
  , L <- lab apc
  = True
  | otherwise
  = False


profileVariations :: Flaggy DynFlags => IO ()
profileVariations
  = do { putStrLn "% Profiling variations"
       ; clear
       ; gen <- newQCGen
       ; r <- quickCheckWithResult stdArgs { maxSuccess = 30000
                                           , replay = Just (gen, 42)
                                           , chatty = False } prop
       ; if latex_output getFlags
         then
            print_nicely r
         else
            print r 
         -- then do { putStrLn "\\begin{tabular}{rl}"
         --         ; let ls = filter (\(l,n) -> n > 0) $
         --                    sortBy (\(l,n) (l',n') -> compare n' n) (labels r)
         --         ; mapM_ (\(l,n) -> printf "%d\\%% & %s \\\\\n" n (read l :: String)) ls
         --         ; putStrLn "\\end{tabular}"
         --         ; putStrLn "\\fi%" }
         -- else putStrLn $ show r
       }
    where
      print_nicely r
        = do { (a1,a2) <- average2
             ; (b1,b2) <- average2' 
             ; let astr1 = formatRealFloat FFFixed (Just 2) (a1-1)
             ; let astr2 = formatRealFloat FFFixed (Just 2) (a2-1)
             ; let bstr1 = formatRealFloat FFFixed (Just 2) (b1-1)
             ; let bstr2 = formatRealFloat FFFixed (Just 2) (b2-1)
             ; putStrLn "\\ifonlylen%"
             ; putStrLn (astr1 ++ "%")
             ; putStrLn "\\else%"
             ; putStrLn "\\begin{tabular}{rll}"
             ; putStrLn " & Generated & Variation \\\\\\midrule"
             ; putStrLn $ " Steps & " ++
                              astr1 ++ " & " ++ astr2 ++ "\\\\\\midrule"
             -- ; putStrLn $ " Avg. exec. steps (low-halt) & " ++
             --                  bstr1 ++ " & " ++ bstr2 ++ "\\\\"
                              
             ; let ls = filter (\(l,n) -> n > 0) $
                        sortBy (\(l,n) (l',n') -> compare n' n) (labels r)
             ; mapM_ (\(l,n) ->
                         let (l1::String,l2::String) = read l
                         in printf "%d\\%% & %s & %s \\\\\n" n l1 l2) ls
             ; putStrLn "\\end{tabular}"
             ; putStrLn "\\fi%"
             }
      
      prop = gen_prop_noninterference observe comp
      observe ass
          | l <- last ass
          , isHaltingAS l = (ass, Just l)
          | otherwise = (ass, Nothing)
      comp (ass1,m1) (ass2,m2)
          = collect (show_wf_nicely $ wf (last ass1),
                        show_wf_nicely $ wf (last ass2)) $
            record2 (length ass1, length ass2) $ -- Record steps
            compare' (length ass1) (length ass2) m1 m2
      compare' l1 l2 (Just as1) (Just as2)
        = record2' (l1,l2) True -- Record low-halting steps
      compare' _ _ _ _ = True
  
      show_wf_nicely WF = "well-formed"
      show_wf_nicely (IF excuse) = excuse


checkProperty :: Flaggy DynFlags => IORef Int -> PropTest -> Integer -> IO (Either Int Result,Integer)
-- Returns used time in microseconds and either number of tests run (until timeout) or a result
checkProperty discard_ref pr microsecs
 = let prop = case pr of
                
                PropSynopsisNonInterference
                  -> propertyF prop_semantic_noninterference
                PropLLNI
                  -> propertyF prop_low_lockstep
                PropSSNI
                  -> propertyF prop_single_step
                PropEENI
                  -> propertyF prop_end_to_end
                PropEENInoLow
                  -> propertyF $ prop_end_to_end_aux True
                                
                PropJustProfile
                  -> error "Not a checkable property!"
                PropJustProfileVariation
                  -> error "Not a checkable property!"
   in do { gen <- newQCGen
         ; let is_chatty = show_counterexamples getFlags
               is_latex  = latex_output getFlags
         ; when is_chatty $ do
             when is_latex $ putStr "% Generator: "
             print gen
         ; tests_run <- newIORef 0 
         ; start <- getCPUTime
                    
         ; r <- timeout' microsecs $
           -- withTimeout $
                quickCheckWithResult
                   stdArgs { maxSuccess      = max_tests         getFlags
                           , maxDiscardRatio = max_discard_ratio getFlags
                           , replay          = Just (gen,42)
                           , chatty          = is_chatty && not is_latex }
                                               -- CH: this also hides exceptions,
                                               --     which is not so good
                      (QCProp.callback (discard_cb tests_run) prop)

         ; aggregateResults -- DV: What is this supposed to do here? 

         ; end <- getCPUTime
                       
         ; when is_chatty $
                case r of
                  Nothing              -> putStrLn "Timeout"
                  Just (Success {})    -> putStrLn "Success"
                  Just (GaveUp {})     -> putStrLn "Gave up"
                  Just (NoExpectedFailure {}) -> putStrLn "No expected failure"
                  Just (Failure { reason = r }) | "timeout" `isInfixOf` r -> 
                       putStrLn "Timeout (caught by QC)" 
                  Just (Failure { numTests = nt, numShrinks = ns })
                      -> do when is_latex $ putStr "% "
                            putStrLn $ "*** Falsifiable, numTests = " ++ show nt ++
                                     ", numShrinks = " ++ show ns
         ; let diff = round ((fromIntegral (end - start) / 10^6) :: Double)
         ; real_tests_run <- readIORef tests_run
         ; let res = case r of
                 Nothing -> Left real_tests_run -- if the exception propagated up
                 Just (Failure { reason = r})
                   | "timeout" `isInfixOf` r -> Left real_tests_run -- if the exception caught by QC 
                 Just r  -> Right r
         ; return (res, diff)
         }
 where propertyF x = (if shrink_nothing getFlags then QCProp.noShrinking else id) $ property x
                                    
       discard_cb tests_run
         = QCProp.PostTest
                     QCProp.NotCounterexample $ \MkState{numDiscardedTests = dn, numSuccessTests= sn} _ ->
                     modifyIORef discard_ref (const dn) >> modifyIORef tests_run (const sn)

data TestCounters 
  = TestCounters { run_c            :: Int
                 , bugs_c           :: Int
                 , disc_c           :: Int
                 , times_c          :: [Integer]
                     -- Time spent on each bug find.
                 , extrapolated     :: Either () (Int,Int,Int)
                     -- Right (run,bugs,disc) if max bugs were hit before
                     -- timeout so the results run_c, bugs_c, disc_c are
                     -- approximate. For debuggin purposes, inside the
                     -- triple we now store the accurate (run,bugs,discards)
                 }
  deriving Show


checkTimeoutProperty :: Flaggy DynFlags => IO TestCounters
-- Returns discards and TestCounters
checkTimeoutProperty
  = do { disc_ref <- newIORef 0  -- discards per loop iteration
       ; check_prop_loop disc_ref init_counters to_microsecs }
  where init_counters = TestCounters { run_c  = 0
                                     , bugs_c = 0
                                     , disc_c = 0
                                     , times_c = []
                                     , extrapolated = Left () }
        to_microsecs = toInteger (timeout getFlags) * 10^6

        extrapolate total_count left_over counters
          = let bump_ratio :: Double = fromIntegral total_count / fromIntegral (total_count - left_over)
            in counters { run_c  = round $ fromIntegral (run_c counters)  * bump_ratio
                        , bugs_c = round $ fromIntegral (bugs_c counters) * bump_ratio
                        , disc_c = round $ fromIntegral (disc_c counters) * bump_ratio
                        , extrapolated = Right (run_c counters, 
                                                  bugs_c counters, 
                                                    disc_c counters) }

        check_prop_loop :: IORef Int -> TestCounters -> Integer -> IO TestCounters
        check_prop_loop disc_ref counters microsecs
          | bugs_c counters >= getMaxBugs getFlags
          -- Here we can extrapolate, since we have reached maximum number of bugs before the timeout expired
          -- We should experiment with commenting this case out too ... 
          = return (extrapolate to_microsecs microsecs counters)
          | microsecs <= 0 
          = return counters
          | otherwise
          = do { writeIORef disc_ref 0
               ; (r,used_microsecs) <- checkProperty disc_ref (prop_test getFlags) microsecs
               ; real_discards <- readIORef disc_ref
               ; case r of
                    Left numTests -- Timeout while having run successfully numTests
                       -> return (counters { run_c  = run_c counters + numTests
                                           , disc_c = disc_c counters + real_discards })
                    Right (Failure { numTests, reason })
                       | "Exception" `isInfixOf` reason
                       -> putStrLn ("Exception while testing!?: " ++ reason) >> error "Bailing out!"
                       | "Falsifiable" `isInfixOf` reason -- Bug found
                       -> let counters' = counters { run_c = run_c counters + numTests
                                                   , bugs_c = bugs_c counters + 1 
                                                   , disc_c = disc_c counters + real_discards
                                                   , times_c = used_microsecs : times_c counters }
                          in check_prop_loop disc_ref counters' (microsecs - used_microsecs)
                       | otherwise 
                       -> putStrLn ("Unknown failure while testing!?: " ++ reason) >> error "Bailing out!"
                    Right (Success { numTests }) -- AAA: This shouldn't happen if the maximum number of
                                                 -- tests is sufficiently large, and it is, for the default values.
                       -> let counters' = counters { run_c = run_c counters + numTests 
                                                   , disc_c = disc_c counters + real_discards }
                          in check_prop_loop disc_ref counters' (microsecs - used_microsecs)
                    Right (GaveUp {}) -> putStrLn "GaveUp!?" >> error "Bailing out!"
                    Right (NoExpectedFailure {}) -> putStrLn "NoExpectedFailure!?" >> error "Bailing out"
               }



