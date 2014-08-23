{-# LANGUAGE RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}
module Driver where

import Test.QuickCheck
import qualified Test.QuickCheck.Property as QCProp
import Test.QuickCheck.Random
import qualified Test.QuickCheck.State as St

import Primitives
import SingleStateArb()
import Shrinking
import SanityChecks
import Generation
import SSNI
import LLNI
import MSNI
import Rules
import Mutate
import Flags

import Text.Printf

import System.CPUTime
import Timeout (timeout')
import Data.IORef

import System.Console.CmdArgs

import Data.List
import Data.Maybe

import Control.Monad

foundBug :: Result -> Bool
foundBug Failure{} = True
foundBug _         = False

message :: Bool -> Int -> Int -> String
message kill n1 n2 = 
    (if kill then "Killed" else "Missed") ++ 
    " mutant " ++ (if kill then "" else "[") ++ show n2
               ++ (if kill then "" else "]") 
    ++ " (" ++ show n1 ++ " frags)"

checkMutants :: Flags -> IO ()
checkMutants flags = do 
  let mutants = mutateTable defaultTable
  putStrLn $ "Fighting " ++ show (length mutants) ++ " mutants"
  _ <- foldM (\(n1,n2) t -> do
           res <- quickCheckWithResult stdArgs{maxSuccess = 100000, chatty = False} 
                  $ mkProperty flags t
           let n1' = n1 + (if foundBug res then 1 else 0)
               msg = message (foundBug res) n1' n2
           putStrLn msg
           return (n1', n2+1)
        ) (0,0) mutants
  return ()

mkProperty :: Flags -> RuleTable -> Property
mkProperty f@Flags{..} t = 
    let shrinkF = if doShrink then shrinkV else const [] in
    case testProp of
      TestSSNI -> 
          forAllShrink (genVariationState f) shrinkF $ \v@(Var _ st _) -> 
              propSSNI f t v .&&. propPreservesWellFormed noSteps t st
      TestLLNI -> 
          forAllShrink (genVariationState f) shrinkF $ \v@(Var _ st _) ->
              propLLNI f t v .&&. propPreservesWellFormed noSteps t st
      TestMSNI -> 
          forAllShrink (genVariationState f) shrinkF $ \v@(Var _ st _) ->
              propMSNI f t v .&&. propPreservesWellFormed noSteps t st

quickCheckN :: Int -> Property -> IO ()
quickCheckN n = quickCheckWith stdArgs{maxSuccess = n}

checkProperty :: Flags -> IORef Int -> RuleTable -> Integer -> IO (Either Int Result,Integer)
-- Returns used time in microseconds and either number of tests run (until timeout) or a result
checkProperty flags discardRef table microsecs = do
    let prop = mkProperty flags table 
        isChatty = showCounters flags
        isLatex  = printLatex   flags
    -- gen <- newQCGen 
    testsRun <- newIORef 0 
    start <- getCPUTime 
    r <- timeout' microsecs $ 
           quickCheckWithResult 
                    stdArgs { maxSuccess      = maxTests     flags
                            , maxDiscardRatio = discardRatio flags
                            -- LL: We're choosing a seed???? 
                            -- , replay          = Just (gen,42) 
                            , chatty          = isChatty && not isLatex }
                    (QCProp.callback (discardCb testsRun) prop)
    end <- getCPUTime
{- LL : TODO: put back printing when latexification is done
         ; when is_chatty $
                case r of
                  Nothing              -> putStrLn "Timeout"
                  Just (Success {})    -> putStrLn "Success"
                  Just (GaveUp {})     -> putStrLn "Gave up"
                  Just (NoExpectedFailure {}) -> putStrLn "No expected failure"
                  Just (Failure { reason = res }) | "timeout" `isInfixOf` res -> 
                       putStrLn "Timeout (caught by QC)" 
                  Just (Failure { numTests = nt, numShrinks = ns })
                      -> do when is_latex $ putStr "% "
                            putStrLn $ "*** Falsifiable, numTests = " ++ show nt ++
                                     ", numShrinks = " ++ show ns
-}
    let diff = round ((fromIntegral (end - start) / 10^6) :: Double)
    realTestsRun <- readIORef testsRun
    let res = case r of
                Nothing -> Left realTestsRun -- if the exception propagated up
                Just (Failure { reason = res}) | "timeout" `isInfixOf` res -> 
                  Left realTestsRun -- if the exception caught by QC 
                Just r  -> Right r
    return (res, diff)

    where discardCb testsRun = 
              QCProp.PostTest QCProp.NotCounterexample $ \s _ -> do
                modifyIORef discardRef (const $ St.numDiscardedTests s)
                modifyIORef testsRun   (const $ St.numSuccessTests   s)

data TestCounters 
  = TestCounters { run_c            :: !Int
                 , bugs_c           :: !Int
                 , disc_c           :: !Int
                 , times_c          :: ![Integer]
                     -- Time spent on each bug find.
                 , extrapolated     :: Either () (Int,Int,Int)
                     -- Right (run,bugs,disc) if max bugs were hit before
                     -- timeout so the results run_c, bugs_c, disc_c are
                     -- approximate. For debugging purposes, inside the
                     -- triple we now store the accurate (run,bugs,discards)
                 }
  deriving Show

checkTimeoutProperty :: Flags -> RuleTable -> IO TestCounters
-- Returns discards and TestCounters
checkTimeoutProperty flags table = do
  discRef <- newIORef 0
  checkPropLoop discRef initCounters toMicrosecs

  where initCounters = TestCounters { run_c  = 0
                                     , bugs_c = 0
                                     , disc_c = 0
                                     , times_c = []
                                     , extrapolated = Left () }
        toMicrosecs = toInteger (timeout flags) * 10^6

        extrapolate totalCount leftOver counters =
          let bumpRatio :: Double = fromIntegral totalCount / fromIntegral (totalCount - leftOver)
          in counters { run_c  = 
                          round $ fromIntegral (run_c counters) * bumpRatio
                      , bugs_c = 
                          round $ fromIntegral (bugs_c counters) * bumpRatio
                      , disc_c = 
                          round $ fromIntegral (disc_c counters) * bumpRatio
                      , extrapolated = Right (run_c counters, 
                                              bugs_c counters, 
                                              disc_c counters) }

        checkPropLoop :: IORef Int -> TestCounters -> Integer -> IO TestCounters
        checkPropLoop discRef counters microsecs
--          | bugs_c counters >= maxBugs flags
          -- Here we can extrapolate, since we have reached maximum number of
          -- bugs before the timeout expired 
          -- We should experiment with commenting this case out too ...
--          = return (extrapolate to_microsecs microsecs counters)
          | microsecs <= 0 = return counters
          | otherwise = do 
              writeIORef discRef 0
              (r, usedMicrosecs) <- checkProperty flags discRef table microsecs
              realDiscards <- readIORef discRef
              case r of
                Left numTests -> -- Timeout while having run successfully numTests
                  return $ counters { run_c  = run_c  counters + numTests
                                    , disc_c = disc_c counters + realDiscards }
                Right (Failure { numTests, reason })
                    | "Exception" `isInfixOf` reason -> do
                       putStrLn ("Exception while testing!?: " ++ reason) 
                       error "Bailing out!"
                    | "Falsifiable" `isInfixOf` reason -> -- Bug found
                       let counters' = counters {
                             run_c  = run_c counters + numTests,
                             bugs_c = bugs_c counters + 1,
                             disc_c = disc_c counters + realDiscards,
                             times_c = usedMicrosecs : times_c counters }
                       in counters' `seq` checkPropLoop discRef counters' 
                              $ microsecs - usedMicrosecs
                    | otherwise -> do
                       putStrLn ("Unknown failure while testing!?: " ++ reason) 
                       error "Bailing out!"
                -- AAA: This shouldn't happen if the maximum number of
                -- tests is sufficiently large, and it is, for the default values.
                Right (Success { numTests }) ->
                    let counters' = counters { run_c  = run_c  counters + numTests 
                               , disc_c = disc_c counters + realDiscards }
                    in counters' `seq` checkPropLoop discRef counters' (microsecs - usedMicrosecs)
                Right (GaveUp {}) -> 
                    putStrLn "GaveUp!?" >> error "Bailing out!"
                Right (NoExpectedFailure {}) -> 
                    putStrLn "NoExpectedFailure!?" >> error "Bailing out"

means :: [Maybe Rational] -> (Maybe Double, Maybe Double, Maybe Double)
means [] = (Just 0,Just 0,Nothing)
means xs =
    let double                        = fromRational :: Rational -> Double
        fins                          = catMaybes xs
        n                             = genericLength fins
        isInfinity                    = elem Nothing xs
        arithmetic | isInfinity       = Nothing
                   | n == 0           = Nothing
                   | otherwise        = Just $ double $ sum fins / n
        geometric  | isInfinity       = Nothing
                   | n == 0           = Nothing
                   | otherwise        = Just $ double (product fins) ** double (recip n)
        harmonic   | 0 `elem` fins    = Just 0 -- See Note [Harmonic Mean]
                   | n == 0           = Nothing
                   | otherwise        = Just $ double $ n / sum (map recip fins)
    in (arithmetic, harmonic, geometric)

computeMTTF :: TestCounters -> Maybe Rational
computeMTTF cs = 
  if null $ times_c cs then Nothing
  else Just $ sum (map fromIntegral $ times_c cs) 
           / (fromIntegral (bugs_c cs) * 1000)

printMR :: Maybe Rational -> String
printMR Nothing = "----"
printMR (Just x) = printf "%0.2f" (fromRational x :: Double)

printMD :: Maybe Double -> String
printMD Nothing = "----"
printMD (Just x) = printf "%0.2f" x 

statsForTable :: Flags -> IO ()
statsForTable flags = do
    putStrLn "\\begin{tabular}{ c c c c c c c}"
    putStrLn "INSTR & SSNI (naive) & SSNI & LLNI (naive) & LLNI & MSNI (naive) & MSNI \\\\ "
    times <- liftM transpose $ statsForTableAux flags $ mutateTable defaultTable
    let ms = map means times
    putStr " AM & " 
    putStr $ concat $ intersperse " & " $ map (\(x,_,_) -> printMD x) ms
    putStrLn " \\\\"
    putStr " GM & " 
    putStr $ concat $ intersperse " & " $ map (\(_,x,_) -> printMD x) ms
    putStrLn " \\\\"
    putStrLn "\\end{tabular}"
    

statsForTableAux :: Flags -> [RuleTable] -> IO [[Maybe Rational]]
statsForTableAux f [] = return []
statsForTableAux f (table:ts) = do
    naiveSsniCounters <- checkTimeoutProperty (naiveSsniConfig f) table
    let naiveSsniStats = computeMTTF naiveSsniCounters
    ssniCounters <- checkTimeoutProperty (ssniConfig f) table
    let ssniStats = computeMTTF ssniCounters
    naiveLlniCounters <- checkTimeoutProperty (naiveLlniConfig f) table
    let naiveLlniStats = computeMTTF naiveLlniCounters
    llniCounters <- checkTimeoutProperty (llniConfig f) table
    let llniStats = computeMTTF llniCounters
    msniCounters <- checkTimeoutProperty (msniConfig f) table
    let msniStats = computeMTTF msniCounters
    naiveMsniCounters <- checkTimeoutProperty (naiveMsniConfig f) table
    let naiveMsniStats = computeMTTF naiveMsniCounters
  -- TODO: Figure out a way to add numbering in the mutatant table thingy
    putStrLn $ showMutantTable table ++ " & "  
             ++ printMR naiveSsniStats ++ " & " 
             ++ printMR ssniStats ++ " & " 
             ++ printMR naiveLlniStats ++ " & " 
             ++ printMR llniStats ++ " & " 
             ++ printMR naiveMsniStats ++ " & "
             ++ printMR msniStats ++ " \\\\ "
    liftM ([naiveSsniStats, ssniStats, naiveLlniStats, llniStats, msniStats, naiveMsniStats]:)
           (statsForTableAux f ts)

testSingle :: Flags -> IO ()
testSingle f@Flags{..} =
  let table = case mutantNo of
                Nothing -> defaultTable
                Just n  -> mutateTable defaultTable !! n in 
  quickCheckWith
    stdArgs { maxSuccess      = maxTests     
            , maxDiscardRatio = discardRatio 
            , chatty          = showCounters && not printLatex }
    $ mkProperty f table

main :: IO ()
main = do
  flags <- cmdArgs defaultFlags
  case mode flags of 
    ModePrintTable -> statsForTable flags
    ModeQuickCheck -> testSingle flags
--    putStrLn "Checking defaultTable: SSNI"
--    quickCheckN 10000 $ mkProperty ssniConfig defaultTable
--    putStrLn "Checking defaultTable: LLNI"
--    cs <- checkTimeoutProperty llniConfig defaultTable
--    putStrLn $ show cs
--    quickCheckN 10000 $ mkProperty llniConfig defaultTable
  -- putStrLn "Checking Mutants with SSNI"
  -- checkMutants (ssniConfig flags)
  -- putStrLn "Checking Mutants with LLNI"
  -- checkMutants (llniConfig flags)
