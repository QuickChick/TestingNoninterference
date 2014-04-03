{-# LANGUAGE ImplicitParams, FlexibleContexts, UndecidableInstances,
    RecordWildCards, TupleSections, ScopedTypeVariables, NamedFieldPuns #-}

module Driver where

import Test.QuickCheck
import Test.QuickCheck.Random

import Control.Monad
import Control.Applicative
import Control.Arrow()
import Data.List (genericLength, unzip4)

import System.Console.CmdArgs
import Text.Printf

import Data.Maybe

import Data.IORef

import Flags
import Machine
import Generation () -- Import just Arbitrary
import ObservableInst ()

import System.Time
import System.Exit


import DriverUtils


-- We make the context be an implicit parameter to be
-- able to pick which flags to use per-run, as a value.
-- This allows us to QC with random configurations too.
-- It's a bit naughty (not allowed in GHC < 7.4) but
-- convenient.
instance (?dfs :: DynFlags) => Flaggy TMUDriver where
  getFlags = ?dfs


show_some_testcases :: Int -> IO ()
show_some_testcases n
  = do { flags <- cmdArgs (dynFlagsDflt)
       ; let ?dfs = flags
       ; gen <- newQCGen
       ; when (latex_output getFlags) (putStr "% ") >> print gen
       
       ; quickCheckWith stdArgs { maxSuccess = n
                                , replay = Just (gen, 42)
                                , chatty = True } $ \(as :: AS) ->
         whenFail (return ()) $
         collect (show as) $
         property True }



main :: IO ()
main = do { flags <- cmdArgs dynFlagsDflt
          ; success <- and <$> mapM (do_strategy flags) (strategies flags)
          ; if success then exitSuccess else exitFailure }
  where means :: [Maybe Rational] -> (Maybe Double, Maybe Double, Maybe Double)
        means [] = (Just 0,Just 0,Nothing)
        means xs =
          let double                        = fromRational :: Rational -> Double
              fins                          = catMaybes xs
              n                             = genericLength fins
              isInfinity                    = any (== Nothing) xs
              arithmetic | isInfinity       = Nothing
                         | n == 0           = Nothing
                         | otherwise        = Just $ double $ sum fins / n
              geometric  | isInfinity       = Nothing
                         | n == 0           = Nothing
                         | otherwise        = Just $ double (product fins) ** double (recip n)
              harmonic   | 0 `elem` fins    = Just 0 -- See Note [Harmonic Mean]
                         | n == 0           = Nothing
                         | otherwise        = Just $ double $ n / sum (map recip fins)
          in (arithmetic, geometric, harmonic)
          -- Note [Harmonic Mean]
          -- Informally, 1/(1/0 + …) ≈ 1/(∞ + …) ≈ 1/∞ ≈ 0.  Also, this
          -- satisfies the AM ≥ GM ≥ HM inequality.  And it concords with this
          -- online Q&A: http://stats.stackexchange.com/q/37628. Also, if any mean
          -- time to failure is infinity, we just have to discard it.
             
        do_strategy f (s :: GenStrategy)
          = do when (run_timeout_tests f) $
                 if print_all_datapoints f then
                   putStrLn "% Format = time to find bug in milli seconds."
                 else
                   putStrLn "% Format = bug & #tests/sec & discard ratio & mean time to failure"
               (success',speeds,discRates,allBugsPerSecs) <-
                 unzip4 <$>
                 if run_timeout_tests f 
                 then mapM (do_ifc (f { gen_strategy = s })) (ifcsem f)
                 else (:[]) <$> do_ifc (f { gen_strategy = s}) undefined
               let success = and success'
                   avSpeed = if null speeds then 0
                             else sum speeds / genericLength speeds
                   avDiscRate = if null discRates then 0
                                else sum discRates / genericLength discRates
               
               when (run_timeout_tests f) $ do
                 case means allBugsPerSecs of
                   (a,g,h) ->
                     let p Nothing = "---"
                         p (Just d) = printf "%.2f" d in
                     when not_profiling $
                     void $ printf "\\means{}{%s}{%s}{%s}\n" (p a) (p g) (p h)
                 timestamp <- getClockTime
                 when not_profiling $ do
                   void $ printf "\\extrabuginfo{%s}{\\%s}{%s}{%s}{\\%s}{%s}{%s}{%d}{%s}\n"
                            (show (gen_instrs f))
                            (show (prop_test f))
                            (show (equiv f))
                            (show (starting_as f))
                            (show s)
                            (show (smart_ints f))
                            (show_timeout (timeout f))
                            (getMaxBugs f)
                            (show timestamp)
                   void $ printf "\\averagespeed{%0.0f}\n" avSpeed
                   void $ printf "\\averagediscrate{%0.0f\\%%}\n" avDiscRate
               return success
          where not_profiling | PropJustProfile <- prop_test f
                              = False
                              | PropJustProfileVariation <- prop_test f
                              = False
                              | otherwise = True

        show_timeout i = show i ++"sec"
        
        do_ifc f b
          | let ?dfs = f
          , PropJustProfile <- prop_test f
          = profileTests >> return (True,0,0,Nothing)
          | let ?dfs = f
          , PropJustProfileVariation <- prop_test f
          = profileVariations >> return (True,0,0,Nothing)

          | not $ run_timeout_tests f
          = do let f' = f {ifc_semantics_singleton = readIfcSemanticsList f}
               let ?dfs = f'
               putStrLn $ "% Flags: "++show f'
               ior <- newIORef 0
               (r,_) <- checkProperty ior (prop_test f') $ 365*24*60*60*10^6 
               pure . (,0,0,Nothing) $ case r of
                 Right (Success{})  -> True
                 Right (GaveUp{..}) -> numTests > 0
                 _          -> False
          
          | otherwise 
          = let bugs_per_sec c =
                  let (s::String) = printf "%0.3f" ( fromIntegral (bugs_c c)
                                                   / fromIntegral (timeout f) :: Double)
                  in s ++ (extrap_info (extrapolated c))
                     
                -- Compute MTTF in ms
                mean_time_to_failure_stats :: TestCounters -> (Maybe Rational, Maybe Rational)
                mean_time_to_failure_stats c
                  | null $ times_c c = (Nothing, Nothing)
                  | otherwise = 
                    let mttf = sum (map fromIntegral $ times_c c) 
                               / (fromIntegral (bugs_c c) * 1000)
                        quadDev t = (mttf - fromIntegral t / 1000) ^ 2
                        var | bugs_c c < 2 = Nothing
                            | otherwise = Just $ sum (map quadDev $ times_c c)
                                          / (fromIntegral (bugs_c c) - 1) in
                    (Just mttf, var)
                                
                extrap_info (Left ()) = " (A)"
                extrap_info (Right (_r,_b,_d)) = " (E)"
                     -- = printf " (E, b = %d, r=%d, d=%d)" b r d

                show_gtz 0 = "0"  -- change to "" if you want to suppress 0
                show_gtz i = show i
            in
            do { counters <- action (f{ifc_semantics_singleton = [b]})
               ; let gen_speed :: Double = 
                        (fromIntegral (run_c counters + disc_c counters) :: Double)
                            / (fromIntegral (timeout f) :: Double)
               ; let disc_rate :: Double = 100.00 * 
                           (fromIntegral (disc_c counters) / 
                              (fromIntegral (run_c counters + disc_c counters):: Double))
               ; let (mttf, var) = mean_time_to_failure_stats counters
               ; let mttfStr = case mttf of
                                 Just mean -> 
                                   printf "%0.2f" (fromRational mean :: Double)
                                   ++ extrap_info (extrapolated counters)
                                 Nothing -> "---"
               ; let varStr = case var of
                                Just var -> 
                                   printf "%0.2f" (fromRational var :: Double)
                                Nothing -> "---"
               ; let acc95Str = case var of
                                  Just var ->
                                    -- We are here assuming that the mean of the data points has
                                    -- a normal distribution with variance given by the expression 
                                    -- below, which is not very accurate when only few bugs were found.
                                    -- Assuming that this approximation is valid, the resulting number
                                    -- v yields a 95% confidence interval of the form 
                                    -- [m-1.96sqrt(v),m+1.96sqrt(v)]
                                    let varAvg = fromRational var / fromIntegral (bugs_c counters) :: Double in
                                    printf "%0.2f" (1.96 * (sqrt varAvg))
                                  Nothing -> "---"
               ; if print_all_datapoints f then do
                   putStrLn ("% " ++ show b)
                   mapM_ (print . (/ 1000) . fromIntegral) $ times_c counters
                 else
                   void $ printf "\\row{\\%s}{%s}{%s}{%s}{%d}{%s}{%s} \n"
                               (show b)
                               (printf "%0.0f" gen_speed :: String)
                               (printf "%0.0f\\%%" disc_rate :: String)
                               mttfStr
                               (bugs_c counters)
                               varStr
                               acc95Str
               ; return $ (True, gen_speed,
                                 disc_rate,
                                 mttf)
               }
            
        strategies flags = [gen_strategy flags]
        ifcsem flags     = readIfcSemanticsList flags

        action :: DynFlags -> IO TestCounters
        action flags = let ?dfs = flags in checkTimeoutProperty
